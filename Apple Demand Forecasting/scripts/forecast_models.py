"""
forecast_models.py
==================
Forecasting models for Apple's quarterly segment and product revenue.

The module provides a `BaseForecaster` interface and six concrete
implementations spanning naive baselines, classical decomposition, and ML.
Plus an `EnsembleForecaster` that blends them via weights optimized on
walk-forward backtests.

Why not just use Prophet / SARIMA?
    With ~9 observations per series, Prophet and SARIMA are statistically
    fragile — those libraries assume enough history to estimate seasonal
    components and trend changepoints reliably. Hand-rolled models on
    explicit features are more honest at this sample size and easier
    to explain in a memo to leadership.

Public API:
    BaseForecaster                — abstract base
    NaiveLastForecaster           — last-value carry-forward
    SeasonalNaiveForecaster       — same quarter, prior year
    CAGRForecaster                — geometric trend extrapolation
    HoltWintersForecaster         — additive triple exponential smoothing
    RidgeForecaster               — Ridge regression on lag+seasonal features
    GBMForecaster                 — sklearn GradientBoostingRegressor
    EnsembleForecaster            — MAPE-optimized weighted blend
    walk_forward_backtest(...)    — utility that returns per-model errors
"""
from __future__ import annotations

from abc import ABC, abstractmethod
from dataclasses import dataclass

import numpy as np
import pandas as pd
from sklearn.ensemble import GradientBoostingRegressor
from sklearn.linear_model import Ridge
from sklearn.preprocessing import StandardScaler


# =============================================================================
# Base interface
# =============================================================================
class BaseForecaster(ABC):
    """All forecasters expose fit(y) → predict(steps) and a `name` property."""
    @property
    @abstractmethod
    def name(self) -> str: ...

    @abstractmethod
    def fit(self, y: np.ndarray) -> "BaseForecaster": ...

    @abstractmethod
    def predict(self, steps: int = 1) -> np.ndarray: ...

    def fit_predict(self, y: np.ndarray, steps: int = 1) -> np.ndarray:
        return self.fit(y).predict(steps)


# =============================================================================
# Naive baselines — the floor every other model must beat
# =============================================================================
class NaiveLastForecaster(BaseForecaster):
    """Predict the last observed value forever."""
    name = "naive_last"
    def fit(self, y):                       # noqa: D401
        self._y_last = float(y[-1])
        return self
    def predict(self, steps: int = 1):
        return np.full(steps, self._y_last)


class SeasonalNaiveForecaster(BaseForecaster):
    """
    Predict from the same quarter last year — usually the strongest naive
    baseline for highly seasonal series. Apple's Q1 holiday spike means
    last-quarter is misleading; same-quarter-last-year is far better.
    """
    name = "seasonal_naive"
    def __init__(self, season_length: int = 4):
        self.season_length = season_length
    def fit(self, y):
        if len(y) < self.season_length:
            raise ValueError(
                f"SeasonalNaive needs at least {self.season_length} obs, got {len(y)}"
            )
        self._y = np.asarray(y, dtype=float)
        return self
    def predict(self, steps: int = 1):
        out = np.empty(steps)
        for h in range(steps):
            # For each forecast horizon, look back season_length quarters.
            # If we forecast beyond a season, recurse on previous predictions.
            idx = -self.season_length + h
            if idx < 0:
                out[h] = self._y[idx]
            else:
                out[h] = out[idx]
        return out


class CAGRForecaster(BaseForecaster):
    """
    Geometric trend extrapolation: fit y_t = y_0 * (1+g)^t, project forward.
    Equivalent to a linear regression on log(y) against time index.
    """
    name = "cagr"
    def fit(self, y):
        y = np.asarray(y, dtype=float)
        # Guard against zero/negative values that would break log
        if (y <= 0).any():
            raise ValueError("CAGR requires strictly positive values")
        t = np.arange(len(y))
        # log(y) = a + g*t  →  g = slope of log(y) on t
        slope, intercept = np.polyfit(t, np.log(y), 1)
        self._intercept = intercept
        self._slope     = slope
        self._n         = len(y)
        return self
    def predict(self, steps: int = 1):
        future_t = np.arange(self._n, self._n + steps)
        return np.exp(self._intercept + self._slope * future_t)


# =============================================================================
# Holt-Winters (additive) — manual triple exponential smoothing
# =============================================================================
class HoltWintersForecaster(BaseForecaster):
    """
    Additive triple exponential smoothing with seasonality.

    Decomposes y into level (L), trend (T), and seasonal (S) components and
    updates each with smoothing constants alpha, beta, gamma. Forecasts use
    L + h*T + S[(h-1) mod m].

    Smoothing constants are searched over a small grid since with 9 obs
    we can't afford a full optimization. We pick the (alpha, beta, gamma)
    that minimizes one-step-ahead RMSE on the training data.
    """
    name = "holt_winters"
    def __init__(self, season_length: int = 4):
        self.season_length = season_length
        # Coarse grid is enough — the loss surface is shallow at this sample size
        self._grid = [0.1, 0.3, 0.5, 0.7, 0.9]

    def _initialize(self, y: np.ndarray) -> tuple[float, float, np.ndarray]:
        """Initialize level, trend, seasonal vectors using the classic recipe."""
        m = self.season_length
        # Level: mean of first season
        L0 = float(np.mean(y[:m]))
        # Trend: average of season-over-season increments
        if len(y) >= 2 * m:
            T0 = float(np.mean((y[m:2*m] - y[:m]) / m))
        else:
            T0 = float((y[-1] - y[0]) / max(1, len(y) - 1))
        # Seasonal: deviation of first season from level
        S0 = y[:m] - L0
        return L0, T0, S0

    def _fit_with_params(self, y: np.ndarray, alpha: float, beta: float, gamma: float):
        m = self.season_length
        L, T, S = self._initialize(y)
        S = list(S)  # mutable rolling buffer of length m
        fitted = []
        for t in range(len(y)):
            S_t_minus_m = S[t]            # seasonal component for this period
            forecast    = L + T + S_t_minus_m
            fitted.append(forecast)
            # Update components after observing y[t]
            L_new = alpha * (y[t] - S_t_minus_m) + (1 - alpha) * (L + T)
            T_new = beta  * (L_new - L)         + (1 - beta)  * T
            S_new = gamma * (y[t] - L_new)      + (1 - gamma) * S_t_minus_m
            L, T = L_new, T_new
            S.append(S_new)
        return np.array(fitted), L, T, S

    def fit(self, y):
        y = np.asarray(y, dtype=float)
        if len(y) < 2 * self.season_length:
            # Not enough to estimate seasonality reliably — fall back to seasonal naive
            self._fallback = SeasonalNaiveForecaster(self.season_length).fit(y)
            return self
        self._fallback = None

        best = None
        for a in self._grid:
            for b in self._grid:
                for g in self._grid:
                    fitted, L, T, S = self._fit_with_params(y, a, b, g)
                    rmse = float(np.sqrt(np.mean((fitted - y) ** 2)))
                    if best is None or rmse < best[0]:
                        best = (rmse, a, b, g, L, T, S)

        self._rmse, self._alpha, self._beta, self._gamma, self._L, self._T, self._S = best
        self._n = len(y)
        return self

    def predict(self, steps: int = 1):
        if self._fallback is not None:
            return self._fallback.predict(steps)
        m = self.season_length
        # Final seasonal component for forecast: cycle through the last m values of S
        S_final = self._S[-m:]
        out = np.empty(steps)
        for h in range(steps):
            out[h] = self._L + (h + 1) * self._T + S_final[h % m]
        return out


# =============================================================================
# ML models — sklearn-based, run on lag and seasonal features
# =============================================================================
@dataclass
class _MLFeatures:
    """Engineered feature matrix for tree/linear models."""
    X: np.ndarray
    y: np.ndarray
    feature_names: list[str]
    last_lags: np.ndarray   # the most recent lag values (for next-step prediction)


def _build_features(y: np.ndarray, season_length: int = 4) -> _MLFeatures:
    """
    Build a supervised-learning matrix from a univariate series:
        target_t = f(y_{t-1}, y_{t-2}, y_{t-season}, quarter_idx, time_idx, rolling_mean_lag1)
    """
    y = np.asarray(y, dtype=float)
    n = len(y)
    rows = []
    targets = []
    feature_names = ["lag_1", "lag_2", "lag_4",
                     "is_q1", "is_q2", "is_q3", "is_q4",
                     "time_idx", "roll_mean_4_lag1"]

    # Need at least season_length + 1 history points to construct one row
    start = season_length
    for t in range(start, n):
        # Lag features
        lag1 = y[t - 1]
        lag2 = y[t - 2]
        lag4 = y[t - season_length]
        # Seasonal one-hot — quarter index modulo 4
        # We assume the series starts at Q1 (index 0 → Q1, 1 → Q2, etc.)
        q = t % season_length
        seas = [int(q == i) for i in range(season_length)]
        # Rolling 4-quarter mean of the lag-1-onwards window
        roll = float(np.mean(y[max(0, t - season_length):t]))
        rows.append([lag1, lag2, lag4, *seas, t, roll])
        targets.append(y[t])

    X = np.array(rows, dtype=float)
    y_target = np.array(targets, dtype=float)

    # Build the feature row for the *next* unseen observation (t = n)
    next_lag1 = y[-1]
    next_lag2 = y[-2]
    next_lag4 = y[-season_length]
    next_q = n % season_length
    next_seas = [int(next_q == i) for i in range(season_length)]
    next_roll = float(np.mean(y[-season_length:]))
    last_lags = np.array([next_lag1, next_lag2, next_lag4, *next_seas, n, next_roll])

    return _MLFeatures(X=X, y=y_target, feature_names=feature_names, last_lags=last_lags)


class _MLForecaster(BaseForecaster):
    """Shared logic for ML-style forecasters that use the lag-feature matrix."""
    def __init__(self, season_length: int = 4):
        self.season_length = season_length

    def _make_model(self):    # subclasses override
        raise NotImplementedError

    def fit(self, y):
        feats = _build_features(np.asarray(y, dtype=float), self.season_length)
        if len(feats.y) < 3:
            # Too few rows to fit anything useful → fall back to seasonal naive
            self._fallback = SeasonalNaiveForecaster(self.season_length).fit(y)
            return self
        self._fallback = None

        self._scaler = StandardScaler()
        X_scaled = self._scaler.fit_transform(feats.X)
        self._model = self._make_model()
        self._model.fit(X_scaled, feats.y)
        self._y_history = np.asarray(y, dtype=float).copy()
        return self

    def predict(self, steps: int = 1):
        if self._fallback is not None:
            return self._fallback.predict(steps)
        # Recursive multi-step: predict, append, rebuild features, repeat
        history = self._y_history.copy()
        out = np.empty(steps)
        for h in range(steps):
            feats = _build_features(history, self.season_length)
            x = self._scaler.transform(feats.last_lags.reshape(1, -1))
            yhat = float(self._model.predict(x)[0])
            out[h] = yhat
            history = np.concatenate([history, [yhat]])
        return out


class RidgeForecaster(_MLForecaster):
    """L2-regularized linear regression on lag + seasonal features."""
    name = "ridge"
    def __init__(self, season_length: int = 4, alpha: float = 1.0):
        super().__init__(season_length)
        self.alpha = alpha
    def _make_model(self):
        return Ridge(alpha=self.alpha, random_state=0)


class GBMForecaster(_MLForecaster):
    """Gradient boosting on lag + seasonal features (sklearn equivalent of XGBoost)."""
    name = "gbm"
    def __init__(self, season_length: int = 4,
                 n_estimators: int = 100, max_depth: int = 3, learning_rate: float = 0.05):
        super().__init__(season_length)
        self.n_estimators  = n_estimators
        self.max_depth     = max_depth
        self.learning_rate = learning_rate
    def _make_model(self):
        return GradientBoostingRegressor(
            n_estimators=self.n_estimators,
            max_depth=self.max_depth,
            learning_rate=self.learning_rate,
            random_state=0,
        )


# =============================================================================
# Walk-forward backtest
# =============================================================================
@dataclass
class BacktestResult:
    model_name:    str
    series_id:     str          # e.g. "Greater China"
    cv_actual:     np.ndarray   # actual values during backtest
    cv_predicted:  np.ndarray   # model predictions
    cv_periods:    list[str]    # period labels (e.g. ['2025-Q1', '2025-Q2', ...])
    mape:          float
    mae:           float
    rmse:          float

    def to_dict(self) -> dict:
        return {
            "model": self.model_name,
            "series": self.series_id,
            "n_predictions": len(self.cv_actual),
            "mape_pct": round(self.mape * 100, 3),
            "mae":      round(float(self.mae), 1),
            "rmse":     round(float(self.rmse), 1),
        }


def walk_forward_backtest(
    y: np.ndarray,
    period_labels: list[str],
    model_factory,
    initial_train: int = 5,
    series_id: str = "",
) -> BacktestResult:
    """
    Run an expanding-window backtest on series y.

    For each step k = initial_train, ..., len(y)-1:
        train on y[:k], predict y[k], record error.

    `model_factory` is a zero-arg callable returning a fresh BaseForecaster.
    """
    y = np.asarray(y, dtype=float)
    if len(y) <= initial_train:
        raise ValueError(f"Need len(y) > initial_train ({initial_train}), got {len(y)}")

    actuals, preds, periods = [], [], []
    for k in range(initial_train, len(y)):
        model = model_factory()
        try:
            model.fit(y[:k])
            yhat = float(model.predict(steps=1)[0])
        except Exception:
            yhat = float("nan")
        actuals.append(float(y[k]))
        preds.append(yhat)
        periods.append(period_labels[k])

    a = np.array(actuals, dtype=float)
    p = np.array(preds,   dtype=float)
    valid = ~np.isnan(p) & (a != 0)
    mape = float(np.mean(np.abs((a[valid] - p[valid]) / a[valid]))) if valid.any() else float("nan")
    mae  = float(np.mean(np.abs(a[valid] - p[valid]))) if valid.any() else float("nan")
    rmse = float(np.sqrt(np.mean((a[valid] - p[valid]) ** 2))) if valid.any() else float("nan")

    name = model_factory().name
    return BacktestResult(
        model_name=name,
        series_id=series_id,
        cv_actual=a, cv_predicted=p, cv_periods=periods,
        mape=mape, mae=mae, rmse=rmse,
    )


# =============================================================================
# Ensemble — weighted blend optimized on backtest MAPE
# =============================================================================
class EnsembleForecaster(BaseForecaster):
    """
    Weighted blend of base models. Weights are inverse-MAPE on the backtest:
    models that backtest well get more weight. Falls back to equal weights
    if MAPE info isn't supplied.
    """
    name = "ensemble"

    def __init__(self, base_factories: list, weights: dict[str, float] | None = None):
        self.base_factories = base_factories
        self.weights        = weights        # name → weight; auto-normalized in fit

    @classmethod
    def from_backtest(cls, base_factories: list, backtest_results: list[BacktestResult]):
        """
        Construct ensemble whose weights are proportional to 1 / MAPE.
        Drops any model with NaN MAPE.
        """
        weights: dict[str, float] = {}
        for r in backtest_results:
            if not np.isnan(r.mape) and r.mape > 0:
                weights[r.model_name] = 1.0 / r.mape
        # Normalize to sum to 1
        if weights:
            total = sum(weights.values())
            weights = {k: v / total for k, v in weights.items()}
        return cls(base_factories=base_factories, weights=weights)

    def fit(self, y):
        self._fitted = []
        for factory in self.base_factories:
            try:
                m = factory().fit(y)
                self._fitted.append(m)
            except Exception:
                # Skip models that can't fit on this series
                pass
        return self

    def predict(self, steps: int = 1):
        preds: list[np.ndarray] = []
        ws:    list[float]      = []
        for m in self._fitted:
            try:
                p = m.predict(steps=steps)
                w = (self.weights or {}).get(m.name, 1.0)  # equal-weight fallback
                preds.append(p)
                ws.append(w)
            except Exception:
                continue
        if not preds:
            raise RuntimeError("All ensemble members failed to predict")
        # Weighted average across models
        P = np.stack(preds)
        W = np.array(ws, dtype=float).reshape(-1, 1)
        W = W / W.sum()
        return (P * W).sum(axis=0)


# =============================================================================
# Convenience: factory list + bootstrap intervals
# =============================================================================
def default_factories(season_length: int = 4) -> list:
    """Return the default list of zero-arg model factories."""
    return [
        lambda: NaiveLastForecaster(),
        lambda: SeasonalNaiveForecaster(season_length),
        lambda: CAGRForecaster(),
        lambda: HoltWintersForecaster(season_length),
        lambda: RidgeForecaster(season_length),
        lambda: GBMForecaster(season_length),
    ]


def bootstrap_intervals(
    backtest: BacktestResult,
    point_forecast: float,
    n_bootstrap: int = 2000,
    quantiles: tuple[float, ...] = (0.1, 0.5, 0.9),
    rng_seed: int = 0,
    bias_correct: bool = True,
    horizon: int = 1,
) -> dict[str, float]:
    """
    Compute prediction intervals by bootstrapping the backtest residuals
    around the point forecast. Returns:
        {'point_corrected': bias-adjusted point estimate,
         'q10':  10th percentile, ..., 'q90': 90th percentile,
         'mean_residual': systematic bias detected in backtest}

    With small samples, parametric intervals (Gaussian, etc.) are unreliable.
    Bootstrap of empirical residuals avoids assuming normality while still
    producing usable bands.

    bias_correct=True (default): if backtest residuals have a non-zero mean
    (indicating systematic over/underprediction), shift the point forecast
    by that mean so it's calibrated to historical bias, then bootstrap on
    demeaned residuals so intervals are symmetric. This is standard practice
    in production forecasting; the raw mean is reported in the output for
    transparency.

    horizon (default 1): scales the bootstrap residuals by sqrt(horizon) to
    widen prediction intervals at longer forecast horizons. This is a
    standard random-walk approximation: the variance of an h-step-ahead
    forecast is approximately h times the 1-step variance under the
    assumption that period-to-period errors are independent. Used here
    because we don't directly backtest at h>1 (sample size won't support
    it cleanly), so we apply the scaling rather than overstate confidence.
    """
    rng = np.random.default_rng(rng_seed)
    residuals = backtest.cv_actual - backtest.cv_predicted
    residuals = residuals[~np.isnan(residuals)]

    if len(residuals) < 2:
        # Not enough residuals — return point forecast for every quantile
        return {
            "point_corrected":  float(point_forecast),
            "mean_residual":    0.0,
            **{f"q{int(q*100)}": float(point_forecast) for q in quantiles},
        }

    mean_resid = float(np.mean(residuals))
    horizon_scale = float(np.sqrt(horizon))
    if bias_correct:
        corrected_point = float(point_forecast) + mean_resid
        demeaned        = (residuals - mean_resid) * horizon_scale
        samples         = corrected_point + rng.choice(demeaned, size=n_bootstrap, replace=True)
    else:
        corrected_point = float(point_forecast)
        scaled          = residuals * horizon_scale
        samples         = corrected_point + rng.choice(scaled, size=n_bootstrap, replace=True)

    return {
        "point_corrected": corrected_point,
        "mean_residual":   mean_resid,
        **{f"q{int(q*100)}": float(np.quantile(samples, q)) for q in quantiles},
    }
