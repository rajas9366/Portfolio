"""
Tests for the forecast module.

Run with:
    pytest tests/test_forecast.py -v

Covers:
  * Each base forecaster fits and predicts without error on representative data
  * Walk-forward backtest produces sensible outputs (right shape, MAPE in a reasonable band)
  * Ensemble weights normalize to 1
  * Bias-correction logic produces lower < point < upper intervals
  * Forward forecast file ties to history (segment & product sums roughly equal)
"""
from __future__ import annotations

import sys
from pathlib import Path

import numpy as np
import pandas as pd
import pytest

PROJECT_ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(PROJECT_ROOT / "scripts"))

from forecast_models import (    # noqa: E402
    NaiveLastForecaster, SeasonalNaiveForecaster, CAGRForecaster,
    HoltWintersForecaster, RidgeForecaster, GBMForecaster,
    EnsembleForecaster, walk_forward_backtest, default_factories,
    bootstrap_intervals,
)


# Reasonable iPhone-shaped fixture with strong Q1 seasonality
IPHONE_QTRLY = np.array([
    69702, 45963, 39296, 46222,    # FY24 Q1-Q4
    69138, 46841, 44582, 49025,    # FY25 Q1-Q4
    85269,                         # FY26 Q1 (the spike)
], dtype=float)
PERIOD_LABELS = [
    "FY24-Q1", "FY24-Q2", "FY24-Q3", "FY24-Q4",
    "FY25-Q1", "FY25-Q2", "FY25-Q3", "FY25-Q4",
    "FY26-Q1",
]


# =============================================================================
# Each base forecaster fits and predicts
# =============================================================================
@pytest.mark.parametrize("factory", default_factories())
def test_each_model_fits_and_predicts(factory):
    m = factory()
    m.fit(IPHONE_QTRLY)
    out = m.predict(steps=1)
    assert out.shape == (1,)
    assert not np.isnan(out).any(), f"{m.name} produced NaN"
    assert out[0] > 0, f"{m.name} produced non-positive forecast {out[0]}"


def test_seasonal_naive_returns_year_ago_value():
    """Seasonal Naive's 1-step prediction must equal y[-4]."""
    m = SeasonalNaiveForecaster(season_length=4).fit(IPHONE_QTRLY)
    expected = IPHONE_QTRLY[-4]      # = 69138 (Q1 FY25)
    assert m.predict(1)[0] == expected


def test_cagr_grows_for_growing_series():
    """CAGR forecast for a strictly increasing series must exceed the last value."""
    growing = np.array([100, 110, 120, 130, 140, 150, 160], dtype=float)
    m = CAGRForecaster().fit(growing)
    assert m.predict(1)[0] > growing[-1]


def test_naive_last_carries_forward():
    m = NaiveLastForecaster().fit(IPHONE_QTRLY)
    assert m.predict(1)[0] == IPHONE_QTRLY[-1]
    # Multi-step should also carry forward
    out = m.predict(3)
    assert all(o == IPHONE_QTRLY[-1] for o in out)


# =============================================================================
# Walk-forward backtest
# =============================================================================
def test_walk_forward_backtest_shape():
    r = walk_forward_backtest(
        IPHONE_QTRLY, PERIOD_LABELS,
        lambda: SeasonalNaiveForecaster(4),
        initial_train=5, series_id="iPhone",
    )
    # We should get len(y) - initial_train = 4 backtest predictions
    assert len(r.cv_actual) == 4
    assert len(r.cv_predicted) == 4
    assert len(r.cv_periods) == 4
    assert r.cv_periods[0] == "FY25-Q2"
    assert r.cv_periods[-1] == "FY26-Q1"


def test_walk_forward_seasonal_naive_iphone_mape():
    """Seasonal naive should achieve <15% MAPE on iPhone — sanity check."""
    r = walk_forward_backtest(
        IPHONE_QTRLY, PERIOD_LABELS,
        lambda: SeasonalNaiveForecaster(4),
        initial_train=5, series_id="iPhone",
    )
    assert r.mape < 0.15, f"Seasonal naive MAPE on iPhone is {r.mape:.3f}, expected < 0.15"


def test_holt_winters_beats_naive_last():
    """Holt-Winters should beat naive-last on iPhone (seasonal series)."""
    hw = walk_forward_backtest(IPHONE_QTRLY, PERIOD_LABELS,
                               lambda: HoltWintersForecaster(4),
                               initial_train=5)
    nl = walk_forward_backtest(IPHONE_QTRLY, PERIOD_LABELS,
                               lambda: NaiveLastForecaster(),
                               initial_train=5)
    assert hw.mape < nl.mape, (
        f"Holt-Winters ({hw.mape:.3f}) failed to beat naive-last ({nl.mape:.3f})"
    )


# =============================================================================
# Ensemble
# =============================================================================
def test_ensemble_weights_normalize():
    factories = default_factories()
    backtests = [
        walk_forward_backtest(IPHONE_QTRLY, PERIOD_LABELS, f, initial_train=5)
        for f in factories
    ]
    ens = EnsembleForecaster.from_backtest(factories, backtests)
    weight_sum = sum(ens.weights.values())
    assert abs(weight_sum - 1.0) < 1e-9, f"Weights sum to {weight_sum}, not 1"


def test_ensemble_predict_shape():
    factories = default_factories()
    backtests = [
        walk_forward_backtest(IPHONE_QTRLY, PERIOD_LABELS, f, initial_train=5)
        for f in factories
    ]
    ens = EnsembleForecaster.from_backtest(factories, backtests).fit(IPHONE_QTRLY)
    out = ens.predict(steps=2)
    assert out.shape == (2,)
    assert (out > 0).all()


# =============================================================================
# Bias correction & bootstrap intervals
# =============================================================================
def test_bias_correction_produces_well_ordered_intervals():
    """After bias correction, lower_80 <= point <= upper_80 must hold."""
    backtest = walk_forward_backtest(IPHONE_QTRLY, PERIOD_LABELS,
                                     lambda: SeasonalNaiveForecaster(4),
                                     initial_train=5)
    intervals = bootstrap_intervals(backtest, point_forecast=50000.0,
                                    bias_correct=True, n_bootstrap=5000, rng_seed=0)
    p = intervals["point_corrected"]
    assert intervals["q10"] <= p <= intervals["q90"]


def test_bootstrap_returns_all_quantiles():
    backtest = walk_forward_backtest(IPHONE_QTRLY, PERIOD_LABELS,
                                     lambda: HoltWintersForecaster(4),
                                     initial_train=5)
    intervals = bootstrap_intervals(backtest, 50000.0,
                                    quantiles=(0.05, 0.25, 0.5, 0.75, 0.95))
    for q in (5, 25, 50, 75, 95):
        assert f"q{q}" in intervals


# =============================================================================
# Output file integrity (assumes run_forecast.py was executed)
# =============================================================================
FC = PROJECT_ROOT / "data" / "processed" / "forecasts"


@pytest.mark.skipif(not (FC / "forecast_v1.csv").exists(),
                    reason="run_forecast.py hasn't been executed")
def test_forecast_csv_has_all_series():
    df = pd.read_csv(FC / "forecast_v1.csv")
    expected = {
        "Americas", "Europe", "Greater China", "Japan", "Rest of Asia Pacific",
        "iPhone", "Mac", "iPad", "Wearables Home and Accessories", "Services",
    }
    assert set(df["series"]) == expected, (
        f"Missing series: {expected - set(df['series'])}"
    )


@pytest.mark.skipif(not (FC / "forecast_v1.csv").exists(),
                    reason="run_forecast.py hasn't been executed")
def test_forecast_intervals_well_ordered():
    df = pd.read_csv(FC / "forecast_v1.csv")
    for _, r in df.iterrows():
        assert r["lower_80"] <= r["point_forecast"] <= r["upper_80"], (
            f"{r['series']}: interval is malformed "
            f"(lower={r['lower_80']}, point={r['point_forecast']}, upper={r['upper_80']})"
        )


@pytest.mark.skipif(not (FC / "forecast_v1.csv").exists(),
                    reason="run_forecast.py hasn't been executed")
def test_segment_and_product_totals_close():
    """Segment-level and product-level forecast totals should agree within 5%
    when summed across all horizons. Per-horizon totals also reconcile (see
    test_per_horizon_segment_product_totals_reconcile)."""
    df = pd.read_csv(FC / "forecast_v1.csv")
    seg_series = {"Americas", "Europe", "Greater China", "Japan", "Rest of Asia Pacific"}
    prd_series = {"iPhone", "Mac", "iPad", "Wearables Home and Accessories", "Services"}
    seg_total  = df[df["series"].isin(seg_series)]["point_forecast"].sum()
    prd_total  = df[df["series"].isin(prd_series)]["point_forecast"].sum()
    rel_diff = abs(seg_total - prd_total) / seg_total
    assert rel_diff < 0.05, (
        f"Segment total ${seg_total:.0f}M and product total ${prd_total:.0f}M differ by {rel_diff*100:.2f}%"
    )


# =============================================================================
# Multi-horizon (Q2 / Q3 / Q4 FY26) forecast tests
# =============================================================================
@pytest.mark.skipif(not (FC / "forecast_v1.csv").exists(),
                    reason="run_forecast.py hasn't been executed")
def test_forecast_csv_has_three_horizons_per_series():
    """forecast_v1.csv should contain one row per (series, horizon) — 30 rows total."""
    df = pd.read_csv(FC / "forecast_v1.csv")
    assert "horizon" in df.columns, "forecast_v1.csv missing 'horizon' column"
    assert set(df["horizon"].unique()) == {1, 2, 3}, (
        f"Expected horizons {{1,2,3}}, got {sorted(df['horizon'].unique())}"
    )
    # 10 series × 3 horizons = 30 rows
    assert len(df) == 30, f"Expected 30 forecast rows, got {len(df)}"
    # Every series should appear exactly 3 times
    counts = df["series"].value_counts()
    assert (counts == 3).all(), f"Some series have != 3 horizon rows: {counts[counts != 3]}"


@pytest.mark.skipif(not (FC / "forecast_v1.csv").exists(),
                    reason="run_forecast.py hasn't been executed")
def test_forecast_periods_are_q2_q3_q4_fy26():
    """The three horizons should map to Q2 / Q3 / Q4 FY2026 in order."""
    df = pd.read_csv(FC / "forecast_v1.csv").sort_values(["series", "horizon"])
    expected = ["2026-Q2", "2026-Q3", "2026-Q4"]
    for sid, group in df.groupby("series"):
        actual = group["forecast_period"].tolist()
        assert actual == expected, f"{sid}: expected {expected}, got {actual}"


@pytest.mark.skipif(not (FC / "forecast_v1.csv").exists(),
                    reason="run_forecast.py hasn't been executed")
def test_ci_widens_with_horizon():
    """Q4 (h=3) CI should be wider than Q2 (h=1) for most series — sqrt(h)
    scaling on bootstrap residuals should produce monotonically widening
    bands. Allow a small number of exceptions (degenerate residuals, etc.)."""
    df = pd.read_csv(FC / "forecast_v1.csv")
    df["ci_width"] = df["upper_80"] - df["lower_80"]
    widening_count = 0
    for sid, group in df.groupby("series"):
        g = group.sort_values("horizon")
        h1_width = g[g["horizon"] == 1]["ci_width"].iloc[0]
        h3_width = g[g["horizon"] == 3]["ci_width"].iloc[0]
        if h3_width > h1_width:
            widening_count += 1
    # All 10 series should widen — the math forces it: residuals scale by sqrt(h).
    # If this ever fails, something has gone wrong with the horizon scaling.
    assert widening_count >= 9, (
        f"CI did not widen with horizon for {10 - widening_count} series — "
        f"sqrt(h) scaling may not be applied correctly"
    )


@pytest.mark.skipif(not (FC / "forecast_v1.csv").exists(),
                    reason="run_forecast.py hasn't been executed")
def test_per_horizon_segment_product_totals_reconcile():
    """For each horizon, the sum of segment forecasts should approximately
    equal the sum of product forecasts (both proxy total revenue)."""
    df = pd.read_csv(FC / "forecast_v1.csv")
    seg = {"Americas", "Europe", "Greater China", "Japan", "Rest of Asia Pacific"}
    prd = {"iPhone", "Mac", "iPad", "Wearables Home and Accessories", "Services"}
    for h in (1, 2, 3):
        h_df = df[df["horizon"] == h]
        seg_total = h_df[h_df["series"].isin(seg)]["point_forecast"].sum()
        prd_total = h_df[h_df["series"].isin(prd)]["point_forecast"].sum()
        rel_diff = abs(seg_total - prd_total) / seg_total
        # Multi-step compounds error so the 5% threshold from h=1 is too tight at h=3.
        # 8% gives some breathing room while still flagging gross misreconciliation.
        assert rel_diff < 0.08, (
            f"horizon={h}: seg total ${seg_total:.0f}M vs prd total ${prd_total:.0f}M "
            f"differ by {rel_diff*100:.2f}%"
        )


def test_bootstrap_intervals_horizon_scales_ci_width():
    """A horizon=4 call should produce CIs roughly 2x wider than horizon=1
    (sqrt(4) = 2). This validates the random-walk approximation directly."""
    from forecast_models import BacktestResult, bootstrap_intervals
    # Synthetic backtest with known residuals
    actual = np.array([100.0, 110.0, 105.0, 120.0, 115.0])
    pred   = np.array([ 95.0, 108.0, 100.0, 125.0, 112.0])  # residuals = actual - pred
    bt = BacktestResult(
        model_name="test", series_id="test",
        cv_actual=actual, cv_predicted=pred,
        cv_periods=["p1","p2","p3","p4","p5"],
        mape=0.05, mae=3.0, rmse=4.0,
    )
    h1 = bootstrap_intervals(bt, point_forecast=100.0, horizon=1, rng_seed=42)
    h4 = bootstrap_intervals(bt, point_forecast=100.0, horizon=4, rng_seed=42)

    h1_width = h1["q90"] - h1["q10"]
    h4_width = h4["q90"] - h4["q10"]
    # Should be ~2x wider with some bootstrap noise tolerance
    ratio = h4_width / h1_width
    assert 1.7 < ratio < 2.3, (
        f"horizon=4 CI width should be ~2x horizon=1 (sqrt(4)=2); got ratio={ratio:.2f}"
    )
