"""
run_forecast.py
===============
Runs the multi-model ensemble forecast across every quarterly time series
(5 segments + 5 product categories), backtests each model, and writes:

    data/processed/forecasts/model_metrics.csv     — per-model, per-series MAPE/MAE/RMSE
    data/processed/forecasts/champions.csv         — single best model per series
    data/processed/forecasts/forecast_v1.csv       — Forecast points + 80% CIs.
                                                     One row per (series, horizon).
                                                     Default horizons: Q2/Q3/Q4 FY26.
    data/processed/forecasts/backtest_predictions.csv — full per-period predictions for chart inputs

Multi-horizon notes:
    The walk-forward backtest is 1-step-ahead — that's where the 9-quarter
    sample size lives most honestly. Multi-step forecasts use each base
    model's native multi-step prediction (recursive feed-forward for
    Ridge/GBM, native for Holt-Winters/CAGR/seasonal_naive), with bootstrap
    CIs scaled by sqrt(horizon) — a standard random-walk approximation that
    widens uncertainty bands at longer horizons. The same 1-step bias
    correction is applied to every horizon; this is a documented assumption
    (see docs/methodology.md).

Usage:
    python scripts/run_forecast.py                   # default: forecast Q2/Q3/Q4 FY26
    python scripts/run_forecast.py --horizons 1 2    # forecast only Q2 and Q3 FY26
    python scripts/run_forecast.py --output-dir results/
"""
from __future__ import annotations

import argparse
import logging
import sqlite3
import sys
from pathlib import Path

import numpy as np
import pandas as pd

sys.path.insert(0, str(Path(__file__).resolve().parent))
from forecast_models import (         # noqa: E402
    BacktestResult,
    EnsembleForecaster,
    bootstrap_intervals,
    default_factories,
    walk_forward_backtest,
)

PROJECT_ROOT = Path(__file__).resolve().parents[1]
DEFAULT_DB   = PROJECT_ROOT / "data" / "processed" / "apple_finance.db"
DEFAULT_OUT  = PROJECT_ROOT / "data" / "processed" / "forecasts"

logger = logging.getLogger("run_forecast")


# =============================================================================
# Data loading
# =============================================================================
def load_series_from_db(conn: sqlite3.Connection) -> dict[str, pd.DataFrame]:
    """
    Return a dict mapping series_id -> DataFrame[period_id, fiscal_year,
    fiscal_quarter, period_label, y].

    Each series is one segment or one product category.
    """
    seg = pd.read_sql(
        """
        SELECT  p.period_id, p.fiscal_year, p.fiscal_quarter,
                s.segment_name AS series_id,
                f.net_sales_millions AS y,
                'segment' AS series_type
        FROM    fact_segment_revenue_quarterly f
        JOIN    dim_period  p USING (period_id)
        JOIN    dim_segment s USING (segment_id)
        WHERE   p.period_type = 'QUARTERLY'
        ORDER BY series_id, p.period_id
        """, conn,
    )
    prod = pd.read_sql(
        """
        SELECT  p.period_id, p.fiscal_year, p.fiscal_quarter,
                d.product_category AS series_id,
                f.net_sales_millions AS y,
                'product' AS series_type
        FROM    fact_product_revenue_quarterly f
        JOIN    dim_period  p USING (period_id)
        JOIN    dim_product d USING (product_id)
        WHERE   p.period_type = 'QUARTERLY'
        ORDER BY series_id, p.period_id
        """, conn,
    )
    full = pd.concat([seg, prod], ignore_index=True)
    full["period_label"] = full["fiscal_year"].astype(str) + "-" + full["fiscal_quarter"]

    # Group by series for downstream consumption
    return {sid: g.reset_index(drop=True) for sid, g in full.groupby("series_id")}


def next_period_label(last_label: str) -> str:
    """e.g. '2026-Q1' → '2026-Q2', '2026-Q4' → '2027-Q1'."""
    yr, q = last_label.split("-Q")
    yr, q = int(yr), int(q)
    if q == 4:
        return f"{yr+1}-Q1"
    return f"{yr}-Q{q+1}"


# =============================================================================
# Per-series forecasting pipeline
# =============================================================================
def forecast_one_series(
    series_id: str,
    df: pd.DataFrame,
    horizons: tuple[int, ...] = (1, 2, 3),
    initial_train: int = 5,
) -> tuple[list[BacktestResult], dict, list[dict]]:
    """
    Returns (backtest_results, champion_summary, forecast_rows).

    backtest_results: BacktestResult per base model
    champion_summary: dict with the best model per series for the champions CSV
    forecast_rows:    list of dicts, one per requested horizon, each with
                      point + bootstrap intervals for forecast_v1.csv
    """
    y_arr  = df["y"].to_numpy(dtype=float)
    labels = df["period_label"].tolist()

    # 1) Walk-forward backtest each base model (always 1-step-ahead)
    results: list[BacktestResult] = []
    for factory in default_factories():
        try:
            r = walk_forward_backtest(
                y_arr, labels, factory,
                initial_train=initial_train, series_id=series_id,
            )
            results.append(r)
        except Exception as e:
            logger.warning("%s: %s failed (%s)", series_id, factory().name, e)

    # 2) Identify champion (lowest 1-step MAPE among non-NaN models)
    valid = [r for r in results if not np.isnan(r.mape)]
    if not valid:
        raise RuntimeError(f"No valid backtest results for {series_id}")
    champion = min(valid, key=lambda r: r.mape)

    champion_summary = {
        "series": series_id,
        "champion_model":  champion.model_name,
        "champion_mape":   round(champion.mape * 100, 3),
        "champion_mae":    round(float(champion.mae), 1),
        "champion_rmse":   round(float(champion.rmse), 1),
        "n_obs":           len(y_arr),
        "n_backtest":      len(champion.cv_actual),
    }

    # 3) Build ensemble using inverse-MAPE weights from the backtest, then
    #    generate point forecasts for the full requested horizon range
    factories = default_factories()
    ensemble  = EnsembleForecaster.from_backtest(factories, valid)
    ensemble.fit(y_arr)
    max_h = max(horizons)
    point_path = ensemble.predict(steps=max_h)   # array of length max_h

    # 4) Backtest the *ensemble* itself so its residuals can drive bootstrap CIs.
    #    Same 1-step backtest as before; multi-horizon CIs scale these residuals
    #    by sqrt(h) per the random-walk approximation (see docs/methodology.md).
    ens_back = walk_forward_backtest(
        y_arr, labels,
        lambda: EnsembleForecaster.from_backtest(default_factories(), valid),
        initial_train=initial_train, series_id=series_id,
    )

    # 5) For each horizon, compute bootstrap intervals scaled by sqrt(h)
    forecast_rows: list[dict] = []
    forecast_label = labels[-1]
    for h in horizons:
        # Walk forecast_label forward h quarters (cumulatively per loop iter, but
        # because we compute each row fresh from labels[-1], do it deterministically)
        label = labels[-1]
        for _ in range(h):
            label = next_period_label(label)

        point_fc = float(point_path[h - 1])
        intervals = bootstrap_intervals(
            ens_back, point_forecast=point_fc, quantiles=(0.1, 0.5, 0.9),
            bias_correct=True, horizon=h,
        )

        forecast_rows.append({
            "series":             series_id,
            "horizon":            h,
            "forecast_period":    label,
            "point_raw":          round(point_fc, 0),
            "point_forecast":     round(intervals["point_corrected"], 0),  # bias-corrected
            "bias_correction":    round(intervals["mean_residual"], 0),
            "lower_80":           round(intervals["q10"], 0),
            "median":             round(intervals["q50"], 0),
            "upper_80":           round(intervals["q90"], 0),
            "ensemble_mape_pct":  round(ens_back.mape * 100, 3) if not np.isnan(ens_back.mape) else None,
            "champion_model":     champion.model_name,
        })

    return results, champion_summary, forecast_rows


# =============================================================================
# Orchestration
# =============================================================================
def run(db_path: Path, output_dir: Path, horizons: tuple[int, ...]) -> int:
    output_dir.mkdir(parents=True, exist_ok=True)
    conn = sqlite3.connect(db_path)
    try:
        series_dict = load_series_from_db(conn)
    finally:
        conn.close()

    logger.info("Forecasting %d series across horizons %s", len(series_dict), list(horizons))

    all_metrics       = []
    all_champions     = []
    all_forecasts     = []
    all_backtest_pred = []

    for series_id, df in series_dict.items():
        if series_id == "_Products subtotal":
            continue   # exclude redundant rollup
        if len(df) < 7:
            logger.warning("Skipping %s — only %d quarters", series_id, len(df))
            continue

        results, champ, fcast_rows = forecast_one_series(series_id, df, horizons=horizons)

        # metrics rows
        for r in results:
            all_metrics.append({**r.to_dict(), "series_type": df["series_type"].iloc[0]})
            for period, actual, pred in zip(r.cv_periods, r.cv_actual, r.cv_predicted):
                all_backtest_pred.append({
                    "series":     series_id,
                    "model":      r.model_name,
                    "period":     period,
                    "actual":     float(actual),
                    "predicted":  float(pred),
                })

        # Replay ensemble backtest so its row-by-row predictions land in the CSV too.
        # This is the same call that drove the bias-correction step inside
        # forecast_one_series — recomputing here is cheap and keeps responsibilities clean.
        ens_back_for_csv = walk_forward_backtest(
            df["y"].to_numpy(dtype=float),
            df["period_label"].tolist(),
            lambda valid=[r for r in results if not np.isnan(r.mape)]:
                EnsembleForecaster.from_backtest(default_factories(), valid),
            initial_train=5, series_id=series_id,
        )
        for period, actual, pred in zip(
            ens_back_for_csv.cv_periods, ens_back_for_csv.cv_actual, ens_back_for_csv.cv_predicted
        ):
            all_backtest_pred.append({
                "series":     series_id,
                "model":      "ensemble",
                "period":     period,
                "actual":     float(actual),
                "predicted":  float(pred),
            })

        all_champions.append(champ)
        all_forecasts.extend(fcast_rows)

        # Compact log line with all forecast quarters for this series
        fc_summary = ", ".join(
            f"{f['forecast_period']}=${f['point_forecast']/1000:.1f}B"
            for f in fcast_rows
        )
        logger.info(
            "%-30s  champion=%-15s  MAPE=%5.2f%%   forecasts: %s",
            series_id, champ["champion_model"], champ["champion_mape"], fc_summary,
        )

    # ---- write outputs ----------------------------------------------------
    pd.DataFrame(all_metrics       ).to_csv(output_dir / "model_metrics.csv",        index=False)
    pd.DataFrame(all_champions     ).to_csv(output_dir / "champions.csv",            index=False)
    pd.DataFrame(all_forecasts     ).to_csv(output_dir / "forecast_v1.csv",          index=False)
    pd.DataFrame(all_backtest_pred ).to_csv(output_dir / "backtest_predictions.csv", index=False)

    logger.info("Wrote outputs to %s (%d forecast rows across %d series × %d horizons)",
                output_dir, len(all_forecasts), len(all_champions), len(horizons))
    return 0


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__.split("\n")[1])
    parser.add_argument("--db-path",    type=Path, default=DEFAULT_DB)
    parser.add_argument("--output-dir", type=Path, default=DEFAULT_OUT)
    parser.add_argument("--horizons",   type=int,  nargs="+", default=[1, 2, 3],
                        help="Forecast horizons in quarters (default: 1 2 3 = next 3 quarters)")
    parser.add_argument("--verbose", "-v", action="store_true")
    args = parser.parse_args()

    logging.basicConfig(
        level=logging.DEBUG if args.verbose else logging.INFO,
        format="%(asctime)s [%(levelname)s] %(message)s",
        datefmt="%H:%M:%S",
    )
    return run(args.db_path, args.output_dir, tuple(args.horizons))


if __name__ == "__main__":
    sys.exit(main())
