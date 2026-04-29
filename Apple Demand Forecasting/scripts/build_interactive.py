"""
build_interactive.py
====================
Generates the standalone interactive forecast viewer at
docs/interactive_forecast.html.

Reads the database and forecast outputs, builds a single JSON payload
containing every series's history, ensemble + base-model backtests,
bias-corrected forecast, and 80% confidence interval — then inlines that
payload into a Plotly-based HTML template.

The output is a single self-contained file. Plotly is loaded from a CDN
at view time; no install needed on the reviewer's end. Open it by double-
clicking docs/interactive_forecast.html in any browser.

Usage:
    python scripts/build_interactive.py
    python scripts/build_interactive.py --output-path some/other/file.html
"""
from __future__ import annotations

import argparse
import json
import logging
import sqlite3
import sys
from pathlib import Path

import pandas as pd

PROJECT_ROOT = Path(__file__).resolve().parents[1]
DEFAULT_DB       = PROJECT_ROOT / "data" / "processed" / "apple_finance.db"
DEFAULT_FC_DIR   = PROJECT_ROOT / "data" / "processed" / "forecasts"
DEFAULT_TEMPLATE = PROJECT_ROOT / "docs" / "interactive_forecast_template.html"
DEFAULT_OUTPUT   = PROJECT_ROOT / "docs" / "interactive_forecast.html"
DEFAULT_JSON_OUT = DEFAULT_FC_DIR / "forecast_data.json"

logger = logging.getLogger("build_interactive")


# =============================================================================
# Data assembly
# =============================================================================
def build_payload(db_path: Path, fc_dir: Path) -> dict:
    """Read DB + forecast CSVs and produce the JSON payload."""
    conn = sqlite3.connect(db_path)
    try:
        seg_h = pd.read_sql("""
            SELECT s.segment_name AS series,
                   p.fiscal_year || '-' || p.fiscal_quarter AS period,
                   f.net_sales_millions AS value,
                   'segment' AS series_type
            FROM   fact_segment_revenue_quarterly f
            JOIN   dim_period  p USING (period_id)
            JOIN   dim_segment s USING (segment_id)
            ORDER BY series, p.period_id
        """, conn)
        prd_h = pd.read_sql("""
            SELECT d.product_category AS series,
                   p.fiscal_year || '-' || p.fiscal_quarter AS period,
                   f.net_sales_millions AS value,
                   'product' AS series_type
            FROM   fact_product_revenue_quarterly f
            JOIN   dim_period  p USING (period_id)
            JOIN   dim_product d USING (product_id)
            ORDER BY series, p.period_id
        """, conn)
    finally:
        conn.close()

    hist  = pd.concat([seg_h, prd_h], ignore_index=True)
    fc    = pd.read_csv(fc_dir / "forecast_v1.csv")
    back  = pd.read_csv(fc_dir / "backtest_predictions.csv")
    champs = pd.read_csv(fc_dir / "champions.csv").set_index("series")

    payload = {
        "meta": {
            "title":            "Apple Quarterly Revenue Forecast",
            "subtitle":         "Interactive view across all 10 series",
            "forecast_periods": sorted(fc["forecast_period"].unique().tolist()),
        },
        "series_list": sorted(hist["series"].unique().tolist()),
        "series": {},
    }

    for sid in payload["series_list"]:
        h        = hist[hist["series"] == sid].sort_values("period")
        sb       = back[back["series"] == sid]
        ens_rows = sb[sb["model"] == "ensemble"].sort_values("period")

        base_models = {}
        for model_name in ("naive_last", "seasonal_naive", "cagr",
                           "holt_winters", "ridge", "gbm"):
            rows = sb[sb["model"] == model_name].sort_values("period")
            base_models[model_name] = [
                {"period": r.period, "predicted": float(r.predicted)}
                for r in rows.itertuples(index=False)
            ]

        # Multi-horizon: collect every forecast row for this series, sorted by horizon
        series_fc = fc[fc["series"] == sid].sort_values("horizon")
        forecasts = [
            {
                "horizon":         int(r.horizon),
                "period":          r.forecast_period,
                "point":           float(r.point_forecast),
                "raw":             float(r.point_raw),
                "lower_80":        float(r.lower_80),
                "upper_80":        float(r.upper_80),
                "bias_correction": float(r.bias_correction),
            }
            for r in series_fc.itertuples(index=False)
        ]

        # Pull champion-level metrics from the row with horizon=1 (consistent
        # across horizons since champion is identified via 1-step backtest)
        first = series_fc.iloc[0]
        payload["series"][sid] = {
            "type":    h["series_type"].iloc[0],
            "history": [
                {"period": r.period, "value": float(r.value)}
                for r in h.itertuples(index=False)
            ],
            "ensemble_backtest": [
                {"period": r.period, "actual": float(r.actual), "predicted": float(r.predicted)}
                for r in ens_rows.itertuples(index=False)
            ],
            "base_model_backtest": base_models,
            "forecasts": forecasts,
            "champion": {
                "model":              champs.loc[sid, "champion_model"],
                "mape_pct":           float(champs.loc[sid, "champion_mape"]),
                "ensemble_mape_pct":  float(first["ensemble_mape_pct"]),
            },
        }
    return payload


# =============================================================================
# HTML template injection
# =============================================================================
def render_html(template_path: Path, payload: dict) -> str:
    """
    Read the template and replace the __DATA_PLACEHOLDER__ token with the
    JSON payload. We use a sentinel instead of `.format()` because the
    template contains plenty of `{` characters that would conflict.
    """
    template = template_path.read_text()
    sentinel = "__DATA_PLACEHOLDER__"
    if sentinel not in template:
        raise RuntimeError(f"Template {template_path} is missing {sentinel}")

    # JSON-encode the payload, but escape any closing </script tags that could
    # break out of the script element. Browser-safe JSON-in-script convention.
    json_text = (
        json.dumps(payload, separators=(",", ":"))
        .replace("</", "<\\/")
    )
    return template.replace(sentinel, json_text)


# =============================================================================
# CLI
# =============================================================================
def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__.split("\n")[1])
    parser.add_argument("--db-path",       type=Path, default=DEFAULT_DB)
    parser.add_argument("--fc-dir",        type=Path, default=DEFAULT_FC_DIR)
    parser.add_argument("--template",      type=Path, default=DEFAULT_TEMPLATE)
    parser.add_argument("--output-path",   type=Path, default=DEFAULT_OUTPUT)
    parser.add_argument("--json-out",      type=Path, default=DEFAULT_JSON_OUT,
                        help="Also write the JSON payload to this path (None to skip)")
    parser.add_argument("--verbose", "-v", action="store_true")
    args = parser.parse_args()

    logging.basicConfig(
        level=logging.DEBUG if args.verbose else logging.INFO,
        format="%(asctime)s [%(levelname)s] %(message)s",
        datefmt="%H:%M:%S",
    )

    payload = build_payload(args.db_path, args.fc_dir)
    logger.info("Built payload: %d series", len(payload["series"]))

    if args.json_out:
        args.json_out.write_text(json.dumps(payload, indent=2))
        logger.info("Wrote JSON payload to %s (%d bytes)",
                    args.json_out, args.json_out.stat().st_size)

    html = render_html(args.template, payload)
    args.output_path.write_text(html)
    logger.info("Wrote interactive HTML to %s (%d bytes)",
                args.output_path, args.output_path.stat().st_size)
    return 0


if __name__ == "__main__":
    sys.exit(main())
