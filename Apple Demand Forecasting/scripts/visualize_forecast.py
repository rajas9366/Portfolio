"""
visualize_forecast.py
=====================
Generate portfolio-quality matplotlib charts from the forecast outputs:
  1. Backtest performance heatmap — MAPE per (model, series)
  2. Per-series time series with forecast and 80% interval
  3. Champion model summary chart

Usage:
    python scripts/visualize_forecast.py
    python scripts/visualize_forecast.py --output-dir docs/figures/

Outputs land as PNG files. Designed for embedding in README, executive memo,
and Tableau dashboard description.
"""
from __future__ import annotations

import argparse
import logging
import sqlite3
import sys
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

PROJECT_ROOT = Path(__file__).resolve().parents[1]
DEFAULT_DB   = PROJECT_ROOT / "data" / "processed" / "apple_finance.db"
DEFAULT_FC   = PROJECT_ROOT / "data" / "processed" / "forecasts"
DEFAULT_OUT  = PROJECT_ROOT / "docs" / "figures"

logger = logging.getLogger("visualize_forecast")

# --- Style settings — clean, finance-presentation-friendly ------------------
plt.rcParams.update({
    "figure.dpi":      110,
    "savefig.dpi":     150,
    "font.family":     "DejaVu Sans",
    "axes.spines.top": False,
    "axes.spines.right": False,
    "axes.grid":       True,
    "grid.alpha":      0.25,
    "grid.linestyle":  "--",
})


# =============================================================================
# Chart 1: MAPE heatmap (model × series)
# =============================================================================
def plot_mape_heatmap(metrics: pd.DataFrame, output_path: Path) -> None:
    pivot = metrics.pivot_table(
        index="model", columns="series", values="mape_pct", aggfunc="mean"
    )
    # Stable order: rows by mean MAPE ascending, columns by series_type then alpha
    pivot = pivot.loc[pivot.mean(axis=1).sort_values().index]

    fig, ax = plt.subplots(figsize=(12, 5))
    im = ax.imshow(pivot.values, aspect="auto", cmap="RdYlGn_r", vmin=0, vmax=30)
    ax.set_xticks(range(len(pivot.columns)))
    ax.set_xticklabels(pivot.columns, rotation=35, ha="right")
    ax.set_yticks(range(len(pivot.index)))
    ax.set_yticklabels(pivot.index)
    ax.set_title("Backtest MAPE (%) by model and series — lower is better",
                 fontsize=12, pad=12)

    # Cell labels
    for i in range(pivot.shape[0]):
        for j in range(pivot.shape[1]):
            v = pivot.values[i, j]
            color = "white" if v > 15 else "black"
            ax.text(j, i, f"{v:.1f}", ha="center", va="center",
                    fontsize=8, color=color)
    cbar = fig.colorbar(im, ax=ax, fraction=0.025, pad=0.02)
    cbar.set_label("MAPE (%)", rotation=270, labelpad=14)
    plt.tight_layout()
    plt.savefig(output_path, bbox_inches="tight")
    plt.close()
    logger.info("Wrote %s", output_path)


# =============================================================================
# Chart 2: per-series forecast with history and CI
# =============================================================================
def plot_per_series_forecast(
    history_df:  pd.DataFrame,
    forecast_df: pd.DataFrame,
    backtest_df: pd.DataFrame,
    output_path: Path,
) -> None:
    """
    One faceted figure: top row = 5 segments, bottom row = 5 products.
    Renders historical actuals, ensemble backtest, and the multi-horizon
    forecast (one diamond per future quarter, error bars sized by horizon).
    """
    seg_series  = ["Americas", "Europe", "Greater China", "Japan", "Rest of Asia Pacific"]
    prod_series = ["iPhone", "Services", "Mac", "iPad", "Wearables Home and Accessories"]
    layout = [seg_series, prod_series]

    fig, axes = plt.subplots(2, 5, figsize=(18, 7), sharex=True)
    for row_idx, series_list in enumerate(layout):
        for col_idx, sid in enumerate(series_list):
            ax = axes[row_idx, col_idx]
            hist = history_df[history_df["series_id"] == sid].sort_values("period_id")
            fc_rows = forecast_df[forecast_df["series"] == sid].sort_values("horizon")

            x_hist = list(range(len(hist)))
            ax.plot(x_hist, hist["y"].values, "o-", color="#1f77b4",
                    label="Actual", linewidth=2, markersize=5)

            # Ensemble backtest (one-step predictions)
            ens_back = backtest_df[(backtest_df["series"] == sid) &
                                   (backtest_df["model"] == "ensemble")]
            if not ens_back.empty:
                period_to_idx = {p: i for i, p in enumerate(hist["period_label"])}
                bx = [period_to_idx[p] for p in ens_back["period"] if p in period_to_idx]
                by = [v for p, v in zip(ens_back["period"], ens_back["predicted"])
                      if p in period_to_idx]
                ax.plot(bx, by, "s--", color="#ff7f0e", alpha=0.8,
                        markersize=4, label="Ensemble backtest")

            # Multi-horizon forecast — one diamond per future quarter,
            # connected by a line so the trajectory reads visually.
            fx_list = [len(hist) + i for i in range(len(fc_rows))]
            fy_list = fc_rows["point_forecast"].tolist()
            ax.plot(fx_list, fy_list, "-", color="#2ca02c", linewidth=1, alpha=0.6)
            ax.errorbar(
                fx_list, fy_list,
                yerr=[
                    (fc_rows["point_forecast"] - fc_rows["lower_80"]).tolist(),
                    (fc_rows["upper_80"]       - fc_rows["point_forecast"]).tolist(),
                ],
                fmt="D", color="#2ca02c", markersize=7, capsize=4,
                label="Forecast (80% CI)",
            )

            # Cosmetics
            title = sid if len(sid) <= 20 else sid[:18] + "..."
            ax.set_title(title, fontsize=10, pad=4)
            xticks = list(range(len(hist))) + fx_list
            xlabels = list(hist["period_label"]) + fc_rows["forecast_period"].tolist()
            ax.set_xticks(xticks)
            ax.set_xticklabels(xlabels, rotation=45, ha="right", fontsize=7)
            ax.tick_params(axis="y", labelsize=8)
            ymax = max(ax.get_ylim()[1], float(fc_rows["upper_80"].max()) * 1.05)
            ax.set_ylim(0, ymax)
            ax.yaxis.set_major_formatter(
                plt.FuncFormatter(lambda x, p: f"${x/1000:.0f}B")
            )
            if col_idx == 0:
                ylabel = "Segments" if row_idx == 0 else "Products"
                ax.set_ylabel(ylabel, fontsize=10)

    # Single shared legend (top-right)
    handles, labels = axes[0, 0].get_legend_handles_labels()
    fig.legend(handles, labels, loc="upper center", ncol=3, fontsize=10,
               bbox_to_anchor=(0.5, 1.02), frameon=False)
    fig.suptitle("Apple quarterly revenue: history, ensemble backtest, and Q2–Q4 FY26 forecast",
                 fontsize=13, y=1.06)
    plt.tight_layout()
    plt.savefig(output_path, bbox_inches="tight")
    plt.close()
    logger.info("Wrote %s", output_path)


# =============================================================================
# Chart 3: champion summary
# =============================================================================
def plot_champion_summary(champions: pd.DataFrame, output_path: Path) -> None:
    df = champions.sort_values("champion_mape").copy()
    df["bar_label"] = df.apply(
        lambda r: f"{r['series']:30s}  {r['champion_model']:14s}  {r['champion_mape']:5.2f}%",
        axis=1,
    )
    fig, ax = plt.subplots(figsize=(10, 5))
    colors = {
        "cagr":           "#1f77b4",
        "holt_winters":   "#2ca02c",
        "seasonal_naive": "#ff7f0e",
        "ridge":          "#9467bd",
        "gbm":            "#d62728",
        "naive_last":     "#7f7f7f",
    }
    bar_colors = [colors.get(m, "#888888") for m in df["champion_model"]]
    ax.barh(df["series"], df["champion_mape"], color=bar_colors, edgecolor="white")
    ax.set_xlabel("Backtest MAPE (%) — lower is better")
    ax.set_title("Champion model per series — different methods win for different patterns",
                 fontsize=12, pad=10)
    ax.invert_yaxis()
    # Annotate each bar with the model name
    for i, (_, r) in enumerate(df.iterrows()):
        ax.text(r["champion_mape"] + 0.15, i, f"  {r['champion_model']}",
                va="center", fontsize=9)
    ax.set_xlim(0, max(df["champion_mape"]) * 1.4)
    plt.tight_layout()
    plt.savefig(output_path, bbox_inches="tight")
    plt.close()
    logger.info("Wrote %s", output_path)


# =============================================================================
# Orchestration
# =============================================================================
def run(db_path: Path, fc_dir: Path, output_dir: Path) -> int:
    output_dir.mkdir(parents=True, exist_ok=True)

    # Load history from the DB
    conn = sqlite3.connect(db_path)
    seg_hist = pd.read_sql(
        """
        SELECT  s.segment_name      AS series_id,
                p.period_id, p.fiscal_year, p.fiscal_quarter,
                p.fiscal_year || '-' || p.fiscal_quarter AS period_label,
                f.net_sales_millions AS y
        FROM    fact_segment_revenue_quarterly f
        JOIN    dim_period  p USING (period_id)
        JOIN    dim_segment s USING (segment_id)
        """, conn,
    )
    prod_hist = pd.read_sql(
        """
        SELECT  d.product_category  AS series_id,
                p.period_id, p.fiscal_year, p.fiscal_quarter,
                p.fiscal_year || '-' || p.fiscal_quarter AS period_label,
                f.net_sales_millions AS y
        FROM    fact_product_revenue_quarterly f
        JOIN    dim_period  p USING (period_id)
        JOIN    dim_product d USING (product_id)
        """, conn,
    )
    history = pd.concat([seg_hist, prod_hist], ignore_index=True)
    conn.close()

    metrics      = pd.read_csv(fc_dir / "model_metrics.csv")
    champions    = pd.read_csv(fc_dir / "champions.csv")
    forecast_df  = pd.read_csv(fc_dir / "forecast_v1.csv")
    backtest_df  = pd.read_csv(fc_dir / "backtest_predictions.csv")

    # ---- Add ensemble backtest predictions for the time-series chart ------
    # The ensemble is rebuilt fresh inside walk_forward; we replay it here.
    # For brevity we approximate with the champion's predictions in the chart.
    # (For formal use, ensemble backtest is captured in run_forecast.py; here
    # we just need a visual reference, so champion is a reasonable stand-in.)

    plot_mape_heatmap(metrics, output_dir / "01_mape_heatmap.png")
    plot_champion_summary(champions, output_dir / "02_champions.png")
    plot_per_series_forecast(history, forecast_df, backtest_df,
                             output_dir / "03_forecast_panels.png")
    return 0


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__.split("\n")[1])
    parser.add_argument("--db-path",    type=Path, default=DEFAULT_DB)
    parser.add_argument("--fc-dir",     type=Path, default=DEFAULT_FC)
    parser.add_argument("--output-dir", type=Path, default=DEFAULT_OUT)
    parser.add_argument("--verbose",   "-v", action="store_true")
    args = parser.parse_args()

    logging.basicConfig(
        level=logging.DEBUG if args.verbose else logging.INFO,
        format="%(asctime)s [%(levelname)s] %(message)s",
        datefmt="%H:%M:%S",
    )
    return run(args.db_path, args.fc_dir, args.output_dir)


if __name__ == "__main__":
    sys.exit(main())
