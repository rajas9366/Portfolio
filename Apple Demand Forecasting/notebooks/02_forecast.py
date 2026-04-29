# ---
# jupyter:
#   jupytext:
#     formats: ipynb,py:percent
#     text_representation:
#       extension: .py
#       format_name: percent
#       format_version: '1.3'
#   kernelspec:
#     display_name: Python 3
#     language: python
#     name: python3
# ---

# %% [markdown]
# # Apple Retail Demand Forecasting — Multi-Model Ensemble
#
# **Goal.** Forecast Q2 FY2026 revenue for each of Apple's 5 reportable segments and 5 product categories,
# using 9 quarters of history extracted from the 10-K and four 10-Q XBRL filings.
#
# **Approach.** Walk-forward backtest of 6 base models, then a stacked ensemble weighted by
# inverse-MAPE, with non-parametric bootstrap prediction intervals.
#
# This notebook is the executive narrative; the production code lives in `scripts/`. To convert to
# a runnable .ipynb: `jupytext --to notebook notebooks/02_forecast.py`.

# %%
import sys
import sqlite3
from pathlib import Path

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

# Make the forecast module importable
PROJECT_ROOT = Path.cwd().parent if Path.cwd().name == "notebooks" else Path.cwd()
sys.path.insert(0, str(PROJECT_ROOT / "scripts"))

from forecast_models import (    # noqa: E402
    EnsembleForecaster, default_factories, walk_forward_backtest, bootstrap_intervals,
)

DB = PROJECT_ROOT / "data" / "processed" / "apple_finance.db"
FC = PROJECT_ROOT / "data" / "processed" / "forecasts"

# %% [markdown]
# ## 1. Load data
# Pull the quarterly time series from the SQLite database. Each series is one segment or one product.

# %%
conn = sqlite3.connect(DB)
seg = pd.read_sql("""
    SELECT s.segment_name AS series_id, p.fiscal_year, p.fiscal_quarter,
           p.fiscal_year || '-' || p.fiscal_quarter AS period_label,
           f.net_sales_millions AS y, 'segment' AS series_type
    FROM   fact_segment_revenue_quarterly f
    JOIN   dim_period p USING (period_id)
    JOIN   dim_segment s USING (segment_id)
    ORDER BY series_id, p.period_id
""", conn)
prod = pd.read_sql("""
    SELECT d.product_category AS series_id, p.fiscal_year, p.fiscal_quarter,
           p.fiscal_year || '-' || p.fiscal_quarter AS period_label,
           f.net_sales_millions AS y, 'product' AS series_type
    FROM   fact_product_revenue_quarterly f
    JOIN   dim_period p USING (period_id)
    JOIN   dim_product d USING (product_id)
    ORDER BY series_id, p.period_id
""", conn)
conn.close()
data = pd.concat([seg, prod], ignore_index=True)
print(f"{data.series_id.nunique()} series x {data.groupby('series_id').size().iloc[0]} quarters")

# %% [markdown]
# ## 2. Visualize the seasonal structure
#
# Apple's revenue is dominated by the holiday quarter (Q1 fiscal). Let's verify visually before
# fitting any model.

# %%
fig, ax = plt.subplots(figsize=(12, 5))
totals = data.groupby(["fiscal_year", "fiscal_quarter"])["y"].sum().reset_index()
for fy in sorted(totals.fiscal_year.unique()):
    sub = totals[totals.fiscal_year == fy].sort_values("fiscal_quarter")
    ax.plot(sub.fiscal_quarter, sub.y / 1000, "o-", linewidth=2, markersize=8, label=f"FY{fy}")
ax.set_ylabel("Total revenue ($B)")
ax.set_title("Apple total quarterly revenue — Q1 holiday spike is consistently ~30% of FY")
ax.legend(); ax.grid(alpha=0.3); plt.show()

# %% [markdown]
# ## 3. Run the full forecast pipeline
#
# Rather than re-implement here, we call the production code in `scripts/run_forecast.py`. The
# orchestrator runs walk-forward CV across all 10 series and writes outputs to `data/processed/forecasts/`.

# %%
# !python ../scripts/run_forecast.py    # uncomment to re-run

# Load the outputs
metrics      = pd.read_csv(FC / "model_metrics.csv")
champions    = pd.read_csv(FC / "champions.csv")
forecasts    = pd.read_csv(FC / "forecast_v1.csv")
backtest_pred = pd.read_csv(FC / "backtest_predictions.csv")

# %% [markdown]
# ## 4. Champion model per series
#
# A real Retail Finance team would not blindly apply one technique to every series. Different
# revenue streams have different structures, and a sensible pipeline picks the best tool for each.

# %%
print(champions.sort_values("champion_mape").to_string(index=False))

# %% [markdown]
# **Read.** Holt-Winters wins six of ten series — it's the only base model with explicit
# level/trend/seasonality decomposition. CAGR wins Services (1.2% MAPE!) because Services
# revenue grows nearly linearly. Seasonal Naive wins Europe/China/Japan where the seasonal
# pattern is more stable than the trend.

# %% [markdown]
# ## 5. The Q2 FY26 forecast
#
# Bias-corrected point estimates with 80% bootstrap intervals.

# %%
forecasts.sort_values("point_forecast", ascending=False).reset_index(drop=True)

# %% [markdown]
# **Three observations worth flagging to leadership.**
#
# 1. **iPhone forecast: $66.1B (vs $46.8B Q2 FY25, +41%).** This includes a +$6.3B bias correction.
#    The unbiased model says $59.8B (+28% YoY). The right answer almost certainly lies between
#    these — the bias correction assumes Q1 FY26's iPhone 17 strength fully carries to Q2, which
#    is the most aggressive assumption.
#
# 2. **Services forecast: $31.1B (+17% YoY).** Tightest 80% interval of any series — the bootstrap
#    band is just $30.9B–$31.3B. Services is the most predictable line in the company.
#
# 3. **Greater China forecast: $20.5B (+28% YoY).** Reverses three years of decline. The model is
#    extrapolating Q1 FY26's surprise inflection forward; this is the highest-conviction call to
#    sanity-check before the actual Q2 print lands.

# %% [markdown]
# ## 6. Sanity check: do the forecasts reconcile?

# %%
seg_total  = forecasts[forecasts.series.isin(seg.series_id.unique())]["point_forecast"].sum()
prod_total = forecasts[forecasts.series.isin(prod.series_id.unique())]["point_forecast"].sum()
print(f"Σ segment forecasts:  ${seg_total/1000:6.1f}B")
print(f"Σ product forecasts:  ${prod_total/1000:6.1f}B")
print(f"Difference:           ${(seg_total - prod_total)/1000:+5.1f}B  ({100*(seg_total-prod_total)/seg_total:+.1f}%)")
print(f"Q2 FY25 actual was $95.4B — both forecasts imply ~25% YoY growth.")

# %% [markdown]
# Segment and product totals agree within 1% — different models for different views, but they
# converge on the same growth narrative. A more sophisticated approach (MinT reconciliation)
# would force exact coherence; with 9 observations it's not worth the methodology risk.

# %% [markdown]
# ## 7. Limitations
#
# - Nine quarters per series is at the lower edge of where any seasonal model is defensible.
# - The iPhone 17 Q1 FY26 spike is unprecedented in our training data — the bias correction
#   assumes it carries forward. If iPhone 17 demand was front-loaded into Q1, our Q2 forecast
#   is too high.
# - No exogenous regressors (FX, tariffs, launch indicators). Adding these is the obvious next
#   iteration.
#
# See `docs/methodology.md` for the full technical writeup.
