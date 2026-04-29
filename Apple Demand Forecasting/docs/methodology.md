# Forecast Methodology

## Problem framing

We forecast next-quarter revenue for ten time series — five geographic
segments (Americas, Europe, Greater China, Japan, Rest of Asia Pacific) and
five product categories (iPhone, Mac, iPad, Wearables Home and Accessories,
Services) — using nine quarters of history extracted from Apple's 10-K and
four 10-Q XBRL filings.

The forecast horizon is **one quarter** (Q2 FY2026). Going further out
would not be defensible at this sample size; we treat the one-step forecast
as a directional read for Retail Finance, not a precise plan number.

## Why this approach is appropriate for the data

Apple's quarterly segment- and product-level disclosure gives us **9 observations
per series**. Heavy machinery like Prophet (typically wants 30+ observations
to estimate seasonality and trend changepoints reliably) or full SARIMA
identification is statistically fragile here. Instead we use a transparent
ensemble of six base models, each with explicit, inspectable structure:

| Model              | What it captures                       | Strength on Apple data |
|--------------------|----------------------------------------|-------------------------|
| `naive_last`       | Recency only                           | Floor baseline          |
| `seasonal_naive`   | Same-quarter-last-year                 | Strong (30%+ Q1 spike)  |
| `cagr`             | Geometric trend                        | Excellent on Services   |
| `holt_winters`     | Level + trend + additive seasonality   | Best overall            |
| `ridge`            | Lag features + seasonal dummies        | Modest                  |
| `gbm`              | Non-linear lag interactions            | Modest                  |

Each base model exposes the same `fit(y).predict(steps)` API
(see `scripts/forecast_models.py`).

## Validation: expanding-window walk-forward

Rather than a single train/test split, we run an **expanding-window
walk-forward backtest**: for each k from 5 to N-1, train on `y[:k]` and
predict `y[k]`. With N=9 this gives **5 backtest predictions per series**
(predicting Q1 FY25, Q2 FY25, Q3 FY25, Q4 FY25, and Q1 FY26). MAPE, MAE,
and RMSE are reported in `data/processed/forecasts/model_metrics.csv`.

This mirrors how a Retail Finance team would re-forecast in production:
each new quarter's actuals enter the training set before the next forecast.

## Champion-challenger: different models win for different series

```
Service             cagr            1.2% MAPE   (Services is near-linear in $ growth)
Wearables H&A       holt_winters    3.8% MAPE
Mac                 cagr            5.0% MAPE
iPad                holt_winters    6.5% MAPE
Americas            holt_winters    7.5% MAPE
iPhone              holt_winters    8.5% MAPE
Europe              seasonal_naive  8.7% MAPE
Greater China       seasonal_naive  9.4% MAPE
Japan               seasonal_naive  10.3% MAPE
Rest of Asia Pacific holt_winters   10.7% MAPE
```

Holt-Winters wins six of ten series — unsurprising given it's the only base
model that explicitly decomposes level, trend, and seasonality. CAGR wins
for Services and Mac because both have unusually clean linear growth in
log-revenue. Seasonal naive wins for Europe, Greater China, and Japan,
where the recent histories show stable seasonality but limited trend
(in China's case, an actively reversing trend that confuses Holt-Winters).

## Ensemble construction

Rather than picking one champion globally, we build an **inverse-MAPE
weighted ensemble**: each base model's weight is proportional to `1/MAPE`
on the backtest. Models that fit poorly are downweighted, but no model
is excluded entirely — a poorly-fit model can still contribute information
about uncertainty.

The ensemble is then itself backtested using the same walk-forward
procedure, and its residuals drive the bootstrap intervals described below.

## Bias correction

In small samples, ensembles of biased models inherit that bias. The Q1
FY26 iPhone result ($85B vs ensemble prediction of ~$63B) was an
unprecedented launch-cycle spike that no model in our backtest could have
predicted from prior history. Across the backtest, residuals were
systematically positive (ensemble underpredicts), with means ranging from
+$84M (Wearables) to +$6,299M (iPhone).

We apply two transformations before reporting:

1. **Point forecast** is shifted by the **mean residual** — calibrating to
   historical bias under the assumption that bias persists.
2. **Bootstrap intervals** are computed on **demeaned residuals**, so
   intervals are symmetric around the bias-corrected point estimate.

Both the raw and bias-corrected point forecasts are surfaced in the output
CSV (`point_raw` and `point_forecast` columns), so a reviewer can choose.

## Bootstrap prediction intervals

We use a non-parametric bootstrap of demeaned backtest residuals (2,000
replicates, fixed seed) to construct 80% prediction intervals. This avoids
assuming normality, which is risky with 5 backtest residuals per series.

The trade-off: with so few residuals, the bootstrap is itself noisy and
may understate true uncertainty. We report the 80% rather than the 95%
band to be honest about this — wider bands at this sample size are not
meaningfully more informative.

## Limitations and honest caveats

1. **Sample size.** Nine quarters per series is at the lower edge of where
   any seasonal model is defensible. A real Retail Finance team would have
   weekly granularity (~150+ observations) and many years of history.
2. **Q4 derivation.** Q4 FY24 and Q4 FY25 are derived by subtraction
   (annual − Q1 − Q2 − Q3) rather than directly observed, since Apple does
   not file a Q4 10-Q. Mathematically exact but flagged via the `is_derived`
   column in the database for filtering.
3. **No reconciliation between segment and product forecasts.** Sum of
   segment forecasts ($119.6B) and sum of product forecasts ($120.7B) are
   close but not identical — different models, different errors. A more
   sophisticated approach (MinT or OLS reconciliation) would force exact
   coherence; we don't apply it because it requires more historical data
   than we have to estimate the covariance matrix reliably.
4. **No exogenous regressors.** FX rates, tariff levels, macro variables,
   and product launch indicators are knowable in advance and would
   plausibly improve forecast quality. We omit them to keep the model
   transparent and avoid over-engineering on small data; this is an
   obvious next iteration.
5. **The iPhone 17 launch.** Q1 FY26 saw a +23% YoY spike in iPhone
   revenue with no precedent in the training history. The ensemble
   couldn't predict it (and shouldn't have, mechanically). The bias
   correction now assumes that strength carries forward — but this is
   itself a judgment call, not a statistical fact.

## What this is and isn't

This pipeline is **a portfolio demonstration of forecasting methodology**:
data engineering discipline, transparent model selection, walk-forward
validation, ensemble construction, and honest treatment of uncertainty.
It is **not** a production forecasting system Apple's Retail Finance team
would deploy — that would require weekly data, hierarchical reconciliation,
exogenous regressors, automated retraining, and a much richer treatment of
launch effects and macro shocks.

The methodology choices above are made with that honest distinction in
mind: every layer of the pipeline can be defended on its own merits, and
limitations are surfaced rather than hidden.
