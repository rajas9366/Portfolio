-- ============================================================================
-- Apple Retail Finance — Quarterly Analytical Queries
-- ============================================================================
-- These queries exploit the quarterly grain loaded from XBRL extracts of
-- Apple's 10-Q filings. They produce the kind of seasonality and YoY-by-quarter
-- analysis a Retail Finance team uses for weekly/quarterly forecasting.
-- ============================================================================


-- ----------------------------------------------------------------------------
-- Q9. Quarterly revenue time series (segment x quarter)
-- ----------------------------------------------------------------------------
-- Why this matters: this is the core forecast input — 9 quarters of segment
-- revenue history. Seasonality is striking: Q1 is consistently 30%+ of the
-- year for every segment.
SELECT
    p.fiscal_year,
    p.fiscal_quarter,
    s.segment_name,
    f.net_sales_millions,
    f.is_derived
FROM    fact_segment_revenue_quarterly f
JOIN    dim_period  p USING (period_id)
JOIN    dim_segment s USING (segment_id)
ORDER BY s.segment_name, p.fiscal_year, p.fiscal_quarter;


-- ----------------------------------------------------------------------------
-- Q10. Seasonality coefficients by segment
-- ----------------------------------------------------------------------------
-- Why this matters: quantifies "how holiday-skewed is each segment". A pure
-- seasonal-naive forecast multiplies an annual estimate by these coefficients.
-- Greater China is the most holiday-dependent segment (37%+ of year in Q1).
WITH avg_by_quarter AS (
    SELECT
        s.segment_name,
        p.fiscal_quarter,
        AVG(f.net_sales_millions) AS avg_qtr_revenue
    FROM    fact_segment_revenue_quarterly f
    JOIN    dim_period  p USING (period_id)
    JOIN    dim_segment s USING (segment_id)
    WHERE   p.fiscal_year IN (2024, 2025)   -- complete years only
    GROUP BY s.segment_name, p.fiscal_quarter
),
annual_avg AS (
    SELECT  segment_name, SUM(avg_qtr_revenue) AS annual
    FROM    avg_by_quarter
    GROUP BY segment_name
)
SELECT
    a.segment_name,
    a.fiscal_quarter,
    ROUND(a.avg_qtr_revenue,        0) AS avg_revenue_millions,
    ROUND(100.0 * a.avg_qtr_revenue / b.annual, 2) AS pct_of_year
FROM    avg_by_quarter a
JOIN    annual_avg     b USING (segment_name)
ORDER BY a.segment_name, a.fiscal_quarter;


-- ----------------------------------------------------------------------------
-- Q11. iPhone quarterly trajectory — the launch-cycle signature
-- ----------------------------------------------------------------------------
-- Why this matters: iPhone is 50% of revenue and is highly cyclical around
-- the September flagship launch. Q1 (holiday quarter following launch) is
-- always the peak. This query exposes the launch lift effect that the
-- Prophet model will need to capture as a regressor.
SELECT
    p.fiscal_year,
    p.fiscal_quarter,
    f.net_sales_millions                                                            AS iphone_revenue,
    LAG(f.net_sales_millions, 4) OVER (ORDER BY p.fiscal_year, p.fiscal_quarter)    AS year_ago,
    ROUND(
        100.0 * (f.net_sales_millions - LAG(f.net_sales_millions, 4) OVER (
            ORDER BY p.fiscal_year, p.fiscal_quarter
        )) / LAG(f.net_sales_millions, 4) OVER (
            ORDER BY p.fiscal_year, p.fiscal_quarter
        ), 2
    ) AS yoy_growth_pct
FROM    fact_product_revenue_quarterly f
JOIN    dim_period  p USING (period_id)
JOIN    dim_product d USING (product_id)
WHERE   d.product_category = 'iPhone'
ORDER BY p.fiscal_year, p.fiscal_quarter;


-- ----------------------------------------------------------------------------
-- Q12. Greater China quarterly inflection check
-- ----------------------------------------------------------------------------
-- Why this matters: the FY24-FY25 China narrative was "structural decline".
-- Q1 FY26 result of $25.5B (vs $18.5B prior-year quarter, +38% YoY) is the
-- first material reversal in 8 quarters. This is exactly the kind of signal
-- a Retail Finance team would flag in a weekly forecast review.
SELECT
    p.fiscal_year,
    p.fiscal_quarter,
    f.net_sales_millions,
    LAG(f.net_sales_millions, 4) OVER (ORDER BY p.fiscal_year, p.fiscal_quarter) AS yr_ago,
    ROUND(
        100.0 * (f.net_sales_millions - LAG(f.net_sales_millions, 4) OVER (
            ORDER BY p.fiscal_year, p.fiscal_quarter
        )) / LAG(f.net_sales_millions, 4) OVER (
            ORDER BY p.fiscal_year, p.fiscal_quarter
        ), 2
    ) AS yoy_growth_pct,
    -- 4-quarter trailing average smooths through seasonality
    ROUND(AVG(f.net_sales_millions) OVER (
        ORDER BY p.fiscal_year, p.fiscal_quarter
        ROWS BETWEEN 3 PRECEDING AND CURRENT ROW
    ), 0) AS ttm_avg_revenue
FROM    fact_segment_revenue_quarterly f
JOIN    dim_period  p USING (period_id)
JOIN    dim_segment s USING (segment_id)
WHERE   s.segment_name = 'Greater China'
ORDER BY p.fiscal_year, p.fiscal_quarter;


-- ----------------------------------------------------------------------------
-- Q13. Services growth — the consistent story
-- ----------------------------------------------------------------------------
-- Why this matters: while hardware oscillates, Services has grown YoY for
-- every quarter in our data. This stability is what makes Services such a
-- good baseline for forecasting — low variance, high trend confidence.
SELECT
    p.fiscal_year,
    p.fiscal_quarter,
    f.net_sales_millions AS services_revenue,
    LAG(f.net_sales_millions, 4) OVER (ORDER BY p.fiscal_year, p.fiscal_quarter) AS year_ago,
    ROUND(
        100.0 * (f.net_sales_millions - LAG(f.net_sales_millions, 4) OVER (
            ORDER BY p.fiscal_year, p.fiscal_quarter
        )) / LAG(f.net_sales_millions, 4) OVER (
            ORDER BY p.fiscal_year, p.fiscal_quarter
        ), 2
    ) AS yoy_growth_pct
FROM    fact_product_revenue_quarterly f
JOIN    dim_period  p USING (period_id)
JOIN    dim_product d USING (product_id)
WHERE   d.product_category = 'Services'
ORDER BY p.fiscal_year, p.fiscal_quarter;


-- ----------------------------------------------------------------------------
-- Q14. Forecasting feature view — wide table with engineered features
-- ----------------------------------------------------------------------------
-- Why this matters: this is the table the ML model in Chunk 2 will read.
-- It includes the lag features, rolling means, and dummy flags the model
-- needs without the notebook having to compute them in pandas.
CREATE VIEW IF NOT EXISTS v_segment_forecast_features AS
SELECT
    p.period_id,
    p.fiscal_year,
    p.fiscal_quarter,
    p.period_end_date,
    s.segment_name,
    f.net_sales_millions                                                              AS y,
    LAG(f.net_sales_millions, 1) OVER (PARTITION BY s.segment_name ORDER BY p.period_id) AS lag_1q,
    LAG(f.net_sales_millions, 4) OVER (PARTITION BY s.segment_name ORDER BY p.period_id) AS lag_4q,
    AVG(f.net_sales_millions) OVER (
        PARTITION BY s.segment_name ORDER BY p.period_id
        ROWS BETWEEN 3 PRECEDING AND 1 PRECEDING
    ) AS rolling_4q_mean_lag1,
    CASE WHEN p.fiscal_quarter = 'Q1' THEN 1 ELSE 0 END AS is_holiday_quarter,
    CASE WHEN p.fiscal_quarter = 'Q4' THEN 1 ELSE 0 END AS is_iphone_launch_quarter,
    s.is_emerging_market
FROM    fact_segment_revenue_quarterly f
JOIN    dim_period  p USING (period_id)
JOIN    dim_segment s USING (segment_id);

-- Test the view
SELECT * FROM v_segment_forecast_features
WHERE  segment_name = 'Greater China'
ORDER BY period_id;
