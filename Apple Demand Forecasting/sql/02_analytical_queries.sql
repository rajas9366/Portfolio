-- ============================================================================
-- Apple Retail Finance — Analytical Queries
-- ============================================================================
-- Each query answers a specific business question a Retail Finance analyst
-- would actually face. Run individually in DB Browser for SQLite, or via:
--     sqlite3 data/processed/apple_finance.db < sql/02_analytical_queries.sql
-- ============================================================================


-- ----------------------------------------------------------------------------
-- Q1. Revenue mix shift: how is the Products vs Services balance evolving?
-- ----------------------------------------------------------------------------
-- Why this matters: Services has nearly 2x the gross margin of Products
-- (75% vs 37%). Mix shift to Services structurally lifts company-wide GM
-- regardless of pricing actions on hardware.
WITH pnl AS (
    SELECT  p.fiscal_year,
            f.line_item,
            f.amount_millions
    FROM    fact_consolidated_pnl_annual f
    JOIN    dim_period p USING (period_id)
    WHERE   f.line_item IN ('products_net_sales',
                            'services_net_sales',
                            'total_net_sales')
)
SELECT
    fiscal_year,
    MAX(CASE WHEN line_item = 'products_net_sales' THEN amount_millions END)  AS products_revenue,
    MAX(CASE WHEN line_item = 'services_net_sales' THEN amount_millions END)  AS services_revenue,
    MAX(CASE WHEN line_item = 'total_net_sales'    THEN amount_millions END)  AS total_revenue,
    ROUND(
        100.0 * MAX(CASE WHEN line_item = 'services_net_sales' THEN amount_millions END)
              / MAX(CASE WHEN line_item = 'total_net_sales'    THEN amount_millions END),
        2
    ) AS services_mix_pct
FROM    pnl
GROUP BY fiscal_year
ORDER BY fiscal_year;


-- ----------------------------------------------------------------------------
-- Q2. YoY growth by segment with 3-year trend
-- ----------------------------------------------------------------------------
-- Why this matters: identifies which markets are accelerating, decelerating,
-- or in decline. Greater China deceleration is the headline story FY23–FY25.
WITH segment_revenue AS (
    SELECT  p.fiscal_year,
            s.segment_name,
            f.net_sales_millions
    FROM    fact_segment_revenue_annual f
    JOIN    dim_segment s USING (segment_id)
    JOIN    dim_period  p USING (period_id)
)
SELECT
    segment_name,
    fiscal_year,
    net_sales_millions,
    LAG(net_sales_millions) OVER (
        PARTITION BY segment_name ORDER BY fiscal_year
    ) AS prior_year_sales,
    ROUND(
        100.0 * (net_sales_millions - LAG(net_sales_millions) OVER (
            PARTITION BY segment_name ORDER BY fiscal_year
        )) / LAG(net_sales_millions) OVER (
            PARTITION BY segment_name ORDER BY fiscal_year
        ),
        2
    ) AS yoy_growth_pct
FROM    segment_revenue
ORDER BY segment_name, fiscal_year;


-- ----------------------------------------------------------------------------
-- Q3. Segment operating margin trajectory
-- ----------------------------------------------------------------------------
-- Why this matters: Greater China's revenue is dropping AND its operating
-- margin compressed — flagged in 10-K Item 7. This query quantifies the
-- "double whammy" Retail Finance leadership cares about.
SELECT
    p.fiscal_year,
    s.segment_name,
    f.net_sales_millions                                                AS net_sales,
    f.operating_income_millions                                         AS op_income,
    ROUND(100.0 * f.operating_income_millions / f.net_sales_millions, 2) AS op_margin_pct
FROM    fact_segment_pnl_annual f
JOIN    dim_segment s USING (segment_id)
JOIN    dim_period  p USING (period_id)
ORDER BY s.segment_name, p.fiscal_year;


-- ----------------------------------------------------------------------------
-- Q4. Product category contribution and growth
-- ----------------------------------------------------------------------------
-- Why this matters: Wearables has now declined two years running (-7%, -4%).
-- Services growth is what's masking hardware softness in the headline number.
WITH product_revenue AS (
    SELECT  p.fiscal_year,
            d.product_category,
            d.revenue_type,
            f.net_sales_millions
    FROM    fact_product_revenue_annual f
    JOIN    dim_product d USING (product_id)
    JOIN    dim_period  p USING (period_id)
)
SELECT
    fiscal_year,
    product_category,
    revenue_type,
    net_sales_millions,
    ROUND(
        100.0 * net_sales_millions / SUM(net_sales_millions) OVER (PARTITION BY fiscal_year),
        2
    ) AS pct_of_total,
    ROUND(
        100.0 * (net_sales_millions - LAG(net_sales_millions) OVER (
            PARTITION BY product_category ORDER BY fiscal_year
        )) / LAG(net_sales_millions) OVER (
            PARTITION BY product_category ORDER BY fiscal_year
        ),
        2
    ) AS yoy_growth_pct
FROM    product_revenue
ORDER BY fiscal_year DESC, net_sales_millions DESC;


-- ----------------------------------------------------------------------------
-- Q5. Gross margin walk: Products vs Services
-- ----------------------------------------------------------------------------
-- Why this matters: Services GM expanded from 70.8% -> 75.4% in two years
-- (+460 bps). Products GM is range-bound at ~37%. This is the core driver
-- of consolidated GM expansion from 44.1% to 46.9%.
WITH gm AS (
    SELECT  p.fiscal_year,
            f.line_item,
            f.amount_millions
    FROM    fact_consolidated_pnl_annual f
    JOIN    dim_period p USING (period_id)
    WHERE   f.line_item IN ('products_net_sales', 'services_net_sales',
                            'products_gross_margin', 'services_gross_margin',
                            'total_net_sales', 'total_gross_margin')
)
SELECT
    fiscal_year,
    ROUND(100.0 * MAX(CASE WHEN line_item='products_gross_margin' THEN amount_millions END)
                / MAX(CASE WHEN line_item='products_net_sales'    THEN amount_millions END), 2) AS products_gm_pct,
    ROUND(100.0 * MAX(CASE WHEN line_item='services_gross_margin' THEN amount_millions END)
                / MAX(CASE WHEN line_item='services_net_sales'    THEN amount_millions END), 2) AS services_gm_pct,
    ROUND(100.0 * MAX(CASE WHEN line_item='total_gross_margin'    THEN amount_millions END)
                / MAX(CASE WHEN line_item='total_net_sales'       THEN amount_millions END), 2) AS total_gm_pct
FROM    gm
GROUP BY fiscal_year
ORDER BY fiscal_year;


-- ----------------------------------------------------------------------------
-- Q6. China concentration and deleveraging
-- ----------------------------------------------------------------------------
-- Why this matters: Long-lived assets in China dropped from $4.8B to $3.6B
-- (-25%) in one year while US assets grew from $35.7B to $40.3B (+13%).
-- Apple is physically deleveraging from China — material to tariff scenarios.
SELECT
    p.fiscal_year,
    f.country,
    f.net_sales_millions,
    f.long_lived_assets_millions,
    ROUND(100.0 * f.net_sales_millions
        / SUM(f.net_sales_millions) OVER (PARTITION BY p.fiscal_year), 2) AS pct_of_total_revenue
FROM    fact_geographic_annual f
JOIN    dim_period p USING (period_id)
ORDER BY p.fiscal_year DESC, f.net_sales_millions DESC;


-- ----------------------------------------------------------------------------
-- Q7. Operating leverage check
-- ----------------------------------------------------------------------------
-- Why this matters: R&D and SG&A both grew ~8% while revenue grew 6%.
-- Slight operating expense deleverage — worth flagging in commentary.
WITH cons AS (
    SELECT  p.fiscal_year, f.line_item, f.amount_millions
    FROM    fact_consolidated_pnl_annual f
    JOIN    dim_period p USING (period_id)
    WHERE   f.line_item IN ('total_net_sales',
                            'research_and_development',
                            'selling_general_administrative',
                            'operating_income')
)
SELECT
    fiscal_year,
    MAX(CASE WHEN line_item='total_net_sales' THEN amount_millions END)        AS revenue,
    MAX(CASE WHEN line_item='research_and_development' THEN amount_millions END)        AS rd,
    MAX(CASE WHEN line_item='selling_general_administrative' THEN amount_millions END)  AS sga,
    MAX(CASE WHEN line_item='operating_income' THEN amount_millions END)       AS op_inc,
    ROUND(100.0 * MAX(CASE WHEN line_item='research_and_development' THEN amount_millions END)
                / MAX(CASE WHEN line_item='total_net_sales' THEN amount_millions END), 2)        AS rd_pct_revenue,
    ROUND(100.0 * MAX(CASE WHEN line_item='selling_general_administrative' THEN amount_millions END)
                / MAX(CASE WHEN line_item='total_net_sales' THEN amount_millions END), 2)        AS sga_pct_revenue,
    ROUND(100.0 * MAX(CASE WHEN line_item='operating_income' THEN amount_millions END)
                / MAX(CASE WHEN line_item='total_net_sales' THEN amount_millions END), 2)        AS op_margin_pct
FROM    cons
GROUP BY fiscal_year
ORDER BY fiscal_year;


-- ----------------------------------------------------------------------------
-- Q8. Two-year CAGR by segment (forecast baseline)
-- ----------------------------------------------------------------------------
-- Why this matters: provides a naive baseline forecast that the ML model
-- in Chunk 2 needs to beat. Anything beating "extrapolate 2yr CAGR" earns
-- its keep; anything worse should be discarded.
WITH endpoints AS (
    SELECT
        s.segment_name,
        MAX(CASE WHEN p.fiscal_year = 2023 THEN f.net_sales_millions END) AS rev_2023,
        MAX(CASE WHEN p.fiscal_year = 2025 THEN f.net_sales_millions END) AS rev_2025
    FROM    fact_segment_revenue_annual f
    JOIN    dim_segment s USING (segment_id)
    JOIN    dim_period  p USING (period_id)
    GROUP BY s.segment_name
)
SELECT
    segment_name,
    rev_2023,
    rev_2025,
    ROUND(100.0 * (POWER(rev_2025 * 1.0 / rev_2023, 0.5) - 1), 2) AS cagr_2yr_pct,
    ROUND(rev_2025 * POWER(rev_2025 * 1.0 / rev_2023, 0.5), 0)    AS naive_fy2026_forecast
FROM    endpoints
ORDER BY rev_2025 DESC;
