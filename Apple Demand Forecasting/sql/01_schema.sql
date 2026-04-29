-- ============================================================================
-- Apple Retail Finance — Analytical Database Schema
-- ============================================================================
-- Author: [Your Name] | Project: Retail Demand Forecasting & Promo Analyzer
-- Source: Apple Inc. Form 10-K, fiscal year ended September 27, 2025
--
-- Design notes:
--   * Star schema — dim tables describe entities, fact tables hold measures.
--   * Period grain is mixed: annual (fact_*_annual) and quarterly (fact_*_quarterly,
--     populated in Chunk 1B once 10-Q data is loaded).
--   * All monetary values stored in USD millions to match 10-K presentation;
--     the convention is documented in column comments to prevent unit drift.
--   * Foreign keys enforced — SQLite requires PRAGMA foreign_keys = ON at
--     connection time (see scripts/build_database.py).
-- ============================================================================

-- ---- Drop existing objects (idempotent rebuilds) ---------------------------
DROP TABLE IF EXISTS fact_geographic_annual;
DROP TABLE IF EXISTS fact_consolidated_pnl_annual;
DROP TABLE IF EXISTS fact_segment_pnl_annual;
DROP TABLE IF EXISTS fact_product_revenue_quarterly;
DROP TABLE IF EXISTS fact_segment_revenue_quarterly;
DROP TABLE IF EXISTS fact_product_revenue_annual;
DROP TABLE IF EXISTS fact_segment_revenue_annual;
DROP TABLE IF EXISTS dim_product_launch;
DROP TABLE IF EXISTS dim_period;
DROP TABLE IF EXISTS dim_product;
DROP TABLE IF EXISTS dim_segment;

-- ============================================================================
-- DIMENSIONS
-- ============================================================================

CREATE TABLE dim_segment (
    segment_id          INTEGER PRIMARY KEY,
    segment_name        TEXT    NOT NULL UNIQUE,
    region_grouping     TEXT    NOT NULL,          -- 'Americas' | 'EMEIA' | 'APAC'
    is_emerging_market  INTEGER NOT NULL DEFAULT 0 -- bool
);

CREATE TABLE dim_product (
    product_id          INTEGER PRIMARY KEY,
    product_category    TEXT    NOT NULL UNIQUE,
    revenue_type        TEXT    NOT NULL,          -- 'Products' | 'Services'
    is_hardware         INTEGER NOT NULL,          -- bool
    typical_launch_qtr  TEXT                       -- e.g. 'Q4' for iPhone
);

CREATE TABLE dim_period (
    period_id           INTEGER PRIMARY KEY,
    fiscal_year         INTEGER NOT NULL,
    fiscal_quarter      TEXT,                      -- NULL for annual rollups
    period_type         TEXT    NOT NULL,          -- 'ANNUAL' | 'QUARTERLY'
    period_end_date     DATE    NOT NULL,
    weeks_in_period     INTEGER NOT NULL,          -- Apple has 52 vs 53 wk yrs
    UNIQUE (fiscal_year, fiscal_quarter, period_type)
);

CREATE TABLE dim_product_launch (
    launch_id              INTEGER PRIMARY KEY AUTOINCREMENT,
    fiscal_year            INTEGER NOT NULL,
    fiscal_quarter         TEXT    NOT NULL,
    product_launched       TEXT    NOT NULL,
    product_category       TEXT    NOT NULL,
    launch_significance    TEXT    NOT NULL        -- 'major' | 'minor'
);

-- ============================================================================
-- FACTS — ANNUAL GRAIN
-- ============================================================================

-- Net sales by reportable segment (10-K Note 13)
CREATE TABLE fact_segment_revenue_annual (
    period_id           INTEGER NOT NULL,
    segment_id          INTEGER NOT NULL,
    net_sales_millions  REAL    NOT NULL,
    PRIMARY KEY (period_id, segment_id),
    FOREIGN KEY (period_id)  REFERENCES dim_period(period_id),
    FOREIGN KEY (segment_id) REFERENCES dim_segment(segment_id)
);

-- Net sales by product category (10-K Note 2)
CREATE TABLE fact_product_revenue_annual (
    period_id           INTEGER NOT NULL,
    product_id          INTEGER NOT NULL,
    net_sales_millions  REAL    NOT NULL,
    PRIMARY KEY (period_id, product_id),
    FOREIGN KEY (period_id)  REFERENCES dim_period(period_id),
    FOREIGN KEY (product_id) REFERENCES dim_product(product_id)
);

-- Full segment P&L incl COGS, S&M, op income (10-K Note 13)
CREATE TABLE fact_segment_pnl_annual (
    period_id                       INTEGER NOT NULL,
    segment_id                      INTEGER NOT NULL,
    net_sales_millions              REAL    NOT NULL,
    cost_of_sales_millions          REAL    NOT NULL,   -- stored negative
    selling_marketing_millions      REAL    NOT NULL,   -- stored negative
    operating_income_millions       REAL    NOT NULL,
    PRIMARY KEY (period_id, segment_id),
    FOREIGN KEY (period_id)  REFERENCES dim_period(period_id),
    FOREIGN KEY (segment_id) REFERENCES dim_segment(segment_id)
);

-- Consolidated income statement (10-K Statements of Operations)
CREATE TABLE fact_consolidated_pnl_annual (
    period_id           INTEGER NOT NULL,
    line_item           TEXT    NOT NULL,
    amount_millions     REAL    NOT NULL,
    PRIMARY KEY (period_id, line_item),
    FOREIGN KEY (period_id) REFERENCES dim_period(period_id)
);

-- Country-level revenue and long-lived assets (10-K Note 13)
CREATE TABLE fact_geographic_annual (
    period_id                       INTEGER NOT NULL,
    country                         TEXT    NOT NULL,
    net_sales_millions              REAL    NOT NULL,
    long_lived_assets_millions      REAL,                -- 2023 not disclosed
    PRIMARY KEY (period_id, country),
    FOREIGN KEY (period_id) REFERENCES dim_period(period_id)
);

-- ============================================================================
-- FACTS — QUARTERLY GRAIN
-- ============================================================================
-- These are populated from XBRL extracts of the four 10-Q filings (Q1-Q3 of
-- FY2024 and FY2025, plus Q1 FY2026), with Q4 derived by subtraction from
-- the 10-K annual totals. See scripts/parse_xbrl_10q.py and
-- scripts/consolidate_quarterly.py.

CREATE TABLE fact_segment_revenue_quarterly (
    period_id           INTEGER NOT NULL,
    segment_id          INTEGER NOT NULL,
    net_sales_millions  REAL    NOT NULL,
    is_derived          INTEGER NOT NULL DEFAULT 0,  -- bool: Q4 was back-calculated
    PRIMARY KEY (period_id, segment_id),
    FOREIGN KEY (period_id)  REFERENCES dim_period(period_id),
    FOREIGN KEY (segment_id) REFERENCES dim_segment(segment_id)
);

CREATE TABLE fact_product_revenue_quarterly (
    period_id           INTEGER NOT NULL,
    product_id          INTEGER NOT NULL,
    net_sales_millions  REAL    NOT NULL,
    is_derived          INTEGER NOT NULL DEFAULT 0,
    PRIMARY KEY (period_id, product_id),
    FOREIGN KEY (period_id)  REFERENCES dim_period(period_id),
    FOREIGN KEY (product_id) REFERENCES dim_product(product_id)
);

-- ============================================================================
-- INDEXES — speed up the queries we'll run repeatedly
-- ============================================================================

CREATE INDEX idx_seg_rev_period   ON fact_segment_revenue_annual(period_id);
CREATE INDEX idx_prod_rev_period  ON fact_product_revenue_annual(period_id);
CREATE INDEX idx_seg_pnl_segment  ON fact_segment_pnl_annual(segment_id);
CREATE INDEX idx_cons_pnl_lineitm ON fact_consolidated_pnl_annual(line_item);
CREATE INDEX idx_geo_country      ON fact_geographic_annual(country);
CREATE INDEX idx_seg_rev_q_period ON fact_segment_revenue_quarterly(period_id);
CREATE INDEX idx_prod_rev_q_perd  ON fact_product_revenue_quarterly(period_id);
