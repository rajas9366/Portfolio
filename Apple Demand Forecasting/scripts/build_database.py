"""
build_database.py
=================
Constructs the analytical SQLite database from raw CSV extracts of Apple's
FY2025 Form 10-K.

Pipeline:
    1. Drop & recreate schema (sql/01_schema.sql).
    2. Populate dimensions (segment, product, period, product_launch).
    3. Load fact tables from data/raw/*.csv.
    4. Run integrity checks (totals reconcile to 10-K; no orphaned FKs).

Usage:
    python scripts/build_database.py
    python scripts/build_database.py --db-path data/processed/apple_finance.db
    python scripts/build_database.py --verbose

Exit codes:
    0  success
    1  schema or load error
    2  reconciliation check failed
"""
from __future__ import annotations

import argparse
import logging
import sqlite3
import sys
from pathlib import Path

import pandas as pd

# ---- Project paths ---------------------------------------------------------
PROJECT_ROOT = Path(__file__).resolve().parents[1]
DATA_RAW     = PROJECT_ROOT / "data" / "raw"
DATA_PROC    = PROJECT_ROOT / "data" / "processed"
SQL_DIR      = PROJECT_ROOT / "sql"

DEFAULT_DB   = DATA_PROC / "apple_finance.db"

# 10-K reported control totals — used in reconciliation
EXPECTED_TOTAL_NET_SALES = {2023: 383285, 2024: 391035, 2025: 416161}

# ---- Logging ---------------------------------------------------------------
logger = logging.getLogger("build_database")


# =============================================================================
# Schema and dimension loading
# =============================================================================
def execute_schema(conn: sqlite3.Connection) -> None:
    """Drop and recreate all tables from the DDL file."""
    schema_path = SQL_DIR / "01_schema.sql"
    logger.info("Executing schema: %s", schema_path)
    conn.executescript(schema_path.read_text())
    conn.commit()


def load_dim_segment(conn: sqlite3.Connection) -> None:
    """Hard-coded segment dimension. Region groupings used in geo rollups."""
    segments = [
        (1, "Americas",              "Americas", 0),
        (2, "Europe",                "EMEIA",    0),  # incl India/MEA
        (3, "Greater China",         "APAC",     1),
        (4, "Japan",                 "APAC",     0),
        (5, "Rest of Asia Pacific",  "APAC",     1),
    ]
    conn.executemany(
        "INSERT INTO dim_segment VALUES (?, ?, ?, ?)", segments
    )
    logger.info("Loaded %d rows into dim_segment", len(segments))


def load_dim_product(conn: sqlite3.Connection) -> None:
    """Product category dimension. typical_launch_qtr seeds the seasonality model."""
    products = [
        (1, "iPhone",                          "Products", 1, "Q4"),
        (2, "Mac",                             "Products", 1, "Q1"),  # late-CY refresh
        (3, "iPad",                            "Products", 1, "Q2"),
        (4, "Wearables Home and Accessories",  "Products", 1, "Q4"),
        (5, "Services",                        "Services", 0, None),
    ]
    conn.executemany(
        "INSERT INTO dim_product VALUES (?, ?, ?, ?, ?)", products
    )
    logger.info("Loaded %d rows into dim_product", len(products))


def load_dim_period(conn: sqlite3.Connection) -> None:
    """
    Period dimension covering FY23–FY25 annual rollups and quarterly rows
    for every quarter where we have data. Note: FY2023 was a 53-week year;
    FY2024 and FY2025 were 52-week years.

    Convention for period_id:
      * Annual rows:    fiscal_year (e.g. 2025)
      * Quarterly rows: fiscal_year * 10 + quarter_num (e.g. 20251 = FY2025 Q1)
    """
    periods: list[tuple] = [
        # period_id, fiscal_year, fiscal_quarter, period_type, period_end_date, weeks
        (2023, 2023, None, "ANNUAL",    "2023-09-30", 53),
        (2024, 2024, None, "ANNUAL",    "2024-09-28", 52),
        (2025, 2025, None, "ANNUAL",    "2025-09-27", 52),
    ]
    # Quarterly periods we have actual data for (FY24 Q1-Q4, FY25 Q1-Q4, FY26 Q1)
    quarterly = [
        (20241, 2024, "Q1", "QUARTERLY", "2023-12-30", 13),
        (20242, 2024, "Q2", "QUARTERLY", "2024-03-30", 13),
        (20243, 2024, "Q3", "QUARTERLY", "2024-06-29", 13),
        (20244, 2024, "Q4", "QUARTERLY", "2024-09-28", 13),
        (20251, 2025, "Q1", "QUARTERLY", "2024-12-28", 13),
        (20252, 2025, "Q2", "QUARTERLY", "2025-03-29", 13),
        (20253, 2025, "Q3", "QUARTERLY", "2025-06-28", 13),
        (20254, 2025, "Q4", "QUARTERLY", "2025-09-27", 13),
        (20261, 2026, "Q1", "QUARTERLY", "2025-12-27", 13),
    ]
    periods.extend(quarterly)
    conn.executemany(
        "INSERT INTO dim_period VALUES (?, ?, ?, ?, ?, ?)", periods
    )
    logger.info("Loaded %d rows into dim_period", len(periods))


# =============================================================================
# Fact loading
# =============================================================================
def _period_id_for_year(year: int) -> int:
    """ANNUAL period_ids equal the fiscal year by convention (see load_dim_period)."""
    return int(year)


def load_fact_segment_revenue(conn: sqlite3.Connection) -> None:
    df = pd.read_csv(DATA_RAW / "segment_revenue_annual.csv")
    seg_map = dict(conn.execute(
        "SELECT segment_name, segment_id FROM dim_segment"
    ).fetchall())

    rows = [
        (_period_id_for_year(r.fiscal_year), seg_map[r.segment], float(r.net_sales_millions))
        for r in df.itertuples(index=False)
    ]
    conn.executemany(
        "INSERT INTO fact_segment_revenue_annual VALUES (?, ?, ?)", rows
    )
    logger.info("Loaded %d rows into fact_segment_revenue_annual", len(rows))


def load_fact_product_revenue(conn: sqlite3.Connection) -> None:
    df = pd.read_csv(DATA_RAW / "product_revenue_annual.csv")
    prod_map = dict(conn.execute(
        "SELECT product_category, product_id FROM dim_product"
    ).fetchall())

    rows = [
        (_period_id_for_year(r.fiscal_year), prod_map[r.product_category], float(r.net_sales_millions))
        for r in df.itertuples(index=False)
    ]
    conn.executemany(
        "INSERT INTO fact_product_revenue_annual VALUES (?, ?, ?)", rows
    )
    logger.info("Loaded %d rows into fact_product_revenue_annual", len(rows))


def load_fact_segment_pnl(conn: sqlite3.Connection) -> None:
    df = pd.read_csv(DATA_RAW / "segment_pnl_annual.csv")
    seg_map = dict(conn.execute(
        "SELECT segment_name, segment_id FROM dim_segment"
    ).fetchall())

    rows = [
        (
            _period_id_for_year(r.fiscal_year),
            seg_map[r.segment],
            float(r.net_sales),
            float(r.cost_of_sales),
            float(r.selling_and_marketing),
            float(r.operating_income),
        )
        for r in df.itertuples(index=False)
    ]
    conn.executemany(
        "INSERT INTO fact_segment_pnl_annual VALUES (?, ?, ?, ?, ?, ?)", rows
    )
    logger.info("Loaded %d rows into fact_segment_pnl_annual", len(rows))


def load_fact_consolidated_pnl(conn: sqlite3.Connection) -> None:
    df = pd.read_csv(DATA_RAW / "consolidated_pnl_annual.csv")
    rows = [
        (_period_id_for_year(r.fiscal_year), r.line_item, float(r.amount_millions))
        for r in df.itertuples(index=False)
    ]
    conn.executemany(
        "INSERT INTO fact_consolidated_pnl_annual VALUES (?, ?, ?)", rows
    )
    logger.info("Loaded %d rows into fact_consolidated_pnl_annual", len(rows))


def load_fact_geographic(conn: sqlite3.Connection) -> None:
    df = pd.read_csv(DATA_RAW / "geographic_detail_annual.csv")
    rows = [
        (
            _period_id_for_year(r.fiscal_year),
            r.country,
            float(r.net_sales_millions),
            None if pd.isna(r.long_lived_assets_millions) else float(r.long_lived_assets_millions),
        )
        for r in df.itertuples(index=False)
    ]
    conn.executemany(
        "INSERT INTO fact_geographic_annual VALUES (?, ?, ?, ?)", rows
    )
    logger.info("Loaded %d rows into fact_geographic_annual", len(rows))


def _period_id_for_quarter(fiscal_year: int, fiscal_quarter: str) -> int:
    """
    Encode (fiscal_year, fiscal_quarter) as a single int period_id.
    Mirrors the convention defined in load_dim_period().
    """
    return int(fiscal_year) * 10 + int(fiscal_quarter[1])


def load_fact_segment_revenue_quarterly(conn: sqlite3.Connection) -> None:
    """Load 9 quarters x 5 segments = up to 45 rows from XBRL extracts."""
    path = DATA_RAW / "segment_revenue_quarterly.csv"
    if not path.exists():
        logger.warning("Quarterly file %s not found — skipping quarterly load. "
                       "Run scripts/parse_xbrl_10q.py + consolidate_quarterly.py first.", path)
        return
    df = pd.read_csv(path)
    seg_map = dict(conn.execute(
        "SELECT segment_name, segment_id FROM dim_segment"
    ).fetchall())

    rows = [
        (
            _period_id_for_quarter(r.fiscal_year, r.fiscal_quarter),
            seg_map[r.segment],
            float(r.net_sales_millions),
            int(bool(r.derived)),
        )
        for r in df.itertuples(index=False)
    ]
    conn.executemany(
        "INSERT INTO fact_segment_revenue_quarterly VALUES (?, ?, ?, ?)", rows
    )
    logger.info("Loaded %d rows into fact_segment_revenue_quarterly", len(rows))


def load_fact_product_revenue_quarterly(conn: sqlite3.Connection) -> None:
    """Load 9 quarters x 5 products = up to 45 rows from XBRL extracts."""
    path = DATA_RAW / "product_revenue_quarterly.csv"
    if not path.exists():
        logger.warning("Quarterly file %s not found — skipping quarterly load.", path)
        return
    df = pd.read_csv(path)
    prod_map = dict(conn.execute(
        "SELECT product_category, product_id FROM dim_product"
    ).fetchall())

    rows = [
        (
            _period_id_for_quarter(r.fiscal_year, r.fiscal_quarter),
            prod_map[r.product_category],
            float(r.net_sales_millions),
            int(bool(r.derived)),
        )
        for r in df.itertuples(index=False)
    ]
    conn.executemany(
        "INSERT INTO fact_product_revenue_quarterly VALUES (?, ?, ?, ?)", rows
    )
    logger.info("Loaded %d rows into fact_product_revenue_quarterly", len(rows))


def load_dim_product_launch(conn: sqlite3.Connection) -> None:
    df = pd.read_csv(DATA_RAW / "product_launch_calendar.csv")
    rows = [
        (None, r.fiscal_year, r.fiscal_quarter, r.product_launched,
         r.product_category, r.launch_significance)
        for r in df.itertuples(index=False)
    ]
    conn.executemany(
        "INSERT INTO dim_product_launch VALUES (?, ?, ?, ?, ?, ?)", rows
    )
    logger.info("Loaded %d rows into dim_product_launch", len(rows))


# =============================================================================
# Reconciliation — does loaded data tie back to the 10-K?
# =============================================================================
def reconcile(conn: sqlite3.Connection) -> bool:
    """
    Verify that segment totals and product totals both reconcile to total
    net sales reported on the 10-K Statements of Operations. If anything
    breaks, we want to know now — before forecasts are built on bad data.

    Also reconciles quarterly data: 4 quarters per fiscal year should sum
    to the annual figures.
    """
    ok = True

    # --- segment total ties to consolidated total ---------------------------
    seg_totals = pd.read_sql(
        """
        SELECT  p.fiscal_year                      AS fiscal_year,
                SUM(f.net_sales_millions)          AS segment_total
        FROM    fact_segment_revenue_annual f
        JOIN    dim_period p USING (period_id)
        GROUP BY p.fiscal_year
        """,
        conn,
    )

    for _, row in seg_totals.iterrows():
        expected = EXPECTED_TOTAL_NET_SALES[int(row.fiscal_year)]
        diff = abs(row.segment_total - expected)
        if diff > 1:  # tolerate <$1M rounding; should be exact
            logger.error(
                "FY%d segment total %.0f != 10-K total %.0f (diff %.0f)",
                row.fiscal_year, row.segment_total, expected, diff,
            )
            ok = False
        else:
            logger.info(
                "FY%d segment total reconciles: $%.0fM",
                row.fiscal_year, row.segment_total,
            )

    # --- product category total ties to consolidated total ------------------
    prod_totals = pd.read_sql(
        """
        SELECT  p.fiscal_year                      AS fiscal_year,
                SUM(f.net_sales_millions)          AS product_total
        FROM    fact_product_revenue_annual f
        JOIN    dim_period p USING (period_id)
        GROUP BY p.fiscal_year
        """,
        conn,
    )

    for _, row in prod_totals.iterrows():
        expected = EXPECTED_TOTAL_NET_SALES[int(row.fiscal_year)]
        diff = abs(row.product_total - expected)
        if diff > 1:
            logger.error(
                "FY%d product total %.0f != 10-K total %.0f (diff %.0f)",
                row.fiscal_year, row.product_total, expected, diff,
            )
            ok = False
        else:
            logger.info(
                "FY%d product total reconciles: $%.0fM",
                row.fiscal_year, row.product_total,
            )

    # --- quarterly: 4 quarters sum to annual --------------------------------
    qtr_seg = pd.read_sql(
        """
        SELECT  p.fiscal_year, s.segment_name,
                SUM(f.net_sales_millions) AS qtr_sum,
                COUNT(*)                  AS n_quarters
        FROM    fact_segment_revenue_quarterly f
        JOIN    dim_period  p USING (period_id)
        JOIN    dim_segment s USING (segment_id)
        WHERE   p.period_type = 'QUARTERLY'
        GROUP BY p.fiscal_year, s.segment_name
        HAVING n_quarters = 4
        """,
        conn,
    )

    annual_seg = pd.read_sql(
        """
        SELECT  p.fiscal_year, s.segment_name, f.net_sales_millions AS annual_val
        FROM    fact_segment_revenue_annual f
        JOIN    dim_period  p USING (period_id)
        JOIN    dim_segment s USING (segment_id)
        """,
        conn,
    )

    if not qtr_seg.empty:
        merged = qtr_seg.merge(annual_seg, on=["fiscal_year", "segment_name"])
        for _, row in merged.iterrows():
            diff = abs(row.qtr_sum - row.annual_val)
            if diff > 1:
                logger.error(
                    "FY%d %s quarterly sum %.0f != annual %.0f (diff %.0f)",
                    row.fiscal_year, row.segment_name, row.qtr_sum, row.annual_val, diff,
                )
                ok = False
            else:
                logger.info(
                    "FY%d %s quarterly sum reconciles: $%.0fM",
                    row.fiscal_year, row.segment_name, row.annual_val,
                )

    return ok


# =============================================================================
# Orchestration
# =============================================================================
def build(db_path: Path) -> int:
    db_path.parent.mkdir(parents=True, exist_ok=True)
    if db_path.exists():
        logger.info("Removing existing DB at %s", db_path)
        db_path.unlink()

    conn = sqlite3.connect(db_path)
    conn.execute("PRAGMA foreign_keys = ON")

    try:
        execute_schema(conn)
        load_dim_segment(conn)
        load_dim_product(conn)
        load_dim_period(conn)
        load_dim_product_launch(conn)
        load_fact_segment_revenue(conn)
        load_fact_product_revenue(conn)
        load_fact_segment_pnl(conn)
        load_fact_consolidated_pnl(conn)
        load_fact_geographic(conn)
        load_fact_segment_revenue_quarterly(conn)
        load_fact_product_revenue_quarterly(conn)
        conn.commit()

        if not reconcile(conn):
            logger.error("Reconciliation FAILED")
            return 2

        logger.info("Build complete: %s", db_path)
        return 0
    finally:
        conn.close()


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__.split("\n")[1])
    parser.add_argument(
        "--db-path", type=Path, default=DEFAULT_DB,
        help=f"Output SQLite database path (default: {DEFAULT_DB})",
    )
    parser.add_argument(
        "--verbose", "-v", action="store_true", help="Enable DEBUG logging",
    )
    args = parser.parse_args()

    logging.basicConfig(
        level=logging.DEBUG if args.verbose else logging.INFO,
        format="%(asctime)s [%(levelname)s] %(message)s",
        datefmt="%H:%M:%S",
    )
    return build(args.db_path)


if __name__ == "__main__":
    sys.exit(main())
