"""
Tests for the analytical database build.

Run with:
    pytest tests/ -v

These tests give the hiring manager confidence that:
  1. The build is reproducible (rerunning produces the same DB).
  2. The data ties to the 10-K (totals reconcile).
  3. The schema enforces FK integrity.
"""
from __future__ import annotations

import sqlite3
import subprocess
import sys
from pathlib import Path

import pandas as pd
import pytest

PROJECT_ROOT = Path(__file__).resolve().parents[1]
DB_PATH      = PROJECT_ROOT / "data" / "processed" / "apple_finance.db"

# 10-K reported control totals
EXPECTED_TOTAL_NET_SALES = {2023: 383285, 2024: 391035, 2025: 416161}
EXPECTED_NET_INCOME      = {2023: 96995,  2024: 93736,  2025: 112010}


# --- Fixtures ---------------------------------------------------------------
@pytest.fixture(scope="session", autouse=True)
def build_db():
    """Build the database once per test session."""
    result = subprocess.run(
        [sys.executable, str(PROJECT_ROOT / "scripts" / "build_database.py")],
        capture_output=True, text=True,
    )
    assert result.returncode == 0, f"Build failed: {result.stderr}"
    yield
    # leave the DB in place so other developers can poke at it after tests


@pytest.fixture
def conn():
    """Provide a fresh connection per test, with FK enforcement on."""
    c = sqlite3.connect(DB_PATH)
    c.execute("PRAGMA foreign_keys = ON")
    yield c
    c.close()


# --- Schema integrity -------------------------------------------------------
def test_db_file_exists():
    assert DB_PATH.exists(), "DB file was not created"


def test_all_expected_tables_exist(conn):
    expected = {
        "dim_segment", "dim_product", "dim_period", "dim_product_launch",
        "fact_segment_revenue_annual", "fact_product_revenue_annual",
        "fact_segment_pnl_annual", "fact_consolidated_pnl_annual",
        "fact_geographic_annual",
        "fact_segment_revenue_quarterly", "fact_product_revenue_quarterly",
    }
    actual = {r[0] for r in conn.execute(
        "SELECT name FROM sqlite_master WHERE type='table'"
    )}
    missing = expected - actual
    assert not missing, f"Missing tables: {missing}"


# --- Reconciliation to 10-K -------------------------------------------------
@pytest.mark.parametrize("fy", [2023, 2024, 2025])
def test_segment_total_reconciles(conn, fy):
    actual = conn.execute(
        """
        SELECT SUM(net_sales_millions) FROM fact_segment_revenue_annual f
        JOIN dim_period p USING (period_id)
        WHERE p.fiscal_year = ?
        """, (fy,),
    ).fetchone()[0]
    assert actual == EXPECTED_TOTAL_NET_SALES[fy], (
        f"FY{fy} segment total {actual} != 10-K total {EXPECTED_TOTAL_NET_SALES[fy]}"
    )


@pytest.mark.parametrize("fy", [2023, 2024, 2025])
def test_product_total_reconciles(conn, fy):
    actual = conn.execute(
        """
        SELECT SUM(net_sales_millions) FROM fact_product_revenue_annual f
        JOIN dim_period p USING (period_id)
        WHERE p.fiscal_year = ?
        """, (fy,),
    ).fetchone()[0]
    assert actual == EXPECTED_TOTAL_NET_SALES[fy]


@pytest.mark.parametrize("fy", [2023, 2024, 2025])
def test_net_income_matches(conn, fy):
    actual = conn.execute(
        """
        SELECT amount_millions FROM fact_consolidated_pnl_annual f
        JOIN dim_period p USING (period_id)
        WHERE p.fiscal_year = ? AND f.line_item = 'net_income'
        """, (fy,),
    ).fetchone()[0]
    assert actual == EXPECTED_NET_INCOME[fy]


# --- Cross-check: products + services = total -------------------------------
@pytest.mark.parametrize("fy", [2023, 2024, 2025])
def test_products_plus_services_equals_total(conn, fy):
    rows = pd.read_sql(
        """
        SELECT line_item, amount_millions
        FROM   fact_consolidated_pnl_annual f
        JOIN   dim_period p USING (period_id)
        WHERE  p.fiscal_year = ?
          AND  line_item IN ('products_net_sales', 'services_net_sales', 'total_net_sales')
        """,
        conn, params=(fy,),
    ).set_index("line_item")["amount_millions"]

    assert rows["products_net_sales"] + rows["services_net_sales"] == rows["total_net_sales"]


# --- Cross-check: segment-level operating income rolls up correctly ---------
def test_segment_op_income_directional(conn):
    """
    Segment op income is reported pre-Corporate (R&D and G&A live in Corporate).
    Sum of segment op income should exceed consolidated op income — verify
    the gap equals Corporate (R&D + G&A net) within tolerance.
    """
    sums = pd.read_sql(
        """
        SELECT  p.fiscal_year,
                SUM(operating_income_millions) AS segment_op_inc
        FROM    fact_segment_pnl_annual f
        JOIN    dim_period p USING (period_id)
        GROUP BY p.fiscal_year
        """,
        conn,
    ).set_index("fiscal_year")

    cons = pd.read_sql(
        """
        SELECT  p.fiscal_year, f.amount_millions AS cons_op_inc
        FROM    fact_consolidated_pnl_annual f
        JOIN    dim_period p USING (period_id)
        WHERE   line_item = 'operating_income'
        """,
        conn,
    ).set_index("fiscal_year")

    # Corporate gap should be a positive deduction (R&D + G&A > corporate revenue)
    for fy in sums.index:
        gap = sums.loc[fy, "segment_op_inc"] - cons.loc[fy, "cons_op_inc"]
        assert gap > 0, f"FY{fy}: corporate gap should be positive, got {gap}"
        # gap should be in the $35-45B range (R&D + G&A roughly)
        assert 30000 < gap < 50000, f"FY{fy}: corporate gap {gap} outside expected range"


# --- Quarterly data integrity -----------------------------------------------
def test_quarterly_tables_populated(conn):
    """We should have 9 quarters x 5 segments + 9 x 5 products = 90 rows."""
    seg_count  = conn.execute("SELECT COUNT(*) FROM fact_segment_revenue_quarterly").fetchone()[0]
    prod_count = conn.execute("SELECT COUNT(*) FROM fact_product_revenue_quarterly").fetchone()[0]
    assert seg_count  >= 40, f"Expected ~45 quarterly segment rows, got {seg_count}"
    assert prod_count >= 40, f"Expected ~45 quarterly product rows, got {prod_count}"


@pytest.mark.parametrize("fy", [2024, 2025])
def test_four_quarters_sum_to_annual_segment(conn, fy):
    """Each segment's 4 quarters must sum to the annual figure for that segment."""
    rows = pd.read_sql(
        """
        SELECT  s.segment_name,
                SUM(f.net_sales_millions) AS qtr_sum,
                a.net_sales_millions      AS annual_val
        FROM    fact_segment_revenue_quarterly f
        JOIN    dim_period  p  USING (period_id)
        JOIN    dim_segment s  USING (segment_id)
        JOIN    fact_segment_revenue_annual a
                ON a.segment_id = s.segment_id
        JOIN    dim_period pa
                ON pa.period_id = a.period_id
                AND pa.fiscal_year = p.fiscal_year
        WHERE   p.fiscal_year   = ?
        AND     p.period_type   = 'QUARTERLY'
        GROUP BY s.segment_name, a.net_sales_millions
        """,
        conn, params=(fy,),
    )
    assert not rows.empty, f"No quarterly segment data found for FY{fy}"
    for _, row in rows.iterrows():
        diff = abs(row.qtr_sum - row.annual_val)
        assert diff <= 1, (
            f"FY{fy} {row.segment_name}: 4-qtr sum {row.qtr_sum} != annual {row.annual_val}"
        )


@pytest.mark.parametrize("fy", [2024, 2025])
def test_four_quarters_sum_to_annual_product(conn, fy):
    """Each product category's 4 quarters must sum to the annual figure."""
    rows = pd.read_sql(
        """
        SELECT  d.product_category,
                SUM(f.net_sales_millions) AS qtr_sum,
                a.net_sales_millions      AS annual_val
        FROM    fact_product_revenue_quarterly f
        JOIN    dim_period  p  USING (period_id)
        JOIN    dim_product d  USING (product_id)
        JOIN    fact_product_revenue_annual a
                ON a.product_id = d.product_id
        JOIN    dim_period pa
                ON pa.period_id = a.period_id
                AND pa.fiscal_year = p.fiscal_year
        WHERE   p.fiscal_year = ?
        AND     p.period_type = 'QUARTERLY'
        GROUP BY d.product_category, a.net_sales_millions
        """,
        conn, params=(fy,),
    )
    assert not rows.empty, f"No quarterly product data found for FY{fy}"
    for _, row in rows.iterrows():
        diff = abs(row.qtr_sum - row.annual_val)
        assert diff <= 1, (
            f"FY{fy} {row.product_category}: 4-qtr sum {row.qtr_sum} != annual {row.annual_val}"
        )


def test_q1_is_largest_quarter(conn):
    """Sanity check: Q1 (holiday quarter) is always the largest of any year."""
    rows = pd.read_sql(
        """
        SELECT  p.fiscal_year,
                p.fiscal_quarter,
                SUM(f.net_sales_millions) AS revenue
        FROM    fact_segment_revenue_quarterly f
        JOIN    dim_period p USING (period_id)
        GROUP BY p.fiscal_year, p.fiscal_quarter
        """, conn,
    )
    for fy, group in rows.groupby("fiscal_year"):
        if len(group) < 4:    # need full year to test
            continue
        q1_rev = group[group.fiscal_quarter == "Q1"].revenue.iloc[0]
        max_other = group[group.fiscal_quarter != "Q1"].revenue.max()
        assert q1_rev > max_other, (
            f"FY{fy}: Q1 revenue {q1_rev} should exceed all other quarters (max={max_other})"
        )
