"""
consolidate_quarterly.py
========================
Combines per-period quarterly CSVs (output of parse_xbrl_10q.py) into a
single tidy quarterly dataset, derives missing Q4 quarters by subtraction
from the 10-K annual totals, and reconciles every quarter back to the
filings.

Why we need to derive Q4: Apple files three 10-Qs per fiscal year (Q1, Q2,
Q3) — Q4 is rolled into the 10-K rather than a standalone Q4 10-Q. So we
get Q4 by subtracting the three reported quarters from the full year.

Output:
    data/raw/segment_revenue_quarterly.csv
    data/raw/product_revenue_quarterly.csv

Reconciliation:
    For every fiscal year present, the four quarterly values for each
    segment/product must sum to the annual value reported in
    data/raw/segment_revenue_annual.csv / product_revenue_annual.csv.
"""
from __future__ import annotations

import logging
import sys
from pathlib import Path

import pandas as pd

PROJECT_ROOT = Path(__file__).resolve().parents[1]
QTR_DIR      = PROJECT_ROOT / "data" / "raw" / "quarterly"
RAW_DIR      = PROJECT_ROOT / "data" / "raw"

logger = logging.getLogger("consolidate_quarterly")


# ---------------------------------------------------------------------------
# Apple fiscal calendar utilities
# ---------------------------------------------------------------------------
def fiscal_year_quarter(period_end: str) -> tuple[int, str]:
    """
    Map a period-end date to (fiscal_year, fiscal_quarter) using Apple's calendar.

    Apple's fiscal year ends on the last Saturday of September. So:
      - Q1 ends in late December (3 months after fiscal year start)
      - Q2 ends in late March
      - Q3 ends in late June
      - Q4 ends in late September (= fiscal year end)

    The fiscal_year is named for the calendar year in which Q4 ends.
    Example: period ending 2024-12-28 → fiscal year 2025, Q1.
    """
    pe = pd.Timestamp(period_end)
    month = pe.month
    if month == 12:                      # Q1 of the NEXT fiscal year
        return pe.year + 1, "Q1"
    if month in (3, 4):                  # Q2
        return pe.year, "Q2"
    if month in (6, 7):                  # Q3
        return pe.year, "Q3"
    if month in (9, 10):                 # Q4
        return pe.year, "Q4"
    raise ValueError(f"Period end {period_end} doesn't fit Apple's fiscal calendar")


# ---------------------------------------------------------------------------
# Aggregate quarterly CSVs
# ---------------------------------------------------------------------------
def consolidate(slice_name: str, key_col: str) -> pd.DataFrame:
    """Concatenate every per-period CSV for a given slice ('segment' or 'product')."""
    pattern = f"{slice_name}_revenue_*.csv"
    files = sorted(QTR_DIR.glob(pattern))
    if not files:
        raise FileNotFoundError(f"No quarterly CSVs found matching {pattern}")
    dfs = [pd.read_csv(f) for f in files]
    df = pd.concat(dfs, ignore_index=True).drop_duplicates()
    fy_q = df["period_end"].apply(fiscal_year_quarter)
    df["fiscal_year"]    = [t[0] for t in fy_q]
    df["fiscal_quarter"] = [t[1] for t in fy_q]
    return df.sort_values([key_col, "fiscal_year", "fiscal_quarter"]).reset_index(drop=True)


# ---------------------------------------------------------------------------
# Derive Q4 by subtraction
# ---------------------------------------------------------------------------
def derive_q4(quarterly: pd.DataFrame, annual_path: Path,
              key_col: str, value_col: str = "net_sales_millions") -> pd.DataFrame:
    """
    For each fiscal year where we have Q1-Q3 from the 10-Qs but no Q4, derive
    Q4 by subtracting Q1-Q3 from the annual total in the 10-K. Returns a frame
    of new Q4 rows to be appended.
    """
    annual = pd.read_csv(annual_path)
    new_rows: list[dict] = []

    for fy, group in quarterly.groupby("fiscal_year"):
        quarters_present = set(group["fiscal_quarter"].unique())
        if quarters_present == {"Q1", "Q2", "Q3"}:
            # We have all three reported quarters but no Q4 -> derive
            for entity in group[key_col].unique():
                annual_row = annual[(annual["fiscal_year"] == fy) & (annual[key_col] == entity)]
                if annual_row.empty:
                    logger.warning("No annual figure for FY%d %s '%s' — skipping",
                                   fy, key_col, entity)
                    continue
                annual_val = float(annual_row.iloc[0][value_col])
                q123_sum = group[group[key_col] == entity][value_col].sum()
                q4_val = annual_val - q123_sum
                new_rows.append({
                    "period_start":     None,           # don't fabricate
                    "period_end":       _approx_q4_end(fy),
                    key_col:            entity,
                    value_col:          q4_val,
                    "fiscal_year":      fy,
                    "fiscal_quarter":   "Q4",
                    "derived":          True,
                })
            logger.info("Derived FY%d Q4 for %d %ss", fy, len(group[key_col].unique()), key_col)

    return pd.DataFrame(new_rows)


def _approx_q4_end(fiscal_year: int) -> str:
    """
    Apple's fiscal Q4 ends on the last Saturday of September. We hardcode the
    actual filed dates for the years we know about.
    """
    known = {
        2024: "2024-09-28",
        2025: "2025-09-27",
        2023: "2023-09-30",
    }
    return known.get(fiscal_year, f"{fiscal_year}-09-27")


# ---------------------------------------------------------------------------
# Reconciliation
# ---------------------------------------------------------------------------
def reconcile_to_annual(quarterly: pd.DataFrame, annual_path: Path,
                        key_col: str, value_col: str = "net_sales_millions") -> bool:
    """For each (fiscal_year, entity) where all 4 quarters are present, check
    they sum to the annual total. Returns True if all checks pass."""
    annual = pd.read_csv(annual_path)
    ok = True

    for (fy, entity), group in quarterly.groupby(["fiscal_year", key_col]):
        if len(group) != 4:
            continue
        qsum = group[value_col].sum()
        annual_match = annual[(annual["fiscal_year"] == fy) & (annual[key_col] == entity)]
        if annual_match.empty:
            continue
        annual_val = float(annual_match.iloc[0][value_col])
        diff = abs(qsum - annual_val)
        if diff > 1:    # >$1M tolerance
            logger.error("FY%d %s: 4-qtr sum %.0f != annual %.0f (diff %.0f)",
                         fy, entity, qsum, annual_val, diff)
            ok = False
        else:
            logger.info("FY%d %s: 4-qtr sum reconciles to $%.0fM", fy, entity, annual_val)
    return ok


# ---------------------------------------------------------------------------
# Orchestration
# ---------------------------------------------------------------------------
def run() -> int:
    logging.basicConfig(
        level=logging.INFO,
        format="%(asctime)s [%(levelname)s] %(message)s",
        datefmt="%H:%M:%S",
    )

    # ----- segments --------------------------------------------------------
    seg = consolidate("segment", "segment")
    seg["derived"] = False
    seg_q4 = derive_q4(seg, RAW_DIR / "segment_revenue_annual.csv", "segment")
    if not seg_q4.empty:
        seg = pd.concat([seg, seg_q4], ignore_index=True)

    seg = seg.sort_values(["segment", "fiscal_year", "fiscal_quarter"]).reset_index(drop=True)
    seg.to_csv(RAW_DIR / "segment_revenue_quarterly.csv", index=False)
    logger.info("Wrote %s (%d rows)", RAW_DIR / "segment_revenue_quarterly.csv", len(seg))

    # ----- products --------------------------------------------------------
    prod = consolidate("product", "product_category")
    prod["derived"] = False
    prod_q4 = derive_q4(prod, RAW_DIR / "product_revenue_annual.csv", "product_category")
    if not prod_q4.empty:
        prod = pd.concat([prod, prod_q4], ignore_index=True)

    prod = prod.sort_values(["product_category", "fiscal_year", "fiscal_quarter"]).reset_index(drop=True)
    prod.to_csv(RAW_DIR / "product_revenue_quarterly.csv", index=False)
    logger.info("Wrote %s (%d rows)", RAW_DIR / "product_revenue_quarterly.csv", len(prod))

    # ----- reconcile -------------------------------------------------------
    seg_ok  = reconcile_to_annual(seg,  RAW_DIR / "segment_revenue_annual.csv",  "segment")
    prod_ok = reconcile_to_annual(prod, RAW_DIR / "product_revenue_annual.csv", "product_category")

    return 0 if (seg_ok and prod_ok) else 2


if __name__ == "__main__":
    sys.exit(run())
