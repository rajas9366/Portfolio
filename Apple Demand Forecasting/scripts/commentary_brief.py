"""
commentary_brief.py
===================
Builds structured "variance briefs" from the analytical database. A brief is
a typed JSON object containing every fact the LLM is permitted to use when
drafting commentary. The model never reads the database directly — it only
sees the brief — so the validator (commentary_validator.py) can later
verify that no numbers were hallucinated.

Three brief flavors are supported:
    - period_over_period   : actual-to-actual (e.g. Q1 FY26 vs Q1 FY25 YoY)
    - year_over_year       : annual recap (e.g. FY25 vs FY24)
    - forecast_vs_baseline : forecast-to-actual (e.g. Q2 FY26 fc vs Q2 FY25 actual)

Public API:
    build_period_brief(conn, current_period_id, prior_period_id) -> dict
    build_annual_brief(conn, current_fy, prior_fy)               -> dict
    build_forecast_brief(conn, forecast_csv, comparison_period_id) -> dict
"""
from __future__ import annotations

import sqlite3
from pathlib import Path
from typing import Any

import pandas as pd


# =============================================================================
# Helpers
# =============================================================================
def _yoy_change(current: float, prior: float) -> dict:
    """Standard YoY delta record — used everywhere."""
    abs_change = current - prior
    pct_change = (abs_change / prior * 100) if prior else None
    return {
        "current_millions": round(float(current), 0),
        "prior_millions":   round(float(prior),   0),
        "change_millions":  round(float(abs_change), 0),
        "change_pct":       round(float(pct_change), 2) if pct_change is not None else None,
    }


def _period_label(fiscal_year: int, fiscal_quarter: str | None) -> str:
    return f"FY{fiscal_year} {fiscal_quarter}" if fiscal_quarter else f"FY{fiscal_year}"


# =============================================================================
# Period-over-period (e.g. Q1 FY26 vs Q1 FY25)
# =============================================================================
def build_period_brief(
    conn: sqlite3.Connection,
    current_period_id: int,
    prior_period_id:   int,
) -> dict[str, Any]:
    """
    Build a YoY brief comparing two quarterly periods. By convention,
    `current_period_id` is the most recent period and `prior_period_id`
    is the same quarter one year earlier.
    """
    # ----- period metadata -------------------------------------------------
    periods = pd.read_sql(
        "SELECT period_id, fiscal_year, fiscal_quarter, period_end_date "
        "FROM dim_period WHERE period_id IN (?, ?)",
        conn, params=(current_period_id, prior_period_id),
    ).set_index("period_id")
    cur = periods.loc[current_period_id]
    pri = periods.loc[prior_period_id]

    # ----- segment revenue -------------------------------------------------
    seg = pd.read_sql(
        """
        SELECT  s.segment_name,
                MAX(CASE WHEN f.period_id = ? THEN f.net_sales_millions END) AS current,
                MAX(CASE WHEN f.period_id = ? THEN f.net_sales_millions END) AS prior
        FROM    fact_segment_revenue_quarterly f
        JOIN    dim_segment s USING (segment_id)
        WHERE   f.period_id IN (?, ?)
        GROUP BY s.segment_name
        """,
        conn, params=(current_period_id, prior_period_id, current_period_id, prior_period_id),
    )
    segments = [
        {"name": r.segment_name, **_yoy_change(r.current, r.prior)}
        for r in seg.itertuples(index=False)
    ]

    # ----- product revenue -------------------------------------------------
    prd = pd.read_sql(
        """
        SELECT  d.product_category,
                MAX(CASE WHEN f.period_id = ? THEN f.net_sales_millions END) AS current,
                MAX(CASE WHEN f.period_id = ? THEN f.net_sales_millions END) AS prior
        FROM    fact_product_revenue_quarterly f
        JOIN    dim_product d USING (product_id)
        WHERE   f.period_id IN (?, ?)
        GROUP BY d.product_category
        """,
        conn, params=(current_period_id, prior_period_id, current_period_id, prior_period_id),
    )
    products = [
        {"name": r.product_category, **_yoy_change(r.current, r.prior)}
        for r in prd.itertuples(index=False)
    ]

    # ----- consolidated total ---------------------------------------------
    cur_total = sum(s["current_millions"] for s in segments)
    pri_total = sum(s["prior_millions"]   for s in segments)
    consolidated = _yoy_change(cur_total, pri_total)

    # ----- key observations (computed from data, NOT inferred) ------------
    observations = _compute_observations(conn, current_period_id, prior_period_id, segments, products)

    # ----- assemble the brief ---------------------------------------------
    return {
        "brief_type":    "period_over_period",
        "comparison":    f"{_period_label(cur.fiscal_year, cur.fiscal_quarter)} vs {_period_label(pri.fiscal_year, pri.fiscal_quarter)}",
        "current_period": {
            "label":     _period_label(cur.fiscal_year, cur.fiscal_quarter),
            "end_date":  cur.period_end_date,
        },
        "prior_period": {
            "label":     _period_label(pri.fiscal_year, pri.fiscal_quarter),
            "end_date":  pri.period_end_date,
        },
        "consolidated": consolidated,
        "by_segment":   segments,
        "by_product":   products,
        "key_observations": observations,
    }


def _compute_observations(
    conn:     sqlite3.Connection,
    cur_pid:  int,
    pri_pid:  int,
    segments: list[dict],
    products: list[dict],
) -> list[str]:
    """
    Derive 3-5 short, factual observations from the data — these point
    the model at patterns it should highlight without giving it license
    to invent context.
    """
    obs: list[str] = []

    # 1) Strongest and weakest segments
    seg_sorted = sorted(segments, key=lambda r: r["change_pct"] or 0)
    if seg_sorted[-1]["change_pct"] is not None and seg_sorted[-1]["change_pct"] > 0:
        top = seg_sorted[-1]
        obs.append(
            f"{top['name']} was the fastest-growing segment at +{top['change_pct']}% YoY "
            f"(${top['current_millions']/1000:.1f}B vs ${top['prior_millions']/1000:.1f}B)."
        )
    if seg_sorted[0]["change_pct"] is not None and seg_sorted[0]["change_pct"] < 0:
        bot = seg_sorted[0]
        obs.append(
            f"{bot['name']} declined {bot['change_pct']:.1f}% YoY "
            f"(${bot['current_millions']/1000:.1f}B vs ${bot['prior_millions']/1000:.1f}B)."
        )

    # 2) Strongest and weakest products
    prd_sorted = sorted(products, key=lambda r: r["change_pct"] or 0)
    if prd_sorted[-1]["change_pct"] is not None:
        top = prd_sorted[-1]
        obs.append(
            f"{top['name']} grew +{top['change_pct']}% YoY, the fastest of any product category."
        )
    if prd_sorted[0]["change_pct"] is not None and prd_sorted[0]["change_pct"] < 0:
        bot = prd_sorted[0]
        obs.append(
            f"{bot['name']} declined {bot['change_pct']:.1f}% YoY."
        )

    # 3) Cross-period context: was current segment growth a reversal of trend?
    obs.extend(_trend_reversal_notes(conn, cur_pid, pri_pid))

    return obs


def _trend_reversal_notes(conn: sqlite3.Connection, cur_pid: int, pri_pid: int) -> list[str]:
    """
    For each segment, check whether the current quarter's YoY direction reverses
    the trend over the preceding 4 quarters. Inflection points are commentary gold.
    """
    notes: list[str] = []
    seg_history = pd.read_sql(
        """
        SELECT  s.segment_name, p.fiscal_year, p.fiscal_quarter,
                p.period_id, f.net_sales_millions
        FROM    fact_segment_revenue_quarterly f
        JOIN    dim_period  p USING (period_id)
        JOIN    dim_segment s USING (segment_id)
        ORDER BY s.segment_name, p.period_id
        """, conn,
    )

    for seg, group in seg_history.groupby("segment_name"):
        group = group.sort_values("period_id").reset_index(drop=True)
        # find current row; we want the 4 preceding YoYs (vs four-quarter-ago each time)
        cur_idx = group.index[group["period_id"] == cur_pid]
        if len(cur_idx) == 0:
            continue
        i = int(cur_idx[0])
        # need at least 5 prior YoY-pair indices: i-4 ... i-1 paired with their year-ago
        if i < 4 + 4:
            continue
        prev_yoy_signs: list[int] = []
        for j in range(i - 4, i):
            year_ago = j - 4
            if year_ago < 0:
                break
            v_now  = group.loc[j,        "net_sales_millions"]
            v_prev = group.loc[year_ago, "net_sales_millions"]
            if v_prev:
                prev_yoy_signs.append(1 if v_now > v_prev else -1)

        cur_v  = group.loc[i,     "net_sales_millions"]
        prev_v = group.loc[i - 4, "net_sales_millions"]  # which equals pri_pid by construction
        cur_sign = 1 if cur_v > prev_v else -1

        if len(prev_yoy_signs) >= 3 and all(s != cur_sign for s in prev_yoy_signs[-3:]):
            direction = "growth" if cur_sign > 0 else "decline"
            opposite  = "decline" if cur_sign > 0 else "growth"
            notes.append(
                f"{seg} returned to {direction} after at least 3 consecutive YoY {opposite} quarters."
            )
    return notes


# =============================================================================
# Annual recap (e.g. FY25 vs FY24)
# =============================================================================
def build_annual_brief(
    conn:    sqlite3.Connection,
    current_fy: int,
    prior_fy:   int,
) -> dict[str, Any]:
    """Annual brief. Same shape as period brief but uses annual fact tables."""
    seg = pd.read_sql(
        """
        SELECT  s.segment_name,
                MAX(CASE WHEN p.fiscal_year = ? THEN f.net_sales_millions END) AS current,
                MAX(CASE WHEN p.fiscal_year = ? THEN f.net_sales_millions END) AS prior
        FROM    fact_segment_revenue_annual f
        JOIN    dim_segment s USING (segment_id)
        JOIN    dim_period  p USING (period_id)
        WHERE   p.fiscal_year IN (?, ?) AND p.period_type = 'ANNUAL'
        GROUP BY s.segment_name
        """, conn, params=(current_fy, prior_fy, current_fy, prior_fy),
    )
    segments = [
        {"name": r.segment_name, **_yoy_change(r.current, r.prior)}
        for r in seg.itertuples(index=False)
    ]

    prd = pd.read_sql(
        """
        SELECT  d.product_category,
                MAX(CASE WHEN p.fiscal_year = ? THEN f.net_sales_millions END) AS current,
                MAX(CASE WHEN p.fiscal_year = ? THEN f.net_sales_millions END) AS prior
        FROM    fact_product_revenue_annual f
        JOIN    dim_product d USING (product_id)
        JOIN    dim_period  p USING (period_id)
        WHERE   p.fiscal_year IN (?, ?) AND p.period_type = 'ANNUAL'
        GROUP BY d.product_category
        """, conn, params=(current_fy, prior_fy, current_fy, prior_fy),
    )
    products = [
        {"name": r.product_category, **_yoy_change(r.current, r.prior)}
        for r in prd.itertuples(index=False)
    ]

    cur_total = sum(s["current_millions"] for s in segments)
    pri_total = sum(s["prior_millions"]   for s in segments)

    # Margin facts from consolidated P&L
    pnl = pd.read_sql(
        """
        SELECT  p.fiscal_year, f.line_item, f.amount_millions
        FROM    fact_consolidated_pnl_annual f
        JOIN    dim_period p USING (period_id)
        WHERE   p.fiscal_year IN (?, ?)
        """, conn, params=(current_fy, prior_fy),
    ).set_index(["fiscal_year", "line_item"])["amount_millions"]

    def _gm_pct(yr: int, slice_: str) -> float:
        return round(100.0 * pnl[(yr, f"{slice_}_gross_margin")]
                            / pnl[(yr, f"{slice_}_net_sales")], 2)

    margins = {
        "products_gm_current_pct":  _gm_pct(current_fy, "products"),
        "products_gm_prior_pct":    _gm_pct(prior_fy,   "products"),
        "services_gm_current_pct":  _gm_pct(current_fy, "services"),
        "services_gm_prior_pct":    _gm_pct(prior_fy,   "services"),
        "total_gm_current_pct":     _gm_pct(current_fy, "total"),
        "total_gm_prior_pct":       _gm_pct(prior_fy,   "total"),
    }

    return {
        "brief_type":    "year_over_year",
        "comparison":    f"FY{current_fy} vs FY{prior_fy}",
        "current_period": {"label": f"FY{current_fy}"},
        "prior_period":   {"label": f"FY{prior_fy}"},
        "consolidated":   _yoy_change(cur_total, pri_total),
        "by_segment":     segments,
        "by_product":     products,
        "margins":        margins,
        "key_observations": _annual_observations(segments, products, margins),
    }


def _annual_observations(segments, products, margins) -> list[str]:
    obs: list[str] = []
    seg_s = sorted(segments, key=lambda r: r["change_pct"] or 0)
    if seg_s[-1]["change_pct"] is not None:
        top = seg_s[-1]
        obs.append(
            f"{top['name']} was the fastest-growing segment at +{top['change_pct']}% YoY."
        )
    if seg_s[0]["change_pct"] is not None and seg_s[0]["change_pct"] < 0:
        obs.append(
            f"{seg_s[0]['name']} declined {seg_s[0]['change_pct']}% YoY."
        )

    services = next((p for p in products if p["name"] == "Services"), None)
    if services:
        obs.append(
            f"Services revenue grew +{services['change_pct']}% YoY to ${services['current_millions']/1000:.1f}B."
        )
    gm_delta = round(margins["total_gm_current_pct"] - margins["total_gm_prior_pct"], 2)
    if gm_delta != 0:
        direction = "expanded" if gm_delta > 0 else "contracted"
        obs.append(
            f"Total gross margin {direction} {abs(gm_delta):.1f} percentage points to "
            f"{margins['total_gm_current_pct']:.1f}%."
        )
    return obs


# =============================================================================
# Forecast vs baseline
# =============================================================================
def build_forecast_brief(
    conn:                 sqlite3.Connection,
    forecast_csv:         Path,
    comparison_period_id: int,
    horizon:              int = 1,
) -> dict[str, Any]:
    """
    Compare a forward forecast against a baseline period (typically the
    same quarter one year earlier). The model's job is to explain WHY
    the forecast looks the way it does and where to focus review.

    The forecast CSV may contain multiple rows per series (one per horizon).
    `horizon` selects which row to use — default 1 = Q2 FY26 (the nearest
    forecast quarter), 2 = Q3 FY26, 3 = Q4 FY26.
    """
    fc = pd.read_csv(forecast_csv)
    # Filter to the requested horizon. CSV may not have a 'horizon' column
    # if it was generated by an earlier (single-horizon) build of run_forecast.py;
    # in that case treat any row as horizon=1.
    if "horizon" in fc.columns:
        fc = fc[fc["horizon"] == horizon]
        if fc.empty:
            available = sorted(pd.read_csv(forecast_csv)["horizon"].unique().tolist())
            raise ValueError(
                f"No forecast rows for horizon={horizon}. Available horizons: {available}"
            )
    forecast_label = fc["forecast_period"].iloc[0]

    seg_baseline = pd.read_sql(
        """
        SELECT  s.segment_name, f.net_sales_millions
        FROM    fact_segment_revenue_quarterly f
        JOIN    dim_segment s USING (segment_id)
        WHERE   f.period_id = ?
        """, conn, params=(comparison_period_id,),
    ).set_index("segment_name")["net_sales_millions"]

    prd_baseline = pd.read_sql(
        """
        SELECT  d.product_category, f.net_sales_millions
        FROM    fact_product_revenue_quarterly f
        JOIN    dim_product d USING (product_id)
        WHERE   f.period_id = ?
        """, conn, params=(comparison_period_id,),
    ).set_index("product_category")["net_sales_millions"]

    cmp_period = pd.read_sql(
        "SELECT fiscal_year, fiscal_quarter, period_end_date "
        "FROM dim_period WHERE period_id = ?", conn, params=(comparison_period_id,),
    ).iloc[0]

    by_segment = []
    for _, r in fc.iterrows():
        if r["series"] in seg_baseline.index:
            base = float(seg_baseline.loc[r["series"]])
            by_segment.append({
                "name": r["series"],
                "forecast_millions": round(float(r["point_forecast"]), 0),
                "lower_80_millions": round(float(r["lower_80"]),       0),
                "upper_80_millions": round(float(r["upper_80"]),       0),
                "baseline_millions": round(base, 0),
                "implied_yoy_pct":   round(100.0 * (r["point_forecast"] - base) / base, 2),
                "champion_model":    r["champion_model"],
                "ensemble_mape_pct": round(float(r["ensemble_mape_pct"]), 2),
            })

    by_product = []
    for _, r in fc.iterrows():
        if r["series"] in prd_baseline.index:
            base = float(prd_baseline.loc[r["series"]])
            by_product.append({
                "name": r["series"],
                "forecast_millions": round(float(r["point_forecast"]), 0),
                "lower_80_millions": round(float(r["lower_80"]),       0),
                "upper_80_millions": round(float(r["upper_80"]),       0),
                "baseline_millions": round(base, 0),
                "implied_yoy_pct":   round(100.0 * (r["point_forecast"] - base) / base, 2),
                "champion_model":    r["champion_model"],
                "ensemble_mape_pct": round(float(r["ensemble_mape_pct"]), 2),
            })

    seg_total = sum(s["forecast_millions"] for s in by_segment)
    prd_total = sum(p["forecast_millions"] for p in by_product)
    base_total = float(seg_baseline.sum())

    return {
        "brief_type":   "forecast_vs_baseline",
        "comparison":   f"{forecast_label} forecast vs {_period_label(cmp_period.fiscal_year, cmp_period.fiscal_quarter)} actual",
        "forecast_period":    forecast_label,
        "comparison_period":  {
            "label":    _period_label(cmp_period.fiscal_year, cmp_period.fiscal_quarter),
            "end_date": cmp_period.period_end_date,
        },
        "consolidated": {
            "forecast_segment_sum_millions": round(seg_total, 0),
            "forecast_product_sum_millions": round(prd_total, 0),
            "baseline_total_millions":       round(base_total, 0),
            "implied_yoy_pct_segment_basis": round(100.0 * (seg_total - base_total) / base_total, 2),
        },
        "by_segment":  by_segment,
        "by_product":  by_product,
        "key_observations": _forecast_observations(by_segment, by_product),
    }


def _forecast_observations(by_segment, by_product) -> list[str]:
    obs: list[str] = []
    seg_s = sorted(by_segment, key=lambda r: r["implied_yoy_pct"], reverse=True)
    obs.append(
        f"Forecast implies the strongest YoY in {seg_s[0]['name']} at +{seg_s[0]['implied_yoy_pct']}%."
    )
    obs.append(
        f"Forecast implies the weakest YoY in {seg_s[-1]['name']} at +{seg_s[-1]['implied_yoy_pct']}%."
    )

    iphone = next((p for p in by_product if p["name"] == "iPhone"), None)
    services = next((p for p in by_product if p["name"] == "Services"), None)
    if iphone:
        obs.append(
            f"iPhone forecast band is wide: ${iphone['lower_80_millions']/1000:.1f}B - ${iphone['upper_80_millions']/1000:.1f}B (80% interval)."
        )
    if services:
        obs.append(
            f"Services forecast band is unusually narrow at ${services['lower_80_millions']/1000:.1f}B - ${services['upper_80_millions']/1000:.1f}B (best backtest MAPE of any series at {services['ensemble_mape_pct']}%)."
        )
    return obs
