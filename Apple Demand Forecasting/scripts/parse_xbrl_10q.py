"""
parse_xbrl_10q.py
=================
Extracts quarterly segment and product revenue from Apple Inc. iXBRL 10-Q
filings downloaded from SEC EDGAR.

Each EDGAR XBRL zip contains an Inline XBRL document (`aapl-YYYYMMDD.htm`)
where every disclosed financial fact is tagged with a context that defines
its period and any business-segment or product dimension members.

Usage:
    python scripts/parse_xbrl_10q.py path/to/filing_dir/aapl-YYYYMMDD.htm
    python scripts/parse_xbrl_10q.py path/to/filing.zip      # also accepts zips

Output:
    Writes two CSVs (one per slice) into data/raw/quarterly/:
        segment_revenue_<period_end>.csv
        product_revenue_<period_end>.csv
    plus a console reconciliation summary.

Design notes:
    * iXBRL is HTML with embedded ix: tags. We parse as XML to preserve
      namespaces — HTML mode loses the prefix structure we need.
    * Each filing contains both the current quarter AND the prior-year
      comparable quarter (XBRL contexts make this trivial). We extract
      both periods so 4 filings yield 8 quarters of data.
    * Apple does not disclose a segment × product cross-tab in 10-Qs —
      only segment totals and product totals separately. Forecast models
      must therefore treat segment and product as parallel dimensions.
"""
from __future__ import annotations

import argparse
import logging
import re
import sys
import zipfile
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable

import pandas as pd
from lxml import etree

# ---- Constants -------------------------------------------------------------
NS = {
    'ix':     'http://www.xbrl.org/2013/inlineXBRL',
    'xbrli':  'http://www.xbrl.org/2003/instance',
    'xbrldi': 'http://xbrl.org/2006/xbrldi',
}

REVENUE_CONCEPT = 'us-gaap:RevenueFromContractWithCustomerExcludingAssessedTax'
SEGMENT_AXIS    = 'us-gaap:StatementBusinessSegmentsAxis'
PRODUCT_AXIS    = 'srt:ProductOrServiceAxis'

# Friendly names for the dimension members Apple tags revenue with
SEGMENT_NAMES = {
    'aapl:AmericasSegmentMember':         'Americas',
    'aapl:EuropeSegmentMember':           'Europe',
    'aapl:GreaterChinaSegmentMember':     'Greater China',
    'aapl:JapanSegmentMember':            'Japan',
    'aapl:RestOfAsiaPacificSegmentMember':'Rest of Asia Pacific',
}

PRODUCT_NAMES = {
    'aapl:IPhoneMember':                       'iPhone',
    'aapl:MacMember':                          'Mac',
    'aapl:IPadMember':                         'iPad',
    'aapl:WearablesHomeandAccessoriesMember':  'Wearables Home and Accessories',
    'us-gaap:ServiceMember':                   'Services',
    # ProductMember is the products subtotal (iPhone+Mac+iPad+Wearables) — kept
    # for reconciliation but excluded from the per-product output
    'us-gaap:ProductMember':                   '_Products subtotal',
}

logger = logging.getLogger('parse_xbrl_10q')


# =============================================================================
# Core parser
# =============================================================================
@dataclass
class Fact:
    period_start: str
    period_end:   str
    segment:      str | None    # raw dimension member URI
    product:      str | None
    value_millions: float


def _build_context_map(root: etree._Element) -> dict[str, dict]:
    """Map context_id -> {start, end, dims{}} for the whole filing."""
    contexts: dict[str, dict] = {}
    for ctx in root.iter('{http://www.xbrl.org/2003/instance}context'):
        period   = ctx.find('xbrli:period', NS)
        start_el = period.find('xbrli:startDate', NS)
        end_el   = period.find('xbrli:endDate',   NS)
        inst_el  = period.find('xbrli:instant',   NS)

        dims: dict[str, str] = {}
        seg = ctx.find('xbrli:entity/xbrli:segment', NS)
        if seg is not None:
            for m in seg.findall('xbrldi:explicitMember', NS):
                dims[m.get('dimension')] = m.text

        contexts[ctx.get('id')] = {
            'start': start_el.text if start_el is not None else (inst_el.text if inst_el is not None else None),
            'end':   end_el.text   if end_el   is not None else (inst_el.text if inst_el is not None else None),
            'dims':  dims,
        }
    return contexts


def _parse_value(fact_el: etree._Element) -> float | None:
    """Decode an ix:nonFraction text into a USD millions value."""
    raw = (fact_el.text or '').replace(',', '').strip()
    if not raw:
        return None
    try:
        scaled = float(raw) * (10 ** int(fact_el.get('scale', 0)))
    except ValueError:
        return None
    sign = -1 if fact_el.get('sign') == '-' else 1
    return scaled * sign / 1e6


def extract_revenue_facts(htm_path: Path) -> list[Fact]:
    """Parse one iXBRL .htm file and return all revenue facts found."""
    tree     = etree.parse(str(htm_path))
    root     = tree.getroot()
    contexts = _build_context_map(root)

    facts: list[Fact] = []
    for el in root.iter('{http://www.xbrl.org/2013/inlineXBRL}nonFraction'):
        if el.get('name') != REVENUE_CONCEPT:
            continue
        ctx = contexts.get(el.get('contextRef'))
        if ctx is None:
            continue
        v = _parse_value(el)
        if v is None:
            continue
        facts.append(Fact(
            period_start = ctx['start'],
            period_end   = ctx['end'],
            segment      = ctx['dims'].get(SEGMENT_AXIS),
            product      = ctx['dims'].get(PRODUCT_AXIS),
            value_millions = v,
        ))

    # Dedupe — the same fact is often tagged in multiple disclosures.
    # We dedupe on the tuple key directly to avoid pandas converting None -> NaN
    # (which would silently break the `is None` filters downstream).
    seen: set[tuple] = set()
    unique: list[Fact] = []
    for f in facts:
        key = (f.period_start, f.period_end, f.segment, f.product, f.value_millions)
        if key not in seen:
            seen.add(key)
            unique.append(f)
    return unique


# =============================================================================
# Tidying — split into segment and product slices
# =============================================================================
def _is_quarterly(period_start: str, period_end: str) -> bool:
    """Return True for ~3-month periods (rules out YTD/9-month rollups)."""
    s = pd.Timestamp(period_start)
    e = pd.Timestamp(period_end)
    days = (e - s).days
    return 80 <= days <= 100  # allow ~13 weeks ± a few days


def to_segment_dataframe(facts: Iterable[Fact]) -> pd.DataFrame:
    rows = [
        {
            'period_start':       f.period_start,
            'period_end':         f.period_end,
            'segment':            SEGMENT_NAMES[f.segment],
            'net_sales_millions': f.value_millions,
        }
        for f in facts
        if f.segment in SEGMENT_NAMES
        and f.product is None
        and _is_quarterly(f.period_start, f.period_end)
    ]
    if not rows:
        return pd.DataFrame(columns=['period_start', 'period_end', 'segment', 'net_sales_millions'])
    return pd.DataFrame(rows).sort_values(['period_end', 'segment']).reset_index(drop=True)


def to_product_dataframe(facts: Iterable[Fact]) -> pd.DataFrame:
    rows = [
        {
            'period_start':       f.period_start,
            'period_end':         f.period_end,
            'product_category':   PRODUCT_NAMES[f.product],
            'net_sales_millions': f.value_millions,
        }
        for f in facts
        if f.product in PRODUCT_NAMES
        and f.product != 'us-gaap:ProductMember'  # exclude subtotal
        and f.segment is None
        and _is_quarterly(f.period_start, f.period_end)
    ]
    if not rows:
        return pd.DataFrame(columns=['period_start', 'period_end', 'product_category', 'net_sales_millions'])
    return pd.DataFrame(rows).sort_values(['period_end', 'product_category']).reset_index(drop=True)


def get_consolidated_total(facts: Iterable[Fact], period_end: str) -> float | None:
    """Return total revenue for a period — fact with no segment and no product."""
    for f in facts:
        if (
            f.period_end == period_end
            and f.segment is None
            and f.product is None
            and _is_quarterly(f.period_start, f.period_end)
        ):
            return f.value_millions
    return None


# =============================================================================
# Reconciliation
# =============================================================================
def reconcile(facts: list[Fact]) -> bool:
    """Verify that segment totals and product totals reconcile to consolidated."""
    seg_df  = to_segment_dataframe(facts)
    prod_df = to_product_dataframe(facts)

    ok = True
    for period_end in sorted(seg_df['period_end'].unique()):
        cons = get_consolidated_total(facts, period_end)
        seg_sum  = seg_df [seg_df ['period_end'] == period_end]['net_sales_millions'].sum()
        prod_sum = prod_df[prod_df['period_end'] == period_end]['net_sales_millions'].sum()

        if cons is None:
            logger.warning('No consolidated total found for period ending %s', period_end)
            continue

        seg_diff  = abs(seg_sum  - cons)
        prod_diff = abs(prod_sum - cons)

        if seg_diff > 1:
            logger.error('Period %s: segment sum %.0f != consolidated %.0f (diff %.0f)',
                         period_end, seg_sum, cons, seg_diff)
            ok = False
        else:
            logger.info('Period %s: segments reconcile to $%.0fM', period_end, cons)
        if prod_diff > 1:
            logger.error('Period %s: product sum %.0f != consolidated %.0f (diff %.0f)',
                         period_end, prod_sum, cons, prod_diff)
            ok = False
        else:
            logger.info('Period %s: products reconcile to $%.0fM', period_end, cons)
    return ok


# =============================================================================
# CLI
# =============================================================================
def _resolve_input(path: Path) -> Path:
    """Accept either a .htm/.xml file directly or a zip — return path to .htm."""
    if path.is_file() and path.suffix.lower() == '.zip':
        extract_dir = path.parent / f'_unzipped_{path.stem}'
        extract_dir.mkdir(exist_ok=True)
        with zipfile.ZipFile(path) as zf:
            zf.extractall(extract_dir)
        candidates = list(extract_dir.glob('aapl-*.htm'))
        # Filter out the exhibit files
        candidates = [c for c in candidates if not re.search(r'exhibit', c.name, re.IGNORECASE)]
        if not candidates:
            raise FileNotFoundError(f'No instance .htm found in {path}')
        return candidates[0]
    if path.is_file() and path.suffix.lower() in ('.htm', '.html', '.xml'):
        return path
    raise FileNotFoundError(f'Cannot resolve filing input: {path}')


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__.split('\n')[1])
    parser.add_argument('input', type=Path, help='Path to filing .htm or .zip')
    parser.add_argument('--output-dir', type=Path,
                        default=Path('data/raw/quarterly'),
                        help='Directory to write CSVs into')
    parser.add_argument('--verbose', '-v', action='store_true')
    args = parser.parse_args()

    logging.basicConfig(
        level=logging.DEBUG if args.verbose else logging.INFO,
        format='%(asctime)s [%(levelname)s] %(message)s',
        datefmt='%H:%M:%S',
    )

    htm = _resolve_input(args.input)
    logger.info('Parsing %s', htm.name)
    facts = extract_revenue_facts(htm)
    logger.info('Extracted %d unique revenue facts', len(facts))

    seg  = to_segment_dataframe(facts)
    prod = to_product_dataframe(facts)

    args.output_dir.mkdir(parents=True, exist_ok=True)
    # Write per-period CSVs so they accumulate cleanly across multiple filings
    for period_end in sorted(seg['period_end'].unique()):
        seg_slice = seg[seg['period_end'] == period_end]
        seg_slice.to_csv(args.output_dir / f'segment_revenue_{period_end}.csv', index=False)
    for period_end in sorted(prod['period_end'].unique()):
        prod_slice = prod[prod['period_end'] == period_end]
        prod_slice.to_csv(args.output_dir / f'product_revenue_{period_end}.csv', index=False)

    return 0 if reconcile(facts) else 2


if __name__ == '__main__':
    sys.exit(main())
