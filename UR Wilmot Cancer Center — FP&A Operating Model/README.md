# UR Wilmot Cancer Center — FP&A Operating Model

> A 10-tab driver-based FP&A model for the University of Rochester's Wilmot
> Cancer Institute Clinical Trials Office. FY2026 budget vs. actual,
> 5-year strategic forecast, trial-level NPV/IRR analysis, and 3-scenario
> stress testing — all anchored to UR's audited FY2025 consolidated
> financials.

[![Tool](https://img.shields.io/badge/built%20in-Microsoft%20Excel-217346)]()
[![Forecast](https://img.shields.io/badge/forecast%20horizon-FY26%E2%80%93FY30-blue)]()
[![Scenarios](https://img.shields.io/badge/scenarios-Base%20%2F%20Upside%20%2F%20Downside-blue)]()
[![Anchor](https://img.shields.io/badge/anchor-UR%20FY25%20audited-brightgreen)]()

## Headline outputs

| Metric                                  | Base case               |
|-----------------------------------------|-------------------------|
| FY26 Total Revenue                      | $27.5M                  |
| FY30 Total Revenue                      | $37.1M                  |
| 5-Year Revenue CAGR                     | 7.8%                    |
| Active Trial Portfolio Net Margin       | $3.1M (10 trials, $11.5M direct revenue) |
| Phase III Industry Trial NPV @ 8%       | $533K                   |
| Break-even Accruals (Phase III industry)| 65 patients             |
| FY26 Cost per Accrual                   | $24.8K (vs $14K target) |

## Workbook map

The model has 10 tabs, organized as inputs → analysis → outputs:

1. **Cover** — model context, data sources, color/formatting conventions
2. **Assumptions** — every driver in one place: rates, ramps, headcount, F&A indirect cost rates, scenario selector
3. **BudgetVsActual** — FY2025 monthly P&L with $ and % variance and commentary
4. **ProgramP&L** — departmental income statement: revenue → comp → supplies → F&A recovery → net margin
5. **Forecast5Yr** — FY2026–FY2030 driver-based forecast (accruals × per-patient revenue × growth rates)
6. **TrialDecision** — per-trial NPV/IRR/break-even analysis with 5×5 accrual-vs-revenue sensitivity table
7. **GrantF&A** — grant portfolio + Indirect Cost (F&A) recovery analysis (federal vs industry vs foundation)
8. **KPIDashboard** — executive KPI dashboard: margin, cost per patient, RVU, productivity, days cash
9. **Scenarios** — Base / Upside / Downside stress test; flip C6 on Assumptions to switch scenarios live
10. **ICSummary** — one-pager investment-committee deliverable: thesis, hurdles, recommendation

## Methodology

- **Macro anchors.** All FY25 totals (operating revenue, compensation, fringe rate, F&A rate) are pulled from UR's audited FY2025 consolidated statements (PwC opinion, 10/17/2025). Documented at source-level in the Assumptions tab.
- **Driver-based forecast.** Revenue = Accruals × Per-Patient Rate × (1 + Growth). Mix shift (industry vs federal vs foundation vs cooperative-group) compounds across the 5-year horizon. F&A recovery layers on top using UR's federally-negotiated 62.5% rate for federal trials and a 30% blended rate for industry sponsors.
- **Trial-level economics.** A 10-trial portfolio sample (Phase Ib through Phase III, mixed sponsor types) drives a per-trial NPV deep-dive. Discounted cash flows over 4-year trial lifecycle (Y0 setup → Y3 last-patient-out → Y4 closeout). Break-even and IRR computed alongside NPV.
- **Sensitivity testing.** A 5×5 grid varies patient accruals (50–120) against per-patient revenue ($22K–$36K) to surface the margin range a single trial can produce. The corner-to-corner swing on the sensitivity table is ~$2M for a Phase III oncology trial.
- **Scenario stress test.** Base (1,250 accruals, 4% growth, status quo NIH funding), Upside (1,500 accruals, 7% growth, expanded portfolio), Downside (950 accruals, 1% growth, NIH budget cuts). The active scenario flows through every tab via a single selector cell.

## Key assumptions you should question

The model is built so a reviewer can challenge each input quickly. The most consequential ones:

- **CTO % of UR research portfolio (7%).** Wilmot is NCI-designated and is in the right band (most NCI cancer centers run 5–10% of their host university's research enterprise), but exact allocations to the CTO sub-unit aren't in the public audit — this is illustrative.
- **Federal F&A rate (62.5% MTDC).** Pulled from UR's negotiated federal rate; this is publicly available but rates renegotiate every 3–5 years.
- **Industry F&A rate (30%).** Industry F&A is sponsor-by-sponsor and the 30% blend is a reasonable academic-CTO benchmark, but actual contracts range 25–40%.
- **Trial duration (36 months avg).** Industry-sponsored Phase II/III oncology trials run 30–48 months in practice; the 36-month assumption is the midpoint and skews shorter for trials that close early.
- **Bias toward CRC headcount (1 CRC per 45 accruals).** The headcount-to-volume ratio is the single biggest expense lever; a 1:50 ratio would meaningfully improve margins.

## Color & formatting conventions

The model follows the standard FP&A color-coding convention so a reviewer can read formulas at a glance:

| Color | Meaning |
|---|---|
| **Blue text** | Hardcoded inputs / drivers — user-editable |
| Black text | Formulas and calculations within the same tab |
| **Green text** | Cross-sheet links pulling from other tabs |
| Yellow fill | Key assumption — attention required |
| Light-blue fill | Sub-totals and grand totals |
| **Green text (variance)** | Favorable variance vs. budget |
| **Red text (variance)** | Unfavorable variance vs. budget |

## Disclaimer

This model uses University of Rochester's publicly audited FY2025 consolidated
figures as macro anchors. Department-level allocations to the Wilmot Cancer
Center / Clinical Trials Office are illustrative — actual department-level data
is not in the public audit and would be available via internal Workday
reporting. Numbers are scaled to plausible departmental size for an
NCI-designated cancer center clinical trials office.

This is a personal portfolio project and is not affiliated with or endorsed
by the University of Rochester or Wilmot Cancer Institute.
