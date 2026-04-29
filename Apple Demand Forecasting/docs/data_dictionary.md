# Data Dictionary — `apple_finance.db`

All monetary values are in **USD millions** unless otherwise stated, matching
the convention of the 10-K. All `period_end_date` values follow Apple's
fiscal calendar — fiscal years end on the last Saturday of September.

---

## Dimensions

### `dim_segment`
Apple's five reportable geographic segments (10-K Note 13).

| Column                | Type    | Description                                                |
|-----------------------|---------|------------------------------------------------------------|
| `segment_id`          | INT PK  | Surrogate key                                              |
| `segment_name`        | TEXT    | Americas / Europe / Greater China / Japan / Rest of Asia Pacific |
| `region_grouping`     | TEXT    | Higher-level grouping: Americas / EMEIA / APAC             |
| `is_emerging_market`  | INT     | 1 for Greater China and Rest of Asia Pacific (used for risk filtering) |

> **Note:** Apple's "Europe" segment includes India, the Middle East, and Africa. The
> repo uses **EMEIA** as the supersegment label to avoid silently rolling India
> into Europe-Europe in downstream analysis.

### `dim_product`
Five product categories per 10-K Note 2.

| Column                | Type    | Description                                                |
|-----------------------|---------|------------------------------------------------------------|
| `product_id`          | INT PK  | Surrogate key                                              |
| `product_category`    | TEXT    | iPhone / Mac / iPad / Wearables Home and Accessories / Services |
| `revenue_type`        | TEXT    | Products or Services (drives gross margin profile)         |
| `is_hardware`         | INT     | Boolean — Services is the only 0                           |
| `typical_launch_qtr`  | TEXT    | Apple's typical major refresh quarter; seeds seasonality model |

### `dim_period`
Annual rollups for FY2023–FY2025. Quarterly rows added in Chunk 1B.

| Column                | Type    | Description                                                |
|-----------------------|---------|------------------------------------------------------------|
| `period_id`           | INT PK  | Equals fiscal_year for ANNUAL rows                         |
| `fiscal_year`         | INT     | 2023, 2024, 2025                                           |
| `fiscal_quarter`      | TEXT    | Q1/Q2/Q3/Q4, NULL for annual rows                          |
| `period_type`         | TEXT    | ANNUAL or QUARTERLY                                        |
| `period_end_date`     | DATE    | Last Saturday of the period                                |
| `weeks_in_period`     | INT     | 52 for FY24/FY25, 53 for FY23                              |

### `dim_product_launch`
Product launch calendar — features for the forecasting model.

| Column                | Type    | Description                                                |
|-----------------------|---------|------------------------------------------------------------|
| `launch_id`           | INT PK  | Auto-increment surrogate                                   |
| `fiscal_year`         | INT     |                                                            |
| `fiscal_quarter`      | TEXT    | Q1–Q4                                                      |
| `product_launched`    | TEXT    | Free-text description (e.g. "iPhone 17 / iPhone Air ...")  |
| `product_category`    | TEXT    | Mapped to `dim_product.product_category`                   |
| `launch_significance` | TEXT    | major (flagship release) or minor (refresh / spec bump)    |

---

## Fact tables — annual grain

### `fact_segment_revenue_annual`
Net sales by reportable segment per fiscal year. PK is `(period_id, segment_id)`.

| Column                | Type | Description                                       |
|-----------------------|------|---------------------------------------------------|
| `period_id`           | INT  | FK → `dim_period`                                 |
| `segment_id`          | INT  | FK → `dim_segment`                                |
| `net_sales_millions`  | REAL | Stored positive                                   |

### `fact_product_revenue_annual`
Net sales by product category. PK is `(period_id, product_id)`.

### `fact_segment_pnl_annual`
Operating P&L by segment, including COGS, S&M, op income.
**Convention:** `cost_of_sales_millions` and `selling_marketing_millions`
are stored as **negative numbers** to match the 10-K presentation, so that
`net_sales + cost_of_sales + selling_marketing = operating_income` works
without sign juggling.

### `fact_consolidated_pnl_annual`
Long-format consolidated income statement. Grain is `(period_id, line_item)`.
Line items include: `products_net_sales`, `services_net_sales`,
`total_net_sales`, `products_cost_of_sales`, `services_cost_of_sales`,
`total_cost_of_sales`, `products_gross_margin`, `services_gross_margin`,
`total_gross_margin`, `research_and_development`,
`selling_general_administrative`, `total_operating_expenses`,
`operating_income`, `other_income_expense_net`, `income_before_taxes`,
`provision_for_income_taxes`, `net_income`.

### `fact_geographic_annual`
Country-level revenue and long-lived assets — for countries individually
exceeding 10% of the respective totals (US, China). All others rolled into
"Other countries". Long-lived assets are **not disclosed for FY2023** so
that field is NULL.

---

## Fact tables — quarterly grain

### `fact_segment_revenue_quarterly`
Net sales by segment per fiscal quarter. PK is `(period_id, segment_id)`.

| Column                | Type | Description                                       |
|-----------------------|------|---------------------------------------------------|
| `period_id`           | INT  | FK → `dim_period` (quarterly row, e.g. 20251 = FY25 Q1) |
| `segment_id`          | INT  | FK → `dim_segment`                                |
| `net_sales_millions`  | REAL | Stored positive                                   |
| `is_derived`          | INT  | 1 if the row was back-calculated from annual minus Q1+Q2+Q3 (Q4 only); 0 if it came directly from a 10-Q |

### `fact_product_revenue_quarterly`
Net sales by product category per fiscal quarter. Same structure and
`is_derived` convention as the segment table.

### `v_segment_forecast_features` (view)
Wide table of engineered features for the ML forecasting model — one row per
(period, segment) with lag-1, lag-4, rolling 4-quarter mean, and dummy flags
for holiday quarter and iPhone launch quarter. Defined in
`sql/03_quarterly_queries.sql`.

---

## Reconciliation rules

The build script enforces these on every run:

| Check                                                         | Tolerance |
|---------------------------------------------------------------|-----------|
| Σ segment net sales = consolidated total net sales (per FY)   | ±$1M     |
| Σ product net sales = consolidated total net sales (per FY)   | ±$1M     |
| Products revenue + Services revenue = total revenue           | exact     |
| Segment op income − consolidated op income = positive (Corporate) | $30B–$50B |
| Σ four quarters = annual (per fiscal year, per segment)       | ±$1M     |
| Σ four quarters = annual (per fiscal year, per product)       | ±$1M     |
| Σ segments per quarter = sum of products per quarter (10-Q)   | ±$1M     |

Build **fails with exit code 2** if any reconciliation breaks.
