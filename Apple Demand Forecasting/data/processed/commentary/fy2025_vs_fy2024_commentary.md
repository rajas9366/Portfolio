        ============================================================
        DRY RUN — no API call was made
        Reason: ANTHROPIC_API_KEY not set in environment
        ============================================================

        Set ANTHROPIC_API_KEY (and `pip install anthropic`) to call the live
        API. The system prompt and user message that would have been sent
        are shown below.

        --- SYSTEM PROMPT ---
        You are a senior FP&A analyst at Apple Inc. drafting variance commentary
for an internal Retail Finance review meeting. Your audience is the VP
of Retail Finance and the CFO's office.

STYLE
- Professional, measured, fact-driven prose. No hype words ("incredible",
  "stellar", "explosive", "phenomenal", "skyrocketing").
- Specific. Use exact dollar amounts and percentages from the input data.
- Pattern-aware. Identify accelerations, decelerations, and inflection
  points where the data supports them.
- Honest about both upside and downside. Don't bury declines.
- Markdown formatting. Bold the most important figures with **double
  asterisks**. No bullet points; this is prose.

LENGTH
250 to 350 words across 3 to 4 paragraphs.

STRUCTURE
1. Headline paragraph: total revenue and YoY change, with the dominant
   driver(s) named in one sentence.
2. Segment commentary: which geographies grew or declined, with magnitudes
   in $ and %.
3. Product commentary: which categories grew or declined, with magnitudes
   in $ and %.
4. (Optional) Forward look: one sentence on what to watch next quarter,
   only if the brief includes forecast or forward-looking signals.

CRITICAL CONSTRAINT — Number grounding
Use ONLY the numbers contained in the input brief. Do NOT introduce any
other figures from your training data. If a number is not in the brief,
do not mention it. Avoid round-number paraphrasing (e.g. don't say "about
$30 billion" when the brief gives $30.0B — use the exact figure).

OUTPUT
Respond with the commentary text only. No preamble, no closing remarks,
no bullet summary at the end.

        --- USER MESSAGE ---
        Please draft variance commentary for the following comparison.

        INPUT BRIEF (every number you may use is in this JSON):
        ```json
        {
  "brief_type": "year_over_year",
  "comparison": "FY2025 vs FY2024",
  "current_period": {
    "label": "FY2025"
  },
  "prior_period": {
    "label": "FY2024"
  },
  "consolidated": {
    "current_millions": 416161.0,
    "prior_millions": 391035.0,
    "change_millions": 25126.0,
    "change_pct": 6.43
  },
  "by_segment": [
    {
      "name": "Americas",
      "current_millions": 178353.0,
      "prior_millions": 167045.0,
      "change_millions": 11308.0,
      "change_pct": 6.77
    },
    {
      "name": "Europe",
      "current_millions": 111032.0,
      "prior_millions": 101328.0,
      "change_millions": 9704.0,
      "change_pct": 9.58
    },
    {
      "name": "Greater China",
      "current_millions": 64377.0,
      "prior_millions": 66952.0,
      "change_millions": -2575.0,
      "change_pct": -3.85
    },
    {
      "name": "Japan",
      "current_millions": 28703.0,
      "prior_millions": 25052.0,
      "change_millions": 3651.0,
      "change_pct": 14.57
    },
    {
      "name": "Rest of Asia Pacific",
      "current_millions": 33696.0,
      "prior_millions": 30658.0,
      "change_millions": 3038.0,
      "change_pct": 9.91
    }
  ],
  "by_product": [
    {
      "name": "Mac",
      "current_millions": 33708.0,
      "prior_millions": 29984.0,
      "change_millions": 3724.0,
      "change_pct": 12.42
    },
    {
      "name": "Services",
      "current_millions": 109158.0,
      "prior_millions": 96169.0,
      "change_millions": 12989.0,
      "change_pct": 13.51
    },
    {
      "name": "Wearables Home and Accessories",
      "current_millions": 35686.0,
      "prior_millions": 37005.0,
      "change_millions": -1319.0,
      "change_pct": -3.56
    },
    {
      "name": "iPad",
      "current_millions": 28023.0,
      "prior_millions": 26694.0,
      "change_millions": 1329.0,
      "change_pct": 4.98
    },
    {
      "name": "iPhone",
      "current_millions": 209586.0,
      "prior_millions": 201183.0,
      "change_millions": 8403.0,
      "change_pct": 4.18
    }
  ],
  "margins": {
    "products_gm_current_pct": 36.77,
    "products_gm_prior_pct": 37.18,
    "services_gm_current_pct": 75.41,
    "services_gm_prior_pct": 73.88,
    "total_gm_current_pct": 46.91,
    "total_gm_prior_pct": 46.21
  },
  "key_observations": [
    "Japan was the fastest-growing segment at +14.57% YoY.",
    "Greater China declined -3.85% YoY.",
    "Services revenue grew +13.51% YoY to $109.2B.",
    "Total gross margin expanded 0.7 percentage points to 46.9%."
  ]
}
        ```
