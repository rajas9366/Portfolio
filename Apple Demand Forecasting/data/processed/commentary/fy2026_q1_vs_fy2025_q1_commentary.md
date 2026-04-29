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
  "brief_type": "period_over_period",
  "comparison": "FY2026 Q1 vs FY2025 Q1",
  "current_period": {
    "label": "FY2026 Q1",
    "end_date": "2025-12-27"
  },
  "prior_period": {
    "label": "FY2025 Q1",
    "end_date": "2024-12-28"
  },
  "consolidated": {
    "current_millions": 143756.0,
    "prior_millions": 124300.0,
    "change_millions": 19456.0,
    "change_pct": 15.65
  },
  "by_segment": [
    {
      "name": "Americas",
      "current_millions": 58529.0,
      "prior_millions": 52648.0,
      "change_millions": 5881.0,
      "change_pct": 11.17
    },
    {
      "name": "Europe",
      "current_millions": 38146.0,
      "prior_millions": 33861.0,
      "change_millions": 4285.0,
      "change_pct": 12.65
    },
    {
      "name": "Greater China",
      "current_millions": 25526.0,
      "prior_millions": 18513.0,
      "change_millions": 7013.0,
      "change_pct": 37.88
    },
    {
      "name": "Japan",
      "current_millions": 9413.0,
      "prior_millions": 8987.0,
      "change_millions": 426.0,
      "change_pct": 4.74
    },
    {
      "name": "Rest of Asia Pacific",
      "current_millions": 12142.0,
      "prior_millions": 10291.0,
      "change_millions": 1851.0,
      "change_pct": 17.99
    }
  ],
  "by_product": [
    {
      "name": "Mac",
      "current_millions": 8386.0,
      "prior_millions": 8987.0,
      "change_millions": -601.0,
      "change_pct": -6.69
    },
    {
      "name": "Services",
      "current_millions": 30013.0,
      "prior_millions": 26340.0,
      "change_millions": 3673.0,
      "change_pct": 13.94
    },
    {
      "name": "Wearables Home and Accessories",
      "current_millions": 11493.0,
      "prior_millions": 11747.0,
      "change_millions": -254.0,
      "change_pct": -2.16
    },
    {
      "name": "iPad",
      "current_millions": 8595.0,
      "prior_millions": 8088.0,
      "change_millions": 507.0,
      "change_pct": 6.27
    },
    {
      "name": "iPhone",
      "current_millions": 85269.0,
      "prior_millions": 69138.0,
      "change_millions": 16131.0,
      "change_pct": 23.33
    }
  ],
  "key_observations": [
    "Greater China was the fastest-growing segment at +37.88% YoY ($25.5B vs $18.5B).",
    "iPhone grew +23.33% YoY, the fastest of any product category.",
    "Mac declined -6.7% YoY."
  ]
}
        ```
