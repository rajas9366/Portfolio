# Memo: Q1 FY26 Read-Through and Q2 Watch Items

**To:** VP, Retail Finance
**From:** [Candidate Name], Senior Financial Analyst (candidate)
**Re:** Q1 FY2026 results, Q2 forecast, and four insights for the next planning cycle
**Date:** Drafted from public 10-K (FY2025) and 10-Qs (Q1 FY25 – Q1 FY26)
**Pages:** 2

---

## Headline

Q1 FY26 revenue of **$143.8B (+15.6% YoY)** was the strongest quarter of growth
in our nine-quarter history, driven by an unprecedented iPhone 17 launch
($85.3B, **+23.3% YoY**) and a sharp reversal in Greater China ($25.5B,
**+37.9% YoY** after four consecutive declining quarters). Our Q2 forecast
calls for **~$120B (+25% YoY)**, which is aggressive — it assumes Q1's
launch strength carries forward, an assumption that warrants explicit review
before commit. The four insights below are framed for the FY27 annual
planning cycle and the Q2 close review.

## Insight 1 — Services has structurally re-rated company gross margin

Services revenue grew from $85.2B in FY23 to **$109.2B in FY25 (+13.5% YoY,
+28.1% over two years)** while company-wide gross margin expanded from
44.1% to 46.9%. The mechanical decomposition: Services GM rose 75.4%
(from 70.8%), Products GM was flat at ~37%. Mix shift alone accounts for
roughly 75% of the GM expansion; the rest is Services GM expansion.

**Why it matters.** Hardware unit forecasting still drives revenue
sensitivity, but **Services is now the GM lever**. The next planning cycle
should split the GM walk into mix-driven and rate-driven components and
isolate what management actions move each. A 1pp swing in Services GM is
worth ~$1.1B in annual gross profit at current run-rate (1pp × $109.2B
FY25 Services revenue) — larger than any realistic action on hardware GM.

## Insight 2 — Greater China is the highest-uncertainty line in the forecast

Three years of context: Greater China revenue went **-7.7% (FY24), -3.9% (FY25),
then +37.9% (Q1 FY26)**. The four-quarter trailing run-rate has only just
turned positive. Long-lived assets in the country dropped from $4.8B to
$3.6B (-25%) over FY25 — capacity that was not added back in Q1 FY26.

Our forecast calls for **Q2 FY26 Greater China at $20.5B (+28% YoY)**, with
an 80% interval of $17.7B – $27.4B — by far the widest band of any segment.
That width is honest: the model has one inflection point and no precedent
for sustained recovery in the training history.

**Why it matters.** Two scenarios are roughly equally consistent with the
data. **Pull-forward thesis:** iPhone 17 demand was concentrated in launch
quarter and Q2 reverts toward the four-quarter trailing average through
Q4 FY25 of **$16.1B**, ~20% below our forecast. **Sustained recovery
thesis:** the macro/competitive backdrop has genuinely improved and Q2
prints in line with our point estimate. Recommend tracking weekly
sell-through telemetry against both scenarios for the first six weeks
of Q2; the divergence will become visible in the data well before the print.

## Insight 3 — Wearables is in a multi-year decline that the bundle narrative is masking

Wearables, Home and Accessories has now declined two consecutive years
(**-7.1% FY24, -3.6% FY25**) and Q1 FY26 extended the trend at **-2.2% YoY**.
Within the broader iPhone-Watch-AirPods bundle, the attached Watch and
AirPods purchase rate is softening even as iPhone units accelerate.

**Why it matters.** Wearables is only $35.7B of revenue (8.6% of total) but
its trajectory affects the **lifetime customer value math** that supports
Services growth assumptions in the long-range plan. If the Watch/AirPods
attach rate continues falling while iPhone holds, the implicit Services
ARPU assumed in those models is drifting low. Worth flagging to FP&A
strategy for the FY27 LRP refresh.

## Insight 4 — The Q2 forecast carries a $6.3B iPhone bias-correction. Show the full picture.

Our ensemble forecast underpredicted Q1 FY26 iPhone by $6.3B because no
prior quarter in the training data resembled the iPhone 17 launch quarter.
The current Q2 forecast applies that residual as a forward bias correction,
producing **$66.1B** vs $59.8B on the unadjusted model — the difference is
material to consolidated.

**Why it matters.** Bias correction is a defensible methodology decision
(and standard practice), but at this small sample size it represents the
strongest single judgment call in the forecast. **Recommendation:** publish
both the raw ensemble forecast ($59.8B, +28% YoY) and the bias-corrected
forecast ($66.1B, +41% YoY) to the planning leadership audience, with the
explicit framing that Q2 iPhone is the single line item where reasonable
analysts could land $6B apart. The raw forecast is the floor case; the
corrected forecast is the central case. Q3 will give us a second
post-launch data point to refine the correction logic.

## What this work product is

This memo is built on a reproducible pipeline:
**(1)** SQL star-schema database loaded from XBRL extracts of Apple's FY25
10-K and four 10-Q filings, reconciled to the dollar across 16 control
totals; **(2)** a six-model walk-forward ensemble forecast with bootstrap
intervals; **(3)** an Anthropic-API-backed variance commentary drafter
with sentence-scoped number-grounding validation. 70 automated tests,
100% reconciliation. Code, data dictionary, and methodology writeup are in
the project repository.
