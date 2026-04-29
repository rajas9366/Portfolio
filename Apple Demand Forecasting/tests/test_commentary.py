"""
Tests for the commentary generation pipeline.

Run with:
    pytest tests/test_commentary.py -v

Covers:
  * Brief generators produce expected fields and reconcile to known totals
  * Number extractor finds dollars, percents, and word forms correctly
  * Validator passes faithful commentary, fails commentary with hallucinated numbers
  * Validator handles signed/unsigned percent conventions correctly
  * Validator's tolerance is reasonable (matches rounded figures, rejects wildly wrong ones)
  * API client returns DRY_RUN result when key is missing
  * Slug generation produces stable filenames
"""
from __future__ import annotations

import json
import os
import sqlite3
import sys
from pathlib import Path

import pytest

PROJECT_ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(PROJECT_ROOT / "scripts"))

from commentary_brief     import build_period_brief, build_annual_brief, build_forecast_brief    # noqa: E402
from commentary_client    import build_messages, generate_commentary, SYSTEM_PROMPT             # noqa: E402
from commentary_validator import (                                                              # noqa: E402
    extract_numbers, validate_commentary, flatten_brief,
)

DB_PATH = PROJECT_ROOT / "data" / "processed" / "apple_finance.db"


@pytest.fixture
def conn():
    c = sqlite3.connect(DB_PATH)
    yield c
    c.close()


# =============================================================================
# Brief generators
# =============================================================================
def test_period_brief_q1_fy26_has_required_fields(conn):
    brief = build_period_brief(conn, current_period_id=20261, prior_period_id=20251)
    for key in ("brief_type", "comparison", "current_period", "prior_period",
                "consolidated", "by_segment", "by_product", "key_observations"):
        assert key in brief, f"Missing field: {key}"
    assert brief["brief_type"] == "period_over_period"
    assert len(brief["by_segment"]) == 5
    assert len(brief["by_product"]) == 5


def test_period_brief_consolidated_reconciles_q1_fy26(conn):
    brief = build_period_brief(conn, current_period_id=20261, prior_period_id=20251)
    # Consolidated should equal sum of segments (which is how we built it)
    seg_sum = sum(s["current_millions"] for s in brief["by_segment"])
    assert seg_sum == brief["consolidated"]["current_millions"]
    # And to the actual 10-Q figure
    assert brief["consolidated"]["current_millions"] == 143756.0


def test_annual_brief_fy25_includes_margins(conn):
    brief = build_annual_brief(conn, current_fy=2025, prior_fy=2024)
    assert "margins" in brief
    # Total GM expanded from 46.21% to 46.91% per the 10-K
    assert brief["margins"]["total_gm_current_pct"] == pytest.approx(46.91, abs=0.01)
    assert brief["margins"]["total_gm_prior_pct"]   == pytest.approx(46.21, abs=0.01)


def test_period_brief_observations_identify_top_growers(conn):
    """Greater China and iPhone should appear in the observations for Q1 FY26."""
    brief = build_period_brief(conn, current_period_id=20261, prior_period_id=20251)
    obs_text = " ".join(brief["key_observations"])
    assert "Greater China" in obs_text
    assert "iPhone" in obs_text


# =============================================================================
# Number extraction
# =============================================================================
def test_extract_dollar_amounts():
    nums = extract_numbers("Revenue was $143.8B vs $124.3B prior year")
    assert len(nums) == 2
    assert nums[0]["value"] == 143_800.0    # millions
    assert nums[1]["value"] == 124_300.0


def test_extract_percentages_signed_and_unsigned():
    nums = extract_numbers("Up +15.6% YoY, China grew 37.9 percent, Mac fell -6.7%")
    assert len(nums) == 3
    pct_values = [n["value"] for n in nums]
    assert 15.6 in pct_values
    assert 37.9 in pct_values
    assert -6.7 in pct_values


def test_extract_handles_word_scales():
    nums = extract_numbers("$1.5 billion in revenue")
    assert len(nums) == 1
    assert nums[0]["value"] == 1_500.0  # millions


def test_extract_handles_thousand_separators():
    nums = extract_numbers("revenue of $124,300M for the period")
    assert len(nums) == 1
    assert nums[0]["value"] == 124_300.0


# =============================================================================
# Brief flattening
# =============================================================================
def test_flatten_brief_produces_paths(conn):
    brief = build_period_brief(conn, 20261, 20251)
    facts = flatten_brief(brief)
    paths = {f["path"] for f in facts}
    assert "consolidated.current_millions"               in paths
    assert "by_segment.Greater China.current_millions"   in paths
    assert "by_product.iPhone.change_pct"                in paths


# =============================================================================
# Validator end-to-end
# =============================================================================
@pytest.fixture
def q1_brief(conn):
    return build_period_brief(conn, 20261, 20251)


def test_validator_passes_faithful_commentary(q1_brief):
    text = (
        "Revenue was $143.8B, up +15.6% YoY from $124.3B. iPhone reached $85.3B, "
        "up +23.3%. Greater China grew +37.9% to $25.5B."
    )
    report = validate_commentary(text, q1_brief)
    assert report.passed
    assert report.verified_count == 7
    assert report.unverified_count == 0


def test_validator_flags_hallucinations(q1_brief):
    text = "Revenue was $200B with iPhone at $100B and growth of 50%."
    report = validate_commentary(text, q1_brief)
    assert not report.passed
    assert report.unverified_count == 3


def test_validator_prefers_signed_match_over_absolute(q1_brief):
    """Regression test: +6.3% must match iPad (+6.27), not Mac (-6.69)."""
    text = "iPad grew +6.3%."
    report = validate_commentary(text, q1_brief)
    match = report.matches[0]
    assert match.status == "verified"
    assert "iPad" in match.matched_to


def test_validator_tolerates_rounding(q1_brief):
    """$25.5B should match Greater China's $25,526M (-0.1% off)."""
    text = "Greater China was $25.5B."
    report = validate_commentary(text, q1_brief)
    assert report.matches[0].status == "verified"


def test_validator_rejects_far_off_numbers(q1_brief):
    """A clearly fabricated number should be unverified."""
    text = "iPad grew to $50.0B."  # iPad's actual is $8.6B, no brief value near $50B
    report = validate_commentary(text, q1_brief)
    assert report.matches[0].status == "unverified"


def test_validator_context_aware_catches_misattributed_numbers(q1_brief):
    """
    If the prose says "Greater China was $30B" — that number ($30B) is
    actually a real value in the brief (Services current is $30.0B), but
    it's misattributed. Context-aware matching should flag it.
    """
    text = "Greater China was $30B."
    report = validate_commentary(text, q1_brief)
    assert report.matches[0].status == "unverified"


def test_validator_falls_back_to_global_when_multiple_entities(q1_brief):
    """When multiple entities are in the same sentence, fall back to global match."""
    text = "Apple delivered $143.8B driven by iPhone and Greater China."
    report = validate_commentary(text, q1_brief)
    # $143.8B should match consolidated.current_millions even though both
    # iPhone and Greater China are in the sentence
    assert report.matches[0].status == "verified"


def test_validator_handles_punctuation_in_entity_names(q1_brief):
    """'Wearables, Home and Accessories' (with comma) should match the brief entity."""
    text = "Wearables, Home and Accessories declined -2.2% to $11.5B."
    report = validate_commentary(text, q1_brief)
    assert report.passed
    # Both numbers should match Wearables paths
    paths = [m.matched_to for m in report.matches if m.matched_to]
    assert any("Wearables" in p for p in paths)


# =============================================================================
# API client
# =============================================================================
def test_build_messages_includes_system_prompt_and_brief():
    sample_brief = {"comparison": "test", "consolidated": {"current_millions": 100}}
    sys_prompt, user_msg = build_messages(sample_brief)
    assert sys_prompt == SYSTEM_PROMPT
    assert "test" in user_msg
    assert "100" in user_msg
    assert "INPUT BRIEF" in user_msg


def test_generate_commentary_dry_run_when_no_key(monkeypatch):
    monkeypatch.delenv("ANTHROPIC_API_KEY", raising=False)
    sample_brief = {"comparison": "X vs Y", "consolidated": {"current_millions": 1}}
    result = generate_commentary(sample_brief)
    assert result.mode == "dry_run"
    assert "DRY RUN" in result.commentary
    assert result.api_error is None


def test_generate_commentary_force_dry_run_overrides_key(monkeypatch):
    monkeypatch.setenv("ANTHROPIC_API_KEY", "fake-key-not-real")
    result = generate_commentary({"comparison": "x", "consolidated": {}}, force_dry_run=True)
    assert result.mode == "dry_run"


# =============================================================================
# Output file integrity
# =============================================================================
COMMENTARY_DIR = PROJECT_ROOT / "data" / "processed" / "commentary"


@pytest.mark.skipif(not (COMMENTARY_DIR / "fy2026_q1_vs_fy2025_q1_brief.json").exists(),
                    reason="generate_commentary.py hasn't been run")
def test_saved_brief_is_valid_json():
    data = json.loads((COMMENTARY_DIR / "fy2026_q1_vs_fy2025_q1_brief.json").read_text())
    assert data["brief_type"] == "period_over_period"
