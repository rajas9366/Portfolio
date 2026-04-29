"""
commentary_client.py
====================
Wrapper around the Anthropic Python SDK that produces variance commentary
from a structured brief. The wrapper is intentionally thin so the main
logic stays testable.

Two execution modes:
    LIVE        Real API call (requires ANTHROPIC_API_KEY env var and the
                anthropic package installed). Returns Claude's response.
    DRY_RUN     Returns the formed prompt without calling the API. Useful
                for prompt-engineering work and for environments without
                network access (CI, sandboxes, etc.).

The choice is automatic:
    - If ANTHROPIC_API_KEY is set AND the anthropic package imports → LIVE
    - Otherwise → DRY_RUN, with a banner explaining why

Public API:
    build_messages(brief, comparison_type) -> tuple[system_prompt, user_message]
    generate_commentary(brief, ...)        -> CommentaryResult
"""
from __future__ import annotations

import json
import logging
import os
import textwrap
from dataclasses import dataclass
from typing import Any

logger = logging.getLogger("commentary_client")


# =============================================================================
# Prompt construction
# =============================================================================
SYSTEM_PROMPT = textwrap.dedent("""\
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
""").strip()


def build_messages(brief: dict[str, Any]) -> tuple[str, str]:
    """
    Build the system prompt and user message that get sent to the model.

    Returns:
        (system_prompt, user_message)
    """
    user_msg = textwrap.dedent(f"""\
        Please draft variance commentary for the following comparison.

        INPUT BRIEF (every number you may use is in this JSON):
        ```json
        {json.dumps(brief, indent=2)}
        ```
    """).strip()
    return SYSTEM_PROMPT, user_msg


# =============================================================================
# API call
# =============================================================================
@dataclass
class CommentaryResult:
    commentary:    str            # the markdown text Claude produced (or the prompt in dry-run)
    mode:          str            # "live" | "dry_run"
    model:         str | None
    input_tokens:  int | None
    output_tokens: int | None
    system_prompt: str            # for the validation log
    user_message:  str
    api_error:     str | None = None


DEFAULT_MODEL       = "claude-sonnet-4-6"
DEFAULT_MAX_TOKENS  = 1024
DEFAULT_TEMPERATURE = 0.4         # low-ish — we want consistent, factual prose


def generate_commentary(
    brief:       dict[str, Any],
    model:       str = DEFAULT_MODEL,
    max_tokens:  int = DEFAULT_MAX_TOKENS,
    temperature: float = DEFAULT_TEMPERATURE,
    force_dry_run: bool = False,
) -> CommentaryResult:
    """
    Generate variance commentary for the given brief.

    If ANTHROPIC_API_KEY is set and the anthropic package is importable,
    we call the Messages API. Otherwise we return DRY_RUN output containing
    the formed prompt.
    """
    system_prompt, user_msg = build_messages(brief)

    if force_dry_run:
        return _dry_run_result(system_prompt, user_msg, reason="--dry-run flag set")

    api_key = os.environ.get("ANTHROPIC_API_KEY")
    if not api_key:
        return _dry_run_result(
            system_prompt, user_msg,
            reason="ANTHROPIC_API_KEY not set in environment",
        )

    try:
        import anthropic
    except ImportError:
        return _dry_run_result(
            system_prompt, user_msg,
            reason="anthropic package not installed (pip install anthropic)",
        )

    try:
        client = anthropic.Anthropic(api_key=api_key)
        response = client.messages.create(
            model=model,
            max_tokens=max_tokens,
            temperature=temperature,
            system=system_prompt,
            messages=[{"role": "user", "content": user_msg}],
        )
        # Concatenate any text blocks in the response
        text_blocks = [b.text for b in response.content if getattr(b, "type", None) == "text"]
        commentary = "\n".join(text_blocks).strip()
        return CommentaryResult(
            commentary    = commentary,
            mode          = "live",
            model         = response.model,
            input_tokens  = response.usage.input_tokens,
            output_tokens = response.usage.output_tokens,
            system_prompt = system_prompt,
            user_message  = user_msg,
        )
    except Exception as e:
        logger.error("Anthropic API call failed: %s", e)
        return CommentaryResult(
            commentary    = "",
            mode          = "live",
            model         = model,
            input_tokens  = None,
            output_tokens = None,
            system_prompt = system_prompt,
            user_message  = user_msg,
            api_error     = str(e),
        )


def _dry_run_result(system_prompt: str, user_msg: str, reason: str) -> CommentaryResult:
    """Build a DRY_RUN result with the prompt that would have been sent."""
    banner = textwrap.dedent(f"""\
        ============================================================
        DRY RUN — no API call was made
        Reason: {reason}
        ============================================================

        Set ANTHROPIC_API_KEY (and `pip install anthropic`) to call the live
        API. The system prompt and user message that would have been sent
        are shown below.

        --- SYSTEM PROMPT ---
        {system_prompt}

        --- USER MESSAGE ---
        {user_msg}
    """)
    return CommentaryResult(
        commentary    = banner,
        mode          = "dry_run",
        model         = None,
        input_tokens  = None,
        output_tokens = None,
        system_prompt = system_prompt,
        user_message  = user_msg,
    )
