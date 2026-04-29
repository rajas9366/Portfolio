"""
commentary_validator.py
=======================
Number-grounding validator. Extracts every dollar, percentage, and counted
figure from a generated commentary and checks each against the source brief.

This is the safety layer that turns "LLM-generated commentary" into something
a finance team would actually deploy. Without grounding, an LLM can fluently
invent plausible-but-wrong numbers — the cardinal sin in any FP&A function.

Validator output for each number found in the prose:
    {
        "raw":          "$25.5B"           # as it appeared
        "value":        25500              # normalized to millions
        "unit":         "millions"         # "millions" | "percent"
        "status":       "verified"         # "verified" | "unverified" | "ambiguous"
        "matched_to":   "by_segment.Greater China.current_millions"   # path in brief
        "tolerance":    50                 # absolute tolerance used for the match
    }

Any "unverified" rows surface to the user as potential hallucinations to
review before publishing the commentary.

Public API:
    extract_numbers(text)              -> list[dict]   # parsed numbers from prose
    flatten_brief(brief)               -> list[dict]   # every value with path
    validate_commentary(text, brief)   -> ValidationReport
"""
from __future__ import annotations

import re
from dataclasses import dataclass, field
from typing import Any


# =============================================================================
# Numeric extraction from prose
# =============================================================================

# Match dollar amounts: $143.8B, $1,234M, $85.3 billion, $1.5 trillion
_DOLLAR_RE = re.compile(
    r"""
    \$\s*                                           # dollar sign + optional space
    (?P<amount>\d{1,3}(?:,\d{3})*(?:\.\d+)?)       # the number
    \s*
    (?P<scale>[BMK]|billion|million|thousand|trillion)?\b
    """,
    re.IGNORECASE | re.VERBOSE,
)

# Percentages: +15.6%, -7.7%, 23.33 percent
_PERCENT_RE = re.compile(
    r"""
    (?<![\w])                                       # not preceded by word char
    (?P<sign>[+-])?
    (?P<value>\d+(?:\.\d+)?)
    \s*
    (?:%|percent\b|percentage\s+points?\b|pp\b|ppt\b)
    """,
    re.IGNORECASE | re.VERBOSE,
)

_SCALE_TO_MILLIONS = {
    "B": 1_000.0, "BILLION": 1_000.0,
    "M": 1.0,     "MILLION": 1.0,
    "K": 0.001,   "THOUSAND": 0.001,
    "T": 1_000_000.0, "TRILLION": 1_000_000.0,
}


def extract_numbers(text: str) -> list[dict[str, Any]]:
    """Extract every dollar amount and percentage in `text`."""
    out: list[dict] = []

    for m in _DOLLAR_RE.finditer(text):
        amount_str = m.group("amount").replace(",", "")
        scale_str = (m.group("scale") or "M").upper()
        try:
            amount = float(amount_str)
        except ValueError:
            continue
        scale = _SCALE_TO_MILLIONS.get(scale_str, 1.0)
        out.append({
            "raw":         m.group(0).strip(),
            "value":       round(amount * scale, 3),  # always millions
            "unit":        "millions",
            "span":        (m.start(), m.end()),
        })

    for m in _PERCENT_RE.finditer(text):
        sign = m.group("sign") or ""
        try:
            v = float(m.group("value"))
        except ValueError:
            continue
        if sign == "-":
            v = -v
        out.append({
            "raw":         m.group(0).strip(),
            "value":       v,
            "unit":        "percent",
            "span":        (m.start(), m.end()),
        })

    return out


# =============================================================================
# Brief flattening — produce searchable list of facts
# =============================================================================
def flatten_brief(brief: dict[str, Any], prefix: str = "") -> list[dict]:
    """
    Recursively walk a brief and produce a list of every numeric value with
    a dotted path. Used to look up whether a claim in the prose corresponds
    to a fact in the brief.
    """
    facts: list[dict] = []

    def _add(path: str, value: float, unit: str) -> None:
        facts.append({"path": path, "value": float(value), "unit": unit})

    def _walk(node: Any, path: str) -> None:
        if isinstance(node, dict):
            for k, v in node.items():
                _walk(v, f"{path}.{k}" if path else k)
        elif isinstance(node, list):
            for i, item in enumerate(node):
                # If items have a 'name' key, use it as the key (more readable paths)
                key = item.get("name") if isinstance(item, dict) and "name" in item else str(i)
                _walk(item, f"{path}.{key}" if path else key)
        elif isinstance(node, (int, float)) and not isinstance(node, bool):
            unit = _infer_unit(path)
            _add(path, node, unit)

    _walk(brief, prefix)
    return facts


def _infer_unit(path: str) -> str:
    """Guess units from key naming conventions used by commentary_brief.py."""
    p = path.lower()
    if p.endswith("_pct") or p.endswith("_percent") or "gm" in p and "pct" in p:
        return "percent"
    if p.endswith("_millions") or p.endswith("_b") or "millions" in p:
        return "millions"
    return "unknown"


# =============================================================================
# Matching — does this prose number tie to a brief fact?
# =============================================================================
@dataclass
class MatchResult:
    raw:        str
    value:      float
    unit:       str
    status:     str             # "verified" | "unverified" | "ambiguous"
    matched_to: str | None = None
    tolerance:  float | None = None
    note:       str | None = None


def _match_number(num: dict, brief_facts: list[dict],
                  context: str | None = None,
                  entity_names: list[str] | None = None) -> MatchResult:
    """
    Return the best match (if any) for a single extracted number.

    If `context` (the surrounding text) and `entity_names` (a list of segment/
    product names from the brief) are provided, we prefer matches scoped to
    whichever entity name appears nearest to the number in the prose. This
    catches cases like "Greater China was $30B" where $30B happens to also
    be a value from another entity's row — the proximity to "Greater China"
    in the text means we should match against China's facts, not Services'.
    """
    candidates = [f for f in brief_facts if f["unit"] == num["unit"]]

    # ----- context-aware filter: scope to whichever entity name is nearby ---
    scoped_path: str | None = None
    if context and entity_names:
        scoped_path = _detect_entity_scope(context, entity_names)
    if scoped_path:
        scoped_candidates = [f for f in candidates if scoped_path in f["path"]]
        if scoped_candidates:
            # First try a match within the scoped candidates only.
            scoped_match = _try_match(num, scoped_candidates)
            if scoped_match.status == "verified":
                return scoped_match
            # If no scoped match, the number is unverified for this entity —
            # do NOT silently fall back to global match (that's how false
            # positives like "Greater China was $30B → Services" sneak in).
            return MatchResult(
                raw=num["raw"], value=num["value"], unit=num["unit"],
                status="unverified",
                note=f"No matching value found under {scoped_path} (entity scope detected from prose)",
            )

    return _try_match(num, candidates)


def _try_match(num: dict, candidates: list[dict]) -> MatchResult:
    """The actual numeric matching logic. Used both globally and within entity scope."""
    # Tolerances tuned for the rounding patterns used in finance memos.
    # Dollar values: 0.5% relative or $50M absolute, whichever is larger.
    # Percentages:   0.5 absolute (so 15.6% matches 15.65%).
    if num["unit"] == "millions":
        for cand in candidates:
            target  = cand["value"]
            abs_tol = max(50.0, abs(target) * 0.005)
            if abs(num["value"] - target) <= abs_tol:
                return MatchResult(
                    raw=num["raw"], value=num["value"], unit=num["unit"],
                    status="verified", matched_to=cand["path"], tolerance=abs_tol,
                )
        return MatchResult(
            raw=num["raw"], value=num["value"], unit=num["unit"],
            status="unverified",
            note="No dollar value in scope within tolerance",
        )

    if num["unit"] == "percent":
        # First pass: prefer DIRECT signed matches across all candidates
        for cand in candidates:
            target = cand["value"]
            if abs(num["value"] - target) <= 0.5:
                return MatchResult(
                    raw=num["raw"], value=num["value"], unit=num["unit"],
                    status="verified", matched_to=cand["path"], tolerance=0.5,
                )
        # Second pass: allow matches against the *negative* of a brief value
        # (the model often phrases "declined X%" without the sign). This is a
        # fallback because direct sign matching is always preferred.
        for cand in candidates:
            target = cand["value"]
            if abs(num["value"] - abs(target)) <= 0.5:
                return MatchResult(
                    raw=num["raw"], value=num["value"], unit=num["unit"],
                    status="verified", matched_to=cand["path"], tolerance=0.5,
                    note="matched against absolute value (sign convention)",
                )
        return MatchResult(
            raw=num["raw"], value=num["value"], unit=num["unit"],
            status="unverified",
            note="No percent value in scope within tolerance",
        )

    return MatchResult(
        raw=num["raw"], value=num["value"], unit=num["unit"],
        status="ambiguous", note="Could not determine unit",
    )


def _detect_entity_scope(context: str, entity_names: list[str]) -> str | None:
    """
    If exactly one entity name from the brief appears in the context window,
    return that name (used as a path-substring filter for matching). If
    multiple appear, we return None so the matcher considers all candidates
    (since it's unclear which entity the number refers to).

    Matching is whitespace-and-punctuation tolerant: "Wearables Home and
    Accessories" in the brief matches "Wearables, Home and Accessories"
    in the prose (the comma is purely a stylistic comma in the rendered text).
    """
    norm_ctx = _normalize(context)
    found = [name for name in entity_names if _normalize(name) in norm_ctx]
    if len(found) == 1:
        return found[0]
    return None


def _normalize(s: str) -> str:
    """Strip commas and collapse whitespace for tolerant entity-name matching."""
    return re.sub(r"\s+", " ", s.replace(",", "")).strip()


# =============================================================================
# Top-level API
# =============================================================================
@dataclass
class ValidationReport:
    matches:       list[MatchResult] = field(default_factory=list)
    verified_count:  int = 0
    unverified_count: int = 0
    ambiguous_count: int = 0

    @property
    def passed(self) -> bool:
        return self.unverified_count == 0

    def to_dict(self) -> dict:
        return {
            "passed":           self.passed,
            "verified_count":   self.verified_count,
            "unverified_count": self.unverified_count,
            "ambiguous_count":  self.ambiguous_count,
            "matches": [
                {
                    "raw":        m.raw,
                    "value":      m.value,
                    "unit":       m.unit,
                    "status":     m.status,
                    "matched_to": m.matched_to,
                    "tolerance":  m.tolerance,
                    "note":       m.note,
                } for m in self.matches
            ],
        }

    def to_text(self) -> str:
        lines = [
            "=" * 70,
            "Number-grounding validation",
            "=" * 70,
            f"Verified:    {self.verified_count}",
            f"Unverified:  {self.unverified_count}  {'(potential hallucinations)' if self.unverified_count else ''}",
            f"Ambiguous:   {self.ambiguous_count}",
            f"Result:      {'PASS' if self.passed else 'FAIL'}",
            "",
        ]
        if self.unverified_count:
            lines.append("UNVERIFIED NUMBERS:")
            for m in self.matches:
                if m.status == "unverified":
                    lines.append(f"  {m.raw:>10s}    {m.note}")
            lines.append("")
        lines.append("ALL MATCHES:")
        for m in self.matches:
            tag = {"verified": "[OK]  ", "unverified": "[FAIL]", "ambiguous": "[?]   "}[m.status]
            target = f" -> {m.matched_to}" if m.matched_to else ""
            lines.append(f"  {tag} {m.raw:>10s}{target}")
        return "\n".join(lines)


def validate_commentary(commentary: str, brief: dict) -> ValidationReport:
    """Run number extraction and matching, return a structured report."""
    numbers = extract_numbers(commentary)
    facts   = flatten_brief(brief)
    entity_names = _collect_entity_names(brief)
    sentence_for_pos = _build_sentence_index(commentary)
    report  = ValidationReport()

    for num in numbers:
        # The number's "context" for entity-scope detection is the SENTENCE
        # containing it. A 60-char window was too greedy — it grabbed entity
        # names from the surrounding sentence on either side and produced
        # false-negative scope mismatches.
        start = num["span"][0]
        context = sentence_for_pos(start)

        m = _match_number(num, facts, context=context, entity_names=entity_names)
        report.matches.append(m)
        if m.status == "verified":   report.verified_count += 1
        elif m.status == "unverified": report.unverified_count += 1
        else:                          report.ambiguous_count += 1
    return report


def _build_sentence_index(text: str):
    """
    Return a function that maps a character offset to the sentence containing it.
    Sentences are split on period+space/newline, exclamation, or question marks
    — adequate for finance prose where complex sentence parsing isn't worth it.
    """
    boundaries = [0]
    for m in re.finditer(r"[.!?](?:\s+|\Z)", text):
        boundaries.append(m.end())
    boundaries.append(len(text))

    def lookup(pos: int) -> str:
        for i in range(len(boundaries) - 1):
            if boundaries[i] <= pos < boundaries[i + 1]:
                return text[boundaries[i]:boundaries[i + 1]]
        return text  # fallback
    return lookup


def _collect_entity_names(brief: dict) -> list[str]:
    """Extract every entity name (segment, product) from the brief for context-aware matching."""
    names: list[str] = []
    for s in brief.get("by_segment", []):
        if isinstance(s, dict) and "name" in s:
            names.append(s["name"])
    for p in brief.get("by_product", []):
        if isinstance(p, dict) and "name" in p:
            names.append(p["name"])
    return names
