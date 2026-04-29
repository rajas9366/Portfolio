"""
generate_commentary.py
======================
End-to-end commentary pipeline: build brief from DB, call Claude, validate
that every number in the response ties to a fact in the brief, and write
all artifacts to disk.

Usage:
    # Q1 FY2026 vs Q1 FY2025 YoY commentary
    python scripts/generate_commentary.py period --current 20261 --prior 20251

    # FY2025 vs FY2024 annual recap
    python scripts/generate_commentary.py annual --current-fy 2025 --prior-fy 2024

    # Q2 FY2026 forecast vs Q2 FY2025 actual
    python scripts/generate_commentary.py forecast --baseline 20252

    # Force dry-run (don't call API even if key is set)
    python scripts/generate_commentary.py period --current 20261 --prior 20251 --dry-run

Outputs (per run, to data/processed/commentary/):
    <slug>_brief.json              The structured input that went to the model
    <slug>_commentary.md           The generated markdown commentary
    <slug>_validation.json         Number-grounding report (machine readable)
    <slug>_validation.txt          Number-grounding report (human readable)
    <slug>_metadata.json           Model name, token counts, mode, errors

Exit codes:
    0  success, validation passed
    1  pipeline error (API failure, file IO, etc.)
    2  validation failed (one or more numbers in commentary not grounded)
"""
from __future__ import annotations

import argparse
import json
import logging
import sqlite3
import sys
from pathlib import Path

PROJECT_ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(PROJECT_ROOT / "scripts"))

from commentary_brief     import build_period_brief, build_annual_brief, build_forecast_brief    # noqa: E402
from commentary_client    import generate_commentary, DEFAULT_MODEL                              # noqa: E402
from commentary_validator import validate_commentary                                             # noqa: E402

DEFAULT_DB           = PROJECT_ROOT / "data" / "processed" / "apple_finance.db"
DEFAULT_OUT          = PROJECT_ROOT / "data" / "processed" / "commentary"
DEFAULT_FORECAST_CSV = PROJECT_ROOT / "data" / "processed" / "forecasts" / "forecast_v1.csv"

logger = logging.getLogger("generate_commentary")


# =============================================================================
# Slug helpers — turn brief metadata into stable filenames
# =============================================================================
def _slug(brief: dict) -> str:
    cmp_text = brief["comparison"].lower()
    return (
        cmp_text.replace(" vs ", "_vs_")
                .replace(" ", "_")
                .replace(",", "")
                .replace("(", "").replace(")", "")
    )


# =============================================================================
# Command handlers
# =============================================================================
def cmd_period(args: argparse.Namespace) -> int:
    conn = sqlite3.connect(args.db_path)
    try:
        brief = build_period_brief(
            conn,
            current_period_id=args.current,
            prior_period_id=args.prior,
        )
    finally:
        conn.close()
    return _run_pipeline(brief, args)


def cmd_annual(args: argparse.Namespace) -> int:
    conn = sqlite3.connect(args.db_path)
    try:
        brief = build_annual_brief(
            conn,
            current_fy=args.current_fy,
            prior_fy=args.prior_fy,
        )
    finally:
        conn.close()
    return _run_pipeline(brief, args)


def cmd_forecast(args: argparse.Namespace) -> int:
    conn = sqlite3.connect(args.db_path)
    try:
        brief = build_forecast_brief(
            conn,
            forecast_csv=args.forecast_csv,
            comparison_period_id=args.baseline,
            horizon=args.horizon,
        )
    finally:
        conn.close()
    return _run_pipeline(brief, args)


# =============================================================================
# Pipeline
# =============================================================================
def _run_pipeline(brief: dict, args: argparse.Namespace) -> int:
    output_dir = args.output_dir
    output_dir.mkdir(parents=True, exist_ok=True)
    slug = _slug(brief)

    # 1) Persist the brief
    brief_path = output_dir / f"{slug}_brief.json"
    brief_path.write_text(json.dumps(brief, indent=2))
    logger.info("Wrote brief to %s", brief_path)

    # 2) Generate commentary
    result = generate_commentary(
        brief,
        model=args.model,
        max_tokens=args.max_tokens,
        temperature=args.temperature,
        force_dry_run=args.dry_run,
    )

    commentary_path = output_dir / f"{slug}_commentary.md"
    commentary_path.write_text(result.commentary)
    logger.info("Wrote commentary to %s (mode=%s)", commentary_path, result.mode)

    metadata = {
        "comparison":    brief["comparison"],
        "mode":          result.mode,
        "model":         result.model,
        "input_tokens":  result.input_tokens,
        "output_tokens": result.output_tokens,
        "api_error":     result.api_error,
    }
    (output_dir / f"{slug}_metadata.json").write_text(json.dumps(metadata, indent=2))

    # If the pipeline didn't actually call the API, skip validation
    if result.mode != "live" or result.api_error:
        logger.info("Skipping validation (no live commentary to validate). "
                    "Run with ANTHROPIC_API_KEY set to validate.")
        return 0 if result.api_error is None else 1

    # 3) Validate
    report = validate_commentary(result.commentary, brief)
    (output_dir / f"{slug}_validation.json").write_text(
        json.dumps(report.to_dict(), indent=2),
    )
    (output_dir / f"{slug}_validation.txt").write_text(report.to_text())
    logger.info("Wrote validation report to %s", output_dir / f"{slug}_validation.txt")

    if report.passed:
        logger.info(
            "VALIDATION PASSED — %d numbers grounded, 0 unverified",
            report.verified_count,
        )
        return 0

    logger.warning(
        "VALIDATION FAILED — %d unverified numbers in commentary",
        report.unverified_count,
    )
    return 2


# =============================================================================
# CLI
# =============================================================================
def _add_common_args(p: argparse.ArgumentParser) -> None:
    p.add_argument("--db-path",     type=Path, default=DEFAULT_DB)
    p.add_argument("--output-dir",  type=Path, default=DEFAULT_OUT)
    p.add_argument("--model",       type=str,  default=DEFAULT_MODEL,
                   help=f"Anthropic model name (default: {DEFAULT_MODEL})")
    p.add_argument("--max-tokens",  type=int,  default=1024)
    p.add_argument("--temperature", type=float, default=0.4)
    p.add_argument("--dry-run", action="store_true",
                   help="Skip the API call and print the prompt that would be sent")
    p.add_argument("--verbose", "-v", action="store_true")


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__.split("\n")[1])
    sub = parser.add_subparsers(dest="cmd", required=True)

    p_period = sub.add_parser("period",   help="YoY commentary on two quarterly periods")
    p_period.add_argument("--current", type=int, required=True,
                          help="period_id of the current quarter (e.g. 20261 = FY26 Q1)")
    p_period.add_argument("--prior",   type=int, required=True,
                          help="period_id of the comparison quarter (e.g. 20251 = FY25 Q1)")
    _add_common_args(p_period)
    p_period.set_defaults(func=cmd_period)

    p_annual = sub.add_parser("annual", help="YoY commentary on two fiscal years")
    p_annual.add_argument("--current-fy", type=int, required=True)
    p_annual.add_argument("--prior-fy",   type=int, required=True)
    _add_common_args(p_annual)
    p_annual.set_defaults(func=cmd_annual)

    p_forecast = sub.add_parser("forecast",
                                help="Commentary explaining a forecast vs a baseline")
    p_forecast.add_argument("--forecast-csv", type=Path, default=DEFAULT_FORECAST_CSV)
    p_forecast.add_argument("--baseline",     type=int, required=True,
                            help="period_id of baseline quarter (e.g. 20252 = FY25 Q2)")
    p_forecast.add_argument("--horizon",      type=int, default=1,
                            help="Which horizon to comment on: 1=Q2, 2=Q3, 3=Q4 (default 1)")
    _add_common_args(p_forecast)
    p_forecast.set_defaults(func=cmd_forecast)

    args = parser.parse_args()
    logging.basicConfig(
        level=logging.DEBUG if args.verbose else logging.INFO,
        format="%(asctime)s [%(levelname)s] %(message)s",
        datefmt="%H:%M:%S",
    )
    return args.func(args)


if __name__ == "__main__":
    sys.exit(main())
