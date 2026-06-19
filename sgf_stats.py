#!/usr/bin/env python3
"""Gather player game counts and win rates from SGF files.

Reads .sgf files from ./sgfs by default and extracts:
  PW[white_name], PB[black_name], RE[result]

For each player, counts total games regardless of color and wins regardless of color,
then prints a table ordered by total games descending.
"""

from __future__ import annotations

import argparse
import re
import sys
import time
from collections import defaultdict
from pathlib import Path


PROP_PATTERNS = {
    name: re.compile(rf"{name}\[((?:\\.|[^\\\]])*)\]")
    for name in ("PW", "PB", "RE")
}


def unescape_sgf_value(value: str) -> str:
    """Unescape a basic SGF property value."""
    return re.sub(r"\\(.)", r"\1", value).strip()


def extract_props(sgf_text: str) -> dict[str, str]:
    """Return PW, PB, and RE property values found in the file."""
    props: dict[str, str] = {}
    for name, pattern in PROP_PATTERNS.items():
        match = pattern.search(sgf_text)
        if match:
            props[name] = unescape_sgf_value(match.group(1))
    return props


def print_progress(done: int, total: int, start_time: float, *, final: bool = False) -> None:
    """Print a single-line progress bar to stderr."""
    if total == 0:
        return
    width = 30
    frac = done / total
    filled = int(width * frac)
    bar = "#" * filled + "-" * (width - filled)
    elapsed = time.monotonic() - start_time
    rate = done / elapsed if elapsed > 0 else 0.0
    eta = (total - done) / rate if rate > 0 else 0.0
    end = "\n" if final else "\r"
    print(
        f"Processing [{bar}] {done}/{total} ({frac * 100:5.1f}%) "
        f"{rate:,.0f} files/s ETA {eta:,.0f}s",
        end=end,
        file=sys.stderr,
        flush=True,
    )


def winner_from_result(result: str) -> str | None:
    """Return 'B', 'W', or None for draws/unknown/void results."""
    result = result.strip().upper()
    if result.startswith("B+"):
        return "B"
    if result.startswith("W+"):
        return "W"
    return None


def main() -> int:
    parser = argparse.ArgumentParser(description="Summarize player stats from SGF files.")
    parser.add_argument("sgf_dir", nargs="?", default="sgfs", help="directory containing .sgf files (default: sgfs)")
    parser.add_argument("--no-progress", action="store_true", help="disable progress output")
    args = parser.parse_args()

    sgf_dir = Path(args.sgf_dir)
    if not sgf_dir.is_dir():
        parser.error(f"not a directory: {sgf_dir}")

    stats = defaultdict(lambda: {"games": 0, "wins": 0})
    files_seen = 0
    files_skipped = 0

    paths = sorted(sgf_dir.rglob("*.sgf"))
    total_files = len(paths)
    start_time = time.monotonic()
    last_progress = 0.0
    final_progress_printed = False
    if not args.no_progress:
        print_progress(0, total_files, start_time)

    for path in paths:
        files_seen += 1
        try:
            text = path.read_text(encoding="utf-8", errors="replace")
        except OSError as exc:
            print(f"\nwarning: could not read {path}: {exc}", file=sys.stderr)
            files_skipped += 1
            now = time.monotonic()
            if not args.no_progress and (now - last_progress >= 0.2 or files_seen == total_files):
                print_progress(files_seen, total_files, start_time, final=(files_seen == total_files))
                last_progress = now
                final_progress_printed = files_seen == total_files
            continue

        props = extract_props(text)
        white = props.get("PW", "").strip()
        black = props.get("PB", "").strip()
        result = props.get("RE", "").strip()

        if not white or not black:
            print(f"\nwarning: skipping {path}: missing PW or PB", file=sys.stderr)
            files_skipped += 1
            now = time.monotonic()
            if not args.no_progress and (now - last_progress >= 0.2 or files_seen == total_files):
                print_progress(files_seen, total_files, start_time, final=(files_seen == total_files))
                last_progress = now
                final_progress_printed = files_seen == total_files
            continue

        stats[white]["games"] += 1
        stats[black]["games"] += 1

        winner = winner_from_result(result)
        if winner == "W":
            stats[white]["wins"] += 1
        elif winner == "B":
            stats[black]["wins"] += 1

        now = time.monotonic()
        if not args.no_progress and (now - last_progress >= 0.2 or files_seen == total_files):
            print_progress(files_seen, total_files, start_time, final=(files_seen == total_files))
            last_progress = now
            final_progress_printed = files_seen == total_files

    if not args.no_progress and total_files and not final_progress_printed:
        print_progress(files_seen, total_files, start_time, final=True)

    rows = sorted(
        ((player, data["games"], data["wins"]) for player, data in stats.items()),
        key=lambda row: (-(row[2] / row[1] if row[1] else 0.0), -row[1], row[0].lower()),
    )

    print(f"Files read: {files_seen - files_skipped}/{files_seen}")
    print()
    print(f"{'Player':40} {'Games':>7} {'Wins':>7} {'Win%':>7}")
    print(f"{'-' * 40} {'-' * 7} {'-' * 7} {'-' * 7}")
    for player, games, wins in rows:
        win_pct = (wins / games * 100.0) if games else 0.0
        print(f"{player[:40]:40} {games:7d} {wins:7d} {win_pct:6.1f}%")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
