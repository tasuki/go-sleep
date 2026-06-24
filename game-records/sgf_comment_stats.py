#!/usr/bin/env python3
"""Summarize KataGo-style numeric comments in SGF files.

The move comments in these files look like:

    C[0.53 0.47 0.00 0.4 v=600]

Based on the data, the fields appear to be:
  1. estimated White win probability
  2. estimated Black win probability
  3. estimated no-result/void probability
  4. estimated score lead from White's point of view (positive = White ahead)
  5. visit count (the integer after v=)

This script parses all matching C[...] comments and prints aggregate summaries.
"""

from __future__ import annotations

import argparse
import math
import re
import sys
import time
from collections import Counter, defaultdict
from dataclasses import dataclass
from pathlib import Path


COMMENT_RE = re.compile(
    r"C\[\s*"
    r"([-+]?(?:\d+(?:\.\d*)?|\.\d+))\s+"
    r"([-+]?(?:\d+(?:\.\d*)?|\.\d+))\s+"
    r"([-+]?(?:\d+(?:\.\d*)?|\.\d+))\s+"
    r"([-+]?(?:\d+(?:\.\d*)?|\.\d+))\s+"
    r"v=(\d+)"
    r"(?:\s+result=([^\]]+))?"
    r"\]"
)
RE_RE = re.compile(r"RE\[((?:\\.|[^\\\]])*)\]")


@dataclass(frozen=True)
class Comment:
    white_win: float
    black_win: float
    no_result: float
    white_score_lead: float
    visits: int
    result: str | None = None


def unescape_sgf_value(value: str) -> str:
    return re.sub(r"\\(.)", r"\1", value).strip()


def result_bucket(result: str | None) -> str:
    if not result:
        return "unknown"
    result = result.strip().upper()
    if result.startswith("W+"):
        return "W win"
    if result.startswith("B+"):
        return "B win"
    if result in {"0", "DRAW", "JIGO"}:
        return "draw/0"
    if result in {"VOID", "?", "UNKNOWN"}:
        return "void/unknown"
    return "other"


def percentile(sorted_values: list[float], pct: float) -> float:
    if not sorted_values:
        return float("nan")
    if len(sorted_values) == 1:
        return sorted_values[0]
    pos = (len(sorted_values) - 1) * pct / 100.0
    lo = math.floor(pos)
    hi = math.ceil(pos)
    if lo == hi:
        return sorted_values[lo]
    frac = pos - lo
    return sorted_values[lo] * (1.0 - frac) + sorted_values[hi] * frac


def summarize(values: list[float]) -> dict[str, float]:
    if not values:
        return {key: float("nan") for key in ("mean", "sd", "min", "p1", "p5", "p50", "p95", "p99", "max")}
    vals = sorted(values)
    mean = sum(vals) / len(vals)
    var = sum((x - mean) ** 2 for x in vals) / len(vals)
    return {
        "mean": mean,
        "sd": math.sqrt(var),
        "min": vals[0],
        "p1": percentile(vals, 1),
        "p5": percentile(vals, 5),
        "p50": percentile(vals, 50),
        "p95": percentile(vals, 95),
        "p99": percentile(vals, 99),
        "max": vals[-1],
    }


def print_summary_row(name: str, values: list[float]) -> None:
    s = summarize(values)
    print(
        f"{name:22} {len(values):10d} {s['mean']:10.4f} {s['sd']:10.4f} "
        f"{s['min']:10.4f} {s['p1']:10.4f} {s['p5']:10.4f} {s['p50']:10.4f} "
        f"{s['p95']:10.4f} {s['p99']:10.4f} {s['max']:10.4f}"
    )


def print_progress(done: int, total: int, start_time: float, *, final: bool = False) -> None:
    if total == 0:
        return
    width = 30
    frac = done / total
    filled = int(width * frac)
    elapsed = time.monotonic() - start_time
    rate = done / elapsed if elapsed > 0 else 0.0
    eta = (total - done) / rate if rate > 0 else 0.0
    end = "\n" if final else "\r"
    print(
        f"Processing [{'#' * filled}{'-' * (width - filled)}] {done}/{total} "
        f"({frac * 100:5.1f}%) {rate:,.0f} files/s ETA {eta:,.0f}s",
        end=end,
        file=sys.stderr,
        flush=True,
    )


def parse_comments(text: str) -> list[Comment]:
    comments: list[Comment] = []
    for match in COMMENT_RE.finditer(text):
        comments.append(
            Comment(
                white_win=float(match.group(1)),
                black_win=float(match.group(2)),
                no_result=float(match.group(3)),
                white_score_lead=float(match.group(4)),
                visits=int(match.group(5)),
                result=match.group(6).strip() if match.group(6) else None,
            )
        )
    return comments


def split_sgf_records(text: str) -> list[str]:
    """Return SGF game records from a file.

    Normal downloaded files contain one game. The files in public/ are newline-delimited
    SGF collections, with one complete game per line.
    """
    lines = [line.strip() for line in text.splitlines() if line.strip()]
    if len(lines) > 1 and all(line.startswith("(;") for line in lines):
        return lines
    return [text]


def main() -> int:
    parser = argparse.ArgumentParser(description="Summarize numeric C[...] comments in SGF files.")
    parser.add_argument(
        "sgf_path",
        nargs="?",
        default="sgfs",
        help=".sgf file or directory containing .sgf files (default: sgfs)",
    )
    parser.add_argument("--no-progress", action="store_true", help="disable progress output")
    parser.add_argument("--limit", type=int, default=0, help="only read this many SGF files (useful for sampling directories)")
    args = parser.parse_args()

    sgf_path = Path(args.sgf_path)
    if sgf_path.is_file():
        paths = [sgf_path]
    elif sgf_path.is_dir():
        paths = sorted(sgf_path.rglob("*.sgf"))
        if args.limit > 0:
            paths = paths[: args.limit]
    else:
        parser.error(f"not a file or directory: {sgf_path}")

    fields: dict[str, list[float]] = defaultdict(list)
    by_result: dict[str, dict[str, list[float]]] = defaultdict(lambda: defaultdict(list))
    result_counts: Counter[str] = Counter()
    files_read = 0
    records_read = 0
    records_with_comments = 0
    comments_seen = 0
    bad_probability_sums = 0

    start_time = time.monotonic()
    last_progress = 0.0
    if not args.no_progress:
        print_progress(0, len(paths), start_time)

    for i, path in enumerate(paths, 1):
        try:
            text = path.read_text(encoding="utf-8", errors="replace")
        except OSError as exc:
            print(f"\nwarning: could not read {path}: {exc}", file=sys.stderr)
            continue

        files_read += 1

        for record in split_sgf_records(text):
            records_read += 1
            re_match = RE_RE.search(record)
            sgf_result = unescape_sgf_value(re_match.group(1)) if re_match else None
            bucket = result_bucket(sgf_result)
            result_counts[bucket] += 1

            comments = parse_comments(record)
            if comments:
                records_with_comments += 1
            comments_seen += len(comments)

            for c in comments:
                prob_sum = c.white_win + c.black_win + c.no_result
                if abs(prob_sum - 1.0) > 0.025:
                    bad_probability_sums += 1
                values = {
                    "white_win_prob": c.white_win,
                    "black_win_prob": c.black_win,
                    "no_result_prob": c.no_result,
                    "white_score_lead": c.white_score_lead,
                    "visits": float(c.visits),
                    "prob_sum": prob_sum,
                }
                for name, value in values.items():
                    fields[name].append(value)
                    by_result[bucket][name].append(value)

            if comments:
                final = comments[-1]
                final_values = {
                    "final_white_win_prob": final.white_win,
                    "final_black_win_prob": final.black_win,
                    "final_no_result_prob": final.no_result,
                    "final_white_score_lead": final.white_score_lead,
                    "final_visits": float(final.visits),
                }
                for name, value in final_values.items():
                    fields[name].append(value)
                    by_result[bucket][name].append(value)

        now = time.monotonic()
        if not args.no_progress and (now - last_progress >= 0.2 or i == len(paths)):
            print_progress(i, len(paths), start_time, final=(i == len(paths)))
            last_progress = now

    print(f"Files read: {files_read}")
    print(f"SGF records read: {records_read}")
    print(f"SGF records with matching comments: {records_with_comments}")
    print(f"Matching comments: {comments_seen}")
    print(f"Comments with probability sum outside 1±0.025: {bad_probability_sums}")
    print("Result buckets:", ", ".join(f"{k}={v}" for k, v in sorted(result_counts.items())))
    print()

    print("All matching comments:")
    print(f"{'Field':20} {'N':>10} {'Mean':>10} {'SD':>10} {'Min':>10} {'P01':>10} {'P05':>10} {'P50':>10} {'P95':>10} {'P99':>10} {'Max':>10}")
    print("-" * 131)
    for name in ("white_win_prob", "black_win_prob", "no_result_prob", "white_score_lead", "visits", "prob_sum"):
        print_summary_row(name, fields[name])

    print("\nFinal comment by game result:")
    for bucket in sorted(by_result):
        if not by_result[bucket].get("final_white_win_prob"):
            continue
        print(f"\n{bucket}:")
        for name in ("final_white_win_prob", "final_black_win_prob", "final_no_result_prob", "final_white_score_lead", "final_visits"):
            print_summary_row(name, by_result[bucket][name])

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
