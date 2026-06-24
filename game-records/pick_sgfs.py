#!/usr/bin/env python3
"""Pick SGFs into `exciting.sgf` and `boring.sgf`.

Reads .sgf files from ./sgfs.

Categories:
  exciting.sgf: games ending in fewer than 100 moves
  boring.sgf:   games with at least 150 moves where, through move 150,
                both players' win percentages stay strictly between 0.4 and 0.6
                The copied SGF is cut to the first 150 moves.

Each output file contains one SGF game tree per line. Comments and RU are
stripped unless --comments is passed. The base filename is written into GN.

If allowed prefixes are supplied, only games where BOTH players' names start with
one of the prefixes are considered.

Example:
  ./pick_sgfs.py kata1-b28c512nbt kata1-b18c384nbt
"""

from __future__ import annotations

import argparse
import math
import re
import sys
import time
from pathlib import Path


SGF_DIR = Path("sgfs")
DIST_DIR = Path("../public")
EXCITING_FILE = DIST_DIR / "exciting.sgf"
BORING_FILE = DIST_DIR / "boring.sgf"

EXCITING_MAX_MOVES = 99
BORING_MOVES = 150
BORING_LOW = 0.4
BORING_HIGH = 0.6

PROP_PATTERNS = {
    name: re.compile(rf"{name}\[((?:\\.|[^\\\]])*)\]")
    for name in ("PW", "PB")
}
PLAYER_RE = re.compile(r"P[WB]\[(?:\\.|[^\\\]])*\]")
MOVE_RE = re.compile(r";[BW]\[(?:\\.|[^\\\]])*\]")
COMMENT_RE = re.compile(r"C\[((?:\\.|[^\\\]])*)\]")
RULESET_RE = re.compile(r"RU\[(?:\\.|[^\\\]])*\]")
GN_RE = re.compile(r"GN\[(?:\\.|[^\\\]])*\]")
ROOT_PROP_RE = re.compile(r"([A-Za-z]+)(?:\[(?:\\.|[^\\\]])*\])+")
WINRATE_RE = re.compile(r"^\s*([0-9]*\.?[0-9]+)\s+([0-9]*\.?[0-9]+)")
ANALYSIS_COMMENT_RE = re.compile(
    r"^\s*"
    r"([0-9]*\.?[0-9]+)\s+"  # black win probability
    r"([0-9]*\.?[0-9]+)\s+"  # white win probability
    r"[0-9]*\.?[0-9]+\s+"  # jigo/no-result probability
    r"([-+]?[0-9]*\.?[0-9]+)"  # point estimate
    r"(?:\s+v=\d+)?"  # visits
    r"(.*?)"
    r"\s*$"
)


def unescape_sgf_value(value: str) -> str:
    return re.sub(r"\\(.)", r"\1", value).strip()


def escape_sgf_value(value: str) -> str:
    return value.replace("\\", "\\\\").replace("]", "\\]")


def extract_players(sgf_text: str) -> tuple[str, str]:
    props: dict[str, str] = {}
    for name, pattern in PROP_PATTERNS.items():
        match = pattern.search(sgf_text)
        if match:
            props[name] = unescape_sgf_value(match.group(1))
    return props.get("PW", ""), props.get("PB", "")


def player_allowed(player: str, allowed_prefixes: list[str]) -> bool:
    return not allowed_prefixes or any(player.startswith(prefix) for prefix in allowed_prefixes)


def move_starts(sgf_text: str) -> list[int]:
    return [match.start() for match in MOVE_RE.finditer(sgf_text)]


def node_text(sgf_text: str, start: int, next_start: int | None) -> str:
    if next_start is None:
        end = sgf_text.rfind(")")
        if end == -1 or end < start:
            end = len(sgf_text)
    else:
        end = next_start
    return sgf_text[start:end]


def node_winrates(node: str) -> tuple[float, float] | None:
    comment = COMMENT_RE.search(node)
    if not comment:
        return None
    match = WINRATE_RE.search(unescape_sgf_value(comment.group(1)))
    if not match:
        return None
    return float(match.group(1)), float(match.group(2))


def is_boring(sgf_text: str, starts: list[int]) -> bool:
    if len(starts) < BORING_MOVES:
        return False

    for i in range(BORING_MOVES):
        start = starts[i]
        next_start = starts[i + 1] if i + 1 < len(starts) else None
        rates = node_winrates(node_text(sgf_text, start, next_start))
        if rates is None:
            return False
        black_wr, white_wr = rates
        if not (BORING_LOW < black_wr < BORING_HIGH and BORING_LOW < white_wr < BORING_HIGH):
            return False

    return True


def strip_comments(sgf_text: str) -> str:
    sgf_text = COMMENT_RE.sub("", sgf_text)
    sgf_text = RULESET_RE.sub("", sgf_text)
    return PLAYER_RE.sub("", sgf_text)


def rounded_int(value: float) -> int:
    if value >= 0:
        return math.floor(value + 0.5)
    return math.ceil(value - 0.5)


def compact_win_probability(value: str) -> int:
    # Keep win probabilities to two digits; 1.00 would otherwise become 100.
    return max(0, min(99, rounded_int(float(value) * 100)))


def compact_analysis_comments(sgf_text: str) -> str:
    def replace_comment(match: re.Match[str]) -> str:
        comment = unescape_sgf_value(match.group(1))
        analysis = ANALYSIS_COMMENT_RE.match(comment)
        if not analysis:
            return match.group(0)

        black = compact_win_probability(analysis.group(1))
        white = compact_win_probability(analysis.group(2))
        score = rounded_int(float(analysis.group(3)))
        rest = analysis.group(4).strip()
        compact = f"{black} {white} {score}"
        if rest:
            compact += f" {rest}"
        return f"C[{escape_sgf_value(compact)}]"

    return COMMENT_RE.sub(replace_comment, sgf_text)


def cut_to_first_moves(sgf_text: str, starts: list[int], move_count: int) -> str:
    if len(starts) <= move_count:
        return sgf_text

    cut_at = starts[move_count]
    prefix = sgf_text[:cut_at].rstrip()

    # Keep the outer game-tree close paren if this is a simple linear SGF.
    if prefix.endswith(")"):
        return prefix + "\n"
    return prefix + ")\n"


def gn_insert_pos(root: str) -> int:
    """Return position just after root FF and GM properties, if present."""
    pos = root.find("(;")
    pos = 0 if pos == -1 else pos + 2
    seen: set[str] = set()

    while True:
        match = ROOT_PROP_RE.match(root, pos)
        if not match:
            return pos
        if match.group(1) in {"FF", "GM"}:
            seen.add(match.group(1))
        pos = match.end()
        if {"FF", "GM"} <= seen:
            return pos


def set_game_name(sgf_text: str, game_name: str) -> str:
    starts = move_starts(sgf_text)
    root_end = starts[0] if starts else sgf_text.rfind(")")
    if root_end == -1:
        root_end = len(sgf_text)

    root = GN_RE.sub("", sgf_text[:root_end])
    rest = sgf_text[root_end:]
    insert_at = gn_insert_pos(root)
    return root[:insert_at] + f"GN[{escape_sgf_value(game_name)}]" + root[insert_at:] + rest


def output_line(src: Path, content: str, *, keep_comments: bool) -> str:
    if keep_comments:
        content = compact_analysis_comments(content)
    else:
        content = strip_comments(content)
    content = set_game_name(content, src.stem)
    return content.replace("\r", " ").replace("\n", " ").strip() + "\n"


def print_progress(done: int, total: int, exciting: int, boring: int, start_time: float, *, final: bool = False) -> None:
    if total == 0:
        return
    width = 30
    frac = done / total
    filled = int(width * frac)
    bar = "#" * filled + "-" * (width - filled)
    elapsed = time.monotonic() - start_time
    rate = done / elapsed if elapsed > 0 else 0.0
    end = "\n" if final else "\r"
    print(
        f"Scanning [{bar}] {done}/{total} ({frac * 100:5.1f}%) "
        f"exciting={exciting} boring={boring} {rate:,.0f} files/s",
        end=end,
        file=sys.stderr,
        flush=True,
    )


def main() -> int:
    parser = argparse.ArgumentParser(description="Pick exciting and boring SGFs from ./sgfs.")
    parser.add_argument("--comments", action="store_true", help="keep SGF comments in output files")
    parser.add_argument("allowed", nargs="*", help="optional allowed player-name prefixes")
    args = parser.parse_args()

    if not SGF_DIR.is_dir():
        parser.error(f"not a directory: {SGF_DIR}")

    DIST_DIR.mkdir(parents=True, exist_ok=True)

    paths = sorted(SGF_DIR.rglob("*.sgf"))
    total = len(paths)
    start_time = time.monotonic()
    last_progress = 0.0
    exciting = 0
    boring = 0
    skipped_players = 0
    skipped_missing_players = 0

    print_progress(0, total, exciting, boring, start_time)
    exciting_records: list[tuple[str, str]] = []
    boring_records: list[tuple[str, str]] = []

    for i, path in enumerate(paths, start=1):
        try:
            text = path.read_text(encoding="utf-8", errors="replace")
        except OSError as exc:
            print(f"\nwarning: could not read {path}: {exc}", file=sys.stderr)
            continue

        white, black = extract_players(text)
        if not white or not black:
            skipped_missing_players += 1
        elif not (player_allowed(white, args.allowed) and player_allowed(black, args.allowed)):
            skipped_players += 1
        else:
            starts = move_starts(text)
            game_hash = path.stem
            if len(starts) <= EXCITING_MAX_MOVES:
                exciting += 1
                exciting_records.append((game_hash, output_line(path, text, keep_comments=args.comments)))
            elif is_boring(text, starts):
                boring += 1
                boring_records.append((
                    game_hash,
                    output_line(
                        path,
                        cut_to_first_moves(text, starts, BORING_MOVES),
                        keep_comments=args.comments,
                    ),
                ))

        now = time.monotonic()
        if now - last_progress >= 0.2 or i == total:
            print_progress(i, total, exciting, boring, start_time, final=(i == total))
            last_progress = now

    with EXCITING_FILE.open("w", encoding="utf-8") as exciting_out:
        exciting_out.writelines(line for _, line in sorted(exciting_records))
    with BORING_FILE.open("w", encoding="utf-8") as boring_out:
        boring_out.writelines(line for _, line in sorted(boring_records))

    print(f"Scanned:  {total}", file=sys.stderr)
    print(f"Exciting: {exciting} -> {EXCITING_FILE}", file=sys.stderr)
    print(f"Boring:   {boring} -> {BORING_FILE} (cut to first {BORING_MOVES} moves)", file=sys.stderr)
    if args.allowed:
        print(f"Skipped by player filter: {skipped_players}", file=sys.stderr)
    if skipped_missing_players:
        print(f"Skipped missing PW/PB: {skipped_missing_players}", file=sys.stderr)

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
