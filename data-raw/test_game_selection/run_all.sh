#!/usr/bin/env bash
# Record condition coverage for all 2001+ games.
#
# Usage (from the package root):
#   bash data-raw/test_game_selection/run_all.sh <raw_dir> [out_dir] [n_processes]
#
# One R process per season, n_processes at a time.

set -euo pipefail

RAW_DIR="${1:?raw directory required}"
OUT_DIR="${2:-data-raw/test_game_selection/hits}"
N_PROC="${3:-7}"

mkdir -p "$OUT_DIR"

# One schedule file for all processes
Rscript -e "saveRDS(nflreadr::load_schedules(2001:2030), '$OUT_DIR/games.rds')"

ls "$RAW_DIR" | grep -E '^[0-9]{4}$' | awk '$1 >= 2001' |
  xargs -P "$N_PROC" -I{} \
    Rscript data-raw/test_game_selection/run_season.R {} "$RAW_DIR" "$OUT_DIR"
