#!/bin/sh
# Checklist hits of all seasons since 2001 (step 0.3 of PLAN.md).
# Usage (from the package root): sh data-raw/calculate_stats_performance/run_all.sh [last_season] [jobs]
# Seasons that already have hits_<season>.rds are skipped.
last=${1:-2025}
jobs=${2:-4}
dir=data-raw/calculate_stats_performance/hits
mkdir -p "$dir"
seq 2001 "$last" | while read s; do
  [ -f "$dir/hits_$s.rds" ] || echo "$s"
done | xargs -P "$jobs" -I{} sh -c \
  "Rscript data-raw/calculate_stats_performance/season_hits.R {} > $dir/log_{}.txt 2>&1; tail -1 $dir/log_{}.txt"
