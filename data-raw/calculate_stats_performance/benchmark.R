# Time the old and the rewritten calculate_stats() (step 0.7 of PLAN.md)
#
# Usage (from the package root):
#   Rscript data-raw/calculate_stats_performance/benchmark.R [season] [runs]
#
# Runs the four combinations of summary_level and stat_type (season_type
# "REG", the default) on one season (default 2023) with the data in memory
# and the loaders mocked, so downloads don't count. Each call runs once to
# warm up and then `runs` times (default 3); the table shows the median
# elapsed seconds. Uses the data of compare.R (hits/data_<season>.rds and
# hits/players.rds, run compare.R first if they're missing) and writes the
# table to hits/benchmark.md.

args <- commandArgs(trailingOnly = TRUE)
season <- if (length(args) >= 1) as.integer(args[[1]]) else 2023L
runs <- if (length(args) >= 2) as.integer(args[[2]]) else 3L
here <- "data-raw/calculate_stats_performance"
hits_dir <- file.path(here, "hits")

suppressMessages(devtools::load_all(quiet = TRUE))
library(data.table)
source(file.path(here, "calculate_stats_old.R"))
old <- source_calculate_stats_old()

dat <- readRDS(file.path(hits_dir, sprintf("data_%d.rds", season)))
players <- readRDS(file.path(hits_dir, "players.rds"))
pbp <- dat$pbp
playstats <- dat$playstats
old$stats_load_from_url_old <- function(urls) playstats
old$stats_load_players_old <- function() players

median_time <- function(fn) {
  invisible(fn())
  times <- vapply(
    seq_len(runs),
    function(i) system.time(fn())[["elapsed"]],
    numeric(1)
  )
  stats::median(times)
}

time_combo <- function(summary_level, stat_type) {
  testthat::local_mocked_bindings(
    stats_load_from_url = function(urls) playstats,
    stats_load_players = function() players,
    .package = "nflfastR"
  )
  call <- function(f) {
    function() {
      suppressMessages(f(
        summary_level = summary_level,
        stat_type = stat_type,
        pbp = pbp
      ))
    }
  }
  data.table(
    combination = paste(summary_level, stat_type, sep = " / "),
    old = median_time(call(old$calculate_stats_old)),
    new = median_time(call(calculate_stats))
  )
}

res <- rbindlist(list(
  time_combo("season", "player"),
  time_combo("season", "team"),
  time_combo("week", "player"),
  time_combo("week", "team")
))
res[, speedup := round(old / new, 1)]

table <- c(
  sprintf(
    "%d season, %s plays, %s play stats rows, median of %d runs (seconds):",
    season,
    format(nrow(pbp), big.mark = ","),
    format(nrow(playstats), big.mark = ","),
    runs
  ),
  "",
  "| `summary_level` / `stat_type` | Old | New | Speedup |",
  "|---|---|---|---|",
  res[, sprintf("| %s | %.2f | %.2f | %.1fx |", combination, old, new, speedup)]
)
writeLines(table)
writeLines(table, file.path(hits_dir, "benchmark.md"))
