# Time every step of build_nflfastR_pbp() on one season and on one game.
# Usage (from the package root):
#   Rscript data-raw/parser_performance/benchmark.R <raw_dir> [season] [game_id] [verbose]
# verbose is TRUE (default) or FALSE and sets options(nflfastR.verbose)
# <raw_dir> is a directory with raw pbp files as written by save_raw_pbp().

args <- commandArgs(TRUE)
dir <- args[1]
season <- as.integer(if (length(args) >= 2) args[2] else 2023)
single_id <- if (length(args) >= 3) args[3] else "2023_05_KC_MIN"
verbose <- if (length(args) >= 4) as.logical(args[4]) else TRUE
options(nflfastR.verbose = verbose)

suppressMessages(devtools::load_all(quiet = TRUE))
future::plan("sequential")
games <- nflreadr::load_schedules(season)
ids <- games$game_id[!is.na(games$result)]
ids <- ids[file.exists(file.path(dir, season, paste0(ids, ".rds")))]

steps <- list(
  get_pbp_nfl = function(x, ids) {
    data.table::rbindlist(
      lapply(ids, get_pbp_nfl, dir = dir),
      fill = TRUE
    )
  },
  add_game_data = function(x, ids) add_game_data(x, games = games),
  add_nflscrapr_mutations = function(x, ids) add_nflscrapr_mutations(x),
  add_ep = function(x, ids) add_ep(x),
  add_air_yac_ep = function(x, ids) add_air_yac_ep(x),
  add_wp = function(x, ids) add_wp(x),
  add_air_yac_wp = function(x, ids) add_air_yac_wp(x),
  add_cp = function(x, ids) add_cp(x),
  add_drive_results = function(x, ids) add_drive_results(x),
  add_series_data = function(x, ids) add_series_data(x),
  restore_kickoff_attempt = function(x, ids) restore_kickoff_attempt(x),
  select_variables = function(x, ids) select_variables(x),
  clean_pbp = function(x, ids) clean_pbp(x, in_builder = TRUE),
  add_qb_epa = function(x, ids) add_qb_epa(x, in_builder = TRUE),
  add_xyac = function(x, ids) add_xyac(x, in_builder = TRUE),
  add_xpass = function(x, ids) add_xpass(x, in_builder = TRUE),
  decode_player_ids = function(x, ids) decode_player_ids(x, in_builder = TRUE)
)

run_steps <- function(ids) {
  x <- NULL
  out <- numeric()
  for (n in names(steps)) {
    s <- proc.time()[["elapsed"]]
    x <- suppressWarnings(suppressMessages(steps[[n]](x, ids)))
    out[n] <- proc.time()[["elapsed"]] - s
  }
  out
}

# untimed warm-up runs, because the first call is always slower (lazy loading,
# model caches, byte compilation and growth of the R heap)
invisible(run_steps(ids))
season_times <- run_steps(ids)
invisible(run_steps(single_id))
single_times <- apply(sapply(1:5, \(i) run_steps(single_id)), 1, stats::median)

res <- data.frame(
  step = names(steps),
  season_s = round(season_times, 2),
  single_game_ms = round(single_times * 1000)
)
print(res, row.names = FALSE)
cat(sprintf(
  "\nTotal: season %.1f s (%d games) | single game %d ms\n",
  sum(season_times),
  length(ids),
  round(sum(single_times) * 1000)
))
