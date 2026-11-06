# Record condition coverage for every game of one season.
#
# Usage (from the package root):
#   Rscript data-raw/test_game_selection/run_season.R <season> <raw_dir> <out_dir>
#
# Writes <out_dir>/hits_<season>.rds, a list with
#   targets: the instrumented code locations (same for every season)
#   games:   one row per game with the hit items, play count, error and time
#
# The schedule is read from <out_dir>/games.rds (see run_all.sh) so that
# every process uses the same file and nothing is downloaded.

args <- commandArgs(trailingOnly = TRUE)
season <- args[[1]]
raw_dir <- args[[2]]
out_dir <- args[[3]]

source("data-raw/test_game_selection/instrument.R")

suppressPackageStartupMessages(ns <- load_instrumented("."))
cov <- cov_state()
future::plan("sequential")
games <- readRDS(file.path(out_dir, "games.rds"))

game_ids <- sub("\\.rds$", "", list.files(file.path(raw_dir, season), "\\.rds$"))

res <- vector("list", length(game_ids))
for (i in seq_along(game_ids)) {
  id <- game_ids[[i]]
  cov$reset()
  t0 <- Sys.time()
  err <- NA_character_
  n_plays <- NA_integer_
  pbp <- tryCatch(
    suppressMessages(suppressWarnings(
      ns$build_nflfastR_pbp(id, dir = raw_dir, games = games, rules = FALSE)
    )),
    error = function(e) {
      err <<- conditionMessage(e)
      NULL
    }
  )
  if (!is.null(pbp)) n_plays <- nrow(pbp)
  res[[i]] <- list(
    game_id = id,
    hits = current_hits(),
    n_plays = n_plays,
    error = err,
    secs = as.numeric(difftime(Sys.time(), t0, units = "secs"))
  )
  if (i %% 25 == 0) message(season, ": ", i, "/", length(game_ids))
}

out <- list(
  targets = cov$targets,
  games = tibble::tibble(
    game_id = vapply(res, `[[`, "", "game_id"),
    hits = lapply(res, `[[`, "hits"),
    n_plays = vapply(res, `[[`, 0L, "n_plays"),
    error = vapply(res, `[[`, "", "error"),
    secs = vapply(res, `[[`, 0, "secs")
  )
)
saveRDS(out, file.path(out_dir, paste0("hits_", season, ".rds")))
message(season, ": done")
