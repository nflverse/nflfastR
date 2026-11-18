# Compare the rewritten calculate_stats() with the old one on full seasons
# (step 0.7 of PLAN.md)
#
# Usage (from the package root):
#   Rscript data-raw/calculate_stats_performance/compare.R [seasons]
#
# seasons: comma separated, default 2001, 2012, 2023 and the most recent
# complete season. For every season and all 12 combinations of
# summary_level, stat_type and season_type, old and new run on the same
# pbp, play stats and players (loaders mocked, no download in the calls).
# Reports identical() per combination. If not identical, it prints the
# differences found by stats_differences() (tests/testthat/helpers.R) and
# the columns whose doubles differ only within its tolerance, for review.
#
# Data: hits/data_<season>.rds (pbp with checklist_pbp_columns and play
# stats, as written by season_hits.R) and hits/players.rds. Missing files are
# downloaded. The results go to hits/compare_results.csv.

args <- commandArgs(trailingOnly = TRUE)
here <- "data-raw/calculate_stats_performance"
hits_dir <- file.path(here, "hits")

suppressMessages(devtools::load_all(quiet = TRUE))
library(data.table)
source(file.path(here, "checklist.R"))
source(file.path(here, "calculate_stats_old.R"))
source("tests/testthat/helpers.R")
old <- source_calculate_stats_old()

# most_recent_season() switches to the new season at kickoff in September.
# From March to August it is complete, otherwise the one before is.
last_complete <- nflreadr::most_recent_season() -
  as.integer(!as.integer(format(Sys.Date(), "%m")) %in% 3:8)
seasons <- if (length(args) >= 1) {
  as.integer(strsplit(args[[1]], ",")[[1]])
} else {
  unique(c(2001L, 2012L, 2023L, last_complete))
}
stopifnot(all(seasons >= 2001L))

# readRDS() of a URL times out after 60 s on slow connections, so download
# to a file first
download_rds <- function(url) {
  file <- tempfile(fileext = ".rds")
  on.exit(unlink(file))
  curl::curl_download(url, file)
  readRDS(file)
}
release <- "https://github.com/nflverse/%s/releases/download/%s/%s.rds"

season_data <- function(season) {
  dir.create(hits_dir, showWarnings = FALSE)
  file <- file.path(hits_dir, sprintf("data_%d.rds", season))
  if (!file.exists(file)) {
    cli::cli_inform("Downloading {season}")
    pbp <- as.data.table(download_rds(sprintf(
      release,
      "nflverse-data",
      "pbp",
      paste0("play_by_play_", season)
    )))[, ..checklist_pbp_columns]
    playstats <- as.data.table(download_rds(sprintf(
      release,
      "nflverse-pbp",
      "playstats",
      paste0("play_stats_", season)
    )))
    saveRDS(list(pbp = pbp, playstats = playstats), file)
  }
  readRDS(file)
}

players_file <- file.path(hits_dir, "players.rds")
if (!file.exists(players_file)) {
  saveRDS(
    download_rds(sprintf(release, "nflverse-data", "players", "players")),
    players_file
  )
}
players <- readRDS(players_file)

combos <- CJ(
  summary_level = c("season", "week"),
  stat_type = c("player", "team"),
  season_type = c("REG", "POST", "REG+POST"),
  sorted = FALSE
)

run_new <- function(pbp, playstats, combo) {
  testthat::local_mocked_bindings(
    stats_load_from_url = function(urls) playstats,
    stats_load_players = function() players,
    .package = "nflfastR"
  )
  suppressMessages(calculate_stats(
    summary_level = combo$summary_level,
    stat_type = combo$stat_type,
    season_type = combo$season_type,
    pbp = pbp
  ))
}

run_old <- function(pbp, playstats, combo) {
  old$stats_load_from_url_old <- function(urls) playstats
  old$stats_load_players_old <- function() players
  suppressMessages(old$calculate_stats_old(
    summary_level = combo$summary_level,
    stat_type = combo$stat_type,
    season_type = combo$season_type,
    pbp = pbp
  ))
}

# columns that aren't identical although stats_differences() finds nothing:
# doubles within the tolerance
within_tolerance <- function(new, exp) {
  cols <- intersect(names(exp), names(new))
  if (nrow(new) != nrow(exp)) {
    return(character())
  }
  differ <- cols[
    !vapply(cols, \(col) identical(new[[col]], exp[[col]]), logical(1))
  ]
  vapply(
    differ,
    function(col) {
      x <- new[[col]]
      y <- exp[[col]]
      rel <- abs(x - y) / pmax(abs(x), abs(y))
      sprintf("%s (max rel. diff %.2g)", col, max(rel, na.rm = TRUE))
    },
    character(1)
  )
}

results <- list()
for (season in seasons) {
  dat <- season_data(season)
  for (i in seq_len(nrow(combos))) {
    combo <- combos[i]
    name <- paste(
      season,
      combo_name(
        combo$summary_level,
        combo$stat_type,
        combo$season_type
      )
    )
    new <- run_new(dat$pbp, dat$playstats, combo)
    exp <- run_old(dat$pbp, dat$playstats, combo)
    same <- identical(new, exp)
    cli::cli_inform("{name}: {if (same) 'identical' else 'DIFFERENT'}")
    msg <- character()
    if (!same) {
      res <- stats_differences(new, exp)
      msg <- res$messages
      if (length(msg) == 0L) {
        msg <- paste("Within tolerance:", toString(within_tolerance(new, exp)))
      }
      cat(paste("  -", msg), sep = "\n")
    }
    results[[name]] <- data.table(
      season = season,
      combo = combo_name(
        combo$summary_level,
        combo$stat_type,
        combo$season_type
      ),
      identical = same,
      rows = nrow(new),
      differences = paste(msg, collapse = " | ")
    )
  }
}
results <- rbindlist(results)
fwrite(results, file.path(hits_dir, "compare_results.csv"))
cli::cli_inform(
  "{sum(results$identical)} of {nrow(results)} combinations identical."
)
