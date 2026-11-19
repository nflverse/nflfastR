# Compare calculate_stats() of two versions of nflfastR on full seasons:
# output and speed.
#
# Usage (from the package root, paths to the source of both versions, e.g. a
# git worktree of master and this checkout):
#   git worktree add ../nflfastR-master master
#   Rscript data-raw/calculate_stats_performance/compare_versions.R \
#     ../nflfastR-master . [seasons] [runs]
#
# seasons: comma separated, default 2001, 2012, 2023 and the most recent
# complete season. runs: timed runs per combination (median, after an untimed
# warm-up), default 3; 0 skips the timing.
#
# For every season, each version runs in a fresh R process that loads the
# package with pkgload::load_all() and runs all 12 combinations of
# summary_level, stat_type and season_type on the same pbp, play stats and
# players (loaders mocked, no download in the calls). The outputs are compared
# with stats_differences() of tests/testthat/helpers.R of the head version:
# "identical", "within tolerance" (doubles that differ only within the
# tolerance of the tests, accepted) or "DIFFERENT". The timings cover the four
# combinations of summary_level and stat_type with season_type "REG".
#
# Data: hits/data_<season>.rds (pbp columns of calculate_stats() and play
# stats) and hits/players.rds, downloaded if missing. Results:
# hits/compare_results.csv and hits/benchmark.md.

args <- commandArgs(trailingOnly = TRUE)

combos <- function() {
  data.table::CJ(
    summary_level = c("season", "week"),
    stat_type = c("player", "team"),
    season_type = c("REG", "POST", "REG+POST"),
    sorted = FALSE
  )
}

# ---- worker: all combinations of one version on one season -------------------
if (identical(args[1], "worker")) {
  pkg_dir <- args[2]
  data_file <- args[3]
  players_file <- args[4]
  out_file <- args[5]
  runs <- as.integer(args[6])

  suppressMessages(pkgload::load_all(pkg_dir, quiet = TRUE))
  ns <- asNamespace("nflfastR")
  if (!all(c("stats_load_from_url", "stats_load_players") %in% names(ns))) {
    stop(
      "The version in ",
      pkg_dir,
      " has no stats_load_from_url() or stats_load_players() to mock (#630)."
    )
  }
  dat <- readRDS(data_file)
  players <- readRDS(players_file)

  run <- function(combo) {
    testthat::local_mocked_bindings(
      stats_load_from_url = function(urls) dat$playstats,
      stats_load_players = function() players,
      .package = "nflfastR"
    )
    suppressMessages(calculate_stats(
      summary_level = combo$summary_level,
      stat_type = combo$stat_type,
      season_type = combo$season_type,
      pbp = dat$pbp
    ))
  }

  cmb <- combos()
  combo_names <- cmb[, paste(summary_level, stat_type, season_type, sep = "/")]
  # also the warm-up of the timings
  outputs <- stats::setNames(
    lapply(seq_len(nrow(cmb)), \(i) run(cmb[i])),
    combo_names
  )
  timed <- cmb[season_type == "REG"]
  times <- if (runs > 0L) {
    vapply(
      seq_len(nrow(timed)),
      function(i) {
        stats::median(vapply(
          seq_len(runs),
          \(r) system.time(run(timed[i]))[["elapsed"]],
          numeric(1)
        ))
      },
      numeric(1)
    )
  } else {
    rep(NA_real_, nrow(timed))
  }
  saveRDS(
    list(
      outputs = outputs,
      times = data.table::data.table(
        combination = timed[, paste(summary_level, stat_type, sep = " / ")],
        seconds = times
      )
    ),
    out_file
  )
  quit(save = "no")
}

# ---- controller ----------------------------------------------------------------
library(data.table)
if (length(args) < 2) {
  stop("Usage: compare_versions.R <base_dir> <head_dir> [seasons] [runs]")
}
base_dir <- normalizePath(args[1])
head_dir <- normalizePath(args[2])
here <- "data-raw/calculate_stats_performance"
hits_dir <- file.path(here, "hits")
dir.create(hits_dir, showWarnings = FALSE)

# most_recent_season() switches to the new season at kickoff in September.
# From March to August it is complete, otherwise the one before is.
last_complete <- nflreadr::most_recent_season() -
  as.integer(!as.integer(format(Sys.Date(), "%m")) %in% 3:8)
seasons <- if (length(args) >= 3) {
  as.integer(strsplit(args[[3]], ",")[[1]])
} else {
  unique(c(2001L, 2012L, 2023L, last_complete))
}
stopifnot(all(seasons >= 2001L))
runs <- if (length(args) >= 4) as.integer(args[[4]]) else 3L

# stats_differences() of the head version
source(file.path(head_dir, "tests", "testthat", "helpers.R"))

# the pbp columns calculate_stats() needs
pbp_columns <- c(
  "season",
  "game_id",
  "play_id",
  "posteam",
  "defteam",
  "special",
  "season_type",
  "away_score",
  "home_score",
  "field_goal_attempt",
  "fixed_drive",
  "score_differential",
  "play_type",
  "week",
  "passer_player_id",
  "qb_epa",
  "cpoe",
  "rusher_player_id",
  "epa",
  "receiver_player_id",
  "posteam_type",
  "defteam_score"
)

# readRDS() of a URL times out after 60 s on slow connections, so download
# to a file first
download_rds <- function(url) {
  file <- tempfile(fileext = ".rds")
  on.exit(unlink(file))
  curl::curl_download(url, file)
  readRDS(file)
}
release <- "https://github.com/nflverse/%s/releases/download/%s/%s.rds"

season_file <- function(season) {
  file <- file.path(hits_dir, sprintf("data_%d.rds", season))
  if (!file.exists(file)) {
    cli::cli_inform("Downloading {season}")
    pbp <- as.data.table(download_rds(sprintf(
      release,
      "nflverse-data",
      "pbp",
      paste0("play_by_play_", season)
    )))[, ..pbp_columns]
    playstats <- as.data.table(download_rds(sprintf(
      release,
      "nflverse-pbp",
      "playstats",
      paste0("play_stats_", season)
    )))
    saveRDS(list(pbp = pbp, playstats = playstats), file)
  }
  normalizePath(file)
}

players_file <- file.path(hits_dir, "players.rds")
if (!file.exists(players_file)) {
  saveRDS(
    download_rds(sprintf(release, "nflverse-data", "players", "players")),
    players_file
  )
}
players_file <- normalizePath(players_file)

# columns that aren't identical although stats_differences() finds nothing:
# doubles within the tolerance
within_tolerance <- function(head, base) {
  if (nrow(head) != nrow(base)) {
    return(character())
  }
  cols <- intersect(names(base), names(head))
  differ <- cols[
    !vapply(cols, \(col) identical(head[[col]], base[[col]]), logical(1))
  ]
  vapply(
    differ,
    function(col) {
      x <- head[[col]]
      y <- base[[col]]
      rel <- abs(x - y) / pmax(abs(x), abs(y))
      sprintf("%s (max rel. diff %.2g)", col, max(rel, na.rm = TRUE))
    },
    character(1)
  )
}

# this script, to start the workers with it
script <- normalizePath(sub(
  "^--file=",
  "",
  grep("^--file=", commandArgs(FALSE), value = TRUE)
))
rscript <- file.path(R.home("bin"), "Rscript")

run_worker <- function(pkg_dir, data_file) {
  out_file <- tempfile(fileext = ".rds")
  status <- system2(
    rscript,
    c(script, "worker", pkg_dir, data_file, players_file, out_file, runs)
  )
  if (status != 0L) {
    stop("The worker for ", pkg_dir, " failed")
  }
  on.exit(unlink(out_file))
  readRDS(out_file)
}

results <- list()
timings <- list()
for (season in seasons) {
  data_file <- season_file(season)
  cli::cli_inform("{season}: running base and head")
  base <- run_worker(base_dir, data_file)
  head <- run_worker(head_dir, data_file)
  for (name in names(base$outputs)) {
    b <- base$outputs[[name]]
    h <- head$outputs[[name]]
    msg <- character()
    status <- "identical"
    if (!identical(h, b)) {
      msg <- stats_differences(h, b)$messages
      status <- "DIFFERENT"
      if (length(msg) == 0L) {
        msg <- paste("Within tolerance:", toString(within_tolerance(h, b)))
        status <- "within tolerance"
      }
    }
    cli::cli_inform("{season} {name}: {status}")
    if (length(msg) > 0L) {
      cat(paste("  -", msg), sep = "\n")
    }
    results[[paste(season, name)]] <- data.table(
      season = season,
      combination = name,
      status = status,
      rows = nrow(h),
      differences = paste(msg, collapse = " | ")
    )
  }
  timings[[as.character(season)]] <- data.table(
    season = season,
    combination = base$times$combination,
    base = base$times$seconds,
    head = head$times$seconds
  )
}

results <- rbindlist(results)
fwrite(results, file.path(hits_dir, "compare_results.csv"))
cli::cli_inform(
  "{sum(results$status == 'identical')} of {nrow(results)} combinations \\
  identical, {sum(results$status != 'DIFFERENT')} identical or within \\
  tolerance."
)

if (runs > 0L) {
  timings <- rbindlist(timings)
  timings[, speedup := round(base / head, 1)]
  table <- c(
    sprintf(
      "`calculate_stats()`, season_type REG, data in memory, median of %d runs (seconds):",
      runs
    ),
    "",
    "| Season | `summary_level` / `stat_type` | Base | Head | Speedup |",
    "|---|---|---|---|---|",
    timings[,
      sprintf(
        "| %d | %s | %.2f | %.2f | %.1fx |",
        season,
        combination,
        base,
        head,
        speedup
      )
    ]
  )
  writeLines(table)
  writeLines(table, file.path(hits_dir, "benchmark.md"))
}
