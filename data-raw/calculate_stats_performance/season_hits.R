# Checklist hits of one season for the game selection (step 0.3 of PLAN.md)
#
# Usage (from the package root):
#   Rscript data-raw/calculate_stats_performance/season_hits.R <season> [hits_dir]
#
# Downloads pbp (only the columns calculate_stats() uses) and playstats of the
# season, runs the checklist on the full season and writes to hits_dir:
#   data_<season>.rds   list(pbp, playstats) of the season
#   hits_<season>.rds   list(items, units, sizes), see below
#   players.rds         nflreadr::load_players(), once for all seasons
#
# items  checklist_items() of the full season
# units  candidate sets of games for items that need more than one game
#        (item, combo, games)
# sizes  number of play stats rows per game, the cost of a game

args <- commandArgs(trailingOnly = TRUE)
season <- as.integer(args[[1]])
hits_dir <- if (length(args) >= 2) {
  args[[2]]
} else {
  "data-raw/calculate_stats_performance/hits"
}
stopifnot(season >= 2001L)
dir.create(hits_dir, showWarnings = FALSE, recursive = TRUE)

suppressMessages(devtools::load_all(quiet = TRUE))
source("data-raw/calculate_stats_performance/checklist.R")

# readRDS() of a URL times out after 60 s on slow connections, so download
# to a file first
download_rds <- function(url) {
  file <- tempfile(fileext = ".rds")
  on.exit(unlink(file))
  curl::curl_download(url, file)
  readRDS(file)
}
release <- "https://github.com/nflverse/%s/releases/download/%s/%s.rds"

players_file <- file.path(hits_dir, "players.rds")
if (!file.exists(players_file)) {
  saveRDS(
    download_rds(sprintf(release, "nflverse-data", "players", "players")),
    players_file
  )
}
players <- readRDS(players_file)

data_file <- file.path(hits_dir, sprintf("data_%d.rds", season))
if (!file.exists(data_file)) {
  pbp <- download_rds(sprintf(
    release,
    "nflverse-data",
    "pbp",
    paste0("play_by_play_", season)
  ))
  pbp <- as.data.table(pbp)[, ..checklist_pbp_columns]
  playstats <- as.data.table(download_rds(sprintf(
    release,
    "nflverse-pbp",
    "playstats",
    paste0("play_stats_", season)
  )))
  stopifnot(nrow(pbp) > 0L, nrow(playstats) > 0L)
  saveRDS(list(pbp = pbp, playstats = playstats), data_file)
}
dat <- readRDS(data_file)
pbp <- dat$pbp
playstats <- dat$playstats

out <- checklist_outputs(pbp, playstats, players)
items <- checklist_items(pbp, playstats, players, out)

# Candidate units for items that need more than one game -----------------------

wp <- as.data.table(out[["week/player/REG+POST"]])
ps <- playstats[
  unique(pbp[, .(game_id, play_id)]),
  on = .(game_id, play_id),
  nomatch = NULL
]
max_units <- 20L

unit <- function(item, combo, games) {
  data.table(item = item, combo = combo, games = list(sort(unique(games))))
}

# a player with targets for two teams: the first game with targets per team
traded <- wp[targets > 0][
  order(week),
  .(game_id = game_id[1L]),
  by = .(player_id, team)
][, if (.N > 1L) .(game_id = game_id[1:2]), by = player_id]
traded_units <- if (nrow(traded) > 0L) {
  traded[,
    unit(
      "player with targets for two teams in a season",
      "season/player/REG+POST",
      game_id
    ),
    by = player_id
  ][seq_len(min(.N, max_units)), !"player_id"]
}

# a player with two names in a season: one game per name. Ties depend on the
# counts in the chosen games, so check all pairs of games of a player.
name_games <- ps[
  !is.na(player_name) & !is.na(gsis_player_id),
  .N,
  by = .(gsis_player_id, player_name, game_id)
]
multi <- name_games[, if (uniqueN(player_name) > 1L) .SD, by = gsis_player_id]
# units of one player with several names; NULL if no pair of games exists
name_pair_units <- function(d) {
  nms <- unique(d$player_name)[1:2]
  ga <- d$game_id[d$player_name == nms[1]]
  gb <- d$game_id[d$player_name == nms[2]]
  pairs <- CJ(a = unique(ga), b = unique(gb))[a != b]
  if (nrow(pairs) == 0L) {
    return(NULL)
  }
  tie <- vapply(
    seq_len(nrow(pairs)),
    function(i) {
      g <- c(pairs$a[i], pairs$b[i])
      n <- d[game_id %chin% g, .(n = sum(N)), by = player_name]$n
      sum(n == max(n)) > 1L
    },
    logical(1)
  )
  first_tie <- which(tie)[1L]
  rbind(
    unit(
      "player with two names in a season",
      "rows",
      c(pairs$a[1L], pairs$b[1L])
    ),
    if (!is.na(first_tie)) {
      unit(
        "player name tie in a season",
        "rows",
        c(pairs$a[first_tie], pairs$b[first_tie])
      )
    }
  )
}
name_units <- if (nrow(multi) > 0L) {
  multi[,
    name_pair_units(.SD),
    by = gsis_player_id,
    .SDcols = c("player_name", "game_id", "N")
  ]
}
# NULL or without an item column if no player has a usable pair of games
if ("item" %chin% names(name_units)) {
  name_units <- name_units[, .SD[seq_len(min(.N, max_units))], by = item][,
    !"gsis_player_id"
  ]
} else {
  name_units <- NULL
}

# a postseason game and a regular season game of the same team
games <- unique(pbp[, .(game_id, season_type, week, posteam)])[!is.na(posteam)]
post <- games[season_type == "POST"]
reg <- games[season_type == "REG"][
  order(week),
  .(reg_game = game_id[1L]),
  by = posteam
]
regpost <- reg[
  unique(post[, .(game_id, posteam)]),
  on = "posteam",
  nomatch = NULL
]
regpost_units <- rbind(
  regpost[,
    unit(
      "season summary with season_type REG+POST",
      "season/player/REG+POST",
      c(game_id, reg_game)
    ),
    by = .(game_id, posteam)
  ],
  regpost[,
    unit(
      "team season summary with season_type REG+POST",
      "season/team/REG+POST",
      c(game_id, reg_game)
    ),
    by = .(game_id, posteam)
  ]
)[, .SD[seq_len(min(.N, max_units))], by = item][, !c("game_id", "posteam")]

# two games of a week with the same play_id
dup <- unique(pbp[, .(week, game_id, play_id)])[,
  if (uniqueN(game_id) > 1L) .(game_id = unique(game_id)[1:2]),
  by = .(week, play_id)
]
dup_units <- if (nrow(dup) > 0L) {
  unique(
    dup[, .(g = paste(sort(game_id), collapse = " ")), by = .(week, play_id)],
    by = "g"
  )[
    seq_len(min(.N, max_units)),
    unit("same play_id in two games of a week", "rows", strsplit(g, " ")[[1]]),
    by = g
  ][, !"g"]
}

units <- rbind(traded_units, name_units, regpost_units, dup_units)
sizes <- playstats[, .N, by = game_id]

saveRDS(
  list(items = items, units = units, sizes = sizes),
  file.path(hits_dir, sprintf("hits_%d.rds", season))
)
cli::cli_inform(
  "{season}: {sum(items$covered)} of {nrow(items)} items, {nrow(units)} units"
)
