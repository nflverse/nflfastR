# Pick a small set of games that covers the calculate_stats() checklist
# (step 0.3 of PLAN.md)
#
# Usage (from the package root, after run_all.sh):
#   Rscript data-raw/calculate_stats_performance/select_games.R [n_games] [hits_dir]
#
# Candidates are single games and "units": sets of games that together reach
# an item no single game can reach (a player with targets for two teams, two
# name spellings of a player in a season, a regular season and a postseason
# game of a team, two games of a week with the same play_id). See
# season_hits.R.
#
# Selection is the greedy algorithm of data-raw/test_game_selection: add the
# candidate that covers the most items not covered yet; ties go to the
# candidate with fewer new play stats rows. Round 1 runs until everything is
# covered, later rounds cover every item a second time until the budget of
# n_games is used.
#
# Items of season summaries can't be attributed to single games, so the
# selection can't count them. Instead, the checklist runs again on the data of
# the selected games only. That run is the real check.
#
# Writes to data-raw/calculate_stats_performance:
#   selected_games.csv   selected games in order (game_id, round, gain, unit)
# and to hits_dir:
#   gaps.csv             items the selected games don't cover

args <- commandArgs(trailingOnly = TRUE)
n_games <- if (length(args) >= 1) as.integer(args[[1]]) else 60L
hits_dir <- if (length(args) >= 2) {
  args[[2]]
} else {
  "data-raw/calculate_stats_performance/hits"
}

suppressMessages(devtools::load_all(quiet = TRUE))
source("data-raw/calculate_stats_performance/checklist.R")

files <- list.files(hits_dir, "^hits_\\d{4}\\.rds$", full.names = TRUE)
runs <- lapply(files, readRDS)
seasons <- as.integer(sub("^hits_(\\d{4})\\.rds$", "\\1", basename(files)))
stopifnot(all(seasons >= 2001L))
cli::cli_inform("Seasons: {seasons}")

item_key <- function(combo, item) paste(combo, item, sep = " | ")

# games per item, over all seasons
game_hits <- rbindlist(lapply(runs, function(r) {
  it <- r$items[lengths(games) > 0L]
  it[, .(game_id = unlist(games)), by = .(key = item_key(combo, item))]
}))
sizes <- rbindlist(lapply(runs, `[[`, "sizes"))
size_of <- setNames(sizes$N, sizes$game_id)

units <- rbindlist(lapply(runs, `[[`, "units"))
units[, key := item_key(combo, item)]

# Candidates: every game with at least one hit, and every unit
all_keys <- sort(unique(c(game_hits$key, units$key)))
items_by_game <- split(match(game_hits$key, all_keys), game_hits$game_id)
cand_games <- c(
  as.list(names(items_by_game)),
  units$games
)
cand_items <- c(
  unname(items_by_game),
  lapply(seq_len(nrow(units)), function(i) {
    g <- units$games[[i]]
    unique(c(match(units$key[i], all_keys), unlist(items_by_game[g])))
  })
)
cand_unit <- c(rep(NA_character_, length(items_by_game)), units$item)
n_items <- length(all_keys)
cands_per_item <- tabulate(unlist(cand_items), n_items)

count <- integer(n_items)
selected <- character()
log <- list()
remaining <- seq_along(cand_items)
m <- 0L
repeat {
  m <- m + 1L
  need <- pmin(m, cands_per_item)
  repeat {
    if (length(remaining) == 0L) {
      break
    }
    if (m > 1L && length(selected) >= n_games) {
      break
    }
    gain <- vapply(
      cand_items[remaining],
      function(x) sum(count[x] < need[x]),
      0L
    )
    if (max(gain) == 0L) {
      break
    }
    cost <- vapply(
      cand_games[remaining],
      function(g) sum(size_of[setdiff(g, selected)], na.rm = TRUE),
      0
    )
    best <- remaining[order(-gain, cost)[1L]]
    new <- setdiff(cand_games[[best]], selected)
    selected <- c(selected, new)
    log[[length(log) + 1L]] <- data.table(
      game_id = new,
      round = m,
      gain = max(gain),
      unit = cand_unit[best]
    )
    count[cand_items[[best]]] <- count[cand_items[[best]]] + 1L
    # games of a unit are now selected and count for later units
    remaining <- setdiff(remaining, best)
  }
  if (
    length(remaining) == 0L ||
      length(selected) >= n_games ||
      all(count >= cands_per_item)
  ) {
    break
  }
}
sel <- rbindlist(log)
sel[, season := as.integer(substr(game_id, 1, 4))]
sel[, playstats_rows := unname(size_of[game_id])]
fwrite(sel, "data-raw/calculate_stats_performance/selected_games.csv")

cli::cli_inform(c(
  "Selected {nrow(sel)} games ({sum(sel$round == 1L)} in round 1) with \\
  {sum(sel$playstats_rows)} play stats rows.",
  "Items covered by the selection (game attributable): \\
  {sum(count > 0L)} of {n_items}, twice or more: {sum(count >= 2L)}."
))

# The real check: checklist on the selected games only ------------------------

dat <- lapply(sort(unique(sel$season)), function(s) {
  d <- readRDS(file.path(hits_dir, sprintf("data_%d.rds", s)))
  list(
    pbp = d$pbp[game_id %chin% sel$game_id],
    playstats = d$playstats[game_id %chin% sel$game_id]
  )
})
pbp <- rbindlist(lapply(dat, `[[`, "pbp"))
playstats <- rbindlist(lapply(dat, `[[`, "playstats"))
players <- readRDS(file.path(hits_dir, "players.rds"))

out <- checklist_outputs(pbp, playstats, players)
items <- checklist_items(pbp, playstats, players, out)

# built on purpose in step 0.4
built_in_0.4 <- c(
  "player id missing from load_players()",
  "play stats without pbp play"
)
gaps <- items[covered == FALSE, .(item, kind, combo)]
gaps[, built_in_0.4 := item %chin% built_in_0.4]
# items no game of any season reaches
any_season <- rbindlist(lapply(runs, `[[`, "items"))[,
  .(any_season = any(covered)),
  by = .(item, combo)
]
gaps <- any_season[gaps, on = .(item, combo)]
fwrite(gaps, file.path(hits_dir, "gaps.csv"))

checklist_report(items)
cli::cli_inform(
  "Gaps not built in 0.4: {sum(!gaps$built_in_0.4)}, of which no season \\
  reaches {sum(!gaps$built_in_0.4 & !(gaps$any_season %in% TRUE))}."
)
print(sel[, .N, by = season][order(season)])
