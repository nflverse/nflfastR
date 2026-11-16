# Pick a small set of games that covers as many parser conditions as possible.
#
# Usage (from the package root, after run_all.sh):
#   Rscript data-raw/test_game_selection/select_games.R [hits_dir] [n_games]
#
# An "item" is one of
#   <id>:r  the condition/function was evaluated at all
#   <id>:t  the condition was TRUE for at least one value
#   <id>:f  the condition was FALSE for at least one value
# where <id> refers to a row of the `targets` table (file, line, code).
#
# Selection is the standard greedy algorithm for maximum coverage: repeatedly
# add the game that covers the most items not covered yet. Ties go to the game
# with fewer plays (smaller test files). Once everything is covered, the
# remaining budget goes to covering each item a second time.
#
# Writes to hits_dir:
#   selected_games.csv   the selected games in order, with gain and coverage
#   coverage_curve.csv   coverage after k games
#   gaps.csv             items covered by some game but not by the selection
#   never_hit.csv        items no game covers (dead code candidates)

args <- commandArgs(trailingOnly = TRUE)
hits_dir <- if (length(args) >= 1) {
  args[[1]]
} else {
  "data-raw/test_game_selection/hits"
}
n_games <- if (length(args) >= 2) as.integer(args[[2]]) else 100L

files <- list.files(hits_dir, "^hits_\\d{4}\\.rds$", full.names = TRUE)
runs <- lapply(files, readRDS)
targets <- runs[[1]]$targets
games <- do.call(rbind, lapply(runs, `[[`, "games"))

failed <- games[!is.na(games$error) | is.na(games$n_plays), ]
if (nrow(failed)) {
  message(nrow(failed), " games failed and are excluded:")
  print(failed[, c("game_id", "error")], n = 50)
}
games <- games[is.na(games$error) & !is.na(games$n_plays), ]

# items as integers for speed
all_items <- sort(unique(unlist(games$hits)))
game_items <- lapply(games$hits, match, table = all_items)
n_items <- length(all_items)
games_per_item <- tabulate(unlist(game_items), n_items)

# Greedy selection in rounds. Round m adds games until every item is covered
# by at least m selected games (or by all games that hit it, if fewer).
# Round 1 is plain maximum coverage. Later rounds use the remaining budget to
# add redundancy, so no item depends on the quirks of a single game.
count <- integer(n_items)
order_sel <- integer()
gains <- integer()
round_sel <- integer()
remaining <- seq_along(game_items)
m <- 0L
while (length(order_sel) < max(n_games, 1L) || m == 0L) {
  m <- m + 1L
  need <- pmin(m, games_per_item)
  repeat {
    if (length(remaining) == 0) {
      break
    }
    gain <- vapply(
      game_items[remaining],
      function(x) sum(count[x] < need[x]),
      0L
    )
    if (max(gain) == 0) {
      break
    }
    # round 1 runs to full coverage, later rounds stop at the budget
    if (m > 1L && length(order_sel) >= n_games) {
      break
    }
    best <- remaining[order(-gain, games$n_plays[remaining])[1]]
    order_sel <- c(order_sel, best)
    gains <- c(gains, max(gain))
    round_sel <- c(round_sel, m)
    count[game_items[[best]]] <- count[game_items[[best]]] + 1L
    remaining <- setdiff(remaining, best)
  }
  if (length(remaining) == 0 || all(count >= games_per_item)) break
}
first_round <- sum(round_sel == 1L)

curve <- data.frame(
  k = seq_along(order_sel),
  game_id = games$game_id[order_sel],
  n_plays = games$n_plays[order_sel],
  round = round_sel,
  gain = gains
)
# share of items covered at least once after k games
first_cover <- integer(n_items)
seen <- logical(n_items)
for (k in seq_along(order_sel)) {
  x <- game_items[[order_sel[k]]]
  first_cover[x[!seen[x]]] <- k
  seen[x] <- TRUE
}
curve$coverage <- round(
  cumsum(tabulate(first_cover, length(order_sel))) / n_items,
  4
)
curve$covered_twice <- vapply(
  seq_along(order_sel),
  function(k) {
    cnt <- tabulate(unlist(game_items[order_sel[seq_len(k)]]), n_items)
    round(mean(cnt >= pmin(2L, games_per_item)), 4)
  },
  0
)
write.csv(curve, file.path(hits_dir, "coverage_curve.csv"), row.names = FALSE)

selected <- curve[seq_len(min(n_games, nrow(curve))), ]
write.csv(
  selected,
  file.path(hits_dir, "selected_games.csv"),
  row.names = FALSE
)

# describe items
describe <- function(items) {
  id <- as.integer(sub(":.*", "", items))
  type <- c(r = "reached", t = "TRUE", f = "FALSE")[sub(".*:", "", items)]
  data.frame(
    item = items,
    outcome = unname(type),
    file = targets$file[id],
    line = targets$line1[id],
    kind = targets$kind[id],
    code = substr(gsub("\\s+", " ", targets$text[id]), 1, 120)
  )
}

sel_covered <- logical(n_items)
sel_covered[unlist(game_items[order_sel[seq_len(nrow(selected))]])] <- TRUE
gaps <- describe(all_items[!sel_covered])
gaps$n_games <- games_per_item[!sel_covered]
gaps <- gaps[order(gaps$file, gaps$line), ]
write.csv(gaps, file.path(hits_dir, "gaps.csv"), row.names = FALSE)

# every possible item: functions can only be reached, conditions have 3
possible <- c(
  paste0(targets$id, ":r"),
  paste0(targets$id[targets$kind != "function"], ":t"),
  paste0(targets$id[targets$kind != "function"], ":f")
)
never <- describe(setdiff(possible, all_items))
never <- never[order(never$file, never$line), ]
write.csv(never, file.path(hits_dir, "never_hit.csv"), row.names = FALSE)

# summary
cat("\nGames analysed:", nrow(games), "\n")
cat("Possible items:", length(possible), "\n")
cat("Items hit by at least one game:", n_items, "\n")
cat("Items hit by every game:", sum(games_per_item == nrow(games)), "\n")
cat("Games needed to cover everything once:", first_round, "\n\n")
for (k in c(1, 5, 10, 25, 50, 75, 100, 150, 200)) {
  if (k <= nrow(curve)) {
    cat(sprintf("  %3d games: %.1f%%\n", k, 100 * curve$coverage[k]))
  }
}
cat(
  "\nSelected",
  nrow(selected),
  "games cover",
  sprintf("%.2f%%", 100 * selected$coverage[nrow(selected)]),
  "of items any game hits. Remaining gaps:",
  nrow(gaps),
  "\n"
)
cat(
  "Items covered at least twice (where 2+ games hit them):",
  sprintf("%.2f%%", 100 * selected$covered_twice[nrow(selected)]),
  "\n"
)
cat("Seasons in selection:\n")
print(table(substr(selected$game_id, 1, 4)))
