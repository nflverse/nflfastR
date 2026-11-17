# Case checklist for the calculate_stats() test data (step 0.2 of PLAN.md)
#
# Line coverage is not enough for calculate_stats(): the big summarise() call
# is one expression, so every stat counts as covered whether or not any row
# matches. This file defines the data cases the test data must reach.
#
# Usage (from the package root):
#   devtools::load_all()
#   source("data-raw/calculate_stats_performance/checklist.R")
#   out <- checklist_outputs(pbp, playstats, players)
#   items <- checklist_items(pbp, playstats, players, out)
#   checklist_report(items)
#
# `pbp`, `playstats` and `players` are what calculate_stats() gets from
# load_pbp(), load_playstats() and nflreadr::load_players().
#
# checklist_items() returns one row per item with
#   item     name of the item
#   kind     "column", "flag" or "case"
#   combo    output the item is checked in (summary_level/stat_type/season_type)
#            or "rows" for items checked on input rows
#   covered  TRUE if the data reach the item
#   games    game_ids that reach the item, NULL where an item can't be
#            attributed to single games (season summaries)

library(data.table)

checklist_combos <- function() {
  # week summaries ignore season_type, so they only need one combination
  rbind(
    CJ(
      summary_level = "season",
      stat_type = c("player", "team"),
      season_type = c("REG", "POST", "REG+POST"),
      sorted = FALSE
    ),
    CJ(
      summary_level = "week",
      stat_type = c("player", "team"),
      season_type = "REG+POST",
      sorted = FALSE
    )
  )
}

combo_name <- function(summary_level, stat_type, season_type) {
  paste(summary_level, stat_type, season_type, sep = "/")
}

# Run the current calculate_stats() for all combinations on the given data,
# without any download
checklist_outputs <- function(pbp, playstats, players) {
  testthat::local_mocked_bindings(
    load_playstats = function(...) playstats,
    stats_load_players = function() players,
    .package = "nflfastR"
  )
  combos <- checklist_combos()
  out <- lapply(seq_len(nrow(combos)), function(i) {
    suppressMessages(calculate_stats(
      summary_level = combos$summary_level[i],
      stat_type = combos$stat_type[i],
      season_type = combos$season_type[i],
      pbp = pbp
    ))
  })
  names(out) <- combo_name(
    combos$summary_level,
    combos$stat_type,
    combos$season_type
  )
  out
}

# One checklist row. `hit` is a logical vector over the rows of a table with
# a game_id column (or NULL), or a single logical.
item_row <- function(item, kind, combo, hit, game_id = NULL) {
  hit <- hit %in% TRUE
  games <- if (is.null(game_id)) NULL else unique(game_id[hit])
  data.table(
    item = item,
    kind = kind,
    combo = combo,
    covered = any(hit),
    games = list(games)
  )
}

# Output columns ---------------------------------------------------------------

# Columns that hold group keys or descriptive info, not stats
checklist_info_columns <- c(
  "player_id",
  "player_name",
  "player_display_name",
  "position",
  "position_group",
  "headshot_url",
  "season",
  "week",
  "team",
  "recent_team",
  "season_type",
  "game_id",
  "opponent_team"
)

checklist_list_columns <- c(
  "fg_made_list",
  "fg_missed_list",
  "fg_blocked_list",
  "gwfg_distance_list"
)

# Every stat must be non-zero somewhere in the four outputs that summarize all
# data (week summaries and REG+POST season summaries). The REG and POST season
# summaries only add the season_type filter, so they only need rows. Zeros are
# required in week player stats, where most stats are zero for most players.
# Team totals of a season are rarely zero.
checklist_full_combos <- c(
  "week/player/REG+POST",
  "week/team/REG+POST",
  "season/player/REG+POST",
  "season/team/REG+POST"
)

column_items <- function(out) {
  full <- rbindlist(lapply(checklist_full_combos, function(combo) {
    o <- out[[combo]]
    gid <- o[["game_id"]]
    num <- names(o)[vapply(o, is.numeric, logical(1))]
    num <- setdiff(num, checklist_info_columns)
    chr <- names(o)[vapply(o, is.character, logical(1))]
    rbind(
      rbindlist(lapply(num, function(col) {
        x <- o[[col]]
        rbind(
          item_row(paste0(col, " != 0"), "column", combo, x != 0, gid),
          if (combo == "week/player/REG+POST") {
            item_row(
              paste0(col, " == 0 or NA"),
              "column",
              combo,
              is.na(x) | x == 0,
              gid
            )
          }
        )
      })),
      rbindlist(lapply(chr, function(col) {
        x <- o[[col]]
        rbind(
          item_row(paste0(col, " not NA"), "column", combo, !is.na(x), gid),
          if (col %chin% checklist_list_columns) {
            item_row(paste0(col, " NA"), "column", combo, is.na(x), gid)
          }
        )
      }))
    )
  }))
  # several values in a list column, in any output
  lists <- rbindlist(lapply(checklist_list_columns, function(col) {
    hits <- rbindlist(lapply(out, function(o) {
      if (!col %chin% names(o)) {
        return(NULL)
      }
      item_row(
        "",
        "",
        "",
        grepl(";", o[[col]], fixed = TRUE),
        o[["game_id"]]
      )
    }))
    data.table(
      item = paste0(col, " several values"),
      kind = "column",
      combo = "any",
      covered = any(hits$covered),
      games = list(unique(unlist(hits$games)))
    )
  }))
  filtered <- rbindlist(lapply(
    setdiff(names(out), checklist_full_combos),
    function(combo) {
      item_row("rows", "column", combo, nrow(out[[combo]]) > 0L)
    }
  ))
  rbind(full, lists, filtered)
}

# has_id() flags ---------------------------------------------------------------

# Flags computed like more_stats, team_stats and all_stats in calculate_stats():
# same grouping keys, but as logicals instead of collapsed strings
playstats_flags <- function(playstats, pbp) {
  ps <- as.data.table(playstats)
  keys <- unique(as.data.table(pbp)[, c("game_id", "play_id")])
  ps <- ps[keys, on = c("game_id", "play_id"), nomatch = NULL]
  ps[,
    `:=`(
      p_fumble = any(stat_id %in% 52:54),
      p_fumble_lost = any(stat_id == 106)
    ),
    by = c("season", "week", "play_id", "gsis_player_id")
  ]
  ps[,
    `:=`(
      t_first_down_rush = any(stat_id == 3),
      t_first_down_pass = any(stat_id == 4)
    ),
    by = c("season", "week", "play_id", "team_abbr")
  ]
  ps[,
    `:=`(
      a_punt_oob = any(stat_id == 37),
      a_punt_downed = any(stat_id == 38),
      a_punt_fair_catch = any(stat_id == 39),
      a_punt_returned = any(stat_id %in% 33:34),
      a_punt_return_td = any(stat_id %in% c(34, 36))
    ),
    by = c("game_id", "play_id")
  ]
  ps[]
}

flag_items <- function(ps) {
  # stat IDs of the counted row, flag, name
  defs <- list(
    list(20, "p_fumble", "sack with fumble (52:54)"),
    list(20, "p_fumble_lost", "sack with fumble lost (106)"),
    list(10:11, "p_fumble", "rush with fumble (52:54)"),
    list(10:11, "p_fumble_lost", "rush with fumble lost (106)"),
    list(21:22, "p_fumble", "reception with fumble (52:54)"),
    list(21:22, "p_fumble_lost", "reception with fumble lost (106)"),
    list(15:16, "t_first_down_pass", "completion with team first down (4)"),
    list(21:22, "t_first_down_pass", "reception with team first down (4)"),
    list(10:11, "t_first_down_rush", "rush with team first down (3)"),
    list(29, "a_punt_oob", "punt out of bounds (37)"),
    list(29, "a_punt_downed", "punt downed (38)"),
    list(29, "a_punt_fair_catch", "punt fair caught (39)"),
    list(c(2, 29, 31, 32), "a_punt_returned", "punt returned (33:34)"),
    list(c(2, 29, 31, 32), "a_punt_return_td", "punt return td (34, 36)")
  )
  rbindlist(lapply(defs, function(d) {
    row <- ps$stat_id %in% d[[1]]
    flag <- ps[[d[[2]]]]
    rbind(
      item_row(d[[3]], "flag", "rows", row & flag, ps$game_id),
      item_row(
        sub(" with ", " without ", d[[3]]),
        "flag",
        "rows",
        row & !flag,
        ps$game_id
      )
    )
  }))
}

# Special cases ----------------------------------------------------------------

case_items <- function(pbp, ps, ps_all, players, out) {
  pbp <- as.data.table(pbp)
  wp <- as.data.table(out[["week/player/REG+POST"]])
  wt <- as.data.table(out[["week/team/REG+POST"]])
  sp <- as.data.table(out[["season/player/REG+POST"]])
  st <- as.data.table(out[["season/team/REG+POST"]])
  wp_combo <- "week/player/REG+POST"
  rb_ids <- players$gsis_id[players$position %chin% c("RB", "FB", "HB")]

  # game winning field goal attempts that don't count
  fg <- copy(pbp)[,
    `:=`(
      final_defteam_score = fifelse(
        posteam_type == "home",
        away_score,
        home_score
      ),
      last_drive = fixed_drive == max(fixed_drive)
    ),
    by = c("game_id", "posteam")
  ]
  fg <- fg[field_goal_attempt == 1 & last_drive]

  # play stat rows with the posteam/defteam of their play
  info <- unique(pbp[, .(game_id, play_id, def = defteam, special)])
  info[, def := team_name_fn(def)]
  psi <- info[ps, on = c("game_id", "play_id")]
  td <- c(11, 13, 16, 18, 22, 24, 26, 28, 34, 36, 46, 48, 64, 108)

  # player names per group, for custom_mode() ties
  names_by <- function(by) {
    n <- ps[
      !is.na(player_name),
      .N,
      by = c(by, "gsis_player_id", "player_name")
    ]
    n[,
      .(n_names = .N, tie = sum(N == max(N)) > 1L),
      by = c(by, "gsis_player_id")
    ]
  }
  nw <- names_by(c("season", "week", "game_id"))
  ns <- names_by("season")

  # players with more than one team in a season and targets for each team
  teams <- wp[
    targets > 0,
    .(n_teams = uniqueN(team)),
    by = .(season, player_id)
  ]
  traded <- teams[n_teams > 1L]

  # play_ids that appear in two games of the same week
  dup_play <- unique(pbp[, .(season, week, game_id, play_id)])[,
    if (uniqueN(game_id) > 1L) .(game_id = game_id),
    by = .(season, week, play_id)
  ]

  pbp_keys <- unique(pbp[, .(game_id, play_id)])
  ps_keys <- unique(ps_all[, .(game_id, play_id)])

  rbind(
    item_row(
      "passing_cpoe NA for a passer with pass plays",
      "case",
      wp_combo,
      !is.na(wp$passing_epa) & is.na(wp$passing_cpoe),
      wp$game_id
    ),
    item_row(
      "passing_cpoe not NA",
      "case",
      wp_combo,
      !is.na(wp$passing_cpoe),
      wp$game_id
    ),
    item_row(
      "fg_pct NA (fg_att == 0)",
      "case",
      wp_combo,
      wp$fg_att == 0,
      wp$game_id
    ),
    item_row(
      "pat_pct NA (pat_att == 0)",
      "case",
      wp_combo,
      wp$pat_att == 0,
      wp$game_id
    ),
    item_row(
      "pacr NA (NaN)",
      "case",
      wp_combo,
      wp$passing_yards == 0 & wp$passing_air_yards == 0,
      wp$game_id
    ),
    item_row(
      "pacr 0 (passing_air_yards <= 0)",
      "case",
      wp_combo,
      wp$passing_air_yards < 0 |
        (wp$passing_air_yards == 0 & wp$passing_yards != 0),
      wp$game_id
    ),
    item_row(
      "pacr regular",
      "case",
      wp_combo,
      wp$passing_air_yards > 0,
      wp$game_id
    ),
    item_row(
      "racr NA (NaN)",
      "case",
      wp_combo,
      wp$receiving_yards == 0 & wp$receiving_air_yards == 0,
      wp$game_id
    ),
    item_row(
      "racr 0 (receiving_air_yards == 0)",
      "case",
      wp_combo,
      wp$receiving_air_yards == 0 & wp$receiving_yards != 0,
      wp$game_id
    ),
    item_row(
      "racr 0 (negative air yards, not RB/FB)",
      "case",
      wp_combo,
      wp$receiving_air_yards < 0 & !wp$player_id %chin% rb_ids,
      wp$game_id
    ),
    item_row(
      "racr negative (negative air yards, RB/FB)",
      "case",
      wp_combo,
      wp$receiving_air_yards < 0 & wp$player_id %chin% rb_ids,
      wp$game_id
    ),
    item_row(
      "racr regular",
      "case",
      wp_combo,
      wp$receiving_air_yards > 0,
      wp$game_id
    ),
    item_row(
      "final drive fg, not gwfg: defteam scored afterwards",
      "case",
      "rows",
      fg[,
        between(score_differential, -2, 0) &
          defteam_score != final_defteam_score
      ],
      fg$game_id
    ),
    item_row(
      "final drive fg, not gwfg: score differential outside -2..0",
      "case",
      "rows",
      fg[, !between(score_differential, -2, 0)],
      fg$game_id
    ),
    item_row(
      "td of the defense on a special teams play",
      "case",
      "rows",
      psi[, team_abbr == def & special == 1 & stat_id %in% td],
      psi$game_id
    ),
    item_row(
      "fumble (52:53) of the defense",
      "case",
      "rows",
      psi[, team_abbr == def & stat_id %in% 52:53],
      psi$game_id
    ),
    item_row(
      "fumble (52:53) of the offense",
      "case",
      "rows",
      psi[, team_abbr != def & stat_id %in% 52:53],
      psi$game_id
    ),
    item_row(
      "player with targets for two teams in a season",
      "case",
      "season/player/REG+POST",
      nrow(traded) > 0L
    ),
    item_row(
      "player with two names in a week",
      "case",
      "rows",
      nw$n_names > 1L,
      nw$game_id
    ),
    item_row(
      "player name tie in a week",
      "case",
      "rows",
      nw$tie,
      nw$game_id
    ),
    item_row(
      "player with two names in a season",
      "case",
      "rows",
      ns$n_names > 1L
    ),
    item_row(
      "player name tie in a season",
      "case",
      "rows",
      ns$tie
    ),
    item_row(
      "play stat without player id",
      "case",
      "rows",
      is.na(ps$gsis_player_id),
      ps$game_id
    ),
    item_row(
      "player id missing from load_players()",
      "case",
      wp_combo,
      !is.na(wp$player_id) & !wp$player_id %chin% players$gsis_id,
      wp$game_id
    ),
    item_row(
      "week summary of a regular season week",
      "case",
      wp_combo,
      wp$season_type == "REG",
      wp$game_id
    ),
    item_row(
      "week summary of a postseason week",
      "case",
      wp_combo,
      wp$season_type == "POST",
      wp$game_id
    ),
    item_row(
      "season summary with season_type REG+POST",
      "case",
      "season/player/REG+POST",
      sp$season_type == "REG+POST"
    ),
    item_row(
      "team season summary with season_type REG+POST",
      "case",
      "season/team/REG+POST",
      st$season_type == "REG+POST"
    ),
    item_row(
      "team week summary with timeouts",
      "case",
      "week/team/REG+POST",
      wt$timeouts > 0,
      wt$game_id
    ),
    item_row(
      "pbp play without play stats",
      "case",
      "rows",
      !pbp_keys[, paste(game_id, play_id)] %chin%
        ps_keys[, paste(game_id, play_id)],
      pbp_keys$game_id
    ),
    item_row(
      "play stats without pbp play",
      "case",
      "rows",
      !ps_keys[, paste(game_id, play_id)] %chin%
        pbp_keys[, paste(game_id, play_id)],
      ps_keys$game_id
    ),
    item_row(
      "same play_id in two games of a week",
      "case",
      "rows",
      nrow(dup_play) > 0L,
      NULL
    )
  )
}

# Everything -------------------------------------------------------------------

checklist_items <- function(pbp, playstats, players, out) {
  ps <- playstats_flags(playstats, pbp)
  rbind(
    column_items(out),
    flag_items(ps),
    case_items(pbp, ps, as.data.table(playstats), players, out)
  )
}

checklist_report <- function(items) {
  cli::cli_inform(
    "{sum(items$covered)} of {nrow(items)} checklist items covered."
  )
  missing <- items[!items$covered]
  if (nrow(missing) > 0L) {
    print(missing[, .(item, kind, combo)], nrows = Inf)
  }
  invisible(missing)
}
