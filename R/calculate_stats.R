################################################################################
# Author: Sebastian Carl
################################################################################

#' Calculate NFL Stats
#'
#' Compute various NFL stats based off nflverse Play-by-Play data.
#'
#' @param seasons A numeric vector of 4-digit years associated with given NFL
#'  seasons - defaults to latest season. If set to TRUE, returns all available
#'  data since 1999. Ignored if argument `pbp` is not `NULL`.
#' @param summary_level Summarize stats by `"season"` or `"week"`.
#' @param stat_type Calculate `"player"` level stats or `"team"` level stats.
#' @param season_type One of `"REG"`, `"POST"`, or `"REG+POST"`. Filters
#'  data to regular season ("REG"), post season ("POST") or keeps all data.
#'  Only applied if `summary_level` == `"season"`.
#' @param pbp This argument allows passing a subset of nflverse play-by-play
#'  data, created with [build_nflfastR_pbp()] or loaded with [load_pbp()].
#'  Stats are then calculated based on the `game_id`s and `play_id`s in this
#'  subset of play-by-play data, rather then using the seasons specified in the
#'  `seasons` argument. The function will error if required variables are
#'  missing from the subset, but lists which variables are missing.
#'  If `pbp = NULL` (the default), all available games and plays from the
#'  `seasons` argument are used to calculate stats.
#'  Please use this responsibly, because the output is structurally identical
#'  to full seasons, even if plays have been filtered out. It may then appear
#'  as if the stats are incorrect. If `pbp` is not `NULL`, the function will add
#'  the attribute `"custom_pbp" = TRUE` to the function output to help identify
#'  stats that are possibly based on play-by-play subsets.
#'
#' @return A tibble of player/team stats summarized by season/week.
#' @seealso [nfl_stats_variables] for a description of all variables.
#' @seealso <https://nflfastr.com/articles/stats_variables.html> for a searchable
#' table of the stats variable descriptions.
#' @export
#'
#' @examples
#' \donttest{
#' try({# to avoid CRAN test problems
#' stats <- calculate_stats(2023, "season", "player")
#' # one line per column, like dplyr::glimpse()
#' str(
#'   stats,
#'   list.len = ncol(stats),
#'   vec.len = 10,
#'   give.attr = FALSE,
#'   strict.width = "cut"
#' )
#' })
#' }
calculate_stats <- function(
  seasons = nflreadr::most_recent_season(),
  summary_level = c("season", "week"),
  stat_type = c("player", "team"),
  season_type = c("REG", "POST", "REG+POST"),
  pbp = NULL
) {
  summary_level <- rlang::arg_match(summary_level)
  stat_type <- rlang::arg_match(stat_type)
  season_type <- rlang::arg_match(season_type)
  custom_pbp <- !is.null(pbp)

  if (!custom_pbp) {
    pbp <- stats_load_pbp(seasons = seasons)
  }

  # make sure (custom) pbp includes all required variables.
  # stats_validate_pbp will return all unique seasons in pbp.
  # We'll use this to download playstats for all seasons listed in pbp.
  seasons_in_pbp <- stats_validate_pbp(pbp)

  # A data.table of the required columns only. .subset() drops groups and
  # works the same for data frames, tibbles and data.tables. We only add
  # columns to it, so it never changes the user's pbp by reference.
  pbp <- as_pbp_dt(.subset(pbp, stats_pbp_columns()))

  if (season_type %in% c("REG", "POST") && summary_level == "season") {
    keep_type <- season_type
    pbp <- pbp[season_type == keep_type]
    if (nrow(pbp) == 0) {
      cli::cli_alert_warning(
        "Filtering {.val {seasons_in_pbp}} data to {.arg season_type} == \\
        {.val {season_type}} resulted in 0 rows. Returning empty tibble."
      )
      # an empty tibble without importing tibble
      return(structure(
        data.frame(),
        class = c("tbl_df", "tbl", "data.frame")
      ))
    }
  }

  # Play information #####################
  # defensive stats require knowledge of which team is on defense
  # special teams stats require knowledge of which plays were special teams plays
  # A game winning field goal attempt is
  # - a field goal attempt,
  # - in the posteam's final drive,
  # - where the posteam trailed the defteam by 2 points or less prior to the kick,
  # - and the defteam did not score afterwards
  # max() is NA if a drive of the group is NA, and every NA condition counts
  # as no attempt.
  pbp[, last_drive := max(fixed_drive), by = c("game_id", "posteam")]
  plays <- pbp[, list(
    game_id,
    play_id,
    off = team_name_fn(posteam),
    def = team_name_fn(defteam),
    special = as.integer(special == 1),
    season_type,
    # integer, so the gwfg stats stay integer
    is_gwfg_attempt = as.integer(data.table::fcoalesce(
      field_goal_attempt == 1 &
        fixed_drive == last_drive &
        data.table::between(score_differential, -2, 0) &
        defteam_score ==
          data.table::fifelse(posteam_type == "home", away_score, home_score),
      FALSE
    ))
  )]

  # load_playstats defined below
  # Keep only the play stats of plays in pbp (it may be a custom subset), in
  # the row order of the play stats, and add the play information.
  playstats <- data.table::as.data.table(load_playstats(seasons_in_pbp))
  in_pbp <- playstats[
    plays,
    on = c("game_id", "play_id"),
    which = TRUE,
    nomatch = NULL
  ]
  playstats <- playstats[sort(unique(in_pbp))]
  playstats[
    plays,
    on = c("game_id", "play_id"),
    c("off", "def", "special", "season_type", "is_gwfg_attempt") := list(
      i.off,
      i.def,
      i.special,
      i.season_type,
      i.is_gwfg_attempt
    )
  ]
  data.table::setnames(
    playstats,
    c("gsis_player_id", "team_abbr"),
    c("player_id", "team")
  )

  # Flags of stat IDs that other rows of the same player, team or play have.
  # We need them to identify things like fumbles depending on playtype or first
  # downs depending on playtype.
  # Player in a play. season, week and play_id identify the play because a
  # player has one game per week.
  flag_groups(
    playstats,
    by = c("season", "week", "play_id", "player_id"),
    flags = list(p_fumble = 52:54, p_fumble_lost = 106)
  )
  # Team in a play
  flag_groups(
    playstats,
    by = c("season", "week", "play_id", "team"),
    flags = list(t_first_down_rush = 3, t_first_down_pass = 4)
  )
  # Play, regardless of team (for punting). game_id is needed here to avoid
  # mixing play IDs of different games in the same week. Stat IDs 37, 38 and 39
  # (punts oob, downed, fair caught) are assigned to the receiving team (or to
  # the receiver in case of 39).
  flag_groups(
    playstats,
    by = c("game_id", "play_id"),
    flags = list(
      a_punt_oob = 37,
      a_punt_downed = 38,
      a_punt_fair_catch = 39,
      a_punt_returned = 33:34,
      a_punt_return_td = c(34, 36)
    )
  )

  # Row values that the totals below sum and that are also contributions of
  # the aggregation. As plain columns, the grouped sums use GForce.
  playstats[,
    c("passing_air_yards", "punt_return_yards", "targets") := list(
      # includes incompletions (111 = complete, 112 = incomplete)
      (stat_id %in% 111:112) * yards,
      (stat_id %in% 33:36) * yards,
      stat_id == 115
    )
  ]
  playstats[,
    team_play_air_yards := sum(passing_air_yards),
    by = c("season", "week", "play_id", "team")
  ]
  playstats[,
    play_punt_return_yards := sum(punt_return_yards),
    by = c("game_id", "play_id")
  ]
  # compute team targets and team air yards for calculation of target share
  # and air yard share. Since it's relative, we need to be careful with the
  # groups depending on summary level
  share_by <- if (summary_level == "season") {
    c("season", "team")
  } else {
    c("season", "week", "team")
  }
  playstats[,
    c("team_targets", "team_air_yards") := list(
      sum(targets),
      sum(passing_air_yards)
    ),
    by = share_by
  ]

  # Check combination of summary_level and stat_type to set a helper that is
  # used to create the grouping variables
  grp_id <- data.table::fcase(
    summary_level == "season" && stat_type == "player",
    "10",
    summary_level == "season" && stat_type == "team",
    "20",
    summary_level == "week" && stat_type == "player",
    "30",
    summary_level == "week" && stat_type == "team",
    "40"
  )
  # grp_vctr are the grouping variables
  grp_vctr <- switch(
    grp_id,
    "10" = c("season", "player_id"),
    "20" = c("season", "team"),
    "30" = c("season", "week", "player_id"),
    "40" = c("season", "week", "team")
  )
  week <- summary_level == "week"
  player <- stat_type == "player"

  # Aggregation #####################
  # Every stat that is a sum is a contribution of each row, computed once on
  # the whole columns and summed by group in one GForce call, together with
  # the maximums fg_long and pt_long. Columns that only some summaries
  # output are computed for all and dropped when the columns are selected.
  # passing_air_yards, punt_return_yards and targets are columns of playstats
  # already (see above).
  contributions <- playstats[,
    c(
      .SD,
      list(
        # Offense #####################
        completions = stat_id %in% 15:16,
        attempts = stat_id %in% c(14:16, 19),
        passing_yards = (stat_id %in% 15:16) * yards,
        passing_tds = stat_id == 16,
        passing_interceptions = stat_id == 19,
        sacks_suffered = stat_id == 20,
        sack_yards_lost = (stat_id == 20) * yards,
        sack_fumbles = stat_id == 20 & p_fumble,
        sack_fumbles_lost = stat_id == 20 & p_fumble_lost,
        # air yards of completed passes, for passing_yards_after_catch
        passing_air_yards_complete = (stat_id == 111) * yards,
        passing_first_downs = (stat_id %in% 15:16) & t_first_down_pass,
        passing_2pt_conversions = stat_id == 77,
        # "Explosives" (see #550 for discussion about the definition)
        passing_10 = (stat_id %in% 15:16) * (yards >= 10),
        passing_16 = (stat_id %in% 15:16) * (yards >= 16),
        passing_20 = (stat_id %in% 15:16) * (yards >= 20),
        passing_40 = (stat_id %in% 15:16) * (yards >= 40),

        carries = stat_id %in% 10:11,
        rushing_yards = (stat_id %in% 10:13) * yards,
        rushing_tds = stat_id %in% c(11, 13),
        rushing_fumbles = (stat_id %in% 10:11) & p_fumble,
        rushing_fumbles_lost = (stat_id %in% 10:11) & p_fumble_lost,
        rushing_first_downs = (stat_id %in% 10:11) & t_first_down_rush,
        rushing_2pt_conversions = stat_id == 75,
        # "Explosives" (see #550 for discussion about the definition)
        rushing_10 = (stat_id %in% 10:13) * (yards >= 10),
        rushing_12 = (stat_id %in% 10:13) * (yards >= 12),
        rushing_20 = (stat_id %in% 10:13) * (yards >= 20),
        rushing_40 = (stat_id %in% 10:13) * (yards >= 40),

        receptions = stat_id %in% 21:22,
        receiving_yards = (stat_id %in% 21:24) * yards,
        receiving_tds = stat_id %in% c(22, 24),
        receiving_fumbles = (stat_id %in% 21:22) & p_fumble,
        receiving_fumbles_lost = (stat_id %in% 21:22) & p_fumble_lost,
        # air_yards are counted in 111:112 but it is a passer stat not a
        # receiver stat so we count team air yards when a player accounted
        # for a reception. team air yards will always equal the correct air
        # yards as 111 and 112 cannot appear more than once per play.
        # If this ever changes, we can use pbp instead.
        # Team stats use passing_air_yards instead (see below).
        receiving_air_yards = (stat_id == 115) * team_play_air_yards,
        receiving_yards_after_catch = (stat_id == 113) * yards,
        receiving_first_downs = (stat_id %in% 21:22) & t_first_down_pass,
        receiving_2pt_conversions = stat_id == 104,
        # "Explosives" (see #550 for discussion about the definition)
        receiving_10 = (stat_id %in% 21:24) * (yards >= 10),
        receiving_16 = (stat_id %in% 21:24) * (yards >= 16),
        receiving_20 = (stat_id %in% 21:24) * (yards >= 20),
        receiving_40 = (stat_id %in% 21:24) * (yards >= 40),

        special_teams_tds = (special == 1) & stat_id %in% td_ids(),

        # Defense #####################
        def_tackles_solo = stat_id == 79,
        def_tackles_with_assist = stat_id == 80,
        def_tackle_assists = stat_id == 82,
        def_tackles_for_loss = stat_id == 402,
        def_tackles_for_loss_yards = (stat_id == 402) * yards,
        def_fumbles_forced = stat_id == 91,
        # full and half sacks, for def_sacks and def_sack_yards
        def_sacks_full = stat_id == 83,
        def_sacks_half = stat_id == 84,
        def_sack_yards_full = (stat_id == 83) * -yards,
        def_sack_yards_half = (stat_id == 84) * -yards,
        def_qb_hits = stat_id == 110,
        def_interceptions = stat_id %in% 25:26,
        def_interception_yards = (stat_id %in% 25:28) * yards,
        def_pass_defended = stat_id == 85,
        def_tds = team == def & special != 1 & stat_id %in% td_ids(),
        # stat ID 54 is a fumble out of bounds. It's never counted alone,
        # always in combination with 52 or 53.
        def_fumbles = (team == def) & stat_id %in% 52:53,
        def_safeties = stat_id == 89,
        def_punt_blocks = stat_id == 86,
        def_pat_blocks = stat_id == 87,
        def_fg_blocks = stat_id == 88,
        def_2pt_atts = stat_id == 403,
        def_2pt_made = stat_id == 404,

        # Misc #####################
        # mostly yards gained after blocked punts or fgs
        misc_yards = (stat_id %in% 63:64) * yards,
        fumble_recovery_own = stat_id %in% 55:56,
        # 57, 58 don't count as recovery because player received a
        # lateral after recovery by other player
        fumble_recovery_yards_own = (stat_id %in% 55:58) * yards,
        fumble_recovery_opp = stat_id %in% 59:60,
        # 61, 62 don't count as recovery because player received a
        # lateral after recovery by other player
        fumble_recovery_yards_opp = (stat_id %in% 59:62) * yards,
        fumble_recovery_tds = stat_id %in% c(56, 58, 60, 62),
        penalties = stat_id == 93,
        penalty_yards = (stat_id == 93) * yards,
        # team stats only
        timeouts = stat_id == 68,
        # we are missing some fumbles on offense (see 515) so we just add
        # totals here. These fumble stats count all fumbles regardless of
        # the unit the player was on. This means that all above fumble stats
        # are included here but we make sure not to loose any fumbles, esp.
        # on offense
        fumbles_forced_by_opp = stat_id == 52,
        fumbles_not_forced = stat_id == 53,
        fumbles_out_of_bounds = stat_id == 54,
        # we could tell users to just add the above three stats but fumbles
        # are a bit confusing overall so it is ok to add a total counter that
        # doesn't miss any fumbles.
        # stat ID 54 is a fumble out of bounds. It's never counted alone,
        # always in combination with 52 or 53. So we cannot add it to the
        # total.
        fumbles_total = stat_id %in% 52:53,
        fumbles_lost_total = stat_id == 106,

        # Returning #####################
        punt_returns = stat_id %in% 33:34,
        # punt return tds are counted in special teams tds atm
        kickoff_returns = stat_id %in% 45:46,
        kickoff_return_yards = (stat_id %in% 45:48) * yards,
        # kickoff return tds are counted in special teams tds atm

        # Kicking #####################
        fg_made = stat_id == 70,
        fg_att = stat_id %in% 69:71,
        fg_missed = stat_id == 69,
        fg_blocked = stat_id == 71,
        fg_made_0_19 = (stat_id == 70) * (yards %between% c(0, 19)),
        fg_made_20_29 = (stat_id == 70) * (yards %between% c(20, 29)),
        fg_made_30_39 = (stat_id == 70) * (yards %between% c(30, 39)),
        fg_made_40_49 = (stat_id == 70) * (yards %between% c(40, 49)),
        fg_made_50_59 = (stat_id == 70) * (yards %between% c(50, 59)),
        fg_made_60_ = (stat_id == 70) * (yards >= 60),
        fg_missed_0_19 = (stat_id == 69) * (yards %between% c(0, 19)),
        fg_missed_20_29 = (stat_id == 69) * (yards %between% c(20, 29)),
        fg_missed_30_39 = (stat_id == 69) * (yards %between% c(30, 39)),
        fg_missed_40_49 = (stat_id == 69) * (yards %between% c(40, 49)),
        fg_missed_50_59 = (stat_id == 69) * (yards %between% c(50, 59)),
        fg_missed_60_ = (stat_id == 69) * (yards >= 60),
        fg_made_distance = (stat_id == 70) * yards,
        fg_missed_distance = (stat_id == 69) * yards,
        fg_blocked_distance = (stat_id == 71) * yards,
        pat_made = stat_id == 72,
        pat_att = stat_id %in% 72:74,
        pat_missed = stat_id == 73,
        pat_blocked = stat_id == 74,
        gwfg_made = (stat_id == 70) * is_gwfg_attempt,
        gwfg_att = (stat_id %in% 69:71) * is_gwfg_attempt,
        gwfg_missed = (stat_id == 69) * is_gwfg_attempt,
        gwfg_blocked = (stat_id == 71) * is_gwfg_attempt,
        # week summaries only
        gwfg_distance = (stat_id %in% 69:71) * is_gwfg_attempt * yards,

        # Punts #####################
        # stat ID 2 counts blocked punts that do not count as punt
        pt_att = stat_id %in% c(29, 31, 32), # 31 probably unnecessary
        pt_blocked = stat_id == 2,
        pt_yards = (stat_id %in% c(29, 32)) * yards,
        pt_inside_20 = stat_id == 30,
        # the following stats are a bit special as we need opponent team
        # stats to get the counts right. That's what the a_punt_* flags are
        # for. stat IDs 37, 38, 39 (punts oob, downed, fair caught) are
        # assigned to the receiving team (or to the receiver in case of 39)
        # Also the number of returns, return TDs and the yardage
        pt_out_of_bounds = (stat_id == 29) & a_punt_oob,
        pt_downed = (stat_id == 29) & a_punt_downed,
        pt_touchback = stat_id == 32,
        pt_fair_caught = (stat_id == 29) & a_punt_fair_catch,
        pt_returned = (stat_id %in% c(2, 29, 31, 32)) & a_punt_returned,
        pt_return_yards = (stat_id %in% c(2, 29, 31, 32)) *
          play_punt_return_yards,
        pt_return_tds = (stat_id %in% c(2, 29, 31, 32)) & a_punt_return_td,

        # maximums, not sums
        fg_long_yards = (stat_id == 70) * yards,
        pt_long_yards = (stat_id %in% c(29, 32)) * yards
      )
    ),
    .SDcols = c(
      grp_vctr,
      "passing_air_yards",
      "punt_return_yards",
      "targets"
    )
  ]
  sum_cols <- setdiff(
    names(contributions),
    c(grp_vctr, "fg_long_yards", "pt_long_yards")
  )
  stats <- contributions[,
    c(
      lapply(.SD, sum),
      list(fg_long = max(fg_long_yards), pt_long = max(pt_long_yards))
    ),
    keyby = grp_vctr,
    .SDcols = sum_cols
  ]
  rm(contributions)

  # Stats that aren't sums. Each is computed only for the summaries that
  # output it, and without R code per group where possible. They depend on
  # the row order of the play stats (see PLAN.md).
  # First values per group with GForce:
  # - season_type of week summaries. It's REG or POST depending on the
  #   argument season_type. Season summaries collapse it below.
  # - game_id of week summaries: not strictly necessary because we have
  #   season, week, team and opponent information, but convenient.
  # - off and def for opponent_team of week summaries.
  # - team of player stats: the first team, output as team by week / player.
  #   For team stats, team is a grouping variable.
  # - team_targets and team_air_yards for target_share and air_yards_share
  #   (computed for team stats as well, where they aren't output).
  first_cols <- setdiff(
    c(
      if (week) c("season_type", "game_id", "off", "def"),
      "team",
      "team_targets",
      "team_air_yards"
    ),
    grp_vctr
  )
  firsts <- playstats[,
    lapply(.SD, data.table::first),
    keyby = grp_vctr,
    .SDcols = first_cols
  ]
  # both are keyed by the same groups, so the rows match
  stats <- cbind(stats, firsts[, !..grp_vctr])
  rm(firsts)

  if (week) {
    stats[, opponent_team := data.table::fifelse(team == off, def, off)]
  } else {
    # Season types in the order of the play stats, collapsed with "+". This
    # makes sure that season_type is only REG+POST if the user asked for it
    # AND if postseason data is available. paste() only runs for the groups
    # with more than one season type.
    season_types <- unique(playstats[,
      c(grp_vctr, "season_type"),
      with = FALSE
    ])
    season_types[, n := .N, by = grp_vctr]
    season_types <- rbind(
      season_types[n == 1L, !"n"],
      season_types[
        n > 1L,
        list(season_type = paste(season_type, collapse = "+")),
        by = grp_vctr
      ]
    )
    stats[season_types, on = grp_vctr, season_type := i.season_type]

    games <- unique(playstats[, c(grp_vctr, "game_id"), with = FALSE])[,
      list(games = .N),
      by = grp_vctr
    ]
    stats[games, on = grp_vctr, games := i.games]

    if (player) {
      recent <- playstats[,
        list(recent_team = data.table::last(team)),
        keyby = grp_vctr
      ]
      stats[recent, on = grp_vctr, recent_team := i.recent_team]
    }
  }

  if (player) {
    # The most common name. Ties go to the name that comes first, like
    # custom_mode(): `by` keeps the groups in the order of their first row
    # and setorderv() is stable. Groups without names get NA from the join.
    name_counts <- playstats[
      !is.na(player_name),
      list(n = .N),
      by = c(grp_vctr, "player_name")
    ]
    data.table::setorderv(
      name_counts,
      c(grp_vctr, "n"),
      order = c(rep(1L, length(grp_vctr)), -1L)
    )
    name_counts <- unique(name_counts, by = grp_vctr)
    stats[name_counts, on = grp_vctr, player_name := i.player_name]
  }

  # Field goal distances in the order of the play stats, separated by ";".
  # Groups without such kicks get NA from the join.
  fg_lists <- list(
    fg_made_list = playstats$stat_id == 70,
    fg_missed_list = playstats$stat_id == 69,
    fg_blocked_list = playstats$stat_id == 71
  )
  if (!week) {
    fg_lists$gwfg_distance_list <- playstats$stat_id %in%
      69:71 &
      playstats$is_gwfg_attempt == 1L
  }
  for (col in names(fg_lists)) {
    fg_list <- playstats[
      fg_lists[[col]],
      list(distances = paste(yards, collapse = ";")),
      by = grp_vctr
    ]
    stats[fg_list, on = grp_vctr, (col) := i.distances]
  }

  # Derived stats #####################
  if (stat_type == "team") {
    stats[, receiving_air_yards := passing_air_yards]
  }
  stats[,
    c(
      "passing_yards_after_catch",
      "pacr",
      "racr",
      "target_share",
      "air_yards_share",
      "def_sacks",
      "def_sack_yards",
      "pt_net_yards"
    ) := list(
      # passing yac equals passing yards - air yards on completed passes
      passing_yards - passing_air_yards_complete,
      passing_yards / passing_air_yards,
      receiving_yards / receiving_air_yards,
      targets / team_targets,
      receiving_air_yards / team_air_yards,
      def_sacks_full + 1 / 2 * def_sacks_half,
      def_sack_yards_full + 1 / 2 * def_sack_yards_half,
      pt_yards - pt_return_yards - pt_touchback * 20L
    )
  ]
  stats[, wopr := 1.5 * target_share + 0.7 * air_yards_share]
  # avoid 0/0 = NaN
  stats[fg_att > 0, fg_pct := fg_made / fg_att]
  stats[pat_att > 0, pat_pct := pat_made / pat_att]
  stats[fg_long == 0L, fg_long := NA_integer_]
  stats[pt_long == 0L, pt_long := NA_integer_]

  # Stats from PBP #####################
  # we want passing epa, rushing epa, and receiving epa
  # since these depend on different player id variables and filters,
  # we aggregate them separately and add them with update joins
  passing_stats_from_pbp <- pbp[
    play_type %chin% c("pass", "qb_spike"),
    list(
      season,
      week,
      team = posteam,
      player_id = passer_player_id,
      qb_epa,
      cpoe
    )
  ][,
    list(
      passing_epa = sum(qb_epa, na.rm = TRUE),
      # mean is NaN if all values are NA, because we remove NA
      passing_cpoe = mean(cpoe, na.rm = TRUE)
    ),
    by = grp_vctr
  ]
  passing_stats_from_pbp[is.nan(passing_cpoe), passing_cpoe := NA_real_]

  rushing_stats_from_pbp <- pbp[
    play_type %chin% c("run", "qb_kneel"),
    list(season, week, team = posteam, player_id = rusher_player_id, epa)
  ][, list(rushing_epa = sum(epa, na.rm = TRUE)), by = grp_vctr]

  receiving_stats_from_pbp <- pbp[
    !is.na(receiver_player_id),
    list(season, week, team = posteam, player_id = receiver_player_id, epa)
  ][, list(receiving_epa = sum(epa, na.rm = TRUE)), by = grp_vctr]

  # Joins match NA keys, like the left_join() of dplyr
  stats[
    passing_stats_from_pbp,
    on = grp_vctr,
    c("passing_epa", "passing_cpoe") := list(i.passing_epa, i.passing_cpoe)
  ]
  stats[rushing_stats_from_pbp, on = grp_vctr, rushing_epa := i.rushing_epa]
  stats[
    receiving_stats_from_pbp,
    on = grp_vctr,
    receiving_epa := i.receiving_epa
  ]

  out_cols <- stats_output_columns(grp_vctr, grp_id, summary_level)
  stats <- stats[, ..out_cols]
  # C locale and NA last, like dplyr::arrange()
  data.table::setorderv(stats, grp_vctr, na.last = TRUE)

  # Apply Player Modifications #####################
  if (stat_type == "player") {
    # need newer version of nflreadr to use load_players
    rlang::check_installed("nflreadr (>= 1.3.0)", "to join player information.")

    player_info <- stats_player_info(stats_load_players())

    # load gsis_ids of RBs, FBs and HBs for RACR
    racr_ids <- player_info[position %chin% c("RB", "FB", "HB"), player_id]

    # all conditions on the original values, so fcase()
    stats[,
      c("pacr", "racr") := list(
        data.table::fcase(
          is.nan(pacr),
          NA_real_,
          passing_air_yards <= 0,
          0,
          default = pacr
        ),
        data.table::fcase(
          is.nan(racr),
          NA_real_,
          receiving_air_yards == 0,
          0,
          # following Josh Hermsmeyer's definition, RACR stays < 0 for RBs
          # (and FBs) and is set to 0 for Receivers. The list "racr_ids"
          # includes all known RB and FB gsis_ids
          receiving_air_yards < 0 & !player_id %chin% racr_ids,
          0,
          default = racr
        )
      )
    ]
    # Fantasy #####################
    stats[,
      fantasy_points := 1 /
        25 *
        passing_yards +
        4 * passing_tds -
        2 * passing_interceptions +
        1 / 10 * (rushing_yards + receiving_yards) +
        6 * (rushing_tds + receiving_tds + special_teams_tds) +
        2 *
          (passing_2pt_conversions +
            rushing_2pt_conversions +
            receiving_2pt_conversions) -
        2 * (sack_fumbles_lost + rushing_fumbles_lost + receiving_fumbles_lost)
    ]
    stats[, fantasy_points_ppr := fantasy_points + receptions]

    # Like a left_join(): keeps the order of stats, NA matches NA and a
    # duplicated gsis_id duplicates rows
    stats <- player_info[stats, on = "player_id", allow.cartesian = TRUE]
    data.table::setcolorder(
      stats,
      c(
        "player_id",
        "player_name",
        "player_display_name",
        "position",
        "position_group",
        "headshot_url"
      )
    )
  }

  # a tibble without importing tibble
  data.table::setDF(stats)
  data.table::setattr(stats, "class", c("tbl_df", "tbl", "data.frame"))

  if (custom_pbp) {
    attr(stats, "custom_pbp") <- TRUE
  }

  stats
}

load_playstats <- function(seasons = nflreadr::most_recent_season()) {
  if (isTRUE(seasons)) {
    seasons <- seq(1999, nflreadr::most_recent_season())
  }

  stopifnot(
    is.numeric(seasons),
    seasons >= 1999,
    seasons <= nflreadr::most_recent_season()
  )

  urls <- paste0(
    "https://github.com/nflverse/nflverse-pbp/releases/download/playstats/play_stats_",
    seasons,
    ".rds"
  )

  out <- stats_load_from_url(urls)

  out
}

# Wrappers of the nflreadr loaders, so tests can mock them with
# local_mocked_bindings() without modifying the nflreadr namespace
stats_load_pbp <- function(seasons) nflreadr::load_pbp(seasons = seasons)

stats_load_players <- function() nflreadr::load_players()

stats_load_from_url <- function(urls) {
  nflreadr::load_from_url(urls, seasons = TRUE, nflverse = FALSE)
}

# Output columns of calculate_stats() before the player information, in order.
stats_output_columns <- function(grp_vctr, grp_id, summary_level) {
  player <- grp_id %in% c("10", "30")
  week <- summary_level == "week"
  cols <- c(
    if (player) "player_name",
    "season_type",
    if (week) "game_id",
    if (grp_id == "10") "recent_team",
    if (grp_id == "30") "team",
    if (week) "opponent_team",
    if (!week) "games",
    "completions",
    "attempts",
    "passing_yards",
    "passing_tds",
    "passing_interceptions",
    "sacks_suffered",
    "sack_yards_lost",
    "sack_fumbles",
    "sack_fumbles_lost",
    "passing_air_yards",
    "passing_yards_after_catch",
    "passing_first_downs",
    "passing_epa",
    "passing_cpoe",
    "passing_2pt_conversions",
    if (player) "pacr",
    "passing_10",
    "passing_16",
    "passing_20",
    "passing_40",
    "carries",
    "rushing_yards",
    "rushing_tds",
    "rushing_fumbles",
    "rushing_fumbles_lost",
    "rushing_first_downs",
    "rushing_epa",
    "rushing_2pt_conversions",
    "rushing_10",
    "rushing_12",
    "rushing_20",
    "rushing_40",
    "receptions",
    "targets",
    "receiving_yards",
    "receiving_tds",
    "receiving_fumbles",
    "receiving_fumbles_lost",
    "receiving_air_yards",
    "receiving_yards_after_catch",
    "receiving_first_downs",
    "receiving_epa",
    "receiving_2pt_conversions",
    "receiving_10",
    "receiving_16",
    "receiving_20",
    "receiving_40",
    if (player) c("racr", "target_share", "air_yards_share", "wopr"),
    "special_teams_tds",
    "def_tackles_solo",
    "def_tackles_with_assist",
    "def_tackle_assists",
    "def_tackles_for_loss",
    "def_tackles_for_loss_yards",
    "def_fumbles_forced",
    "def_sacks",
    "def_sack_yards",
    "def_qb_hits",
    "def_interceptions",
    "def_interception_yards",
    "def_pass_defended",
    "def_tds",
    "def_fumbles",
    "def_safeties",
    "def_punt_blocks",
    "def_pat_blocks",
    "def_fg_blocks",
    "def_2pt_atts",
    "def_2pt_made",
    "misc_yards",
    "fumble_recovery_own",
    "fumble_recovery_yards_own",
    "fumble_recovery_opp",
    "fumble_recovery_yards_opp",
    "fumble_recovery_tds",
    "penalties",
    "penalty_yards",
    if (!player) "timeouts",
    "fumbles_forced_by_opp",
    "fumbles_not_forced",
    "fumbles_out_of_bounds",
    "fumbles_total",
    "fumbles_lost_total",
    "punt_returns",
    "punt_return_yards",
    "kickoff_returns",
    "kickoff_return_yards",
    "fg_made",
    "fg_att",
    "fg_missed",
    "fg_blocked",
    "fg_long",
    "fg_pct",
    "fg_made_0_19",
    "fg_made_20_29",
    "fg_made_30_39",
    "fg_made_40_49",
    "fg_made_50_59",
    "fg_made_60_",
    "fg_missed_0_19",
    "fg_missed_20_29",
    "fg_missed_30_39",
    "fg_missed_40_49",
    "fg_missed_50_59",
    "fg_missed_60_",
    "fg_made_list",
    "fg_missed_list",
    "fg_blocked_list",
    "fg_made_distance",
    "fg_missed_distance",
    "fg_blocked_distance",
    "pat_made",
    "pat_att",
    "pat_missed",
    "pat_blocked",
    "pat_pct",
    "gwfg_made",
    "gwfg_att",
    "gwfg_missed",
    "gwfg_blocked",
    if (week) "gwfg_distance" else "gwfg_distance_list",
    "pt_att",
    "pt_blocked",
    "pt_long",
    "pt_yards",
    "pt_inside_20",
    "pt_out_of_bounds",
    "pt_downed",
    "pt_touchback",
    "pt_fair_caught",
    "pt_returned",
    "pt_return_yards",
    "pt_return_tds",
    "pt_net_yards"
  )
  c(grp_vctr, cols)
}

# The player information of calculate_stats() from load_players(), with a
# clear error if it failed (nflreadr returns empty data with a warning)
stats_player_info <- function(players, call = rlang::caller_env()) {
  cols <- c("gsis_id", "display_name", "position", "position_group", "headshot")
  missing <- setdiff(cols, names(players))
  if (length(missing) > 0) {
    cli::cli_abort(
      c(
        "Failed to load the player information with \\
        {.fun nflreadr::load_players}.",
        "i" = "It is missing the variable{?s} {.val {missing}}."
      ),
      call = call
    )
  }
  players <- data.table::as.data.table(.subset(players, cols))
  data.table::setnames(
    players,
    c("gsis_id", "display_name", "headshot"),
    c("player_id", "player_display_name", "headshot_url")
  )
  data.table::setcolorder(
    players,
    c(
      "player_id",
      "player_display_name",
      "position",
      "position_group",
      "headshot_url"
    )
  )
  players
}

# Adds a logical column per element of flags: TRUE in all rows of the groups
# (by) that have one of the stat IDs of that element. Joins match NA keys like
# NA groups. Changes dt by reference.
flag_groups <- function(dt, by, flags) {
  for (flag in names(flags)) {
    ids <- flags[[flag]]
    keys <- unique(dt[stat_id %in% ids, ..by])
    data.table::set(dt, j = flag, value = FALSE)
    dt[keys, on = by, (flag) := TRUE]
  }
  invisible(dt)
}

td_ids <- function() {
  c(
    11,
    13,
    16,
    18,
    22,
    24,
    26,
    28,
    34,
    36,
    46,
    48,
    # 56, 58, 60, 62, # 56-62 are separately counted in fumble_recovery_tds
    64,
    108
  )
}

stats_validate_pbp <- function(pbp) {
  required_names <- stats_pbp_columns()
  available_names <- names(pbp)
  missing <- required_names[!required_names %in% available_names]
  if (length(missing) > 0) {
    cli::cli_abort(
      "You have passed custom pbp to the argument {.arg pbp} but \\
      it is missing the following required variable{?s}: {.val {missing}}",
      call = rlang::caller_env()
    )
  }
  unique(pbp$season) |>
    stats::na.omit() |>
    as.vector()
}

# Columns of pbp that calculate_stats() uses
stats_pbp_columns <- function() {
  c(
    "season",
    "game_id",
    "play_id",
    "posteam",
    "defteam",
    "special",
    "season_type",
    "away_score",
    "home_score",
    "posteam_type",
    "defteam_score",
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
    "receiver_player_id"
  )
}
