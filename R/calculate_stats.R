calculate_stats <- function(seasons = nflreadr::most_recent_season(),
                            summary_level = c("season", "week"),
                            stat_type = c("player", "team")){

  # testing
  # seasons = 2023
  # summary_level = "week"
  # stat_type = "player"

  summary_level <- rlang::arg_match(summary_level)
  stat_type <- rlang::arg_match(stat_type)

  pbp <- nflreadr::load_pbp(seasons = seasons)

  playinfo <- pbp %>%
    dplyr::group_by(.data$game_id, .data$play_id) %>%
    dplyr::summarise(
      off = nflreadr::clean_team_abbrs(posteam),
      def = nflreadr::clean_team_abbrs(defteam),
      special = as.integer(special == 1)
    ) %>%
    dplyr::ungroup()

  # Function defined below
  # more_stats = all stat IDs of one player in a single play
  # team_stats = all stat IDs of one team in a single play
  # we need those to identify things like fumbles depending on playtype or
  # first downs depending on playtype
  playstats <- load_playstats(seasons = seasons) %>%
    dplyr::group_by(.data$season, .data$week, .data$play_id, .data$gsis_player_id) %>%
    dplyr::mutate(
      # we append a collapse separator to the string in order to search for matches
      # including the separator to avoid 1 matching 10
      more_stats = paste0(paste(stat_id, collapse = ";"), ";")
    ) %>%
    dplyr::group_by(.data$season, .data$week, .data$play_id, .data$team_abbr) %>%
    dplyr::mutate(
      # we append a collapse separator to the string in order to search for matches
      # including the separator to avoid 1 matching 10
      team_stats = paste0(paste(stat_id, collapse = ";"), ";"),
    ) %>%
    dplyr::group_by(.data$season, .data$week, .data$team_abbr) %>%
    dplyr::mutate(
      team_targets = sum(stat_id == 115),
      team_air_yards = sum((stat_id %in% 111:112) * yards)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(
      playinfo, by = c("game_id", "play_id")
    )

  if (stat_type == "player"){
    # need newer version of nflreadr to use load_players
    rlang::check_installed("nflreadr (>= 1.3.0)", "to join player information.")

    player_info <- nflreadr::load_players() %>%
      dplyr::select(
        "player_id" = "gsis_id",
        "player_display_name" = "display_name",
        "player_name" = "short_name",
        "position",
        "position_group",
        "headshot_url" = "headshot"
      )
  }

  # Check combination of summary_level and stat_type to set a helper that is
  # used to create the grouping variables
  grp_id <- data.table::fcase(
    summary_level == "season" && stat_type == "player", "10",
    summary_level == "season" && stat_type == "team",   "20",
    summary_level == "week"   && stat_type == "player", "30",
    summary_level == "week"   && stat_type == "team",   "40"
  )

  grp_vars <- switch (grp_id,
    "10" = rlang::data_syms(c("season", "player_id" = "gsis_player_id")),
    "20" = rlang::data_syms(c("season", "team_abbr")),
    "30" = rlang::data_syms(c("season", "week", "player_id" = "gsis_player_id")),
    "40" = rlang::data_syms(c("season", "week", "team_abbr"))
  )

  # Silence global vars NOTE
  # We do this differently here because it's only a bunch of variables
  # and the code is more readable
  utils::globalVariables(c(
    "stat_id", "yards", "more_stats", "team_stats", "team_abbr",
    "def", "off", "special"
  ))

  stats <- playstats %>%
    dplyr::group_by(!!!grp_vars) %>%
    dplyr::summarise(

      # Offense #####################
      completions = sum(stat_id %in% 15:16),
      attempts = sum(stat_id %in% c(14:16, 19)),
      passing_yards = sum((stat_id %in% 15:16) * yards),
      passing_tds = sum(stat_id == 16),
      passing_interceptions = sum(stat_id == 19),
      sacks_suffered = sum(stat_id == 20),
      sack_yards_lost = sum((stat_id == 20) * yards),
      sack_fumbles = sum(stat_id == 20 & any(has_id(52, more_stats), has_id(53, more_stats), has_id(54, more_stats))),
      sack_fumbles_lost = sum(stat_id == 20 & has_id(106, more_stats)),
      passing_air_yards = sum((stat_id %in% 111:112) * yards),
      # passing_yards_after_catch = 15:16 - 111,
      passing_first_downs = sum((stat_id %in% 15:16) & has_id(4, team_stats)),
      # passing_epa = requires pbp,
      passing_2pt_conversions = sum(stat_id == 77),
      pacr = .data$passing_yards / .data$passing_air_yards,
      # dakota = requires pbp,

      carries = sum(stat_id %in% 10:11),
      rushing_yards = sum((stat_id %in% 10:13) * yards),
      rushing_tds = sum(stat_id %in% c(11,13)),
      rushing_fumbles = sum((stat_id %in% 10:11) & any(has_id(52, more_stats), has_id(53, more_stats), has_id(54, more_stats))),
      rushing_fumbles_lost = sum((stat_id %in% 10:11) & has_id(106, more_stats)),
      rushing_first_downs = sum((stat_id %in% 10:11) & has_id(3, team_stats)),
      # rushing_epa = requires pbp,
      rushing_2pt_conversions = sum(stat_id == 75),

      receptions = sum(stat_id %in% 21:22),
      targets = sum(stat_id == 115),
      receiving_yards = sum((stat_id %in% 21:24) * yards),
      receiving_tds = sum(stat_id %in% c(22,24)),
      receiving_fumbles = sum((stat_id %in% 21:22) & any(has_id(52, more_stats), has_id(53, more_stats), has_id(54, more_stats))),
      receiving_fumbles_lost = sum((stat_id %in% 21:22) & has_id(106, more_stats)),
      # receiving_air_yards = that's in 111:112 but it is a passer stat not a receiver stat,
      receiving_yards_after_catch = sum((stat_id == 113) * yards),
      receiving_first_downs = sum((stat_id %in% 21:22) & has_id(4, team_stats)),
      # receiving_epa = requires pbp,
      receiving_2pt_conversions = sum(stat_id == 104),
      # racr = .data$receiving_yards / .data$receiving_air_yards,
      target_share = .data$targets / .data$team_targets,
      # air_yards_share = .data$receiving_air_yards / .data$team_air_yards,
      # wopr = 1.5 * .data$target_share + 0.7 * .data$air_yards_share,
      special_teams_tds = sum((special == 1) & stat_id %in% td_ids()),

      # fantasy_points =
      #   1 / 25 * .data$passing_yards +
      #   4 * .data$passing_tds +
      #   -2 * .data$interceptions +
      #   1 / 10 * (.data$rushing_yards + .data$receiving_yards) +
      #   6 * (.data$rushing_tds + .data$receiving_tds + .data$special_teams_tds) +
      #   2 * (.data$passing_2pt_conversions + .data$rushing_2pt_conversions + .data$receiving_2pt_conversions) +
      #   -2 * (.data$sack_fumbles_lost + .data$rushing_fumbles_lost + .data$receiving_fumbles_lost),

      # fantasy_points_ppr = .data$fantasy_points + .data$receptions,

      # Defense #####################
      # def_tackles = ,
      def_tackles_solo = sum(stat_id == 79),
      def_tackles_with_assist = sum(stat_id == 80),
      def_tackle_assists = sum(stat_id == 82),
      def_tackles_for_loss = sum(stat_id == 402),
      def_tackles_for_loss_yards = sum((stat_id == 402) * yards),
      def_fumbles_forced = sum(stat_id == 91),
      def_sacks = sum(stat_id == 83) + 1 / 2 * sum(stat_id == 84),
      def_sack_yards = sum((stat_id == 83) * -yards) + 1 / 2 * sum((stat_id == 84) * -yards),
      def_qb_hits = sum(stat_id == 110),
      def_interceptions = sum(stat_id %in% 25:26),
      def_interception_yards = sum((stat_id %in% 25:28) * yards),
      def_pass_defended = sum(stat_id == 85),
      def_tds = sum((team_abbr == def) & stat_id %in% td_ids()),
      def_fumbles = sum((team_abbr == def) & stat_id %in% 52:54),
      def_fumble_recovery_own = sum((team_abbr == def) & stat_id %in% 55:56),
      def_fumble_recovery_yards_own = sum((team_abbr == def) & stat_id %in% 55:58),
      def_fumble_recovery_opp = sum((team_abbr == def) & stat_id %in% 59:60),
      def_fumble_recovery_yards_opp = sum((team_abbr == def) & stat_id %in% 59:62),
      def_safety = sum(stat_id == 89),
      def_penalty = sum((team_abbr == def) & stat_id == 93),
      def_penalty_yards = sum((team_abbr == def & stat_id == 93) * yards),

      # Kicking #####################
      fg_made = sum(stat_id == 70),
      fg_att = sum(stat_id %in% 69:71),
      fg_missed = sum(stat_id == 69),
      fg_blocked = sum(stat_id == 71),
      fg_long = max((stat_id == 70) * yards) %0% NA_integer_,
      fg_pct = round(.data$fg_made / .data$fg_att, 3L),
      fg_made_0_19 =  sum((stat_id == 70) * (yards %between% c(0, 19))),
      fg_made_20_29 = sum((stat_id == 70) * (yards %between% c(20, 29))),
      fg_made_30_39 = sum((stat_id == 70) * (yards %between% c(30, 39))),
      fg_made_40_49 = sum((stat_id == 70) * (yards %between% c(40, 49))),
      fg_made_50_59 = sum((stat_id == 70) * (yards %between% c(50, 59))),
      fg_made_60_ =   sum((stat_id == 70) * (yards > 60)),
      fg_missed_0_19 =  sum((stat_id == 69) * (yards %between% c(0, 19))),
      fg_missed_20_29 = sum((stat_id == 69) * (yards %between% c(20, 29))),
      fg_missed_30_39 = sum((stat_id == 69) * (yards %between% c(30, 39))),
      fg_missed_40_49 = sum((stat_id == 69) * (yards %between% c(40, 49))),
      fg_missed_50_59 = sum((stat_id == 69) * (yards %between% c(50, 59))),
      fg_missed_60_ =   sum((stat_id == 69) * (yards > 60)),
      fg_made_list =    fg_list(stat_id, yards, collapse_id = 70),
      fg_missed_list =  fg_list(stat_id, yards, collapse_id = 69),
      fg_blocked_list = fg_list(stat_id, yards, collapse_id = 71),
      fg_made_distance = sum((stat_id == 70) * yards),
      fg_missed_distance = sum((stat_id == 69) * yards),
      fg_blocked_distance = sum((stat_id == 71) * yards),
      pat_made = sum(stat_id == 72),
      pat_att = sum(stat_id %in% 72:74),
      pat_missed = sum(stat_id == 73),
      pat_blocked = sum(stat_id == 74),
      pat_pct = round(.data$pat_made / .data$pat_att, 3L),
      # gwfg_att = ,
      # gwfg_distance = ,
      # gwfg_made = ,
      # gwfg_missed = ,
      # gwfg_blocked =
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      pacr = dplyr::case_when(
        is.nan(.data$pacr) ~ NA_real_,
        .data$passing_air_yards <= 0 ~ 0,
        TRUE ~ .data$pacr
      )
    ) %>%
    dplyr::mutate_if(
      .predicate = is.character,
      .funs = ~ dplyr::na_if(.x, "")
    ) %>%
    dplyr::left_join(player_info, by = "player_id") %>%
    dplyr::select(
      "player_id",
      "player_name",
      "player_display_name",
      "position",
      "position_group",
      "headshot_url",
      dplyr::everything()
    )

    # set grouping variables based off summary_level and stat_type
    #
    # sumarise epa stats and dakota using pbp
    #
    # summarise all other stats using playstats. That's a big call to summarise
    # where we create all sorts of stats with the various stat IDs
    #
    # load player data if stat_type is player to joing player info
    #
    # join everything

}

load_playstats <- function(seasons = nflreadr::most_recent_season()) {

  if(isTRUE(seasons)) seasons <- seq(1999, nflreadr::most_recent_season())

  stopifnot(is.numeric(seasons),
            seasons >= 1999,
            seasons <= nflreadr::most_recent_season())

  urls <- paste0("https://github.com/nflverse/nflverse-pbp/releases/download/playstats/play_stats_",
                 seasons, ".rds")

  out <- nflreadr::load_from_url(urls, seasons = TRUE, nflverse = FALSE)

  out
}

fg_list <- function(stat_ids, yards, collapse_id){
  paste(
    yards[stat_ids == collapse_id],
    collapse = ";"
  )
}

`%0%` <- function(lhs, rhs) if (lhs != 0) lhs else rhs

has_id <- function(id, all_ids){
  grepl(paste0(id, ";"), all_ids, fixed = TRUE, useBytes = TRUE)
}

td_ids <- function(){
  c(
    11, 13, 16, 18, 22, 24, 26, 28, 34,
    36, 46, 48, 56, 58, 60, 62, 64, 108
  )
}
