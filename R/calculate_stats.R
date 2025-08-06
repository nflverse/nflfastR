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
#' @seealso <https://www.nflfastr.com/articles/stats_variables.html> for a searchable
#' table of the stats variable descriptions.
#' @export
#'
#' @examples
#' \donttest{
#' try({# to avoid CRAN test problems
#' stats <- calculate_stats(2023, "season", "player")
#' dplyr::glimpse(stats)
#' })
#' }
calculate_stats <- function(seasons = nflreadr::most_recent_season(),
                            summary_level = c("season", "week"),
                            stat_type = c("player", "team"),
                            season_type = c("REG", "POST", "REG+POST"),
                            pbp = NULL){

  summary_level <- rlang::arg_match(summary_level)
  stat_type <- rlang::arg_match(stat_type)
  season_type <- rlang::arg_match(season_type)
  custom_pbp <- !is.null(pbp)

  if (!custom_pbp) pbp <- nflreadr::load_pbp(seasons = seasons)

  # make sure (custom) pbp includes all required variables.
  # stats_validate_pbp will return all unique seasons in pbp.
  # We'll use this to download playstats for all seasons listed in pbp.
  seasons_in_pbp <- stats_validate_pbp(pbp)

  # we don't want groups to mess up something or slow us down.
  # this is only relevant if a user supplies grouped pbp data
  pbp <- dplyr::ungroup(pbp)

  if (season_type %in% c("REG", "POST") && summary_level == "season") {
    pbp <- dplyr::filter(pbp, .data$season_type == .env$season_type)
    if (nrow(pbp) == 0){
      cli::cli_alert_warning(
        "Filtering {.val {seasons}} data to {.arg season_type} == \\
        {.val {season_type}} resulted in 0 rows. Returning empty tibble."
      )
      return(tibble::tibble())
    }
  }

  # defensive stats require knowledge of which team is on defense
  # special teams stats require knowledge of which plays were special teams plays
  playinfo <- pbp |>
    dplyr::group_by(.data$game_id, .data$play_id) |>
    dplyr::summarise(
      off = .data$posteam,
      def = .data$defteam,
      special = as.integer(.data$special == 1)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate_at(
      .vars = dplyr::vars("off", "def"),
      .funs = team_name_fn
    )

  season_type_from_pbp <- pbp |>
    dplyr::select("game_id", "season_type") |>
    dplyr::distinct()
  s_type_vctr <- season_type_from_pbp$season_type |>
    rlang::set_names(season_type_from_pbp$game_id)

  gwfg_attempts_from_pbp <- pbp |>
    dplyr::mutate(
      # final_posteam_score = data.table::fifelse(.data$posteam_type == "home", .data$home_score, .data$away_score),
      final_defteam_score = data.table::fifelse(.data$posteam_type == "home", .data$away_score, .data$home_score),
      identifier = paste(.data$game_id, .data$play_id, sep = "_")
    ) |>
    dplyr::group_by(.data$game_id, .data$posteam) |>
    dplyr::mutate(
      # A game winning field goal attempt is
      # - a field goal attempt,
      # - in the posteam's final drive,
      # - where the posteam trailed the defteam by 2 points or less prior to the kick,
      # - and the defteam did not score afterwards
      is_gwfg_attempt = dplyr::case_when(
        .data$field_goal_attempt == 1 &
          .data$fixed_drive == max(.data$fixed_drive) &
          dplyr::between(.data$score_differential, -2, 0) &
          .data$defteam_score == .data$final_defteam_score ~ 1L,
        TRUE ~ 0L
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(
      is_gwfg_attempt == 1L
    ) |>
    dplyr::select("identifier", "is_gwfg_attempt")
  gwfg_vctr <- gwfg_attempts_from_pbp$is_gwfg_attempt |>
    rlang::set_names(gwfg_attempts_from_pbp$identifier)

  # load_playstats defined below
  # more_stats = all stat IDs of one player in a single play
  # team_stats = all stat IDs of one team in a single play
  # we need those to identify things like fumbles depending on playtype or
  # first downs depending on playtype
  playstats <- load_playstats(seasons = seasons_in_pbp) |>
    # apply filtering on play stats so that it matches only plays included
    # in pbp in case it was provided manually
    dplyr::semi_join(pbp, by = c("game_id", "play_id")) |>
    dplyr::rename("player_id" = "gsis_player_id", "team" = "team_abbr") |>
    dplyr::group_by(.data$season, .data$week, .data$play_id, .data$player_id) |>
    dplyr::mutate(
      # we append a collapse separator to the string in order to search for matches
      # including the separator to avoid 1 matching 10
      more_stats = paste0(paste(stat_id, collapse = ";"), ";")
    ) |>
    dplyr::group_by(.data$season, .data$week, .data$play_id, .data$team) |>
    dplyr::mutate(
      # we append a collapse separator to the string in order to search for matches
      # including the separator to avoid 1 matching 10
      team_stats = paste0(paste(stat_id, collapse = ";"), ";"),
      team_play_air_yards = sum((stat_id %in% 111:112) * yards)
    ) |>
    # compute team targets and team air yards for calculation of target share
    # and air yard share. Since it's relative, we need to be careful with the groups
    # depending on summary level
    dplyr::group_by(!!!rlang::data_syms(
      if (summary_level == "season") c("season", "team") else c("season", "week", "team")
    )) |>
    dplyr::mutate(
      team_targets = sum(stat_id == 115),
      team_air_yards = sum((stat_id %in% 111:112) * yards)
    ) |>
    dplyr::ungroup() |>
    dplyr::left_join(
      playinfo, by = c("game_id", "play_id")
    ) |>
    dplyr::mutate(
      season_type = unname(s_type_vctr[.data$game_id]),
      is_gwfg_attempt = unname(gwfg_vctr[paste(.data$game_id, .data$play_id, sep = "_")]) %ifna% 0L
    )

  # Check combination of summary_level and stat_type to set a helper that is
  # used to create the grouping variables
  grp_id <- data.table::fcase(
    summary_level == "season" && stat_type == "player", "10",
    summary_level == "season" && stat_type == "team",   "20",
    summary_level == "week"   && stat_type == "player", "30",
    summary_level == "week"   && stat_type == "team",   "40"
  )
  # grp_vctr is used as character vector for joining pbp stats
  grp_vctr <- switch (grp_id,
    "10" = c("season", "player_id"),
    "20" = c("season", "team"),
    "30" = c("season", "week", "player_id"),
    "40" = c("season", "week", "team")
  )
  # grp_vars is used as grouping variables
  grp_vars <- rlang::data_syms(grp_vctr)

  # Stats from PBP #####################
  # we want passing epa, rushing epa, and receiving epa
  # since these depend on different player id variables and filters,
  # we create separate dfs for these stats
  passing_stats_from_pbp <- pbp |>
    dplyr::filter(.data$play_type %in% c("pass", "qb_spike")) |>
    dplyr::select(
      "season", "week", "team" = "posteam",
      "player_id" = "passer_player_id", "qb_epa", "cpoe"
    ) |>
    dplyr::group_by(!!!grp_vars) |>
    dplyr::summarise(
      passing_epa = sum(.data$qb_epa, na.rm = TRUE),
      # mean will return NaN if all values are NA, because we remove NA
      passing_cpoe = if (any(!is.na(.data$cpoe))) mean(.data$cpoe, na.rm = TRUE) else NA_real_
    ) |>
    dplyr::ungroup()

  rushing_stats_from_pbp <- pbp |>
    dplyr::filter(.data$play_type %in% c("run", "qb_kneel")) |>
    dplyr::select(
      "season", "week", "team" = "posteam",
      "player_id" = "rusher_player_id", "epa"
    ) |>
    dplyr::group_by(!!!grp_vars) |>
    dplyr::summarise(
      rushing_epa = sum(.data$epa, na.rm = TRUE)
    ) |>
    dplyr::ungroup()

  receiving_stats_from_pbp <- pbp |>
    dplyr::filter(!is.na(.data$receiver_player_id)) |>
    dplyr::select(
      "season", "week", "team" = "posteam",
      "player_id" = "receiver_player_id", "epa"
    ) |>
    dplyr::group_by(!!!grp_vars) |>
    dplyr::summarise(
      receiving_epa = sum(.data$epa, na.rm = TRUE)
    ) |>
    dplyr::ungroup()

  stats <- playstats |>
    dplyr::group_by(!!!grp_vars) |>
    dplyr::summarise(
      player_name = if (.env$stat_type == "player") custom_mode(.data$player_name, na.rm = TRUE) else NULL,
      # Season Type #####################
      # if summary level is week, then we have to use the season type variable
      # from playstats as it could be REG or POST depending on the value of
      # the argument season_type
      # if summary level is season, then we collapse the values of season_type
      # this will make sure that season_type is only REG+POST if the user asked
      # for it AND if postseason data is available
      season_type = if (.env$summary_level == "week") dplyr::first(.data$season_type) else paste(unique(.data$season_type), collapse = "+"),

      # Team Info #####################
      # recent_team if we do a season summary of player stats
      # team if we do a week summary of player stats
      recent_team = if (.env$grp_id == "10") dplyr::last(.data$team) else NULL,
      team = if (.env$grp_id == "30") dplyr::first(.data$team) else NULL,
      # opponent team if we do week summaries
      opponent_team = if (.env$summary_level == "week"){
        data.table::fifelse(
          dplyr::first(.data$team) == dplyr::first(.data$off),
          dplyr::first(.data$def),
          dplyr::first(.data$off)
        )
      } else NULL,

      # number of games is only relevant if we summarise the season
      games = if (.env$summary_level == "season") dplyr::n_distinct(.data$game_id) else NULL,

      # Offense #####################
      completions = sum(stat_id %in% 15:16),
      attempts = sum(stat_id %in% c(14:16, 19)),
      passing_yards = sum((stat_id %in% 15:16) * yards),
      passing_tds = sum(stat_id == 16),
      passing_interceptions = sum(stat_id == 19),
      sacks_suffered = sum(stat_id == 20),
      sack_yards_lost = sum((stat_id == 20) * yards),
      sack_fumbles = sum(stat_id == 20 & has_id(52:54, more_stats)),
      sack_fumbles_lost = sum(stat_id == 20 & has_id(106, more_stats)),
      # includes incompletions (111 = complete, 112 = incomplete)
      passing_air_yards = sum((stat_id %in% 111:112) * yards),
      # passing yac equals passing yards - air yards on completed passes
      passing_yards_after_catch = .data$passing_yards - sum((stat_id == 111) * yards),
      passing_first_downs = sum((stat_id %in% 15:16) & has_id(4, team_stats)),
      passing_2pt_conversions = sum(stat_id == 77),
      # this is a player stat and we skip it in team stats
      pacr = if (.env$stat_type == "player") .data$passing_yards / .data$passing_air_yards else NULL,
      # dakota = requires pbp,

      carries = sum(stat_id %in% 10:11),
      rushing_yards = sum((stat_id %in% 10:13) * yards),
      rushing_tds = sum(stat_id %in% c(11,13)),
      rushing_fumbles = sum((stat_id %in% 10:11) & has_id(52:54, more_stats)),
      rushing_fumbles_lost = sum((stat_id %in% 10:11) & has_id(106, more_stats)),
      rushing_first_downs = sum((stat_id %in% 10:11) & has_id(3, team_stats)),
      rushing_2pt_conversions = sum(stat_id == 75),

      receptions = sum(stat_id %in% 21:22),
      targets = sum(stat_id == 115),
      receiving_yards = sum((stat_id %in% 21:24) * yards),
      receiving_tds = sum(stat_id %in% c(22,24)),
      receiving_fumbles = sum((stat_id %in% 21:22) & has_id(52:54, more_stats)),
      receiving_fumbles_lost = sum((stat_id %in% 21:22) & has_id(106, more_stats)),
      # air_yards are counted in 111:112 but it is a passer stat not a receiver stat
      # so we count team air yards when a player accounted for a reception
      # team air yards will always equal the correct air yards as 111 and 112
      # cannot appear more than once per play.
      # If this ever changes, we can use pbp instead.
      receiving_air_yards = if (.env$stat_type == "player"){
        sum( (stat_id == 115) * .data$team_play_air_yards )
      } else .data$passing_air_yards,
      receiving_yards_after_catch = sum((stat_id == 113) * yards),
      receiving_first_downs = sum((stat_id %in% 21:22) & has_id(4, team_stats)),
      receiving_2pt_conversions = sum(stat_id == 104),
      # these are player stats and we skip them in team stats
      racr = if (.env$stat_type == "player") .data$receiving_yards / .data$receiving_air_yards else NULL,
      target_share = if (.env$stat_type == "player") .data$targets / dplyr::first(.data$team_targets) else NULL,
      air_yards_share = if (.env$stat_type == "player") .data$receiving_air_yards / dplyr::first(.data$team_air_yards) else NULL,
      wopr = if (.env$stat_type == "player") 1.5 * .data$target_share + 0.7 * .data$air_yards_share else NULL,

      special_teams_tds = sum((special == 1) & stat_id %in% td_ids()),

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
      def_tds = sum(team == def & special != 1 & stat_id %in% td_ids()),
      def_fumbles = sum((team == def) & stat_id %in% 52:54),
      def_safeties = sum(stat_id == 89),

      # Misc #####################
      # mostly yards gained after blocked punts or fgs
      misc_yards = sum((stat_id %in% 63:64) * yards),
      fumble_recovery_own = sum(stat_id %in% 55:56),
      # 57, 58 don't count as recovery because player received a
      # lateral after recovery by other player
      fumble_recovery_yards_own = sum(stat_id %in% 55:58),
      fumble_recovery_opp = sum(stat_id %in% 59:60),
      # 61, 62 don't count as recovery because player received a
      # lateral after recovery by other player
      fumble_recovery_yards_opp = sum(stat_id %in% 59:62),
      fumble_recovery_tds = sum(stat_id %in% c(56, 58, 60, 62)),
      penalties = sum(stat_id == 93),
      penalty_yards = sum((stat_id == 93) * yards),
      timeouts = if (.env$stat_type == "team") sum(stat_id == 68) else NULL,

      # Returning #####################
      punt_returns = sum(stat_id %in% 33:34),
      punt_return_yards = sum((stat_id %in% 33:36) * yards),
      # punt return tds are counted in special teams tds atm
      # punt_return_tds = sum(stat_id %in% c(34, 36)),
      kickoff_returns = sum(stat_id %in% 45:46),
      kickoff_return_yards = sum((stat_id %in% 45:48) * yards),
      # kickoff return tds are counted in special teams tds atm
      # kickoff_return_tds = sum(stat_id %in% c(46, 48)),

      # Kicking #####################
      fg_made = sum(stat_id == 70),
      fg_att = sum(stat_id %in% 69:71),
      fg_missed = sum(stat_id == 69),
      fg_blocked = sum(stat_id == 71),
      fg_long = max((stat_id == 70) * yards) %0% NA_integer_,
      # avoid 0/0 = NaN
      fg_pct = if (.data$fg_att > 0) .data$fg_made / .data$fg_att else NA_real_,
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
      # avoid 0/0 = NaN
      pat_pct = if (.data$pat_att > 0) .data$pat_made / .data$pat_att else NA_real_,
      gwfg_made = sum((stat_id == 70) * is_gwfg_attempt),
      gwfg_att = sum((stat_id %in% 69:71) * is_gwfg_attempt),
      gwfg_missed = sum((stat_id == 69) * is_gwfg_attempt),
      gwfg_blocked = sum((stat_id == 71) * is_gwfg_attempt),
      gwfg_distance = if (.env$summary_level == "week") sum((stat_id %in% 69:71) * is_gwfg_attempt * yards) else NULL,
      gwfg_distance_list = if (.env$summary_level == "season") fg_list(stat_id, yards, collapse_id = 69:71, gwfg = is_gwfg_attempt) else NULL,
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate_if(
      .predicate = is.character,
      .funs = ~ dplyr::na_if(.x, "")
    ) |>
    # Join PBP Stats #####################
    dplyr::left_join(passing_stats_from_pbp,   by = grp_vctr) |>
    dplyr::left_join(rushing_stats_from_pbp,   by = grp_vctr) |>
    dplyr::left_join(receiving_stats_from_pbp, by = grp_vctr) |>
    # relocate epa variables. This could be done with dplyr::relocate
    # but we want to be compatible with older dplyr versions
    dplyr::select(
      "season":"passing_first_downs",
      "passing_epa", "passing_cpoe",
      "passing_2pt_conversions":"rushing_first_downs",
      "rushing_epa",
      "rushing_2pt_conversions":"receiving_first_downs",
      "receiving_epa",
      dplyr::everything()
    ) |>
    dplyr::arrange(!!!grp_vars)

  # Apply Player Modifications #####################
  if (stat_type == "player"){
    # need newer version of nflreadr to use load_players
    rlang::check_installed("nflreadr (>= 1.3.0)", "to join player information.")

    player_info <- nflreadr::load_players() |>
      dplyr::select(
        "player_id" = "gsis_id",
        "player_display_name" = "display_name",
        # "player_name" = "short_name",
        "position",
        "position_group",
        "headshot_url" = "headshot"
      )

    # load gsis_ids of RBs, FBs and HBs for RACR
    racr_ids <- player_info |>
      dplyr::filter(.data$position %in% c("RB", "FB", "HB")) |>
      dplyr::pull("player_id")

    stats <- stats |>
      dplyr::mutate(
        pacr = dplyr::case_when(
          is.nan(.data$pacr) ~ NA_real_,
          .data$passing_air_yards <= 0 ~ 0,
          TRUE ~ .data$pacr
        ),
        racr = dplyr::case_when(
          is.nan(.data$racr) ~ NA_real_,
          .data$receiving_air_yards == 0 ~ 0,
          # following Josh Hermsmeyer's definition, RACR stays < 0 for RBs (and FBs) and is set to
          # 0 for Receivers. The list "racr_ids" includes all known RB and FB gsis_ids
          .data$receiving_air_yards < 0 & !.data$player_id %in% racr_ids ~ 0,
          TRUE ~ .data$racr
        ),
        # Fantasy #####################
        fantasy_points =
          1 / 25 * .data$passing_yards +
          4 *      .data$passing_tds +
          -2 *     .data$passing_interceptions +
          1 / 10 * (.data$rushing_yards + .data$receiving_yards) +
          6 *      (.data$rushing_tds + .data$receiving_tds + .data$special_teams_tds) +
          2 *      (.data$passing_2pt_conversions + .data$rushing_2pt_conversions + .data$receiving_2pt_conversions) +
          -2 *     (.data$sack_fumbles_lost + .data$rushing_fumbles_lost + .data$receiving_fumbles_lost),

        fantasy_points_ppr = .data$fantasy_points + .data$receptions
      ) |>
      dplyr::left_join(player_info, by = "player_id") |>
      dplyr::select(
        "player_id",
        "player_name",
        "player_display_name",
        "position",
        "position_group",
        "headshot_url",
        dplyr::everything()
      )
  }

  if (custom_pbp) attr(stats, "custom_pbp") <- TRUE

  stats
}

# Silence global vars NOTE
# We do this differently here because it's only a bunch of variables
# and the code is more readable
utils::globalVariables(c(
  "stat_id", "yards", "more_stats", "team_stats", "team",
  "def", "off", "special", "is_gwfg_attempt"
))

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

fg_list <- function(stat_ids, yards, collapse_id, gwfg = NULL){
  if (is.null(gwfg)) {
    paste(
      yards[stat_ids == collapse_id],
      collapse = ";"
    )
  } else {
    paste(
      yards[stat_ids %in% collapse_id & gwfg == 1L],
      collapse = ";"
    )
  }
}

`%0%` <- function(lhs, rhs) if (lhs != 0) lhs else rhs

`%ifna%` <- function(lhs, rhs) data.table::fifelse(is.na(lhs), rhs, lhs)

has_id <- function(id, all_ids){
  stringr::str_detect(all_ids, paste0(id, ";", collapse = "|"))
}

td_ids <- function(){
  c(
    11, 13, 16, 18, 22, 24, 26, 28, 34,
    36, 46, 48,
    # 56, 58, 60, 62, # 56-62 are separately counted in fumble_recovery_tds
    64, 108
  )
}

stats_validate_pbp <- function(pbp) {
  required_names <- c(
    "season", "game_id", "play_id", "posteam", "defteam", "special",
    "season_type", "away_score", "home_score", "field_goal_attempt",
    "fixed_drive", "score_differential", "play_type", "week",
    "passer_player_id", "qb_epa", "cpoe", "rusher_player_id", "epa",
    "receiver_player_id"
  )
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
