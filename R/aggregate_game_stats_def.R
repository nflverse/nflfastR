################################################################################
# Author: Christian Lohr, Sebastian Carl, Tan Ho
# Styleguide: styler::tidyverse_style()
################################################################################

#' Get Official Game Stats on Defense
#'
#' @param pbp A Data frame of NFL play-by-play data typically loaded with
#'   [load_pbp()] or [build_nflfastR_pbp()]. If the data doesn't include the variable
#'   `qb_epa`, the function `add_qb_epa()` will be called to add it.
#' @param weekly If `TRUE`, returns week-by-week stats, otherwise, stats
#'   for the entire Data frame.
#' @description Build columns that aggregate official defense stats
#'   either at the game level or at the level of the entire data frame passed.
#' @return A data frame of defensive player stats. See dictionary (# TODO)
#' @export
#' @seealso The function [load_player_stats()] and the corresponding examples
#' on [the nflfastR website](https://www.nflfastr.com/articles/nflfastR.html#example-11-replicating-official-stats)
#' @examples
#' \donttest{
#' try({# to avoid CRAN test problems
#'   pbp <- nflfastR::load_pbp(2020)
#'
#'   weekly <- calculate_player_stats_def(pbp, weekly = TRUE)
#'   dplyr::glimpse(weekly)
#'
#'   overall <- calculate_player_stats_def(pbp, weekly = FALSE)
#'   dplyr::glimpse(overall)
#' })
#' }
#'

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# what do we need:
#
# Solo Tackles --> done
# Tackles With Assist --> done
# Assisted Tackles --> done
# Tackles for Loss --> done
# TFL Yards --> done
# Sacks --> done
# Sack Yards --> done
# QB Hits --> done
# Passes Defensed --> done
# Interceptions --> done
# Interception Yards --> done
# Interception Return TDs ///// --> only "TD" for defense
# Forced Fumbles --> done
# Opp Fumble Recoveries --> done
# Opp Fumble Recovery Yards --> done
# Opp Fumble Recovery TDs ///// --> only "TD" for defense
# Safeties --> done
# Penalties --> done
# Penalty Yards --> done
# Fumbles --> done
# Own Fumble Recoveries --> done
# Own Fumble Recovery Yards --> done
# Own Fumble Recovery TDs ///// --> only "TD" for defense

calculate_player_stats_def <- function(pbp, weekly = FALSE) {

  # need newer version of nflreadr to use load_players
  rlang::check_installed("nflreadr (>= 1.3.0)")

  # Prepare data ------------------------------------------------------------

  suppressMessages({
    # 1. for "normal" plays: get plays that count in official stats
    # we exclude special teams and 2pts here for now
    data <- pbp %>%
      dplyr::filter(
        !is.na(.data$down),
        .data$play_type %in% c("pass", "qb_kneel", "qb_spike", "run")
      ) %>%
      nflfastR::decode_player_ids()

    # 2. filter penalty plays for penalty stats
    penalty_data <- pbp %>%
      dplyr::filter(.data$penalty == 1) %>%
      nflfastR::decode_player_ids()

  })

  stype <- data %>%
    dplyr::select("season", "week", "season_type") %>%
    dplyr::distinct()

  # Tackling stats -----------------------------------------------------------

  tackle_vars <- c(
    "solo_tackle_1_player_id",
    "tackle_for_loss_1_player_id",
    "assist_tackle_1_player_id",
    "tackle_with_assist_1_player_id",
    "solo_tackle_2_player_id",
    "forced_fumble_player_1_player_id",
    "assist_tackle_2_player_id",
    "forced_fumble_player_2_player_id"
  )

  # get tackling stats
  tackle_df <- data %>%
    dplyr::select("season", "week", "defteam", tidyselect::any_of(tackle_vars)) %>%
    tidyr::pivot_longer(
      cols = tidyselect::any_of(tackle_vars),
      names_to = "desc",
      values_to = "tackle_player_id",
      values_drop_na = TRUE
    ) %>%
    dplyr::count(.data$tackle_player_id, .data$defteam, .data$season, .data$week, .data$desc) %>%
    dplyr::mutate(
      desc = stringr::str_remove_all(.data$desc,"_player_id") %>%
        stringr::str_remove_all("_[0-9]")
    ) %>%
    tidyr::pivot_wider(
      names_from = .data$desc,
      values_from = .data$n,
      values_fill = 0L,
      values_fn = sum
    ) %>%
    add_column_if_missing(
      "solo_tackle", "tackle_with_assist", "tackle_for_loss", "assist_tackle",
      "forced_fumble_player"
    ) %>%
    dplyr::mutate(
      tackles = .data$solo_tackle + .data$tackle_with_assist
    ) %>%
    dplyr::select(
      "season",
      "week",
      "team" = "defteam",
      "player_id" = "tackle_player_id",
      "tackles",
      "tackles_solo" = "solo_tackle",
      "tackles_with_assist" = "tackle_with_assist",
      "tackle_assists" = "assist_tackle",
      "forced_fumbles" = "forced_fumble_player",
      "tackles_for_loss" = "tackle_for_loss"
    ) %>%
    dplyr::group_by(.data$season, .data$week, .data$team, .data$player_id) %>%
    dplyr::summarise(
      tackles = sum(.data$tackles, na.rm = TRUE),
      tackles_solo = sum(.data$tackles_solo,na.rm = TRUE),
      tackles_with_assist = sum(.data$tackles_with_assist, na.rm=TRUE),
      tackle_assists = sum(.data$tackle_assists, na.rm = TRUE),
      forced_fumbles = sum(.data$forced_fumbles, na.rm=  TRUE),
      tackles_for_loss = sum(.data$tackles_for_loss, na.rm = TRUE)
    ) %>%
    dplyr::ungroup()

  # get tackle for loss yards
  tackle_yards_df <- data %>%
    dplyr::filter(.data$tackled_for_loss == 1, .data$fumble == 0, .data$sack == 0) %>%
    dplyr::select("season","week","team" = "defteam",
                  "tackle_for_loss_1_player_id", "tackle_for_loss_2_player_id",
                  "yards_gained") %>%
    tidyr::pivot_longer(
      cols = c("tackle_for_loss_1_player_id","tackle_for_loss_2_player_id"),
      names_to = "desc",
      values_to = "player_id",
      values_drop_na = TRUE
    ) %>%
    dplyr::group_by(.data$season, .data$week, .data$team, .data$player_id) %>%
    dplyr::summarise(
      tfl_yards = sum(-.data$yards_gained, na.rm = TRUE)
    ) %>%
    dplyr::ungroup()

  # Sack and QB Hits stats -----------------------------------------------------------

  # get sack and pressure stats
  pressure_df <- data %>%
    dplyr::select(
      "season",
      "week",
      "team"="defteam",
      tidyselect::contains("sack_"),
      "yards_gained",
      tidyselect::starts_with("qb_hit_"),
      -tidyselect::contains("_name")) %>%
    tidyr::pivot_longer(
      cols = c(
        tidyselect::contains("sack_"),
        tidyselect::starts_with("qb_hit_")
      ),
      names_to = "desc",
      names_prefix = "sk_",
      values_to = "player_id",
      values_drop_na = TRUE
    ) %>%
    dplyr::mutate(
      n = dplyr::case_when(
        .data$desc %in% c("half_sack_1_player_id", "half_sack_2_player_id") ~ 0.5,
        TRUE ~ 1
      ),
      desc = stringr::str_remove_all(.data$desc,"_player_id") %>%
        stringr::str_remove_all("_[0-9]") %>%
        stringr::str_remove("half_")
    ) %>%
    dplyr::mutate(
      sack_yards = .data$n * .data$yards_gained * -1
    ) %>%
    tidyr::pivot_wider(
      names_from = .data$desc,
      values_from = c(.data$n, .data$sack_yards),
      values_fn = sum,
      values_fill = 0L
    ) %>%
    add_column_if_missing("n_sack", "n_qb_hit", "sack_yards_sack") %>%
    dplyr::select(
      "season",
      "week",
      "team",
      "player_id",
      "sacks" = "n_sack",
      "qb_hit" = "n_qb_hit",
      "sack_yards" = "sack_yards_sack"
    ) %>%
    dplyr::group_by(.data$season, .data$week, .data$team, .data$player_id) %>%
    dplyr::summarise(
      sacks = sum(.data$sacks, na.rm = TRUE),
      qb_hit = sum(.data$qb_hit, na.rm = TRUE),
      sack_yards = sum(.data$sack_yards, na.rm = TRUE)
    ) %>%
    dplyr::ungroup()

  # Interception and Deflection stats ---------------------------------------------------------

  # get int and def stats
  int_df <- data %>%
    dplyr::select(
      "season", "week","return_yards","team"="defteam",
      tidyselect::starts_with("interception_"),
      tidyselect::starts_with("pass_defense_"),
      -tidyselect::contains("_name")) %>%
    tidyr::pivot_longer(
      cols = c(
        tidyselect::starts_with("interception_"),
        tidyselect::starts_with("pass_defense_")
      ),
      names_to = "desc",
      names_prefix = "int_",
      values_to = "db_player_id",
      values_drop_na = TRUE
    ) %>%
    dplyr::mutate(
      n = 1,
      desc = stringr::str_remove_all(.data$desc,"_player_id") %>%
        stringr::str_remove_all("_[0-9]")
    ) %>%
    tidyr::pivot_wider(
      names_from = "desc",
      values_from = c("n","return_yards"),
      values_fn = sum,
      values_fill = 0L
    ) %>%
    add_column_if_missing(
      "n_interception", "n_pass_defense", "return_yards_interception"
    ) %>%
    dplyr::select(
      "season",
      "week",
      "team",
      "player_id" = "db_player_id",
      "int" = "n_interception",
      "pass_defended" = "n_pass_defense",
      "int_yards" = "return_yards_interception"
    ) %>%
    dplyr::group_by(.data$season, .data$week, .data$team, .data$player_id) %>%
    dplyr::summarise(
      int = sum(.data$int, na.rm = TRUE),
      pass_defended = sum(.data$pass_defended, na.rm = TRUE),
      int_yards = sum(.data$int_yards, na.rm = TRUE)
    ) %>%
    dplyr::ungroup()

  # Safety stats -----------------------------------------------------------

  safety_df <- data %>%
    dplyr::filter(.data$safety == 1, !is.na(.data$safety_player_id)) %>%
    dplyr::select("season","week","team" = "defteam","player_id" = "safety_player_id") %>%
    dplyr::count(.data$season, .data$week, .data$team, .data$player_id, name = "safety") %>%
    dplyr::group_by(.data$season, .data$week, .data$team, .data$player_id) %>%
    dplyr::summarise(
      safety = sum(.data$safety,na.rm = TRUE)
    ) %>%
    dplyr::ungroup()

  # Fumble stats -----------------------------------------------------------

  # get fumble stats for fumbles and own fumble recoveries
  fumble_df_own <- data %>%
    dplyr::filter(.data$fumble == 1 | .data$fumble_lost == 1) %>%
    dplyr::filter(
      .data$defteam == .data$fumbled_1_team |
        .data$defteam == .data$fumbled_2_team
    ) %>%
    dplyr::mutate(
      fumbled_1_player_id =
        dplyr::if_else(.data$defteam == .data$fumbled_1_team, .data$fumbled_1_player_id, NA_character_, NA_character_)
    ) %>%
    dplyr::select(
      "season", "week",
      tidyselect::matches("^fumble.+team"),
      tidyselect::matches("^fumble.+player_id")
    ) %>%
    tidyr::pivot_longer(
      cols = tidyselect::contains("fumble"),
      names_pattern = "(.+)_(team|player_id)",
      names_to = c("desc",".value")
    ) %>%
    dplyr::mutate(
      n = 1,
      desc = stringr::str_remove_all(.data$desc, "_[0-9]")
    ) %>%
    tidyr::pivot_wider(
      names_from = .data$desc,
      values_from = .data$n,
      values_fn = sum,
      values_fill = 0L
    ) %>%
    # Renaming fails if the columns don't exist. So we row bind a dummy tibble
    # including the relevant columns. The row will be filtered after renaming
    dplyr::bind_rows(
      tibble::tibble(
        player_id = NA_character_, fumbled = numeric(), fumble_recovery = numeric()
      )
    ) %>%
    dplyr::rename(
      "fumble" = "fumbled",
      "fumble_recovery_own" = "fumble_recovery"
    ) %>%
    dplyr::filter(!is.na(.data$player_id)) %>%
    dplyr::group_by(.data$season, .data$week, .data$team, .data$player_id) %>%
    dplyr::summarise(
      fumble = sum(.data$fumble, na.rm = TRUE),
      fumble_recovery_own = sum(.data$fumble_recovery_own, na.rm = TRUE)
    ) %>%
    dplyr::ungroup()

  # get fumble stats for opponent recoveries
  fumble_df_opp <- data %>%
    dplyr::filter(.data$fumble == 1 | .data$fumble_lost == 1) %>%
    dplyr::filter(
      .data$defteam == .data$fumble_recovery_1_team |
        .data$defteam == .data$fumble_recovery_2_team
    ) %>%
    dplyr::mutate(
      # use data.table fifelse because base ifelse changed data type to logical
      # if there are 0 rows
      fumble_recovery_1_player_id =
        data.table::fifelse(.data$defteam != .data$fumbled_1_team, .data$fumble_recovery_1_player_id, NA_character_),
      fumble_recovery_2_player_id =
        data.table::fifelse(.data$defteam != .data$fumbled_2_team, .data$fumble_recovery_2_player_id, NA_character_)
    ) %>%
    dplyr::select(
      "season", "week",
      tidyselect::matches("^fumble_recovery.+team"),
      tidyselect::matches("^fumble_recovery.+player_id")
    ) %>%
    tidyr::pivot_longer(
      cols = tidyselect::contains("fumble"),
      names_pattern = "(.+)_(team|player_id)",
      names_to = c("desc",".value")
    ) %>%
    dplyr::mutate(
      n = 1,
      desc = stringr::str_remove_all(.data$desc, "_[0-9]")
    ) %>%
    tidyr::pivot_wider(
      names_from = .data$desc,
      values_from = .data$n,
      values_fn = sum,
      values_fill = 0L
    ) %>%
    dplyr::filter(!is.na(.data$player_id)) %>%
    add_column_if_missing("fumble_recovery") %>%
    dplyr::rename("fumble_recovery_opp" = "fumble_recovery") %>%
    dplyr::group_by(.data$season, .data$week, .data$team, .data$player_id) %>%
    dplyr::summarise(
      fumble_recovery_opp = sum(.data$fumble_recovery_opp,na.rm = TRUE)
    ) %>%
    dplyr::ungroup()

  # get fumble yards for own recoveries
  fumble_yds_own_data <- data %>%
    dplyr::filter(.data$fumble == 1 | .data$fumble_lost == 1) %>%
    dplyr::filter(
      .data$defteam == .data$fumbled_1_team |
        .data$defteam == .data$fumbled_2_team
    )

  fumble_yds_own_df <- fumble_yds_own_data %>%
    dplyr::group_by(
      .data$season, .data$week, "team" = .data$fumble_recovery_1_team,
      "player_id" = .data$fumble_recovery_1_player_id
    ) %>%
    dplyr::summarise(recovery_yards = sum(.data$fumble_recovery_1_yards)) %>%
    dplyr::filter(!is.na(.data$player_id)) %>% ### this happens when a fumble goes out of bounds. Noone gets yards --> NA/NA
    dplyr::bind_rows(
      fumble_yds_own_data %>%
        dplyr::group_by(
          .data$season, .data$week, "team" = .data$fumble_recovery_2_team,
          "player_id" = .data$fumble_recovery_2_player_id
        ) %>%
        dplyr::summarise(recovery_yards = sum(.data$fumble_recovery_2_yards)) %>%
        dplyr::filter(!is.na(.data$player_id))
    ) %>%
    dplyr::group_by(.data$season, .data$week, .data$team, .data$player_id) %>%
    dplyr::summarise(fumble_recovery_yards_own = sum(.data$recovery_yards)) %>%
    dplyr::ungroup()

  # get fumble yards for opp recoveries
  fumble_yds_opp_data <- data %>%
    dplyr::filter(.data$fumble == 1 | .data$fumble_lost == 1) %>%
    dplyr::filter(
      .data$defteam == .data$fumble_recovery_1_team,
      .data$defteam != .data$fumbled_1_team
    )

  fumble_yds_opp_df <- fumble_yds_opp_data %>%
    dplyr::group_by(
      .data$season, .data$week, "team" = .data$fumble_recovery_1_team,
      "player_id" = .data$fumble_recovery_1_player_id
    ) %>%
    dplyr::summarise(recovery_yards = sum(.data$fumble_recovery_1_yards)) %>%
    dplyr::filter(!is.na(.data$player_id)) %>%
    dplyr::bind_rows(
      fumble_yds_opp_data %>%
        dplyr::group_by(
          .data$season, .data$week, "team" = .data$fumble_recovery_2_team,
          "player_id" = .data$fumble_recovery_2_player_id
        ) %>%
        dplyr::summarise(recovery_yards = sum(.data$fumble_recovery_2_yards)) %>%
        dplyr::filter(!is.na(.data$player_id))
    ) %>%
    dplyr::group_by(.data$season, .data$week, .data$team, .data$player_id) %>%
    dplyr::summarise(fumble_recovery_yards_opp = sum(.data$recovery_yards)) %>%
    dplyr::ungroup()

  # Penalty stats -----------------------------------------------------------

  # get penalty stats
  penalty_df <- penalty_data %>%
    dplyr::filter(
      !is.na(.data$penalty_player_id),
      .data$defteam == .data$penalty_team
    ) %>%
    dplyr::select(
      "season", "week", "penalty_yards", "penalty_team", "penalty_player_id"
    ) %>%
    tidyr::pivot_longer(
      cols = tidyselect::contains("penalty"),
      names_pattern = "(.+)_(team|player_id|yards)",
      names_to = c("desc",".value"),
      values_drop_na = TRUE
    ) %>%
    dplyr::mutate(n = 1) %>%
    tidyr::pivot_wider(
      names_from = .data$desc,
      values_from = c(.data$n, .data$yards),
      values_fn = sum,
      values_fill = 0L
    ) %>%
    add_column_if_missing("n_penalty", "yards_penalty") %>%
    dplyr::select(
      "season", "week", "team", "player_id",
      "penalty" = "n_penalty",
      "penalty_yards" = "yards_penalty"
    ) %>%
    dplyr::group_by(.data$season, .data$week, .data$team, .data$player_id) %>%
    dplyr::summarise(
      penalty = sum(.data$penalty,na.rm = TRUE),
      penalty_yards = sum(.data$penalty_yards,na.rm = TRUE)
    ) %>%
    dplyr::ungroup()

  # Touchdown stats -----------------------------------------------------------

  # get defensive touchdowns
  touchdown_df <- data %>%
    dplyr::filter(.data$touchdown == 1) %>%
    dplyr::filter(.data$defteam == .data$td_team) %>%
    dplyr::group_by(
      .data$season, .data$week, "team" = .data$td_team,
      "player_id" = .data$td_player_id
    ) %>%
    dplyr::summarise(td = sum(.data$touchdown)) %>%
    dplyr::ungroup()

  # Combine all stats -------------------------------------------------------

  # combine all the stats together

  player_df <- tackle_df %>%
    dplyr::full_join(tackle_yards_df, by = c("season","week","player_id", "team")) %>%
    dplyr::full_join(pressure_df, by = c("season","week","player_id", "team")) %>%
    dplyr::full_join(int_df, by = c("season","week","player_id", "team")) %>%
    dplyr::full_join(safety_df, by = c("season","week","player_id", "team")) %>%
    dplyr::full_join(fumble_df_own, by = c("season","week","player_id", "team")) %>%
    dplyr::full_join(fumble_df_opp, by = c("season","week","player_id", "team")) %>%
    dplyr::full_join(fumble_yds_own_df, by = c("season","week","player_id", "team")) %>%
    dplyr::full_join(fumble_yds_opp_df, by = c("season","week","player_id", "team")) %>%
    dplyr::full_join(penalty_df, by = c("season","week","player_id", "team")) %>%
    dplyr::full_join(touchdown_df, by = c("season","week","player_id", "team")) %>%
    dplyr::mutate_if(is.numeric, tidyr::replace_na, 0) %>%
    dplyr::left_join(
      nflreadr::load_players() %>%
        dplyr::select(
          "player_id" = "gsis_id",
          "player_display_name" = "display_name",
          "player_name" = "short_name",
          "position",
          "position_group",
          "headshot_url" = "headshot"
        ),
      by = "player_id"
    ) %>%
    dplyr::left_join(stype, by = c("season", "week")) %>%
    dplyr::select(tidyselect::any_of(c(

      # game information
      "season",
      "week",
      "season_type",

      # id information
      "player_id",
      "player_name",
      "player_display_name",
      "position",
      "position_group",
      "headshot_url",
      "team",

      # tackle stats
      "def_tackles" = "tackles",
      "def_tackles_solo" = "tackles_solo",
      "def_tackles_with_assist" = "tackles_with_assist",
      "def_tackle_assists" = "tackle_assists",
      "def_tackles_for_loss" = "tackles_for_loss",
      "def_tackles_for_loss_yards" = "tfl_yards",
      "def_fumbles_forced" = "forced_fumbles",

      # pressure stats
      "def_sacks"="sacks",
      "def_sack_yards"="sack_yards",
      "def_qb_hits"="qb_hit",

      # coverage stats
      "def_interceptions"="int",
      "def_interception_yards"="int_yards",
      "def_pass_defended"="pass_defended",

      # misc stats
      "def_tds"="td",
      "def_fumbles"="fumble",
      "def_fumble_recovery_own"="fumble_recovery_own",
      "def_fumble_recovery_yards_own"="fumble_recovery_yards_own",
      "def_fumble_recovery_opp"="fumble_recovery_opp",
      "def_fumble_recovery_yards_opp"="fumble_recovery_yards_opp",
      "def_safety"="safety",
      "def_penalty"="penalty",
      "def_penalty_yards"="penalty_yards"
    ))) %>%
    dplyr::filter(!is.na(.data$player_id)) %>%
    dplyr::arrange(.data$player_id, .data$season, .data$week)

  # if user doesn't want week-by-week input, aggregate the whole df
  if (isFALSE(weekly)) {
    player_df <- player_df %>%
      dplyr::group_by(.data$player_id, .data$team) %>%
      dplyr::summarise(
        player_name = custom_mode(.data$player_name),
        player_display_name = custom_mode(.data$player_display_name),
        games = dplyr::n(),
        position = custom_mode(.data$position),
        position_group = custom_mode(.data$position_group),
        headshot_url = custom_mode(.data$headshot_url),
        def_tackles = sum(.data$def_tackles),
        def_tackles_solo = sum(.data$def_tackles_solo),
        def_tackles_with_assist = sum(.data$def_tackles_with_assist),
        def_tackle_assists = sum(.data$def_tackle_assists),
        def_tackles_for_loss = sum(.data$def_tackles_for_loss),
        def_tackles_for_loss_yards = sum(.data$def_tackles_for_loss_yards),
        def_fumbles_forced = sum(.data$def_fumbles_forced),
        def_sacks = sum(.data$def_sacks),
        def_sack_yards = sum(.data$def_sack_yards),
        def_qb_hits = sum(.data$def_qb_hits),
        def_interceptions = sum(.data$def_interceptions),
        def_interception_yards = sum(.data$def_interception_yards),
        def_pass_defended = sum(.data$def_pass_defended),
        def_tds = sum(.data$def_tds),
        def_fumbles = sum(.data$def_fumbles),
        def_fumble_recovery_own = sum(.data$def_fumble_recovery_own),
        def_fumble_recovery_yards_own = sum(.data$def_fumble_recovery_yards_own),
        def_fumble_recovery_opp = sum(.data$def_fumble_recovery_opp),
        def_fumble_recovery_yards_opp = sum(.data$def_fumble_recovery_yards_opp),
        def_safety = sum(.data$def_safety),
        def_penalty = sum(.data$def_penalty),
        def_penalty_yards = sum(.data$def_penalty_yards)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(
        "player_id",
        "player_name",
        "player_display_name",
        "games",
        "position",
        "position_group",
        "headshot_url",
        "team",
        dplyr::everything()
      )
  }

  player_df
}

# This function checks if the variables in ... exists as column
# names in the argument .data. If not, it adds those columns and assigns
# them the value in the argument value
add_column_if_missing <- function(.data, ..., value = 0L){
  dots <- rlang::list2(...)
  new_cols <- dots[!dots %in% names(.data)]
  .data[,unlist(new_cols)] <- value
  .data
}

