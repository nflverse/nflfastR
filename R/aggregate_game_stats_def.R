################################################################################
# Author: Christian Lohr, Sebastian Carl, Tan Ho
# Styleguide: styler::tidyverse_style()
################################################################################

#' Get Official Game Stats on Defense
#'
#' @param pbp A Data frame of NFL play-by-play data typically loaded with
#' [load_pbp()] or [build_nflfastR_pbp()]. If the data doesn't include the variable
#' `qb_epa`, the function `add_qb_epa()` will be called to add it.
#' @param weekly If `TRUE`, returns week-by-week stats, otherwise, stats
#' for the entire Data frame.
#' @description Build columns that aggregate official passing, rushing, and receiving stats
#' either at the game level or at the level of the entire data frame passed.
#' @return A data frame including the following columns (all ID columns are
#' decoded to the gsis ID format):
#' \describe{
#' \item{player_id}{ID of the player. Use this to join to other sources.}
#' \item{player_name}{Name of the player}
#' \item{games}{The number of games where the player recorded passing, rushing or receiving stats.}
#' \item{recent_team}{Most recent team player appears in `pbp` with.}
#' \item{season}{Season if `weekly` is `TRUE`}
#' \item{week}{Week if `weekly` is `TRUE`}
#' }
#' @export
#' @seealso The function [load_player_stats()] and the corresponding examples
#' on [the nflfastR website](https://www.nflfastr.com/articles/nflfastR.html#example-11-replicating-official-stats)
#' @examples
#' \donttest{
#'   pbp <- nflfastR::load_pbp(2020)
#'
#'   weekly <- calculate_player_stats_def(pbp, weekly = TRUE)
#'   dplyr::glimpse(weekly)
#'
#'   overall <- calculate_player_stats_def(pbp, weekly = FALSE)
#'   dplyr::glimpse(overall)
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
# Penalty Yards /////
# Fumbles --> done
# Own Fumble Recoveries --> done
# Own Fumble Recovery Yards --> done
# Own Fumble Recovery TDs ///// --> only "TD" for defense

calculate_player_stats_def <- function(pbp, weekly = FALSE) {

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

    # 2. filter penalty plays that are no_plays for penalty stats
    penalty_data <- pbp %>%
      dplyr::filter(
        !is.na(.data$down),
        .data$play_type == "no_play",
        .data$penalty == 1
      ) %>%
      nflfastR::decode_player_ids()

  })

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
    dplyr::mutate(
      tackles = .data$solo_tackle + .data$tackle_with_assist
    ) %>%
    dplyr::select(
      "season",
      "week",
      "team"="defteam",
      "player_id" = "tackle_player_id",
      "tackles",
      "tackles_solo" = "solo_tackle",
      "tackles_with_assist" = "tackle_with_assist",
      "tackle_assists" = "assist_tackle",
      "forced_fumbles" = "forced_fumble_player",
      "tackles_for_loss" = "tackle_for_loss"
    )

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
        desc %in% c("half_sack_1_player_id", "half_sack_2_player_id") ~ 0.5,
        TRUE ~ 1
      ),
      desc = stringr::str_remove_all(.data$desc,"_player_id") %>%
        stringr::str_remove_all("_[0-9]") %>%
        stringr::str_remove("half_")
    ) %>%
    tidyr::pivot_wider(
      names_from = .data$desc,
      values_from = c(.data$n,.data$yards_gained),
      values_fn = sum,
      values_fill = 0
    ) %>%
    dplyr::select(
      "season",
      "week",
      "team",
      "player_id",
      "sacks" = "n_sack",
      "qb_hit" = "n_qb_hit",
      "sack_yards" = "yards_gained_sack"
    )

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
    tidyr::pivot_wider(names_from = "desc",
                       values_from = c("n","return_yards"),
                       values_fn = sum,
                       values_fill = 0) %>%
    dplyr::select(
      "season",
      "week",
      "team",
      "player_id" = "db_player_id",
      "int" = "n_interception",
      "pass_defended" = "n_pass_defense",
      "int_yards" = "return_yards_interception"
    )

  # Safety stats -----------------------------------------------------------

  safety_df <- data %>%
    dplyr::filter(.data$safety==1,!is.na(.data$safety_player_id)) %>%
    dplyr::select("season","week","team"="defteam","player_id" = "safety_player_id") %>%
    dplyr::count(.data$season,.data$week,.data$team,.data$player_id, name = "safety")

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
        ifelse(.data$defteam == .data$fumbled_1_team, .data$fumbled_1_player_id, NA)
    ) %>%
    dplyr::select(
      "season", "week",
      tidyselect::starts_with("fumbled_") & tidyselect::ends_with("_id"),
      tidyselect::starts_with("fumble_recovery_") & tidyselect::ends_with("_id"),
      -(tidyselect::ends_with("_yards"))
    ) %>%
    tidyr::pivot_longer(
      cols = tidyselect::ends_with("_id"),
      names_to = "desc",
      names_prefix = "fm_",
      values_to = "fumble_player_id",
      values_drop_na = TRUE
    ) %>%
    dplyr::mutate(
      n = 1,
      desc = stringr::str_remove_all(.data$desc,"_player_id") %>%
        stringr::str_remove_all("_[0-9]")
    ) %>%
    tidyr::pivot_wider(
      names_from = desc,
      values_from = n,
      values_fn = sum,
      values_fill = 0
    ) %>%
    dplyr::rename(
      "fumble" = "fumbled",
      "fumble_recovery_own" = "fumble_recovery",
      "player_id" = "fumble_player_id"
    )

  # get fumble stats for opponent recoveries
  fumble_df_opp <- data %>%
    dplyr::filter(.data$fumble == 1 | .data$fumble_lost == 1) %>%
    dplyr::filter(
      .data$defteam == .data$fumble_recovery_1_team |
        .data$defteam == .data$fumble_recovery_2_team
    ) %>%
    dplyr::mutate(
      fumble_recovery_1_player_id =
        ifelse(.data$defteam != .data$fumbled_1_team, .data$fumble_recovery_1_player_id, NA),
      fumble_recovery_2_player_id =
        ifelse(.data$defteam != .data$fumbled_2_team, .data$fumble_recovery_2_player_id, NA)
    ) %>%
    dplyr::select(
      "season", "week",
      tidyselect::starts_with("fumble_recovery_") & tidyselect::ends_with("_id"),
      -(tidyselect::ends_with("_yards"))
    ) %>%
    tidyr::pivot_longer(
      cols = tidyselect::ends_with("_id"),
      names_to = "desc",
      names_prefix = "fm_",
      values_to = "fumble_player_id",
      values_drop_na = TRUE
    ) %>%
    dplyr::mutate(
      n = 1,
      desc = stringr::str_remove_all(.data$desc,"_player_id") %>%
        stringr::str_remove_all("_[0-9]")
    ) %>%
    tidyr::pivot_wider(
      names_from = desc,
      values_from = n,
      values_fn = sum,
      values_fill = 0
    ) %>%
    dplyr::rename(
      "player_id" = "fumble_player_id",
      "fumble_recovery_opp" = "fumble_recovery"
    )

  # get fumble yards for own recoveries
  fumble_yds_own_data <- data %>%
    dplyr::filter(.data$fumble == 1 | .data$fumble_lost == 1) %>%
    dplyr::filter(
      .data$defteam == .data$fumbled_1_team |
        .data$defteam == .data$fumbled_2_team
      )

  fumble_yds_own_df <- fumble_yds_own_data %>%
    dplyr::group_by(
      .data$season, .data$week,
      "player_id" = .data$fumble_recovery_1_player_id
    ) %>%
    dplyr::summarise(recovery_yards = sum(.data$fumble_recovery_1_yards)) %>%
    dplyr::filter(!is.na(.data$player_id)) %>% ### this happens when a fumble goes out of bounds. Noone gets yards --> NA/NA
    dplyr::bind_rows(
      fumble_yds_own_data %>%
        dplyr::group_by(
          .data$season, .data$week,
          "player_id" = .data$fumble_recovery_2_player_id
        ) %>%
        dplyr::summarise(recovery_yards = sum(.data$fumble_recovery_2_yards)) %>%
        dplyr::filter(!is.na(.data$player_id))
    ) %>%
    dplyr::group_by(.data$season, .data$week, .data$player_id) %>%
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
      .data$season, .data$week,
      "player_id" = .data$fumble_recovery_1_player_id
    ) %>%
    dplyr::summarise(recovery_yards = sum(.data$fumble_recovery_1_yards)) %>%
    dplyr::filter(!is.na(.data$player_id)) %>%
    dplyr::bind_rows(
      fumble_yds_opp_data %>%
        dplyr::group_by(
          .data$season, .data$week,
          "player_id" = .data$fumble_recovery_2_player_id
        ) %>%
        dplyr::summarise(recovery_yards = sum(.data$fumble_recovery_2_yards)) %>%
        dplyr::filter(!is.na(.data$player_id))
    ) %>%
    dplyr::group_by(.data$season, .data$week, .data$player_id) %>%
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
      "season", "week", "yards_gained",
      tidyselect::contains("penalty_player_") & tidyselect::ends_with("_id")
    ) %>%
    tidyr::pivot_longer(
      cols = c(
        tidyselect::contains("penalty_player_")
      ),
      names_to = "desc",
      names_prefix = "penalty_",
      values_to = "penalty_player_id",
      values_drop_na = TRUE
    ) %>%
    dplyr::mutate(n = 1) %>%
    tidyr::pivot_wider(
      names_from = desc,
      values_from = n,
      values_fn = sum,
      values_fill = 0
    ) %>%
    dplyr::select(
      "season", "week",
      "player_id" = "penalty_player_id",
      "penalty" = "player_id"
    )

  # Touchdown stats -----------------------------------------------------------

  # get defensive touchdowns
  touchdown_df <- data %>%
    dplyr::filter(.data$touchdown == 1) %>%
    dplyr::filter(.data$defteam == .data$td_team) %>%
    dplyr::group_by(
      .data$season, .data$week,
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
    dplyr::full_join(fumble_df_own, by = c("season","week","player_id")) %>%
    dplyr::full_join(fumble_df_opp, by = c("season","week","player_id")) %>%
    dplyr::full_join(fumble_yds_own_df, by = c("season","week","player_id")) %>%
    dplyr::full_join(fumble_yds_opp_df, by = c("season","week","player_id")) %>%
    dplyr::full_join(penalty_df, by = c("season","week","player_id")) %>%
    dplyr::full_join(touchdown_df, by = c("season","week","player_id")) %>%
    dplyr::mutate_if(is.numeric, tidyr::replace_na, 0) %>%
    dplyr::left_join(
      nflreadr::load_players() %>%
        dplyr::select(
          "player_name" = "display_name",
          "player_id" = "gsis_id",
          "position"
        ),
      by = "player_id"
    ) %>%
    dplyr::select(tidyselect::any_of(c(

      # game information
      "season", "week",

      # id information
      "player_id", "player_name", "team", "recent_team", "position",

      # tackle stats
      "tackles", "tackles_solo", "tackles_with_assist", "tackle_assists",
      "tackles_for_loss", "tackles_for_loss_yards" = "tfl_yards", "forced_fumbles",

      # pressure stats
      "sacks", "sack_yards", "qb_hit",

      # coverage stats
      "int", "pass_defended", "int_yards",

      # misc stats
      "td", "fumble", "fumble_recovery_own", "fumble_recovery_yards_own",
      "fumble_recovery_opp", "fumble_recovery_yards_opp", "safety", "penalty"

    ))) %>%
    dplyr::filter(!is.na(.data$player_id), !is.na(.data$player_name)) %>%  ### player_name only because of Terell Bonds
    dplyr::arrange(.data$player_id, .data$season, .data$week)

  # if user doesn't want week-by-week input, aggregate the whole df
  if (isFALSE(weekly)) {
    player_df <- player_df %>%
      dplyr::group_by(.data$player_id) %>%
      dplyr::summarise(
        player_name = custom_mode(.data$player_name),
        games = dplyr::n(),
        recent_team = dplyr::last(.data$team),
        position = custom_mode(.data$position),
        tackles = sum(.data$tackles),
        tackles_solo = sum(.data$tackles_solo),
        tackles_with_assist = sum(.data$tackles_with_assist),
        tackle_assists = sum(.data$tackle_assists),
        tackles_for_loss = sum(.data$tackles_for_loss),
        tackles_for_loss_yards = sum(.data$tackles_for_loss_yards),
        forced_fumbles = sum(.data$forced_fumbles),
        sacks = sum(.data$sacks),
        sack_yards = sum(.data$sack_yards),
        qb_hit = sum(.data$qb_hit),
        int = sum(.data$int),
        int_yards = sum(.data$int_yards),
        pass_defended = sum(.data$pass_defended),
        td = sum(.data$td),
        fumble = sum(.data$fumble),
        fumble_recovery_own = sum(.data$fumble_recovery_own),
        fumble_recovery_yards_own = sum(.data$fumble_recovery_yards_own),
        fumble_recovery_opp = sum(.data$fumble_recovery_opp),
        fumble_recovery_yards_opp = sum(.data$fumble_recovery_yards_opp),
        safety = sum(.data$safety),
        penalty = sum(.data$penalty)
      ) %>%
      dplyr::ungroup()
  }

  player_df
}
