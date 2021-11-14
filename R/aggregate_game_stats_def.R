################################################################################
# Author: Christian Lohr, Sebastian Carl
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
#  #-----------------------------------------------------------------------------------------------------
#'
#'
calculate_player_stats_def <- function(pbp, weekly = FALSE) {

  # Prepare data ------------------------------------------------------------

  suppressMessages({
    # 1. for "normal" plays: get plays that count in official stats
    data <- pbp %>%
      dplyr::filter(
        !is.na(.data$down),
        .data$play_type %in% c("pass", "qb_kneel", "qb_spike", "run")
      ) %>%
      nflfastR::decode_player_ids()

    # # 2. for 2pt conversions only, get those plays
    # two_points <- pbp %>%
    #   dplyr::filter(.data$two_point_conv_result == "success") %>%
    #   dplyr::select(
    #     "week", "season", "posteam",
    #     "pass_attempt", "rush_attempt",
    #     "passer_player_name", "passer_player_id",
    #     "rusher_player_name", "rusher_player_id",
    #     "lateral_rusher_player_name", "lateral_rusher_player_id",
    #     "receiver_player_name", "receiver_player_id",
    #     "lateral_receiver_player_name", "lateral_receiver_player_id"
    #   ) %>%
    #   nflfastR::decode_player_ids()
    #
    # if (!"special" %in% names(pbp)) {
    #   # we need this column for the special teams tds
    #   pbp <- pbp %>%
    #     dplyr::mutate(
    #       special = dplyr::if_else(
    #         .data$play_type %in% c("extra_point","field_goal","kickoff","punt"),
    #         1, 0
    #       )
    #     ) %>%
    #     nflfastR::decode_player_ids()
    # }
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
      # tkl = sum_cols(., "solo_tackle_1_player_id", "solo_tackle_2_player_id", "tackle_with_assist_1_player_id"),
      # tkl_solo = sum_cols(., "solo_tackle_1_player_id", "solo_tackle_2_player_id"),
      # assist = sum_cols(., "assist_tackle_1_player_id", "assist_tackle_2_player_id"),
      # forced_fml = sum_cols(., "forced_fumble_player_1_player_id", "forced_fumble_player_2_player_id")
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
      "pass_defensed" = "n_pass_defense",
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
    dplyr::filter(defteam == fumbled_1_team | defteam==fumbled_2_team) %>%
    dplyr::mutate(fumbled_1_player_id = ifelse(defteam==fumbled_1_team,fumbled_1_player_id,NA)) %>%
    dplyr::select(
      tidyselect::starts_with("fumbled_"),
      tidyselect::starts_with("fumble_recovery_"),
      -(tidyselect::ends_with("_yards"))
    ) %>%
    tidyr::pivot_longer(cols = c(
      tidyselect::starts_with("fumbled_"),
      tidyselect::starts_with("fumble_recovery_"),
    ),
    names_to = "desc",
    names_prefix = "fm_",
    values_to = "fumble_player_id",
    values_drop_na = TRUE,
    values_ptypes = list(fumble_player_id=character())
    ) %>%
    dplyr::filter(grepl("_id",desc)) %>%
    dplyr::mutate(n = 1) %>%
    tidyr::pivot_wider(
      names_from = desc,
      values_from = n,
      values_fn = sum
    ) %>%
    dplyr::group_by(.data$fumble_player_id) %>%
    dplyr::mutate(
      fm = sum(
        data.frame(
          .data$fumbled_1_player_id,
          .data$fumbled_2_player_id
        ), na.rm = TRUE
      ),
      fm_recovery_own = sum(
        data.frame(
          .data$fumble_recovery_1_player_id,
          .data$fumble_recovery_2_player_id
        ), na.rm = TRUE
      )) %>%
    dplyr::select(
      player_id = .data$fumble_player_id,
      .data$fm,
      .data$fm_recovery_own) %>%
    dplyr::ungroup()

  fumble_df_own_nas <- is.na(fumble_df_own)
  fumble_df_own[fumble_df_own_nas] <- 0

  # get fumble stats for opponent recoveries
  fumble_df_opp <- data %>%
    dplyr::filter(fumble == 1 | fumble_lost == 1) %>%
    dplyr::filter(defteam==fumble_recovery_1_team | defteam==fumble_recovery_2_team) %>%
    dplyr::mutate(fumble_recovery_1_player_id = ifelse(defteam!=fumbled_1_team,fumble_recovery_1_player_id,NA),
                  fumble_recovery_2_player_id = ifelse(defteam!=fumbled_2_team,fumble_recovery_2_player_id,NA)
    ) %>%
    dplyr::select(
      tidyselect::starts_with("fumble_recovery_"),
      -(tidyselect::ends_with("_yards"))
    ) %>%
    tidyr::pivot_longer(cols = c(
      tidyselect::starts_with("fumble_recovery_"),
    ),
    names_to = "desc",
    names_prefix = "fm_",
    values_to = "fumble_player_id",
    values_drop_na = TRUE
    ) %>%
    dplyr::filter(grepl("_id",desc)) %>%
    dplyr::mutate(n = 1) %>%
    tidyr::pivot_wider(
      names_from = desc,
      values_from = n,
      values_fn = sum
    ) %>%
    dplyr::group_by(.data$fumble_player_id) %>%
    dplyr::mutate(
      fm_recovery_opp = sum(
        data.frame(
          .data$fumble_recovery_1_player_id,
          .data$fumble_recovery_2_player_id
        ), na.rm = TRUE
      )) %>%
    dplyr::select(
      player_id = .data$fumble_player_id,
      .data$fm_recovery_opp) %>%
    dplyr::ungroup()

  fumble_df_opp_nas <- is.na(fumble_df_opp)
  fumble_df_opp[fumble_df_opp_nas] <- 0

  # get fumble yards for own recoveries
  fumble_yds_own_df <- data %>%
    dplyr::filter(fumble == 1 | fumble_lost == 1) %>%
    dplyr::filter(defteam==fumbled_1_team | defteam==fumbled_2_team) %>%
    dplyr::group_by(player_id=fumble_recovery_1_player_id) %>%
    dplyr::summarise(recovery_yards = sum(fumble_recovery_1_yards)) %>%
    dplyr::filter(!is.na(player_id)) %>% ### this happens when a fumble goes out of bounds. Noone gets yards --> NA/NA
    dplyr::bind_rows(
      data %>%
        dplyr::filter(fumble == 1 | fumble_lost == 1) %>%
        dplyr::filter(defteam==fumbled_1_team | defteam==fumbled_2_team) %>%
        dplyr::group_by(player_id=fumble_recovery_2_player_id) %>%
        dplyr::summarise(recovery_yards = sum(fumble_recovery_2_yards)) %>%
        dplyr::filter(!is.na(player_id))
    ) %>%
    dplyr::group_by(player_id) %>%
    dplyr::summarise(fm_recovery_yards_own = sum(recovery_yards))

  fumble_yds_own_df_nas <- is.na(fumble_yds_own_df)
  fumble_yds_own_df[fumble_yds_own_df_nas] <- 0

  # get fumble yards for opp recoveries

  fumble_yds_opp_df <- data %>%
    dplyr::filter(fumble == 1 | fumble_lost == 1) %>%
    dplyr::filter(defteam==fumble_recovery_1_team,defteam!=fumbled_1_team) %>%
    dplyr::group_by(player_id=fumble_recovery_1_player_id) %>%
    dplyr::summarise(recovery_yards = sum(fumble_recovery_1_yards)) %>%
    dplyr::filter(!is.na(player_id)) %>%
    dplyr::bind_rows(
      data %>%
        dplyr::filter(fumble == 1 | fumble_lost == 1) %>%
        dplyr::filter(defteam==fumble_recovery_1_team,defteam!=fumbled_1_team) %>%
        dplyr::group_by(player_id=fumble_recovery_2_player_id) %>%
        dplyr::summarise(recovery_yards = sum(fumble_recovery_2_yards)) %>%
        dplyr::filter(!is.na(player_id))
    ) %>%
    dplyr::group_by(player_id) %>%
    dplyr::summarise(fm_recovery_yards_opp = sum(recovery_yards))

  fumble_yds_opp_df_nas <- is.na(fumble_yds_opp_df)
  fumble_yds_opp_df[fumble_yds_opp_df_nas] <- 0


  # Penalty stats -----------------------------------------------------------

  # get penalty stats
  penalty_df <- data %>%
    dplyr::filter(!is.na(penalty_player_id), defteam == penalty_team) %>%
    dplyr::select(
      "season","week","yards_gained","team"="defteam",
      tidyselect::contains("penalty_player_")
    ) %>%
    tidyr::pivot_longer(cols = c(
      tidyselect::contains("penalty_player_")
    ),
    names_to = "desc",
    names_prefix = "penalty_",
    values_to = "penalty_player_id",
    values_drop_na = TRUE
    ) %>%
    dplyr::filter(grepl("_id",desc)) %>%
    dplyr::mutate(n = 1) %>%
    tidyr::pivot_wider(
      names_from = desc,
      values_from = n,
      values_fn = sum
    ) %>%
    dplyr::select(
      player_id = penalty_player_id,
      penalty = .data$player_id
    ) %>%
    dplyr::ungroup()

  penalty_df_nas <- is.na(penalty_df)
  penalty_df[penalty_df_nas] <- 0

  # Touchdown stats -----------------------------------------------------------

  # get defensive touchdowns
  touchdown_df <- data %>%
    dplyr::filter(touchdown==1) %>%
    dplyr::filter(defteam==td_team) %>%
    dplyr::group_by(player_id=td_player_id) %>%
    dplyr::summarise(td = sum(touchdown)) %>%
    dplyr::ungroup()

  touchdown_df_nas <- is.na(touchdown_df)
  touchdown_df[touchdown_df_nas] <- 0


  # Combine all stats -------------------------------------------------------

  # combine all the stats together

  player_df <- tackle_df %>%
    dplyr::full_join(tackle_yards_df, by = c("season","week","player_id","team")) %>%
    dplyr::full_join(pressure_df, by = c("season","week","player_id","team")) %>%
    dplyr::full_join(int_df, by = c("season","week","player_id","team")) %>%
    dplyr::full_join(safety_df, by = c("season","week","player_id","team")) %>%
    dplyr::mutate_if(is.numeric,tidyr::replace_na,0)
    # dplyr::full_join(fumble_df_own, by = c("player_id")) %>%
    # dplyr::full_join(fumble_df_opp, by = c("player_id")) %>%
    # dplyr::full_join(fumble_yds_own_df, by = c("player_id")) %>%
    # dplyr::full_join(fumble_yds_opp_df, by = c("player_id")) %>%
    # dplyr::full_join(penalty_df, by = c("player_id")) %>%
    # dplyr::full_join(touchdown_df, by = c("player_id")) %>%
    dplyr::left_join(nflfastR::fast_scraper_roster(2020) %>%
                       dplyr::select(player_id=gsis_id,
                                     player_name=full_name,
                                     recent_team=team,
                                     position),
                     by = "player_id"
    ) %>%
    dplyr::select(tidyselect::any_of(c(

      # id information
      "player_id", "player_name", "recent_team", "position",

      # tackle stats
      "tkl", "tkl_solo", "tkl_with_assist", "assist", "tfl", "tfl_yards", "forced_fml",

      # pressure stats
      "sk", "sack_yards", "qb_hit",

      # coverage stats
      "INT", "PD",

      # misc stats
      "td", "fm", "fm_recovery_own", "fm_recovery_yards_own",
      "fm_recovery_opp", "fm_recovery_yards_opp", "safety", "penalty"

    ))) %>%
    dplyr::filter(!is.na(.data$player_id), !is.na(.data$player_name)) ### player_name only because of Terell Bonds

  player_df_nas <- is.na(player_df)
  player_df[player_df_nas] <- 0

  # player_df <- player_df %>%
  # dplyr::mutate(
  # fantasy_points_123 =
  #   1 * (assist + tkl_with_assist + qb_hit) +
  #   2 * (tkl_solo + tfl) +
  #   3 * (PD + forced_fml + fm_recovery_own + fm_recovery_opp + safety) +
  #   6 * (sk + INT + td)
  # fantasy_points_mppr =
  #   -4 * (fm) +
  #   -0.5 * (sk) +
  #   -0.2 * (penalty) + #usually penalty yards, need to add that
  #   0.15 * (fm_recovery_yards_opp + fm_recovery_yards_own) +
  #   0.2 * (sack_yards) +
  #   1 * (qb_hit) +
  #   2 * (tfl + safety) +
  #   4 * (fm_recovery_own) +
  #   5 * (fm_recovery_opp + td) +
  #   6 * (forced_fml + INT) +
  #   dplyr::case_when(position %in% c("DT","DE","LB","ILB","OLB") ~ 3 * PD,
  #                                    position %in% c("CB","S","FS","SS") ~ 4 * PD,
  #                                                    TRUE ~ 0) +
  #   dplyr::case_when(position=="DT" ~ 2.5 * tkl_solo + 1.5 * tkl_with_assist + 1.5 * assist,
  #                    position %in% c("DE","OLB") ~ 2 * tkl_solo + 1 * tkl_with_assist + 1 * assist,
  #                    position %in% c("LB","ILB") ~ 1 * tkl_solo + 0.5 * tkl_with_assist + 0.5 * assist,
  #                    position=="CB" ~ 1 * tkl_solo + 1 * tkl_with_assist + 1 * assist,
  #                    position %in% c("S","FS","SS") ~ 1 * tkl_solo + 0.5 * tkl_with_assist + 0.5 * assist,
  #                    TRUE ~ 0)
  # ) %>%
  # dplyr::arrange(.data$player_id)

  return(player_df)
}


sum_cols <- function(data, ...){
  purrr::reduce(dplyr::select(data, tidyselect::any_of(c(...))), `+`)
}
