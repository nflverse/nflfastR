################################################################################
# Author: Christian Lohr
# Styleguide: styler::tidyverse_style()
################################################################################

# dunno what that outcoded stuff below is about, just copied it from the off player stat code.. I'm a fucking noob in coding.

#' Get Official Game Stats
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
#' \item{completions}{The number of completed passes.}
#' \item{attempts}{The number of pass attempts as defined by the NFL.}
#' \item{passing_yards}{Yards gained on pass plays.}
#' \item{passing_tds}{The number of passing touchdowns.}
#' \item{interceptions}{The number of interceptions thrown.}
#' \item{sacks}{Number of times sacked.}
#' \item{sack_fumbles_lost}{The number of sacks with a lost fumble.}
#' \item{passing_air_yards}{Passing air yards (includes incomplete passes).}
#' \item{passing_yards_after_catch}{Yards after the catch gained on plays in
#' which player was the passer (this is an unofficial stat and may differ slightly
#' between different sources).}
#' \item{passing_first_downs}{First downs on pass attempts.}
#' \item{passing_epa}{Total expected points added on pass attempts and sacks.
#' NOTE: this uses the variable `qb_epa`, which gives QB credit for EPA for up
#' to the point where a receiver lost a fumble after a completed catch and makes
#' EPA work more like passing yards on plays with fumbles.}
#' \item{passing_2pt_conversions}{Two-point conversion passes.}
#' \item{dakota}{Adjusted EPA + CPOE composite based on coefficients which best predict adjusted EPA/play in the following year.}
#' \item{carries}{The number of official rush attempts (incl. scrambles and kneel downs).
#' Rushes after a lateral reception don't count as carry.}
#' \item{rushing_yards}{Yards gained when rushing with the ball (incl. scrambles and kneel downs).
#' Also includes yards gained after obtaining a lateral on a play that started
#' with a rushing attempt.}
#' \item{rushing_tds}{The number of rushing touchdowns (incl. scrambles).
#' Also includes touchdowns after obtaining a lateral on a play that started
#' with a rushing attempt.}
#' \item{rushing_fumbles_lost}{The number of rushes with a lost fumble.}
#' \item{rushing_first_downs}{First downs on rush attempts (incl. scrambles).}
#' \item{rushing_epa}{Expected points added on rush attempts (incl. scrambles and kneel downs).}
#' \item{rushing_2pt_conversions}{Two-point conversion rushes}
#' \item{receptions}{The number of pass receptions. Lateral receptions officially
#' don't count as reception.}
#' \item{targets}{The number of pass plays where the player was the targeted receiver.}
#' \item{receiving_yards}{Yards gained after a pass reception. Includes yards
#' gained after receiving a lateral on a play that started as a pass play.}
#' \item{receiving_tds}{The number of touchdowns following a pass reception.
#' Also includes touchdowns after receiving a lateral on a play that started
#' as a pass play.}
#' \item{receiving_air_yards}{Receiving air yards (incl. incomplete passes).}
#' \item{receiving_yards_after_catch}{Yards after the catch gained on plays in
#' which player was receiver (this is an unofficial stat and may differ slightly
#' between different sources).}
#' \item{receiving_fumbles_lost}{The number of fumbles after a pass reception.}
#' \item{receiving_2pt_conversions}{Two-point conversion receptions}
#' \item{fantasy_points}{Standard fantasy points.}
#' \item{fantasy_points_ppr}{PPR fantasy points.}
#' }
#' @export
#' @seealso The function [load_player_stats()] and the corresponding examples
#' on [the nflfastR website](https://www.nflfastr.com/articles/nflfastR.html#example-11-replicating-official-stats)
#' @examples
#' \donttest{
# pbp <- nflfastR::load_pbp(2020)
#'
# weekly <- calculate_player_stats(pbp, weekly = TRUE)
#' dplyr::glimpse(weekly)
#'
#' overall <- calculate_player_stats(pbp, weekly = FALSE)
#' dplyr::glimpse(overall)
#' }
#'
#'
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
# Interception Yards /////
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

# load plays with multiple laterals
con <- url("https://github.com/mrcaseb/nfl-data/blob/master/data/lateral_yards/multiple_lateral_yards.rds?raw=true")
mult_lats <- readRDS(con) %>%
  dplyr::mutate(
    season = substr(.data$game_id, 1, 4) %>% as.integer(),
    week = substr(.data$game_id, 6, 7) %>% as.integer()
  ) %>%
  dplyr::filter(.data$yards != 0) %>%
  # the list includes all plays with multiple laterals
  # and all receivers. Since the last one already is in the
  # pbp data, we have to drop him here so the entry isn't duplicated
  dplyr::group_by(.data$game_id, .data$play_id) %>%
  dplyr::slice(seq_len(dplyr::n() - 1)) %>%
  dplyr::ungroup()
close(con)

# filter down to the 2 dfs we need
suppressMessages({
  # 1. for "normal" plays: get plays that count in official stats
  data <- pbp %>%
    dplyr::filter(
      !is.na(.data$down),
      .data$play_type %in% c("pass", "qb_kneel", "qb_spike", "run")
    ) %>%
    nflfastR::decode_player_ids()

  # if (!"qb_epa" %in% names(data)) data <- add_qb_epa(data)

  # 2. for 2pt conversions only, get those plays
  two_points <- pbp %>%
    dplyr::filter(.data$two_point_conv_result == "success") %>%
    dplyr::select(
      "week", "season", "posteam",
      "pass_attempt", "rush_attempt",
      "passer_player_name", "passer_player_id",
      "rusher_player_name", "rusher_player_id",
      "lateral_rusher_player_name", "lateral_rusher_player_id",
      "receiver_player_name", "receiver_player_id",
      "lateral_receiver_player_name", "lateral_receiver_player_id"
    ) %>%
    nflfastR::decode_player_ids()
})

if (!"special" %in% names(pbp)) {# we need this column for the special teams tds
  pbp <- pbp %>%
    dplyr::mutate(
      special = dplyr::if_else(
        .data$play_type %in% c("extra_point","field_goal","kickoff","punt"),
        1, 0
      )
    )
}


# Tackling stats -----------------------------------------------------------

# get tackling stats
tackle_df <- data %>%
  dplyr::select(
    tidyselect::contains("_tackle_"),
    tidyselect::starts_with("tackle_for_loss"),
    tidyselect::starts_with("tackle_with_assist_"),
    tidyselect::starts_with("forced_fumble_player_")
  ) %>%
  tidyr::pivot_longer(cols = c(
    tidyselect::contains("_tackle_"),
    tidyselect::starts_with("tackle_for_loss"),
    tidyselect::starts_with("tackle_with_assist_"),
    tidyselect::starts_with("forced_fumble_player_")
  ),
  names_to = "desc",
  names_prefix = "tkl_",
  values_to = "tackle_player_id",
  values_drop_na = TRUE
  ) %>%
  dplyr::filter(grepl("_id",desc)) %>%
  dplyr::mutate(n = 1) %>%
  tidyr::pivot_wider(
    names_from = desc,
    values_from = n,
    values_fn = sum
  ) %>%
  dplyr::group_by(.data$tackle_player_id) %>%
  dplyr::mutate(
    tkl = sum(
      data.frame(
        .data$solo_tackle_1_player_id,
        .data$solo_tackle_2_player_id,
        .data$tackle_with_assist_1_player_id
        #what happens here, if e.g. tackle_with_assist_1_player_id is filled. If it's not it gives me an error, so I deleted it in the first place.
        #is there something equal to tidyselect::starts_with() within the data.frame? this could help I guess
      ), na.rm = TRUE
    ),
    tkl_solo = sum(
      data.frame(
        .data$solo_tackle_1_player_id,
        .data$solo_tackle_2_player_id
      ), na.rm = TRUE
    ),
    assist = sum(
      data.frame(
        .data$assist_tackle_1_player_id,
        .data$assist_tackle_2_player_id
      ), na.rm = TRUE
    ),
    forced_fml = sum(
      data.frame(
        .data$forced_fumble_player_1_player_id,
        .data$forced_fumble_player_2_player_id
      ), na.rm = TRUE
    )) %>%
  dplyr::select(
    player_id = tackle_player_id,
    tkl,
    tkl_solo,
    tkl_with_assist = tackle_with_assist_1_player_id,
    assist,
    forced_fml,
    tfl = tackle_for_loss_1_player_id
  ) %>%
  dplyr::ungroup()

tackle_df_nas <- is.na(tackle_df)
tackle_df[tackle_df_nas] <- 0

# get tackle for loss yards
tackle_yds_df <- data %>%
  dplyr::filter(tackled_for_loss == 1, fumble==0, sack==0) %>% ### fumbles will be in df 'fumble_yds_df' later on.
  dplyr::group_by(player_id=tackle_for_loss_1_player_id) %>%
  dplyr::summarise(tfl_yards = sum(yards_gained))%>%
  dplyr::filter(!is.na(player_id)) %>% ### there are many plays where the tfl_id is empty, but also the solo_tackle and other ids. what's going on here???
  dplyr::bind_rows(
    data %>%
      dplyr::filter(tackled_for_loss == 1, fumble==0) %>% ### fumbles will be in df 'fumble_yds_df' later on.
      dplyr::group_by(player_id=tackle_for_loss_2_player_id) %>%
      dplyr::summarise(tfl_yards = sum(yards_gained))%>%
    dplyr::filter(!is.na(player_id))
  ) %>%
  dplyr::group_by(player_id) %>%
  dplyr::summarise(tfl_yards = sum(tfl_yards)*-1)

tackle_yds_df_nas <- is.na(tackle_yds_df)
tackle_yds_df[tackle_yds_df_nas] <- 0

# Sack and Pressure stats -----------------------------------------------------------

# get sack and pressure stats
pressure_df <- data %>%
  dplyr::select(tidyselect::contains("sack_"),
                tidyselect::starts_with("qb_hit_")) %>%
  tidyr::pivot_longer(
    cols = c(
      tidyselect::contains("sack_"),
      tidyselect::starts_with("qb_hit_")
    ),
    names_to = "desc",
    names_prefix = "sk_",
    values_to = "pressure_player_id",
    values_drop_na = TRUE
  ) %>%
  dplyr::filter(grepl("_id", desc)) %>%
  dplyr::mutate(n =
                  dplyr::case_when(
                    desc %in% c("half_sack_1_player_id", "half_sack_2_player_id") ~ 0.5,
                    TRUE ~ 1
                  )) %>%
  tidyr::pivot_wider(
    names_from = desc,
    values_from = n,
    values_fn = sum
  ) %>%
  dplyr::group_by(.data$pressure_player_id) %>%
  dplyr::mutate(
    sk = sum(
      data.frame(
        .data$sack_player_id,
        .data$half_sack_1_player_id,
        .data$half_sack_2_player_id
      ), na.rm = TRUE
    ),
    qb_hit = sum(
      data.frame(
        .data$qb_hit_1_player_id,
        .data$qb_hit_2_player_id
      ), na.rm = TRUE
    )
  ) %>%
  dplyr::select(
    -tidyselect::contains("player_id"),
    player_id = pressure_player_id
  ) %>%
  dplyr::ungroup()

pressure_df_nas <- is.na(pressure_df)
pressure_df[pressure_df_nas] <- 0

# get sack yards
sack_yds_df <- data %>%
  dplyr::filter(tackled_for_loss == 0, fumble==0, sack==1) %>% ### fumbles will be in df 'fumble_yds_df' later on.
  dplyr::group_by(player_id=sack_player_id) %>%
  dplyr::summarise(sk_yards = sum(yards_gained)) %>%
  dplyr::filter(!is.na(player_id)) %>% ### team sacks are NA in each players stat because they gangbang each other to death
  dplyr::bind_rows(
    data %>%
      dplyr::filter(tackled_for_loss == 0, fumble==0, sack==1) %>% ### fumbles will be in df 'fumble_yds_df' later on.
      dplyr::group_by(player_id=half_sack_1_player_id) %>%
      dplyr::summarise(sk_yards = sum(yards_gained)) %>%
      dplyr::filter(!is.na(player_id))
    ) %>%
      dplyr::bind_rows(
        data %>%
          dplyr::filter(tackled_for_loss == 0, fumble==0, sack==1) %>% ### fumbles will be in df 'fumble_yds_df' later on.
          dplyr::group_by(player_id=half_sack_2_player_id) %>%
          dplyr::summarise(sk_yards = sum(yards_gained)) %>%
          dplyr::filter(!is.na(player_id))
  ) %>%
  dplyr::group_by(player_id) %>%
  dplyr::summarise(sack_yards = sum(sk_yards)*-1)

sack_yds_df_nas <- is.na(sack_yds_df)
sack_yds_df[sack_yds_df_nas] <- 0

# Interception and Deflection stats ---------------------------------------------------------

# get int and def stats
int_df <- data %>%
  dplyr::select(tidyselect::starts_with("interception_"),
                tidyselect::starts_with("pass_defense_")) %>%
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
  dplyr::filter(grepl("_id", desc)) %>%
  dplyr::mutate(n = 1) %>%
  tidyr::pivot_wider(names_from = desc,
                     values_from = n,
                     values_fn = sum) %>%
  dplyr::group_by(.data$db_player_id) %>%
  dplyr::mutate(
    PD = sum(
      data.frame(
        .data$pass_defense_1_player_id,
        .data$pass_defense_2_player_id
      ), na.rm = TRUE
    )
  ) %>%
  dplyr::select(
    player_id = db_player_id,
    INT = interception_player_id,
    PD
  ) %>%
  dplyr::ungroup()

int_df_nas <- is.na(int_df)
int_df[int_df_nas] <- 0

# Fumble stats -----------------------------------------------------------

# get fumble stats for fumbles and own fumble recoveries
fumble_df_own <- data %>%
  dplyr::filter(fumble == 1 | fumble_lost == 1) %>%
  dplyr::filter(defteam==fumbled_1_team | defteam==fumbled_2_team) %>%
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

# Safety stats -----------------------------------------------------------

# get safety stats
safety_df <- data %>%
  dplyr::filter(safety==1) %>% ######## delete!  --> why did I write that u should delete this...?
  dplyr::select(
    tidyselect::contains("safety_player_")
  ) %>%
  tidyr::pivot_longer(cols = c(
    tidyselect::contains("safety_player_")
  ),
  names_to = "desc",
  names_prefix = "safety_",
  values_to = "safety_player_id",
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
    player_id = safety_player_id,
    safety = .data$player_id
  ) %>%
  dplyr::ungroup()

safety_df_nas <- is.na(safety_df)
safety_df[safety_df_nas] <- 0

# Penalty stats -----------------------------------------------------------

# get penalty stats
penalty_df <- data %>%
  ### the following lines are just to separate defense from offense --> is there a more effitient way???
  dplyr::filter(!(is.na(penalty_player_id))) %>%
  dplyr::filter(defteam==penalty_team) %>%
  ###
  dplyr::select(
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
  dplyr::full_join(tackle_yds_df, by = c("player_id")) %>%
  dplyr::full_join(pressure_df, by = c("player_id")) %>%
  dplyr::full_join(int_df, by = c("player_id")) %>%
  dplyr::full_join(sack_yds_df, by = c("player_id")) %>%
  dplyr::full_join(fumble_df_own, by = c("player_id")) %>%
  dplyr::full_join(fumble_df_opp, by = c("player_id")) %>%
  dplyr::full_join(fumble_yds_own_df, by = c("player_id")) %>%
  dplyr::full_join(fumble_yds_opp_df, by = c("player_id")) %>%
  dplyr::full_join(safety_df, by = c("player_id")) %>%
  dplyr::full_join(penalty_df, by = c("player_id")) %>%
  dplyr::full_join(touchdown_df, by = c("player_id")) %>%
  dplyr::left_join(nflfastR::fast_scraper_roster(2020) %>% dplyr::select(player_id=gsis_id,player_name=full_name,recent_team=team,position)
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
    "td", "fm", "fm_recovery_own", "fm_recovery_yards_own", "fm_recovery_opp", "fm_recovery_yards_opp", "safety", "penalty"

))) %>%
  dplyr::filter(!is.na(.data$player_id),!is.na(.data$player_name)) ### player_name only because of Terell Bonds

player_df_nas <- is.na(player_df)
player_df[player_df_nas] <- 0

player_df <- player_df %>%
  dplyr::mutate(
    fantasy_points_123 =
      1 * (assist + tkl_with_assist + qb_hit) +
      2 * (tkl_solo + tfl) +
      3 * (PD + forced_fml + fm_recovery_own + fm_recovery_opp + safety) +
      6 * (sk + INT + td),

    fantasy_points_mppr =
      -4 * (fm) +
      -0.5 * (sk) +
      -0.2 * (penalty) + #usually penalty yards, need to add that
      0.15 * (fm_recovery_yards_opp + fm_recovery_yards_own) +
      0.2 * (sack_yards) +
      1 * (qb_hit) +
      2 * (tfl + safety) +
      4 * (fm_recovery_own) +
      5 * (fm_recovery_opp + td) +
      6 * (forced_fml + INT) +
      dplyr::case_when(position %in% c("DT","DE","LB","ILB","OLB") ~ 3 * PD,
                                       position %in% c("CB","S","FS","SS") ~ 4 * PD,
                                                       TRUE ~ 0) +
      dplyr::case_when(position=="DT" ~ 2.5 * tkl_solo + 1.5 * tkl_with_assist + 1.5 * assist,
                       position %in% c("DE","OLB") ~ 2 * tkl_solo + 1 * tkl_with_assist + 1 * assist,
                       position %in% c("LB","ILB") ~ 1 * tkl_solo + 0.5 * tkl_with_assist + 0.5 * assist,
                       position=="CB" ~ 1 * tkl_solo + 1 * tkl_with_assist + 1 * assist,
                       position %in% c("S","FS","SS") ~ 1 * tkl_solo + 0.5 * tkl_with_assist + 0.5 * assist,
                       TRUE ~ 0)
  ) %>%
  dplyr::arrange(.data$player_id)

return(player_df)
}
