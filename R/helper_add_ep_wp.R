################################################################################
# Author: Sebastian Carl
# Purpose: Functions to add ep(a) and wp(a) variables
# Code Style Guide: styler::tidyverse_style()
################################################################################

# For now those functions are only a call to nflscrapR but we may want to
# use other models and epa wpa definitions. This is the place where this
# could happen

add_ep <- function(pbp) {
  out <-
    pbp %>%
    nflscrapR::add_ep_variables()
  message("added ep variables")
  return(out)
}

add_air_yac_ep <- function(pbp) {
  if (nrow(pbp %>% dplyr::filter(!is.na(air_yards))) == 0) {
    out <- pbp %>%
      dplyr::mutate(
        air_epa = NA_real_,
        yac_epa = NA_real_,
        comp_air_epa = NA_real_,
        comp_yac_epa = NA_real_,
        home_team_comp_air_epa = NA_real_,
        away_team_comp_air_epa = NA_real_,
        home_team_comp_yac_epa = NA_real_,
        away_team_comp_yac_epa = NA_real_,
        total_home_comp_air_epa = NA_real_,
        total_away_comp_air_epa = NA_real_,
        total_home_comp_yac_epa = NA_real_,
        total_away_comp_yac_epa = NA_real_,
        home_team_raw_air_epa = NA_real_,
        away_team_raw_air_epa = NA_real_,
        home_team_raw_yac_epa = NA_real_,
        away_team_raw_yac_epa = NA_real_,
        total_home_raw_air_epa = NA_real_,
        total_away_raw_air_epa = NA_real_,
        total_home_raw_yac_epa = NA_real_,
        total_away_raw_yac_epa = NA_real_
      )
    message("No non-NA air_yards detected. air_yac_ep variables set to NA")
  } else {
    calc <-
      pbp %>%
      dplyr::filter(!is.na(air_yards)) %>%
      nflscrapR::add_air_yac_ep_variables() %>%
      dplyr::select(game_id, play_id, air_epa:total_away_raw_yac_epa)
    out <-
      pbp %>%
      dplyr::left_join(calc, by = c("game_id", "play_id"))
    message("added air_yac_ep variables")
  }
  return(out)
}

add_wp <- function(pbp) {
  out <-
    pbp %>%
    nflscrapR::add_wp_variables()
  message("added wp variables")
  return(out)
}

add_air_yac_wp <- function(pbp) {
  if (nrow(pbp %>% dplyr::filter(!is.na(air_yards))) == 0) {
    out <- pbp %>%
      dplyr::mutate(
        air_wpa = NA_real_,
        yac_wpa = NA_real_,
        comp_air_wpa = NA_real_,
        comp_yac_wpa = NA_real_,
        home_team_comp_air_wpa = NA_real_,
        away_team_comp_air_wpa = NA_real_,
        home_team_comp_yac_wpa = NA_real_,
        away_team_comp_yac_wpa = NA_real_,
        total_home_comp_air_wpa = NA_real_,
        total_away_comp_air_wpa = NA_real_,
        total_home_comp_yac_wpa = NA_real_,
        total_away_comp_yac_wpa = NA_real_,
        home_team_raw_air_wpa = NA_real_,
        away_team_raw_air_wpa = NA_real_,
        home_team_raw_yac_wpa = NA_real_,
        away_team_raw_yac_wpa = NA_real_,
        total_home_raw_air_wpa = NA_real_,
        total_away_raw_air_wpa = NA_real_,
        total_home_raw_yac_wpa = NA_real_,
        total_away_raw_yac_wpa = NA_real_
      )
    message("No non-NA air_yards detected. air_yac_wp variables set to NA")
  } else {
    calc <-
      pbp %>%
      dplyr::filter(!is.na(air_yards)) %>%
      nflscrapR::add_air_yac_wp_variables() %>%
      dplyr::select(game_id, play_id, air_wpa:total_away_raw_yac_wpa)
    out <-
      pbp %>%
      dplyr::left_join(calc, by = c("game_id", "play_id"))
    message("added air_yac_wp variables")
  }
  return(out)
}
