################################################################################
# Author: Ben Baldwin
# Purpose: Build the final output of the pbp functions
# Code Style Guide: styler::tidyverse_style()
################################################################################

select_variables <- function(pbp) {

    suppressWarnings(
      out <-
        pbp %>%
        dplyr::select(
          tidyselect::one_of(
            c(nflscrapr_cols, new_cols, api_cols)
          )
        )
    )

  return(out)
}

# columns that are not in gamecenter that we created
new_cols <- c("season", "cp", "cpoe", "series", "series_success")

# original nflscrapr columns
nflscrapr_cols <-
  c(
    "play_id", "game_id", "home_team", "away_team",
    #added these to new gc scraper
    "season_type", "week",
    "posteam", "posteam_type", "defteam", "side_of_field", "yardline_100",
    "game_date", "quarter_seconds_remaining", "half_seconds_remaining",
    "game_seconds_remaining", "game_half", "quarter_end", "drive", "sp", "qtr", "down", "goal_to_go", "time", "yrdln", "ydstogo", "ydsnet",
    "desc", "play_type", "yards_gained", "shotgun", "no_huddle", "qb_dropback", "qb_kneel", "qb_spike",
    "qb_scramble", "pass_length", "pass_location", "air_yards", "yards_after_catch",
    "run_location", "run_gap", "field_goal_result", "kick_distance", "extra_point_result", "two_point_conv_result",
    "home_timeouts_remaining", "away_timeouts_remaining", "timeout", "timeout_team", "td_team",
    "posteam_timeouts_remaining", "defteam_timeouts_remaining",
    "total_home_score", "total_away_score", "posteam_score",
    "defteam_score", "score_differential", "posteam_score_post",
    "defteam_score_post", "score_differential_post",
    "no_score_prob", "opp_fg_prob", "opp_safety_prob", "opp_td_prob", "fg_prob",
    "safety_prob", "td_prob", "extra_point_prob",
    "two_point_conversion_prob", "ep", "epa",
    "total_home_epa", "total_away_epa",
    "total_home_rush_epa", "total_away_rush_epa",
    "total_home_pass_epa", "total_away_pass_epa", "air_epa", "yac_epa", "comp_air_epa",
    "comp_yac_epa", "total_home_comp_air_epa", "total_away_comp_air_epa",
    "total_home_comp_yac_epa", "total_away_comp_yac_epa",
    "total_home_raw_air_epa", "total_away_raw_air_epa", "total_home_raw_yac_epa",
    "total_away_raw_yac_epa",
    "wp", "def_wp", "home_wp", "away_wp", "wpa", "home_wp_post", "away_wp_post",
    "vegas_wp", "vegas_home_wp",
    "total_home_rush_wpa", "total_away_rush_wpa",
    "total_home_pass_wpa", "total_away_pass_wpa", "air_wpa", "yac_wpa", "comp_air_wpa", "comp_yac_wpa",
    "total_home_comp_air_wpa", "total_away_comp_air_wpa",
    "total_home_comp_yac_wpa", "total_away_comp_yac_wpa",
    "total_home_raw_air_wpa", "total_away_raw_air_wpa", "total_home_raw_yac_wpa",
    "total_away_raw_yac_wpa", "punt_blocked", "first_down_rush", "first_down_pass",
    "first_down_penalty", "third_down_converted", "third_down_failed",
    "fourth_down_converted", "fourth_down_failed", "incomplete_pass", "touchback",
    "interception", "punt_inside_twenty", "punt_in_endzone", "punt_out_of_bounds",
    "punt_downed", "punt_fair_catch", "kickoff_inside_twenty", "kickoff_in_endzone",
    "kickoff_out_of_bounds", "kickoff_downed", "kickoff_fair_catch", "fumble_forced", "fumble_not_forced",
    "fumble_out_of_bounds", "solo_tackle", "safety", "penalty", "tackled_for_loss",
    "fumble_lost", "own_kickoff_recovery", "own_kickoff_recovery_td", "qb_hit",
    "rush_attempt", "pass_attempt", "sack",
    "touchdown", "pass_touchdown", "rush_touchdown", "return_touchdown", "extra_point_attempt",
    "two_point_attempt", "field_goal_attempt", "kickoff_attempt", "punt_attempt", "fumble",
    "complete_pass", "assist_tackle", "lateral_reception", "lateral_rush", "lateral_return",
    "lateral_recovery", "passer_player_id", "passer_player_name", "receiver_player_id",
    "receiver_player_name", "rusher_player_id", "rusher_player_name", "lateral_receiver_player_id",
    "lateral_receiver_player_name", "lateral_rusher_player_id", "lateral_rusher_player_name",
    "lateral_sack_player_id", "lateral_sack_player_name", "interception_player_id",
    "interception_player_name", "lateral_interception_player_id", "lateral_interception_player_name",
    "punt_returner_player_id", "punt_returner_player_name", "lateral_punt_returner_player_id",
    "lateral_punt_returner_player_name", "kickoff_returner_player_name", "kickoff_returner_player_id",
    "lateral_kickoff_returner_player_id", "lateral_kickoff_returner_player_name", "punter_player_id",
    "punter_player_name", "kicker_player_name", "kicker_player_id", "own_kickoff_recovery_player_id",
    "own_kickoff_recovery_player_name", "blocked_player_id", "blocked_player_name",
    "tackle_for_loss_1_player_id", "tackle_for_loss_1_player_name", "tackle_for_loss_2_player_id",
    "tackle_for_loss_2_player_name", "qb_hit_1_player_id", "qb_hit_1_player_name",
    "qb_hit_2_player_id", "qb_hit_2_player_name", "forced_fumble_player_1_team",
    "forced_fumble_player_1_player_id", "forced_fumble_player_1_player_name",
    "forced_fumble_player_2_team", "forced_fumble_player_2_player_id",
    "forced_fumble_player_2_player_name", "solo_tackle_1_team", "solo_tackle_2_team",
    "solo_tackle_1_player_id", "solo_tackle_2_player_id", "solo_tackle_1_player_name",
    "solo_tackle_2_player_name", "assist_tackle_1_player_id", "assist_tackle_1_player_name",
    "assist_tackle_1_team", "assist_tackle_2_player_id", "assist_tackle_2_player_name",
    "assist_tackle_2_team", "assist_tackle_3_player_id", "assist_tackle_3_player_name",
    "assist_tackle_3_team", "assist_tackle_4_player_id", "assist_tackle_4_player_name",
    "assist_tackle_4_team", "pass_defense_1_player_id", "pass_defense_1_player_name",
    "pass_defense_2_player_id", "pass_defense_2_player_name", "fumbled_1_team",
    "fumbled_1_player_id", "fumbled_1_player_name", "fumbled_2_player_id",
    "fumbled_2_player_name", "fumbled_2_team", "fumble_recovery_1_team",
    "fumble_recovery_1_yards", "fumble_recovery_1_player_id", "fumble_recovery_1_player_name",
    "fumble_recovery_2_team", "fumble_recovery_2_yards", "fumble_recovery_2_player_id",
    "fumble_recovery_2_player_name", "return_team",
    "return_yards", "penalty_team", "penalty_player_id",
    "penalty_player_name", "penalty_yards", "replay_or_challenge",
    "replay_or_challenge_result", "penalty_type",
    "defensive_two_point_attempt",
    "defensive_two_point_conv",
    "defensive_extra_point_attempt",
    "defensive_extra_point_conv"
  )


# these are columns in the RS data that aren't in nflscrapR
rs_cols <- c(
  "season_type", "week", "game_key", "game_time_eastern", "game_time_local",
  "iso_time", "game_type", "site_id", "site_city", "site_fullname", "site_state",
  "roof_type", "drive_start_time", "drive_end_time", "drive_start_yardline",
  "drive_end_yardline", "drive_how_started", "drive_how_ended",
  "drive_play_count", "drive_yards_penalized",
  "drive_time_of_possession", "drive_inside20", "drive_first_downs",
  "drive_possession_team_abbr", "scoring_team_abbr", "scoring_type",
  "alert_play_type", "play_type_nfl", "time_of_day",
  "yards", "end_yardline_side", "end_yardline_number"
)


# these are columns in the new API that aren't in nflscrapR
api_cols <- c(
  "start_time",
  "stadium", "weather", "nfl_api_id",
  "play_clock", "play_deleted",
  "play_type_nfl",
  "end_clock_time", "end_yard_line",
  "drive_real_start_time",

  "drive_play_count", "drive_time_of_possession",
  "drive_first_downs", "drive_inside20", "drive_ended_with_score",
  "drive_quarter_start", "drive_quarter_end",
  "drive_yards_penalized",

  "drive_start_transition", "drive_end_transition",

  "drive_game_clock_start", "drive_game_clock_end",
  "drive_start_yard_line", "drive_end_yard_line",
  "drive_play_id_started", "drive_play_id_ended",
  "away_score", "home_score", "location", "result",
  "total", "spread_line", "total_line", "div_game", "roof",
  "surface", "temp", "wind", "home_coach", "away_coach",
  "stadium_id", "game_stadium"
)


