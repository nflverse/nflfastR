################################################################################
# Author: Sebastian Carl
# Purpose: Create a single row with all play stats of a given play built in the
#          Scraper Functions
# Stlyeguide: styler::tidyverse_style()
################################################################################

# Build a single row for tidy data structure
#
# This is a sub-function for the get_pbp_rs and get_pbp_gc functions.
#
# @param play_Id (integer) Specifies the play_Id for which the stats
# @param stats A dataframe including multiple rows for each play_Id holding
# gsis stat ids and stats

sum_play_stats <- function(play_Id, stats) {
  play_stats <- stats %>% dplyr::filter(playId == play_Id)

  row <-
    tibble::as_tibble(
      matrix(NA,ncol = length(pbp_stat_columns) - 1),
      .name_repair = "minimal"
    ) %>%
    purrr::prepend(as.integer(play_Id)) %>%
    purrr::set_names(pbp_stat_columns) %>%
    purrr::modify_at(indicator_stats, function(x) {
      x <- 0
    }) %>%
    purrr::modify_if(is.na, function(x) {
      x <- NA_character_
    }) %>%
    purrr::modify_at(c(
      "air_yards", "yards_after_catch", "penalty_yards", "kick_distance",
      "fumble_recovery_1_yards", "fumble_recovery_2_yards"
    ), as.integer)

  for (index in 1:nrow(play_stats)) {
    if (play_stats$statId[index] == 2) {
      row$punt_blocked <- 1
      row$punt_attempt <- 1
    } else if (play_stats$statId[index] == 3) {
      row$first_down_rush <- 1
    } else if (play_stats$statId[index] == 4) {
      row$first_down_pass <- 1
    } else if (play_stats$statId[index] == 5) {
      row$first_down_penalty <- 1
    } else if (play_stats$statId[index] == 6) {
      row$third_down_converted <- 1
    } else if (play_stats$statId[index] == 7) {
      row$third_down_failed <- 1
    } else if (play_stats$statId[index] == 8) {
      row$fourth_down_converted <- 1
    } else if (play_stats$statId[index] == 9) {
      row$fourth_down_failed <- 1
    } else if (play_stats$statId[index] == 10) {
      row$rush_attempt <- 1
      row$rusher_player_id <- play_stats$player.esbId[index]
      row$rusher_player_name <- play_stats$player.displayName[index]
      row$yards_gained <- play_stats$yards[index]
      row$penalty_fix <- 1
    } else if (play_stats$statId[index] == 11) {
      row$rush_attempt <- 1
      row$touchdown <- 1
      row$rush_touchdown <- 1
      row$rusher_player_id <- play_stats$player.esbId[index]
      row$rusher_player_name <- play_stats$player.displayName[index]
      row$yards_gained <- play_stats$yards[index]
      row$td_team <- play_stats$teamAbbr[index]
      row$penalty_fix <- 1
    } else if (play_stats$statId[index] == 12) {
      row$rush_attempt <- 1
      row$lateral_rush <- 1
      row$lateral_rusher_player_id <- play_stats$player.esbId[index]
      row$lateral_rusher_player_name <- play_stats$player.displayName[index]
      row$yards_gained <- play_stats$yards[index]
      row$penalty_fix <- 1
    } else if (play_stats$statId[index] == 13) {
      row$rush_attempt <- 1
      row$touchdown <- 1
      row$rush_touchdown <- 1
      row$lateral_rush <- 1
      row$lateral_rusher_player_id <- play_stats$player.esbId[index]
      row$lateral_rusher_player_name <- play_stats$player.displayName[index]
      row$td_team <- play_stats$teamAbbr[index]
      row$yards_gained <- play_stats$yards[index]
      row$penalty_fix <- 1
    } else if (play_stats$statId[index] == 14) {
      row$incomplete_pass <- 1
      row$pass_attempt <- 1
      row$passer_player_id <- play_stats$player.esbId[index]
      row$passer_player_name <- play_stats$player.displayName[index]
    } else if (play_stats$statId[index] == 15) {
      row$pass_attempt <- 1
      row$complete_pass <- 1
      row$passer_player_id <- play_stats$player.esbId[index]
      row$passer_player_name <- play_stats$player.displayName[index]
      row$yards_gained <- play_stats$yards[index]
      row$penalty_fix <- 1
    } else if (play_stats$statId[index] == 16) {
      row$pass_attempt <- 1
      row$touchdown <- 1
      row$pass_touchdown <- 1
      row$complete_pass <- 1
      row$passer_player_id <- play_stats$player.esbId[index]
      row$passer_player_name <- play_stats$player.displayName[index]
      row$yards_gained <- play_stats$yards[index]
      row$td_team <- play_stats$teamAbbr[index]
      row$penalty_fix <- 1
    } else if (play_stats$statId[index] == 19) {
      row$interception <- 1
      row$pass_attempt <- 1
      row$passer_player_id <- play_stats$player.esbId[index]
      row$passer_player_name <- play_stats$player.displayName[index]
    } else if (play_stats$statId[index] == 20) {
      row$pass_attempt <- 1
      row$sack <- 1
      row$passer_player_id <- play_stats$player.esbId[index]
      row$passer_player_name <- play_stats$player.displayName[index]
      row$yards_gained <- play_stats$yards[index]
      row$penalty_fix <- 1
    } else if (play_stats$statId[index] == 21) {
      row$pass_attempt <- 1
      row$complete_pass <- 1
      row$receiver_player_id <- play_stats$player.esbId[index]
      row$receiver_player_name <- play_stats$player.displayName[index]
      row$yards_gained <- play_stats$yards[index]
      row$penalty_fix <- 1
    } else if (play_stats$statId[index] == 22) {
      row$pass_attempt <- 1
      row$touchdown <- 1
      row$pass_touchdown <- 1
      row$complete_pass <- 1
      row$receiver_player_id <- play_stats$player.esbId[index]
      row$receiver_player_name <- play_stats$player.displayName[index]
      row$td_team <- play_stats$teamAbbr[index]
      row$yards_gained <- play_stats$yards[index]
      row$penalty_fix <- 1
    } else if (play_stats$statId[index] == 23) {
      row$pass_attempt <- 1
      row$complete_pass <- 1
      row$lateral_reception <- 1
      row$lateral_receiver_player_id <- play_stats$player.esbId[index]
      row$lateral_receiver_player_name <- play_stats$player.displayName[index]
      row$yards_gained <- play_stats$yards[index]
      row$penalty_fix <- 1
    } else if (play_stats$statId[index] == 24) {
      row$pass_attempt <- 1
      row$touchdown <- 1
      row$pass_touchdown <- 1
      row$complete_pass <- 1
      row$lateral_reception <- 1
      row$lateral_receiver_player_id <- play_stats$player.esbId[index]
      row$lateral_receiver_player_name <- play_stats$player.displayName[index]
      row$td_team <- play_stats$teamAbbr[index]
      row$yards_gained <- play_stats$yards[index]
      row$penalty_fix <- 1
    } else if (play_stats$statId[index] == 25) {
      row$pass_attempt <- 1
      row$interception_player_id <- play_stats$player.esbId[index]
      row$interception_player_name <- play_stats$player.displayName[index]
      row$return_team <- play_stats$teamAbbr[index]
      row$return_yards <- play_stats$yards[index]
      row$return_penalty_fix <- 1
    } else if (play_stats$statId[index] == 26) {
      row$pass_attempt <- 1
      row$touchdown <- 1
      row$return_touchdown <- 1
      row$interception_player_id <- play_stats$player.esbId[index]
      row$interception_player_name <- play_stats$player.displayName[index]
      row$td_team <- play_stats$teamAbbr[index]
      row$return_yards <- play_stats$yards[index]
      row$return_penalty_fix <- 1
    } else if (play_stats$statId[index] == 27) {
      row$pass_attempt <- 1
      row$lateral_return <- 1
      row$lateral_interception_player_id <- play_stats$player.esbId[index]
      row$lateral_interception_player_name <- play_stats$player.displayName[index]
      row$return_yards <- play_stats$yards[index]
      row$return_penalty_fix <- 1
    } else if (play_stats$statId[index] == 28) {
      row$pass_attempt <- 1
      row$touchdown <- 1
      row$return_touchdown <- 1
      row$lateral_return <- 1
      row$lateral_interception_player_id <- play_stats$player.esbId[index]
      row$lateral_interception_player_name <- play_stats$player.displayName[index]
      row$td_team <- play_stats$teamAbbr[index]
      row$return_yards <- play_stats$yards[index]
      row$return_penalty_fix <- 1
    } else if (play_stats$statId[index] == 29) {
      row$punt_attempt <- 1
      row$punter_player_id <- play_stats$player.esbId[index]
      row$punter_player_name <- play_stats$player.displayName[index]
      row$kick_distance <- play_stats$yards[index]
    } else if (play_stats$statId[index] == 30) {
      row$punt_inside_twenty <- 1
      row$punt_attempt <- 1
      row$punter_player_id <- play_stats$player.esbId[index]
      row$punter_player_name <- play_stats$player.displayName[index]
    } else if (play_stats$statId[index] == 31) {
      row$punt_in_endzone <- 1
      row$punt_attempt <- 1
      row$punter_player_id <- play_stats$player.esbId[index]
      row$punter_player_name <- play_stats$player.displayName[index]
    } else if (play_stats$statId[index] == 32) {
      row$punt_attempt <- 1
      row$punter_player_id <- play_stats$player.esbId[index]
      row$punter_player_name <- play_stats$player.displayName[index]
    } else if (play_stats$statId[index] == 33) {
      row$punt_attempt <- 1
      row$punt_returner_player_id <- play_stats$player.esbId[index]
      row$punt_returner_player_name <- play_stats$player.displayName[index]
      row$return_yards <- play_stats$yards[index]
      row$return_team <- play_stats$teamAbbr[index]
      row$return_penalty_fix <- 1
    } else if (play_stats$statId[index] == 34) {
      row$touchdown <- 1
      row$return_touchdown <- 1
      row$punt_attempt <- 1
      row$punt_returner_player_id <- play_stats$player.esbId[index]
      row$punt_returner_player_name <- play_stats$player.displayName[index]
      row$td_team <- play_stats$teamAbbr[index]
      row$return_team <- play_stats$teamAbbr[index]
      row$return_yards <- play_stats$yards[index]
      row$return_penalty_fix <- 1
    } else if (play_stats$statId[index] == 35) {
      row$punt_attempt <- 1
      row$lateral_return <- 1
      row$lateral_punt_returner_player_id <- play_stats$player.esbId[index]
      row$lateral_punt_returner_player_name <- play_stats$player.displayName[index]
      row$return_yards <- play_stats$yards[index]
      row$return_penalty_fix <- 1
    } else if (play_stats$statId[index] == 36) {
      row$touchdown <- 1
      row$return_touchdown <- 1
      row$punt_attempt <- 1
      row$lateral_return <- 1
      row$lateral_punt_returner_player_id <- play_stats$player.esbId[index]
      row$lateral_punt_returner_player_name <- play_stats$player.displayName[index]
      row$td_team <- play_stats$teamAbbr[index]
      row$return_yards <- play_stats$yards[index]
      row$return_team <- play_stats$teamAbbr[index]
      row$return_penalty_fix <- 1
    } else if (play_stats$statId[index] == 37) {
      row$punt_out_of_bounds <- 1
      row$punt_attempt <- 1
      row$return_yards <- 0
      row$return_team <- play_stats$teamAbbr[index]
    } else if (play_stats$statId[index] == 38) {
      row$punt_downed <- 1
      row$punt_attempt <- 1
      row$return_team <- play_stats$teamAbbr[index]
    } else if (play_stats$statId[index] == 39) {
      row$punt_fair_catch <- 1
      row$punt_attempt <- 1
      row$punt_returner_player_id <- play_stats$player.esbId[index]
      row$punt_returner_player_name <- play_stats$player.displayName[index]
      row$return_team <- play_stats$teamAbbr[index]
    } else if (play_stats$statId[index] == 40) {
      row$punt_attempt <- 1
      row$return_team <- play_stats$teamAbbr[index]
    } else if (play_stats$statId[index] == 41) {
      row$kickoff_attempt <- 1
      row$kicker_player_id <- play_stats$player.esbId[index]
      row$kicker_player_name <- play_stats$player.displayName[index]
      row$kick_distance <- play_stats$yards[index]
    } else if (play_stats$statId[index] == 42) {
      row$kickoff_inside_twenty <- 1
      row$kickoff_attempt <- 1
      row$kicker_player_id <- play_stats$player.esbId[index]
      row$kicker_player_name <- play_stats$player.displayName[index]
    } else if (play_stats$statId[index] == 43) {
      row$kickoff_in_endzone <- 1
      row$kickoff_attempt <- 1
      row$kicker_player_id <- play_stats$player.esbId[index]
      row$kicker_player_name <- play_stats$player.displayName[index]
    } else if (play_stats$statId[index] == 44) {
      row$kickoff_attempt <- 1
      row$kicker_player_id <- play_stats$player.esbId[index]
      row$kicker_player_name <- play_stats$player.displayName[index]
    } else if (play_stats$statId[index] == 45) {
      row$kickoff_attempt <- 1
      row$kickoff_returner_player_id <- play_stats$player.esbId[index]
      row$kickoff_returner_player_name <- play_stats$player.displayName[index]
      row$return_yards <- play_stats$yards[index]
      row$return_team <- play_stats$teamAbbr[index]
      row$return_penalty_fix <- 1
    } else if (play_stats$statId[index] == 46) {
      row$touchdown <- 1
      row$return_touchdown <- 1
      row$kickoff_attempt <- 1
      row$kickoff_returner_player_id <- play_stats$player.esbId[index]
      row$kickoff_returner_player_name <- play_stats$player.displayName[index]
      row$td_team <- play_stats$teamAbbr[index]
      row$return_yards <- play_stats$yards[index]
      row$return_team <- play_stats$teamAbbr[index]
      row$return_penalty_fix <- 1
    } else if (play_stats$statId[index] == 47) {
      row$kickoff_attempt <- 1
      row$lateral_return <- 1
      row$lateral_kickoff_returner_player_id <- play_stats$player.esbId[index]
      row$lateral_kickoff_returner_player_name <- play_stats$player.displayName[index]
      row$return_yards <- play_stats$yards[index]
      row$return_team <- play_stats$teamAbbr[index]
      row$return_penalty_fix <- 1
    } else if (play_stats$statId[index] == 48) {
      row$touchdown <- 1
      row$return_touchdown <- 1
      row$kickoff_attempt <- 1
      row$lateral_return <- 1
      row$lateral_kickoff_returner_player_id <- play_stats$player.esbId[index]
      row$lateral_kickoff_returner_player_name <- play_stats$player.displayName[index]
      row$td_team <- play_stats$teamAbbr[index]
      row$return_yards <- play_stats$yards[index]
      row$return_team <- play_stats$teamAbbr[index]
      row$return_penalty_fix <- 1
    } else if (play_stats$statId[index] == 49) {
      row$kickoff_out_of_bounds <- 1
      row$kickoff_attempt <- 1
      row$return_team <- play_stats$teamAbbr[index]
    } else if (play_stats$statId[index] == 50) {
      row$kickoff_fair_catch <- 1
      row$kickoff_attempt <- 1
      row$kickoff_returner_player_id <- play_stats$player.esbId[index]
      row$kickoff_returner_player_name <- play_stats$player.displayName[index]
      row$return_team <- play_stats$teamAbbr[index]
    } else if (play_stats$statId[index] == 51) {
      row$kickoff_attempt <- 1
      row$return_team <- play_stats$teamAbbr[index]
    } else if (play_stats$statId[index] == 52) {
      row$fumble_forced <- 1
      row$fumble <- 1
      row$fumbled_1_player_id <-
        dplyr::if_else(
          is.na(row$fumbled_1_player_id),
          play_stats$player.esbId[index],
          row$fumbled_1_player_id
        )
      row$fumbled_1_player_name <-
        dplyr::if_else(
          is.na(row$fumbled_1_player_name),
          play_stats$player.displayName[index],
          row$fumbled_1_player_name
        )
      row$fumbled_1_team <-
        dplyr::if_else(
          is.na(row$fumbled_1_team),
          play_stats$teamAbbr[index],
          row$fumbled_1_team
        )
      row$fumbled_2_player_id <-
        dplyr::if_else(
          is.na(row$fumbled_2_player_id) &
            row$fumbled_1_player_id != play_stats$player.esbId[index],
          play_stats$player.esbId[index],
          row$fumbled_2_player_id
        )
      row$fumbled_2_player_name <-
        dplyr::if_else(
          is.na(row$fumbled_2_player_name) &
            row$fumbled_1_player_name != play_stats$player.displayName[index],
          play_stats$player.displayName[index],
          row$fumbled_2_player_name
        )
      row$fumbled_2_team <-
        dplyr::if_else(
          is.na(row$fumbled_2_team) &
            row$fumbled_1_player_name != play_stats$player.displayName[index],
            # row$fumbled_1_team != play_stats$teamAbbr[index], # can't use team here because multiple players of the same team are possible
          play_stats$teamAbbr[index],
          row$fumbled_2_team
        )
    } else if (play_stats$statId[index] == 53) {
      row$fumble_not_forced <- 1
      row$fumble <- 1
      row$fumbled_1_player_id <-
        dplyr::if_else(
          is.na(row$fumbled_1_player_id),
          play_stats$player.esbId[index],
          row$fumbled_1_player_id
        )
      row$fumbled_1_player_name <-
        dplyr::if_else(
          is.na(row$fumbled_1_player_name),
          play_stats$player.displayName[index],
          row$fumbled_1_player_name
        )
      row$fumbled_1_team <-
        dplyr::if_else(
          is.na(row$fumbled_1_team),
          play_stats$teamAbbr[index],
          row$fumbled_1_team
        )
      row$fumbled_2_player_id <-
        dplyr::if_else(
          is.na(row$fumbled_2_player_id) &
            row$fumbled_1_player_id != play_stats$player.esbId[index],
          play_stats$player.esbId[index],
          row$fumbled_2_player_id
        )
      row$fumbled_2_player_name <-
        dplyr::if_else(
          is.na(row$fumbled_2_player_name) &
            row$fumbled_1_player_name != play_stats$player.displayName[index],
          play_stats$player.displayName[index],
          row$fumbled_2_player_name
        )
      row$fumbled_2_team <-
        dplyr::if_else(
          is.na(row$fumbled_2_team) &
            row$fumbled_1_player_name != play_stats$player.displayName[index],
            # row$fumbled_1_team != play_stats$teamAbbr[index],
          play_stats$teamAbbr[index],
          row$fumbled_2_team
        )
    } else if (play_stats$statId[index] == 54) {
      row$fumble_out_of_bounds <- 1
      row$fumble <- 1
      row$fumbled_1_player_id <-
        dplyr::if_else(
          is.na(row$fumbled_1_player_id),
          play_stats$player.esbId[index],
          row$fumbled_1_player_id
        )
      row$fumbled_1_player_name <-
        dplyr::if_else(
          is.na(row$fumbled_1_player_name),
          play_stats$player.displayName[index],
          row$fumbled_1_player_name
        )
      row$fumbled_1_team <-
        dplyr::if_else(
          is.na(row$fumbled_1_team),
          play_stats$teamAbbr[index],
          row$fumbled_1_team
        )
      row$fumbled_2_player_id <-
        dplyr::if_else(
          is.na(row$fumbled_2_player_id) &
            row$fumbled_1_player_id != play_stats$player.esbId[index],
          play_stats$player.esbId[index],
          row$fumbled_2_player_id
        )
      row$fumbled_2_player_name <-
        dplyr::if_else(
          is.na(row$fumbled_2_player_name) &
            row$fumbled_1_player_name != play_stats$player.displayName[index],
          play_stats$player.displayName[index],
          row$fumbled_2_player_name
        )
      row$fumbled_2_team <-
        dplyr::if_else(
          is.na(row$fumbled_2_team) &
            row$fumbled_1_player_name != play_stats$player.displayName[index],
            # row$fumbled_1_team != play_stats$teamAbbr[index],
          play_stats$teamAbbr[index],
          row$fumbled_2_team
        )
    } else if (play_stats$statId[index] == 55) {
      row$fumble <- 1
      row$fumble_recovery_1_player_id <-
        dplyr::if_else(
          is.na(row$fumble_recovery_1_player_id),
          play_stats$player.esbId[index],
          row$fumble_recovery_1_player_id
        )
      row$fumble_recovery_1_player_name <-
        dplyr::if_else(
          is.na(row$fumble_recovery_1_player_name),
          play_stats$player.displayName[index],
          row$fumble_recovery_1_player_name
        )
      row$fumble_recovery_1_team <-
        dplyr::if_else(
          is.na(row$fumble_recovery_1_team),
          play_stats$teamAbbr[index],
          row$fumble_recovery_1_team
        )
      row$fumble_recovery_1_yards <-
        dplyr::if_else(
          is.na(row$fumble_recovery_1_yards),
          play_stats$yards[index],
          row$fumble_recovery_1_yards
        )
      row$fumble_recovery_2_player_id <-
        dplyr::if_else(
          is.na(row$fumble_recovery_2_player_id) &
            row$fumble_recovery_1_player_id != play_stats$player.esbId[index],
          play_stats$player.esbId[index],
          row$fumble_recovery_2_player_id
        )
      row$fumble_recovery_2_player_name <-
        dplyr::if_else(
          is.na(row$fumble_recovery_2_player_name) &
            row$fumble_recovery_1_player_name != play_stats$player.displayName[index],
          play_stats$player.displayName[index],
          row$fumble_recovery_2_player_name
        )
      row$fumble_recovery_2_team <-
        dplyr::if_else(
          is.na(row$fumble_recovery_2_team) &
            row$fumble_recovery_1_player_name != play_stats$player.displayName[index],
            # row$fumble_recovery_1_team != play_stats$teamAbbr[index],
          play_stats$teamAbbr[index],
          row$fumble_recovery_2_team
        )
      row$fumble_recovery_2_yards <-
        dplyr::if_else(
          is.na(row$fumble_recovery_2_yards) &
            row$fumble_recovery_1_player_name != play_stats$player.displayName[index],
            # row$fumble_recovery_1_yards != play_stats$yards[index],
          play_stats$yards[index],
          row$fumble_recovery_2_yards
        )
    } else if (play_stats$statId[index] == 56) {
      row$touchdown <- 1
      row$fumble <- 1
      row$td_team <- play_stats$teamAbbr[index]
      row$fumble_recovery_1_player_id <-
        dplyr::if_else(
          is.na(row$fumble_recovery_1_player_id),
          play_stats$player.esbId[index],
          row$fumble_recovery_1_player_id
        )
      row$fumble_recovery_1_player_name <-
        dplyr::if_else(
          is.na(row$fumble_recovery_1_player_name),
          play_stats$player.displayName[index],
          row$fumble_recovery_1_player_name
        )
      row$fumble_recovery_1_team <-
        dplyr::if_else(
          is.na(row$fumble_recovery_1_team),
          play_stats$teamAbbr[index],
          row$fumble_recovery_1_team
        )
      row$fumble_recovery_1_yards <-
        dplyr::if_else(
          is.na(row$fumble_recovery_1_yards),
          play_stats$yards[index],
          row$fumble_recovery_1_yards
        )
      row$fumble_recovery_2_player_id <-
        dplyr::if_else(
          is.na(row$fumble_recovery_2_player_id) &
            row$fumble_recovery_1_player_id != play_stats$player.esbId[index],
          play_stats$player.esbId[index],
          row$fumble_recovery_2_player_id
        )
      row$fumble_recovery_2_player_name <-
        dplyr::if_else(
          is.na(row$fumble_recovery_2_player_name) &
            row$fumble_recovery_1_player_name != play_stats$player.displayName[index],
          play_stats$player.displayName[index],
          row$fumble_recovery_2_player_name
        )
      row$fumble_recovery_2_team <-
        dplyr::if_else(
          is.na(row$fumble_recovery_2_team) &
            row$fumble_recovery_1_player_name != play_stats$player.displayName[index],
            # row$fumble_recovery_1_team != play_stats$teamAbbr[index],
          play_stats$teamAbbr[index],
          row$fumble_recovery_2_team
        )
      row$fumble_recovery_2_yards <-
        dplyr::if_else(
          is.na(row$fumble_recovery_2_yards) &
            row$fumble_recovery_1_player_name != play_stats$player.displayName[index],
            # row$fumble_recovery_1_yards != play_stats$yards[index],
          play_stats$yards[index],
          row$fumble_recovery_2_yards
        )
    } else if (play_stats$statId[index] == 57) {
      row$fumble <- 1
      row$lateral_recovery <- 1
    } else if (play_stats$statId[index] == 58) {
      row$touchdown <- 1
      row$td_team <- play_stats$teamAbbr[index]
      row$fumble <- 1
      row$lateral_recovery <- 1
    } else if (play_stats$statId[index] == 59) {
      row$fumble <- 1
      row$fumble_recovery_1_player_id <-
        dplyr::if_else(
          is.na(row$fumble_recovery_1_player_id),
          play_stats$player.esbId[index],
          row$fumble_recovery_1_player_id
        )
      row$fumble_recovery_1_player_name <-
        dplyr::if_else(
          is.na(row$fumble_recovery_1_player_name),
          play_stats$player.displayName[index],
          row$fumble_recovery_1_player_name
        )
      row$fumble_recovery_1_team <-
        dplyr::if_else(
          is.na(row$fumble_recovery_1_team),
          play_stats$teamAbbr[index],
          row$fumble_recovery_1_team
        )
      row$fumble_recovery_1_yards <-
        dplyr::if_else(
          is.na(row$fumble_recovery_1_yards),
          play_stats$yards[index],
          row$fumble_recovery_1_yards
        )
      row$fumble_recovery_2_player_id <-
        dplyr::if_else(
          is.na(row$fumble_recovery_2_player_id) &
            row$fumble_recovery_1_player_id != play_stats$player.esbId[index],
          play_stats$player.esbId[index],
          row$fumble_recovery_2_player_id
        )
      row$fumble_recovery_2_player_name <-
        dplyr::if_else(
          is.na(row$fumble_recovery_2_player_name) &
            row$fumble_recovery_1_player_name != play_stats$player.displayName[index],
          play_stats$player.displayName[index],
          row$fumble_recovery_2_player_name
        )
      row$fumble_recovery_2_team <-
        dplyr::if_else(
          is.na(row$fumble_recovery_2_team) &
            row$fumble_recovery_1_player_name != play_stats$player.displayName[index],
            # row$fumble_recovery_1_team != play_stats$teamAbbr[index],
          play_stats$teamAbbr[index],
          row$fumble_recovery_2_team
        )
      row$fumble_recovery_2_yards <-
        dplyr::if_else(
          is.na(row$fumble_recovery_2_yards) &
            row$fumble_recovery_1_player_name != play_stats$player.displayName[index],
            # row$fumble_recovery_1_yards != play_stats$yards[index],
          play_stats$yards[index],
          row$fumble_recovery_2_yards
        )
    } else if (play_stats$statId[index] == 60) {
      row$touchdown <- 1
      row$td_team <- play_stats$teamAbbr[index]
      row$fumble <- 1
      row$fumble_recovery_1_player_id <-
        dplyr::if_else(
          is.na(row$fumble_recovery_1_player_id),
          play_stats$player.esbId[index],
          row$fumble_recovery_1_player_id
        )
      row$fumble_recovery_1_player_name <-
        dplyr::if_else(
          is.na(row$fumble_recovery_1_player_name),
          play_stats$player.displayName[index],
          row$fumble_recovery_1_player_name
        )
      row$fumble_recovery_1_team <-
        dplyr::if_else(
          is.na(row$fumble_recovery_1_team),
          play_stats$teamAbbr[index],
          row$fumble_recovery_1_team
        )
      row$fumble_recovery_1_yards <-
        dplyr::if_else(
          is.na(row$fumble_recovery_1_yards),
          play_stats$yards[index],
          row$fumble_recovery_1_yards
        )
      row$fumble_recovery_2_player_id <-
        dplyr::if_else(
          is.na(row$fumble_recovery_2_player_id) &
            row$fumble_recovery_1_player_id != play_stats$player.esbId[index],
          play_stats$player.esbId[index],
          row$fumble_recovery_2_player_id
        )
      row$fumble_recovery_2_player_name <-
        dplyr::if_else(
          is.na(row$fumble_recovery_2_player_name) &
            row$fumble_recovery_1_player_name != play_stats$player.displayName[index],
          play_stats$player.displayName[index],
          row$fumble_recovery_2_player_name
        )
      row$fumble_recovery_2_team <-
        dplyr::if_else(
          is.na(row$fumble_recovery_2_team) &
            row$fumble_recovery_1_player_name != play_stats$player.displayName[index],
            # row$fumble_recovery_1_team != play_stats$teamAbbr[index],
          play_stats$teamAbbr[index],
          row$fumble_recovery_2_team
        )
      row$fumble_recovery_2_yards <-
        dplyr::if_else(
          is.na(row$fumble_recovery_2_yards) &
            row$fumble_recovery_1_player_name != play_stats$player.displayName[index],
            # row$fumble_recovery_1_yards != play_stats$yards[index],
          play_stats$yards[index],
          row$fumble_recovery_2_yards
        )
    } else if (play_stats$statId[index] == 61) {
      row$fumble <- 1
      row$lateral_recovery <- 1
    } else if (play_stats$statId[index] == 62) {
      row$touchdown <- 1
      row$td_team <- play_stats$teamAbbr[index]
      row$fumble <- 1
      row$lateral_recovery <- 1
    } else if (play_stats$statId[index] == 63) {
      NULL
    } else if (play_stats$statId[index] == 64) {
      row$touchdown <- 1
      row$td_team <- play_stats$teamAbbr[index]
    } else if (play_stats$statId[index] == 68) {
      row$timeout <- 1
      row$timeout_team <- play_stats$teamAbbr[index]
    } else if (play_stats$statId[index] == 69) {
      row$field_goal_missed <- 1
      row$field_goal_attempt <- 1
      row$kicker_player_id <- play_stats$player.esbId[index]
      row$kicker_player_name <- play_stats$player.displayName[index]
      row$kick_distance <- play_stats$yards[index]
    } else if (play_stats$statId[index] == 70) {
      row$field_goal_made <- 1
      row$field_goal_attempt <- 1
      row$kicker_player_id <- play_stats$player.esbId[index]
      row$kicker_player_name <- play_stats$player.displayName[index]
      row$kick_distance <- play_stats$yards[index]
    } else if (play_stats$statId[index] == 71) {
      row$field_goal_blocked <- 1
      row$field_goal_attempt <- 1
      row$kicker_player_id <- play_stats$player.esbId[index]
      row$kicker_player_name <- play_stats$player.displayName[index]
      row$kick_distance <- play_stats$yards[index]
    } else if (play_stats$statId[index] == 72) {
      row$extra_point_good <- 1
      row$extra_point_attempt <- 1
      row$kicker_player_id <- play_stats$player.esbId[index]
      row$kicker_player_name <- play_stats$player.displayName[index]
    } else if (play_stats$statId[index] == 73) {
      row$extra_point_failed <- 1
      row$extra_point_attempt <- 1
      row$kicker_player_id <- play_stats$player.esbId[index]
      row$kicker_player_name <- play_stats$player.displayName[index]
    } else if (play_stats$statId[index] == 74) {
      row$extra_point_blocked <- 1
      row$extra_point_attempt <- 1
      row$kicker_player_id <- play_stats$player.esbId[index]
      row$kicker_player_name <- play_stats$player.displayName[index]
    } else if (play_stats$statId[index] == 75) {
      row$two_point_rush_good <- 1
      row$rush_attempt <- 1
      row$two_point_attempt <- 1
      row$rusher_player_id <- play_stats$player.esbId[index]
      row$rusher_player_name <- play_stats$player.displayName[index]
    } else if (play_stats$statId[index] == 76) {
      row$two_point_rush_failed <- 1
      row$rush_attempt <- 1
      row$two_point_attempt <- 1
      row$rusher_player_id <- play_stats$player.esbId[index]
      row$rusher_player_name <- play_stats$player.displayName[index]
    } else if (play_stats$statId[index] == 77) {
      row$two_point_pass_good <- 1
      row$pass_attempt <- 1
      row$two_point_attempt <- 1
      row$passer_player_id <- play_stats$player.esbId[index]
      row$passer_player_name <- play_stats$player.displayName[index]
    } else if (play_stats$statId[index] == 78) {
      row$two_point_pass_failed <- 1
      row$pass_attempt <- 1
      row$two_point_attempt <- 1
      row$passer_player_id <- play_stats$player.esbId[index]
      row$passer_player_name <- play_stats$player.displayName[index]
    } else if (play_stats$statId[index] == 79) {
      row$solo_tackle <- 1
      row$solo_tackle_1_player_id <-
        dplyr::if_else(
          is.na(row$solo_tackle_1_player_id),
          play_stats$player.esbId[index],
          row$solo_tackle_1_player_id
        )
      row$solo_tackle_1_player_name <-
        dplyr::if_else(
          is.na(row$solo_tackle_1_player_name),
          play_stats$player.displayName[index],
          row$solo_tackle_1_player_name
        )
      row$solo_tackle_1_team <-
        dplyr::if_else(
          is.na(row$solo_tackle_1_team),
          play_stats$teamAbbr[index],
          row$solo_tackle_1_team
        )
      row$solo_tackle_2_player_id <-
        dplyr::if_else(
          is.na(row$solo_tackle_2_player_id) &
            row$solo_tackle_1_player_id != play_stats$player.esbId[index],
          play_stats$player.esbId[index],
          row$solo_tackle_2_player_id
        )
      row$solo_tackle_2_player_name <-
        dplyr::if_else(
          is.na(row$solo_tackle_2_player_name) &
            row$solo_tackle_1_player_name != play_stats$player.displayName[index],
          play_stats$player.displayName[index],
          row$solo_tackle_2_player_name
        )
      row$solo_tackle_2_team <-
        dplyr::if_else(
          is.na(row$solo_tackle_2_team) &
            row$solo_tackle_1_player_name != play_stats$player.displayName[index],
            # row$solo_tackle_1_team != play_stats$teamAbbr[index],
          play_stats$teamAbbr[index],
          row$solo_tackle_2_team
        )
    } else if (play_stats$statId[index] == 80) {
      row$assist_tackle <- 1
      row$assist_tackle_1_player_id <-
        dplyr::if_else(
          is.na(row$assist_tackle_1_player_id),
          play_stats$player.esbId[index],
          row$assist_tackle_1_player_id
        )
      row$assist_tackle_1_player_name <-
        dplyr::if_else(
          is.na(row$assist_tackle_1_player_name),
          play_stats$player.displayName[index],
          row$assist_tackle_1_player_name
        )
      row$assist_tackle_1_team <-
        dplyr::if_else(
          is.na(row$assist_tackle_1_team),
          play_stats$teamAbbr[index],
          row$assist_tackle_1_team
        )
      row$assist_tackle_2_player_id <-
        dplyr::if_else(
          is.na(row$assist_tackle_2_player_id) &
            row$assist_tackle_1_player_id != play_stats$player.esbId[index],
          play_stats$player.esbId[index],
          row$assist_tackle_2_player_id
        )
      row$assist_tackle_2_player_name <-
        dplyr::if_else(
          is.na(row$assist_tackle_2_player_name) &
            row$assist_tackle_1_player_name != play_stats$player.displayName[index],
          play_stats$player.displayName[index],
          row$assist_tackle_2_player_name
        )
      row$assist_tackle_2_team <-
        dplyr::if_else(
          is.na(row$assist_tackle_2_team) &
            row$assist_tackle_1_player_name != play_stats$player.displayName[index],
            # row$assist_tackle_1_team != play_stats$teamAbbr[index],
          play_stats$teamAbbr[index],
          row$assist_tackle_2_team
        )
      row$assist_tackle_3_player_id <-
        dplyr::if_else(
          (is.na(row$assist_tackle_3_player_id) &
            row$assist_tackle_1_player_id != play_stats$player.esbId[index] &
             row$assist_tackle_2_player_id != play_stats$player.esbId[index]),
          play_stats$player.esbId[index],
          row$assist_tackle_3_player_id
        )
      row$assist_tackle_3_player_name <-
        dplyr::if_else(
          (is.na(row$assist_tackle_3_player_name) &
            row$assist_tackle_1_player_name != play_stats$player.displayName[index] &
             row$assist_tackle_2_player_name != play_stats$player.displayName[index]),
          play_stats$player.displayName[index],
          row$assist_tackle_3_player_name
        )
      row$assist_tackle_3_team <-
        dplyr::if_else(
          (is.na(row$assist_tackle_3_team) &
             row$assist_tackle_1_player_name != play_stats$player.displayName[index] &
             row$assist_tackle_2_player_name != play_stats$player.displayName[index]),
             # row$assist_tackle_1_team != play_stats$teamAbbr[index] &
             #  row$assist_tackle_2_team != play_stats$teamAbbr[index]),
          play_stats$teamAbbr[index],
          row$assist_tackle_3_team
        )
      row$assist_tackle_4_player_id <-
        dplyr::if_else(
          (is.na(row$assist_tackle_4_player_id) &
            row$assist_tackle_1_player_id != play_stats$player.esbId[index] &
             row$assist_tackle_2_player_id != play_stats$player.esbId[index] &
             row$assist_tackle_3_player_id != play_stats$player.esbId[index]),
          play_stats$player.esbId[index],
          row$assist_tackle_4_player_id
        )
      row$assist_tackle_4_player_name <-
        dplyr::if_else(
          (is.na(row$assist_tackle_4_player_name) &
            row$assist_tackle_1_player_name != play_stats$player.displayName[index] &
             row$assist_tackle_2_player_name != play_stats$player.displayName[index] &
             row$assist_tackle_3_player_name != play_stats$player.displayName[index]),
          play_stats$player.displayName[index],
          row$assist_tackle_4_player_name
        )
      row$assist_tackle_4_team <-
        dplyr::if_else(
          (is.na(row$assist_tackle_4_team) &
             row$assist_tackle_1_player_name != play_stats$player.displayName[index] &
             row$assist_tackle_2_player_name != play_stats$player.displayName[index] &
             row$assist_tackle_3_player_name != play_stats$player.displayName[index]),
            # row$assist_tackle_1_team != play_stats$teamAbbr[index] &
            #  row$assist_tackle_2_team != play_stats$teamAbbr[index] &
            #  row$assist_tackle_3_team != play_stats$teamAbbr[index]),
          play_stats$teamAbbr[index],
          row$assist_tackle_4_team
        )
    } else if (play_stats$statId[index] == 82) { # =81
      row$assist_tackle <- 1
      row$assist_tackle_1_player_id <-
        dplyr::if_else(
          is.na(row$assist_tackle_1_player_id),
          play_stats$player.esbId[index],
          row$assist_tackle_1_player_id
        )
      row$assist_tackle_1_player_name <-
        dplyr::if_else(
          is.na(row$assist_tackle_1_player_name),
          play_stats$player.displayName[index],
          row$assist_tackle_1_player_name
        )
      row$assist_tackle_1_team <-
        dplyr::if_else(
          is.na(row$assist_tackle_1_team),
          play_stats$teamAbbr[index],
          row$assist_tackle_1_team
        )
      row$assist_tackle_2_player_id <-
        dplyr::if_else(
          is.na(row$assist_tackle_2_player_id) &
            row$assist_tackle_1_player_id != play_stats$player.esbId[index],
          play_stats$player.esbId[index],
          row$assist_tackle_2_player_id
        )
      row$assist_tackle_2_player_name <-
        dplyr::if_else(
          is.na(row$assist_tackle_2_player_name) &
            row$assist_tackle_1_player_name != play_stats$player.displayName[index],
          play_stats$player.displayName[index],
          row$assist_tackle_2_player_name
        )
      row$assist_tackle_2_team <-
        dplyr::if_else(
          is.na(row$assist_tackle_2_team) &
            row$assist_tackle_1_player_name != play_stats$player.displayName[index],
            # row$assist_tackle_1_team != play_stats$teamAbbr[index],
          play_stats$teamAbbr[index],
          row$assist_tackle_2_team
        )
      row$assist_tackle_3_player_id <-
        dplyr::if_else(
          (is.na(row$assist_tackle_3_player_id) &
            row$assist_tackle_1_player_id != play_stats$player.esbId[index] &
             row$assist_tackle_2_player_id != play_stats$player.esbId[index]),
          play_stats$player.esbId[index],
          row$assist_tackle_3_player_id
        )
      row$assist_tackle_3_player_name <-
        dplyr::if_else(
          (is.na(row$assist_tackle_3_player_name) &
            row$assist_tackle_1_player_name != play_stats$player.displayName[index] &
             row$assist_tackle_2_player_name != play_stats$player.displayName[index]),
          play_stats$player.displayName[index],
          row$assist_tackle_3_player_name
        )
      row$assist_tackle_3_team <-
        dplyr::if_else(
          (is.na(row$assist_tackle_3_team) &
             row$assist_tackle_1_player_name != play_stats$player.displayName[index] &
             row$assist_tackle_2_player_name != play_stats$player.displayName[index]),
            # row$assist_tackle_1_team != play_stats$teamAbbr[index] &
            #  row$assist_tackle_2_team != play_stats$teamAbbr[index]),
          play_stats$teamAbbr[index],
          row$assist_tackle_3_team
        )
      row$assist_tackle_4_player_id <-
        dplyr::if_else(
          (is.na(row$assist_tackle_4_player_id) &
            row$assist_tackle_1_player_id != play_stats$player.esbId[index] &
             row$assist_tackle_2_player_id != play_stats$player.esbId[index] &
             row$assist_tackle_3_player_id != play_stats$player.esbId[index]),
          play_stats$player.esbId[index],
          row$assist_tackle_4_player_id
        )
      row$assist_tackle_4_player_name <-
        dplyr::if_else(
          (is.na(row$assist_tackle_4_player_name) &
            row$assist_tackle_1_player_name != play_stats$player.displayName[index] &
             row$assist_tackle_2_player_name != play_stats$player.displayName[index] &
             row$assist_tackle_3_player_name != play_stats$player.displayName[index]),
          play_stats$player.displayName[index],
          row$assist_tackle_4_player_name
        )
      row$assist_tackle_4_team <-
        dplyr::if_else(
          (is.na(row$assist_tackle_4_team) &
             row$assist_tackle_1_player_name != play_stats$player.displayName[index] &
             row$assist_tackle_2_player_name != play_stats$player.displayName[index] &
             row$assist_tackle_3_player_name != play_stats$player.displayName[index]),
            # row$assist_tackle_1_team != play_stats$teamAbbr[index] &
            #  row$assist_tackle_2_team != play_stats$teamAbbr[index] &
            #  row$assist_tackle_3_team != play_stats$teamAbbr[index]),
          play_stats$teamAbbr[index],
          row$assist_tackle_4_team
        )
    } else if (play_stats$statId[index] == 83) {
      row$sack <- 1
    } else if (play_stats$statId[index] == 84) {
      row$sack <- 1
      row$assist_tackle <- 1
    } else if (play_stats$statId[index] == 85) {
      row$pass_defense_1_player_id <-
        dplyr::if_else(
          is.na(row$pass_defense_1_player_id),
          play_stats$player.esbId[index],
          row$pass_defense_1_player_id
        )
      row$pass_defense_1_player_name <-
        dplyr::if_else(
          is.na(row$pass_defense_1_player_name),
          play_stats$player.displayName[index],
          row$pass_defense_1_player_name
        )
      row$pass_defense_2_player_id <-
        dplyr::if_else(
          is.na(row$pass_defense_2_player_id) &
            row$pass_defense_1_player_id != play_stats$player.esbId[index],
          play_stats$player.esbId[index],
          row$pass_defense_2_player_id
        )
      row$pass_defense_2_player_name <-
        dplyr::if_else(
          is.na(row$pass_defense_2_player_name) &
            row$pass_defense_1_player_name != play_stats$player.displayName[index],
          play_stats$player.displayName[index],
          row$pass_defense_2_player_name
        )
    } else if (play_stats$statId[index] == 86) {
      row$punt_attempt <- 1
      row$blocked_player_id <- play_stats$player.esbId[index]
      row$blocked_player_name <- play_stats$player.displayName[index]
    } else if (play_stats$statId[index] == 87) {
      row$blocked_player_id <- play_stats$player.esbId[index]
      row$blocked_player_name <- play_stats$player.displayName[index]
    } else if (play_stats$statId[index] == 88) {
      row$field_goal_attempt <- 1
      row$blocked_player_id <- play_stats$player.esbId[index]
      row$blocked_player_name <- play_stats$player.displayName[index]
    } else if (play_stats$statId[index] == 89) {
      row$safety <- 1
    } else if (play_stats$statId[index] == 91) {
      row$fumble <- 1
      row$forced_fumble_player_1_player_id <-
        dplyr::if_else(
          is.na(row$forced_fumble_player_1_player_id),
          play_stats$player.esbId[index],
          row$forced_fumble_player_1_player_id
        )
      row$forced_fumble_player_1_player_name <-
        dplyr::if_else(
          is.na(row$forced_fumble_player_1_player_name),
          play_stats$player.displayName[index],
          row$forced_fumble_player_1_player_name
        )
      row$forced_fumble_player_1_team <-
        dplyr::if_else(
          is.na(row$forced_fumble_player_1_team),
          play_stats$teamAbbr[index],
          row$forced_fumble_player_1_team
        )
      row$forced_fumble_player_2_player_id <-
        dplyr::if_else(
          is.na(row$forced_fumble_player_2_player_id) &
            row$forced_fumble_player_1_player_id != play_stats$player.esbId[index],
          play_stats$player.esbId[index],
          row$forced_fumble_player_2_player_id
        )
      row$forced_fumble_player_2_player_name <-
        dplyr::if_else(
          is.na(row$forced_fumble_player_2_player_name) &
            row$forced_fumble_player_1_player_name != play_stats$player.displayName[index],
          play_stats$player.displayName[index],
          row$forced_fumble_player_2_player_name
        )
      row$forced_fumble_player_2_team <-
        dplyr::if_else(
          is.na(row$forced_fumble_player_2_team) &
            row$forced_fumble_player_1_player_name != play_stats$player.displayName[index],
            # row$forced_fumble_player_1_team != play_stats$teamAbbr[index],
          play_stats$teamAbbr[index],
          row$forced_fumble_player_2_team
        )
    } else if (play_stats$statId[index] == 93) {
      row$penalty <- 1
      row$penalty_player_id <- play_stats$player.esbId[index]
      row$penalty_player_name <- play_stats$player.displayName[index]
      row$penalty_team <- play_stats$teamAbbr[index]
      row$penalty_yards <- play_stats$yards[index]
    } else if (play_stats$statId[index] == 95) {
      row$tackled_for_loss <- 1
    } else if (play_stats$statId[index] == 96) {
      row$extra_point_safety <- 1
      row$extra_point_attempt <- 1
    } else if (play_stats$statId[index] == 99) {
      row$two_point_rush_safety <- 1
      row$rush_attempt <- 1
      row$two_point_attempt <- 1
      row$rusher_player_id <- play_stats$player.esbId[index]
      row$rusher_player_name <- play_stats$player.displayName[index]
    } else if (play_stats$statId[index] == 100) {
      row$two_point_pass_safety <- 1
      row$pass_attempt <- 1
      row$two_point_attempt <- 1
      row$passer_player_id <- play_stats$player.esbId[index]
      row$passer_player_name <- play_stats$player.displayName[index]
    } else if (play_stats$statId[index] == 102) {
      row$kickoff_downed <- 1
      row$kickoff_attempt <- 1
    } else if (play_stats$statId[index] == 103) {
      row$lateral_sack_player_id <- play_stats$player.esbId[index]
      row$lateral_sack_player_name <- play_stats$player.displayName[index]
    } else if (play_stats$statId[index] == 104) {
      row$two_point_pass_reception_good <- 1
      row$pass_attempt <- 1
      row$two_point_attempt <- 1
      row$receiver_player_id <- play_stats$player.esbId[index]
      row$receiver_player_name <- play_stats$player.displayName[index]
    } else if (play_stats$statId[index] == 105) {
      row$two_point_pass_reception_failed <- 1
      row$pass_attempt <- 1
      row$two_point_attempt <- 1
      row$receiver_player_id <- play_stats$player.esbId[index]
      row$receiver_player_name <- play_stats$player.displayName[index]
    } else if (play_stats$statId[index] == 106) {
      row$fumble_lost <- 1
      row$fumble <- 1
      row$fumbled_1_player_id <-
        dplyr::if_else(
          is.na(row$fumbled_1_player_id),
          play_stats$player.esbId[index],
          row$fumbled_1_player_id
        )
      row$fumbled_1_player_name <-
        dplyr::if_else(
          is.na(row$fumbled_1_player_name),
          play_stats$player.displayName[index],
          row$fumbled_1_player_name
        )
      row$fumbled_1_team <-
        dplyr::if_else(
          is.na(row$fumbled_1_team),
          play_stats$teamAbbr[index],
          row$fumbled_1_team
        )
      row$fumbled_2_player_id <-
        dplyr::if_else(
          is.na(row$fumbled_2_player_id) &
            row$fumbled_1_player_id != play_stats$player.esbId[index],
          play_stats$player.esbId[index],
          row$fumbled_2_player_id
        )
      row$fumbled_2_player_name <-
        dplyr::if_else(
          is.na(row$fumbled_2_player_name) &
            row$fumbled_1_player_name != play_stats$player.displayName[index],
          play_stats$player.displayName[index],
          row$fumbled_2_player_name
        )
      row$fumbled_2_team <-
        dplyr::if_else(
          is.na(row$fumbled_2_team) &
            row$fumbled_1_player_name != play_stats$player.displayName[index],
            # row$fumbled_1_team != play_stats$teamAbbr[index],
          play_stats$teamAbbr[index],
          row$fumbled_2_team
        )
    } else if (play_stats$statId[index] == 107) {
      row$own_kickoff_recovery <- 1
      row$kickoff_attempt <- 1
      row$own_kickoff_recovery_player_id <- play_stats$player.esbId[index]
      row$own_kickoff_recovery_player_name <- play_stats$player.displayName[index]
    } else if (play_stats$statId[index] == 108) {
      row$own_kickoff_recovery_td <- 1
      row$touchdown <- 1
      row$td_team <- play_stats$teamAbbr[index]
      row$kickoff_attempt <- 1
      row$own_kickoff_recovery_player_id <- play_stats$player.esbId[index]
      row$own_kickoff_recovery_player_name <- play_stats$player.displayName[index]
    } else if (play_stats$statId[index] == 110) {
      row$qb_hit <- 1
      row$qb_hit_1_player_id <-
        dplyr::if_else(
          is.na(row$qb_hit_1_player_id),
          play_stats$player.esbId[index],
          row$qb_hit_1_player_id
        )
      row$qb_hit_1_player_name <-
        dplyr::if_else(
          is.na(row$qb_hit_1_player_name),
          play_stats$player.displayName[index],
          row$qb_hit_1_player_name
        )
      row$qb_hit_2_player_id <-
        dplyr::if_else(
          is.na(row$qb_hit_2_player_id) &
            row$qb_hit_1_player_id != play_stats$player.esbId[index],
          play_stats$player.esbId[index],
          row$qb_hit_2_player_id
        )
      row$qb_hit_2_player_name <-
        dplyr::if_else(
          is.na(row$qb_hit_2_player_name) &
            row$qb_hit_1_player_name != play_stats$player.displayName[index],
          play_stats$player.displayName[index],
          row$qb_hit_2_player_name
        )
    } else if (play_stats$statId[index] == 111) {
      row$pass_attempt <- 1
      row$complete_pass <- 1
      row$passer_player_id <- play_stats$player.esbId[index]
      row$passer_player_name <- play_stats$player.displayName[index]
      row$air_yards <- play_stats$yards[index]
    } else if (play_stats$statId[index] == 112) {
      row$pass_attempt <- 1
      row$passer_player_id <- play_stats$player.esbId[index]
      row$passer_player_name <- play_stats$player.displayName[index]
      row$air_yards <- play_stats$yards[index]
    } else if (play_stats$statId[index] == 113) {
      row$pass_attempt <- 1
      row$complete_pass <- 1
      if (is.na(row$receiver_player_id)) {
        row$receiver_player_id <- play_stats$player.esbId[index]
        row$receiver_player_name <- play_stats$player.displayName[index]
      }
      if (is.na(row$yards_after_catch)) {
        row$yards_after_catch <- play_stats$yards[index]
      }
    } else if (play_stats$statId[index] == 115) {
      row$pass_attempt <- 1
      row$receiver_player_id <- play_stats$player.esbId[index]
      row$receiver_player_name <- play_stats$player.displayName[index]
    } else if (play_stats$statId[index] == 120) {
      row$tackle_for_loss_1_player_id <-
        dplyr::if_else(
          is.na(row$tackle_for_loss_1_player_id),
          play_stats$player.esbId[index],
          row$tackle_for_loss_1_player_id
        )
      row$tackle_for_loss_1_player_name <-
        dplyr::if_else(
          is.na(row$tackle_for_loss_1_player_name),
          play_stats$player.displayName[index],
          row$tackle_for_loss_1_player_name
        )
      row$tackle_for_loss_2_player_id <-
        dplyr::if_else(
          is.na(row$tackle_for_loss_2_player_id) &
            row$tackle_for_loss_1_player_id != play_stats$player.esbId[index],
          play_stats$player.esbId[index],
          row$tackle_for_loss_2_player_id
        )
      row$tackle_for_loss_2_player_name <-
        dplyr::if_else(
          is.na(row$tackle_for_loss_2_player_name) &
            row$tackle_for_loss_1_player_name != play_stats$player.displayName[index],
          play_stats$player.displayName[index],
          row$tackle_for_loss_2_player_name
        )
    } else if (play_stats$statId[index] == 301) {
      row$extra_point_aborted <- 1
      row$extra_point_attempt <- 1
    } else if (play_stats$statId[index] == 402) {
      NULL
    } else if (play_stats$statId[index] == 403) {
      row$defensive_two_point_attempt <- 1
    } else if (play_stats$statId[index] == 404) {
      row$defensive_two_point_conv <- 1
    } else if (play_stats$statId[index] == 405) {
      row$defensive_extra_point_attempt <- 1
    } else if (play_stats$statId[index] == 406) {
      row$defensive_extra_point_conv <- 1
    } else if (play_stats$statId[index] == 410) {
      row$kickoff_attempt <- 1
      row$kicker_player_id <- play_stats$player.esbId[index]
      row$kicker_player_name <- play_stats$player.displayName[index]
    } else if (play_stats$statId[index] == 420) {
      row$two_point_return <- 1
      row$two_point_attempt <- 1
    } else {
      NULL
    }
  }
  return(row)
}



# stats character vectors -------------------------------------------------

pbp_stat_columns <-
  c(
    "play_id",
    "punt_blocked",
    "first_down_rush",
    "first_down_pass",
    "first_down_penalty",
    "third_down_converted",
    "third_down_failed",
    "fourth_down_converted",
    "fourth_down_failed",
    "incomplete_pass",
    "interception",
    "punt_inside_twenty",
    "punt_in_endzone",
    "punt_out_of_bounds",
    "punt_downed",
    "punt_fair_catch",
    "kickoff_inside_twenty",
    "kickoff_in_endzone",
    "kickoff_out_of_bounds",
    "kickoff_fair_catch",
    "fumble_forced",
    "fumble_not_forced",
    "fumble_out_of_bounds",
    "timeout",
    "field_goal_missed",
    "field_goal_made",
    "field_goal_blocked",
    "extra_point_good",
    "extra_point_failed",
    "extra_point_blocked",
    "two_point_rush_good",
    "two_point_rush_failed",
    "two_point_pass_good",
    "two_point_pass_failed",
    "solo_tackle",
    "safety",
    "penalty",
    "tackled_for_loss",
    "extra_point_safety",
    "two_point_rush_safety",
    "two_point_pass_safety",
    "kickoff_downed",
    "two_point_pass_reception_good",
    "two_point_pass_reception_failed",
    "fumble_lost",
    "own_kickoff_recovery",
    "own_kickoff_recovery_td",
    "qb_hit",
    "extra_point_aborted",
    "two_point_return",
    "rush_attempt",
    "pass_attempt",
    "sack",
    "touchdown",
    "pass_touchdown",
    "rush_touchdown",
    "return_touchdown",
    "extra_point_attempt",
    "two_point_attempt",
    "field_goal_attempt",
    "kickoff_attempt",
    "punt_attempt",
    "fumble",
    "complete_pass",
    "assist_tackle",
    "lateral_reception",
    "lateral_rush",
    "lateral_return",
    "lateral_recovery",
    "passer_player_id",
    "passer_player_name",
    "receiver_player_id",
    "receiver_player_name",
    "rusher_player_id",
    "rusher_player_name",
    "lateral_receiver_player_id",
    "lateral_receiver_player_name",
    "lateral_rusher_player_id",
    "lateral_rusher_player_name",
    "lateral_sack_player_id",
    "lateral_sack_player_name",
    "interception_player_id",
    "interception_player_name",
    "lateral_interception_player_id",
    "lateral_interception_player_name",
    "punt_returner_player_id",
    "punt_returner_player_name",
    "lateral_punt_returner_player_id",
    "lateral_punt_returner_player_name",
    "kickoff_returner_player_name",
    "kickoff_returner_player_id",
    "lateral_kickoff_returner_player_id",
    "lateral_kickoff_returner_player_name",
    "punter_player_id",
    "punter_player_name",
    "kicker_player_name",
    "kicker_player_id",
    "own_kickoff_recovery_player_id",
    "own_kickoff_recovery_player_name",
    "blocked_player_id",
    "blocked_player_name",
    "tackle_for_loss_1_player_id",
    "tackle_for_loss_1_player_name",
    "tackle_for_loss_2_player_id",
    "tackle_for_loss_2_player_name",
    "qb_hit_1_player_id",
    "qb_hit_1_player_name",
    "qb_hit_2_player_id",
    "qb_hit_2_player_name",
    "forced_fumble_player_1_team",
    "forced_fumble_player_1_player_id",
    "forced_fumble_player_1_player_name",
    "forced_fumble_player_2_team",
    "forced_fumble_player_2_player_id",
    "forced_fumble_player_2_player_name",
    "solo_tackle_1_team",
    "solo_tackle_2_team",
    "solo_tackle_1_player_id",
    "solo_tackle_2_player_id",
    "solo_tackle_1_player_name",
    "solo_tackle_2_player_name",
    "assist_tackle_1_player_id",
    "assist_tackle_1_player_name",
    "assist_tackle_1_team",
    "assist_tackle_2_player_id",
    "assist_tackle_2_player_name",
    "assist_tackle_2_team",
    "assist_tackle_3_player_id",
    "assist_tackle_3_player_name",
    "assist_tackle_3_team",
    "assist_tackle_4_player_id",
    "assist_tackle_4_player_name",
    "assist_tackle_4_team",
    "pass_defense_1_player_id",
    "pass_defense_1_player_name",
    "pass_defense_2_player_id",
    "pass_defense_2_player_name",
    "fumbled_1_team",
    "fumbled_1_player_id",
    "fumbled_1_player_name",
    "fumbled_2_player_id",
    "fumbled_2_player_name",
    "fumbled_2_team",
    "fumble_recovery_1_team",
    "fumble_recovery_1_yards",
    "fumble_recovery_1_player_id",
    "fumble_recovery_1_player_name",
    "fumble_recovery_2_team",
    "fumble_recovery_2_yards",
    "fumble_recovery_2_player_id",
    "fumble_recovery_2_player_name",
    "td_team",
    "return_team",
    "timeout_team",
    "yards_gained",
    "return_yards",
    "air_yards",
    "yards_after_catch",
    "penalty_team",
    "penalty_player_id",
    "penalty_player_name",
    "penalty_yards",
    "kick_distance",
    "defensive_two_point_attempt",
    "defensive_two_point_conv",
    "defensive_extra_point_attempt",
    "defensive_extra_point_conv",
    "penalty_fix",
    "return_penalty_fix"
  )

indicator_stats <- c(
  "punt_blocked",
  "first_down_rush",
  "first_down_pass",
  "first_down_penalty",
  "third_down_converted",
  "third_down_failed",
  "fourth_down_converted",
  "fourth_down_failed",
  "incomplete_pass",
  "interception",
  "punt_inside_twenty",
  "punt_in_endzone",
  "punt_out_of_bounds",
  "punt_downed",
  "punt_fair_catch",
  "kickoff_inside_twenty",
  "kickoff_in_endzone",
  "kickoff_out_of_bounds",
  "kickoff_fair_catch",
  "fumble_forced",
  "fumble_not_forced",
  "fumble_out_of_bounds",
  "timeout",
  "field_goal_missed",
  "field_goal_made",
  "field_goal_blocked",
  "extra_point_good",
  "extra_point_failed",
  "extra_point_blocked",
  "two_point_rush_good",
  "two_point_rush_failed",
  "two_point_pass_good",
  "two_point_pass_failed",
  "solo_tackle",
  "safety",
  "penalty",
  "tackled_for_loss",
  "extra_point_safety",
  "two_point_rush_safety",
  "two_point_pass_safety",
  "kickoff_downed",
  "two_point_pass_reception_good",
  "two_point_pass_reception_failed",
  "fumble_lost",
  "own_kickoff_recovery",
  "own_kickoff_recovery_td",
  "qb_hit",
  "extra_point_aborted",
  "two_point_return",
  "defensive_two_point_attempt",
  "defensive_two_point_conv",
  "defensive_extra_point_attempt",
  "defensive_extra_point_conv",
  "rush_attempt",
  "pass_attempt",
  "sack",
  "touchdown",
  "pass_touchdown",
  "rush_touchdown",
  "return_touchdown",
  "extra_point_attempt",
  "two_point_attempt",
  "field_goal_attempt",
  "kickoff_attempt",
  "punt_attempt",
  "fumble",
  "complete_pass",
  "assist_tackle",
  "lateral_reception",
  "lateral_rush",
  "lateral_return",
  "lateral_recovery",
  "penalty_fix",
  "yards_gained",
  "return_yards",
  "return_penalty_fix"
)
