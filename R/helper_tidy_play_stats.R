################################################################################
# Author: Sebastian Carl
# Purpose: Create one row with all play stats per play, built in the scraper
#          functions
# Stlyeguide: styler::tidyverse_style()
################################################################################

# Build one row per play for a tidy data structure
#
# This is a sub-function for the get_pbp_nfl and get_pbp_gc functions.
#
# @param stats A data frame with one row per gsis stat and the columns playId,
# statId, yards, teamAbbr, player.displayName and player.esbId
# @return A data.table with one row per play in stats (in order of first
# appearance), the column play_id and one column per play stat variable
#
# The stats of each play are processed in their original order, so later stats
# overwrite earlier ones, and numbered slots (e.g. fumbled_1, fumbled_2) are
# filled in order. Output columns are local vectors that are modified in place.
tidy_play_stats <- function(stats) {
  play_ids <- unique(stats$playId)
  n <- length(play_ids)
  play_index <- match(stats$playId, play_ids)
  s_stat <- stats$statId
  s_yards <- stats$yards
  s_team <- stats$teamAbbr
  s_name <- stats$player.displayName
  s_id <- stats$player.esbId

  punt_blocked <- rep(0, n)
  first_down_rush <- rep(0, n)
  first_down_pass <- rep(0, n)
  first_down_penalty <- rep(0, n)
  third_down_converted <- rep(0, n)
  third_down_failed <- rep(0, n)
  fourth_down_converted <- rep(0, n)
  fourth_down_failed <- rep(0, n)
  incomplete_pass <- rep(0, n)
  interception <- rep(0, n)
  punt_inside_twenty <- rep(0, n)
  punt_in_endzone <- rep(0, n)
  punt_out_of_bounds <- rep(0, n)
  punt_downed <- rep(0, n)
  punt_fair_catch <- rep(0, n)
  kickoff_inside_twenty <- rep(0, n)
  kickoff_in_endzone <- rep(0, n)
  kickoff_out_of_bounds <- rep(0, n)
  kickoff_fair_catch <- rep(0, n)
  fumble_forced <- rep(0, n)
  fumble_not_forced <- rep(0, n)
  fumble_out_of_bounds <- rep(0, n)
  timeout <- rep(0, n)
  field_goal_missed <- rep(0, n)
  field_goal_made <- rep(0, n)
  field_goal_blocked <- rep(0, n)
  extra_point_good <- rep(0, n)
  extra_point_failed <- rep(0, n)
  extra_point_blocked <- rep(0, n)
  two_point_rush_good <- rep(0, n)
  two_point_rush_failed <- rep(0, n)
  two_point_pass_good <- rep(0, n)
  two_point_pass_failed <- rep(0, n)
  solo_tackle <- rep(0, n)
  safety <- rep(0, n)
  penalty <- rep(0, n)
  tackled_for_loss <- rep(0, n)
  extra_point_safety <- rep(0, n)
  two_point_rush_safety <- rep(0, n)
  two_point_pass_safety <- rep(0, n)
  kickoff_downed <- rep(0, n)
  two_point_pass_reception_good <- rep(0, n)
  two_point_pass_reception_failed <- rep(0, n)
  fumble_lost <- rep(0, n)
  own_kickoff_recovery <- rep(0, n)
  own_kickoff_recovery_td <- rep(0, n)
  qb_hit <- rep(0, n)
  extra_point_aborted <- rep(0, n)
  two_point_return <- rep(0, n)
  rush_attempt <- rep(0, n)
  pass_attempt <- rep(0, n)
  sack <- rep(0, n)
  touchdown <- rep(0, n)
  pass_touchdown <- rep(0, n)
  rush_touchdown <- rep(0, n)
  return_touchdown <- rep(0, n)
  extra_point_attempt <- rep(0, n)
  two_point_attempt <- rep(0, n)
  field_goal_attempt <- rep(0, n)
  kickoff_attempt <- rep(0, n)
  punt_attempt <- rep(0, n)
  fumble <- rep(0, n)
  complete_pass <- rep(0, n)
  assist_tackle <- rep(0, n)
  lateral_reception <- rep(0, n)
  lateral_rush <- rep(0, n)
  lateral_return <- rep(0, n)
  lateral_recovery <- rep(0, n)
  passer_player_id <- rep(NA_character_, n)
  passer_player_name <- rep(NA_character_, n)
  receiver_player_id <- rep(NA_character_, n)
  receiver_player_name <- rep(NA_character_, n)
  rusher_player_id <- rep(NA_character_, n)
  rusher_player_name <- rep(NA_character_, n)
  lateral_receiver_player_id <- rep(NA_character_, n)
  lateral_receiver_player_name <- rep(NA_character_, n)
  lateral_rusher_player_id <- rep(NA_character_, n)
  lateral_rusher_player_name <- rep(NA_character_, n)
  lateral_sack_player_id <- rep(NA_character_, n)
  lateral_sack_player_name <- rep(NA_character_, n)
  interception_player_id <- rep(NA_character_, n)
  interception_player_name <- rep(NA_character_, n)
  lateral_interception_player_id <- rep(NA_character_, n)
  lateral_interception_player_name <- rep(NA_character_, n)
  punt_returner_player_id <- rep(NA_character_, n)
  punt_returner_player_name <- rep(NA_character_, n)
  lateral_punt_returner_player_id <- rep(NA_character_, n)
  lateral_punt_returner_player_name <- rep(NA_character_, n)
  kickoff_returner_player_name <- rep(NA_character_, n)
  kickoff_returner_player_id <- rep(NA_character_, n)
  lateral_kickoff_returner_player_id <- rep(NA_character_, n)
  lateral_kickoff_returner_player_name <- rep(NA_character_, n)
  punter_player_id <- rep(NA_character_, n)
  punter_player_name <- rep(NA_character_, n)
  kicker_player_name <- rep(NA_character_, n)
  kicker_player_id <- rep(NA_character_, n)
  own_kickoff_recovery_player_id <- rep(NA_character_, n)
  own_kickoff_recovery_player_name <- rep(NA_character_, n)
  blocked_player_id <- rep(NA_character_, n)
  blocked_player_name <- rep(NA_character_, n)
  tackle_for_loss_1_player_id <- rep(NA_character_, n)
  tackle_for_loss_1_player_name <- rep(NA_character_, n)
  tackle_for_loss_2_player_id <- rep(NA_character_, n)
  tackle_for_loss_2_player_name <- rep(NA_character_, n)
  qb_hit_1_player_id <- rep(NA_character_, n)
  qb_hit_1_player_name <- rep(NA_character_, n)
  qb_hit_2_player_id <- rep(NA_character_, n)
  qb_hit_2_player_name <- rep(NA_character_, n)
  forced_fumble_player_1_team <- rep(NA_character_, n)
  forced_fumble_player_1_player_id <- rep(NA_character_, n)
  forced_fumble_player_1_player_name <- rep(NA_character_, n)
  forced_fumble_player_2_team <- rep(NA_character_, n)
  forced_fumble_player_2_player_id <- rep(NA_character_, n)
  forced_fumble_player_2_player_name <- rep(NA_character_, n)
  solo_tackle_1_team <- rep(NA_character_, n)
  solo_tackle_2_team <- rep(NA_character_, n)
  solo_tackle_1_player_id <- rep(NA_character_, n)
  solo_tackle_2_player_id <- rep(NA_character_, n)
  solo_tackle_1_player_name <- rep(NA_character_, n)
  solo_tackle_2_player_name <- rep(NA_character_, n)
  assist_tackle_1_player_id <- rep(NA_character_, n)
  assist_tackle_1_player_name <- rep(NA_character_, n)
  assist_tackle_1_team <- rep(NA_character_, n)
  assist_tackle_2_player_id <- rep(NA_character_, n)
  assist_tackle_2_player_name <- rep(NA_character_, n)
  assist_tackle_2_team <- rep(NA_character_, n)
  assist_tackle_3_player_id <- rep(NA_character_, n)
  assist_tackle_3_player_name <- rep(NA_character_, n)
  assist_tackle_3_team <- rep(NA_character_, n)
  assist_tackle_4_player_id <- rep(NA_character_, n)
  assist_tackle_4_player_name <- rep(NA_character_, n)
  assist_tackle_4_team <- rep(NA_character_, n)
  tackle_with_assist <- rep(0, n)
  tackle_with_assist_1_player_id <- rep(NA_character_, n)
  tackle_with_assist_1_player_name <- rep(NA_character_, n)
  tackle_with_assist_1_team <- rep(NA_character_, n)
  tackle_with_assist_2_player_id <- rep(NA_character_, n)
  tackle_with_assist_2_player_name <- rep(NA_character_, n)
  tackle_with_assist_2_team <- rep(NA_character_, n)
  pass_defense_1_player_id <- rep(NA_character_, n)
  pass_defense_1_player_name <- rep(NA_character_, n)
  pass_defense_2_player_id <- rep(NA_character_, n)
  pass_defense_2_player_name <- rep(NA_character_, n)
  fumbled_1_team <- rep(NA_character_, n)
  fumbled_1_player_id <- rep(NA_character_, n)
  fumbled_1_player_name <- rep(NA_character_, n)
  fumbled_2_player_id <- rep(NA_character_, n)
  fumbled_2_player_name <- rep(NA_character_, n)
  fumbled_2_team <- rep(NA_character_, n)
  fumble_recovery_1_team <- rep(NA_character_, n)
  fumble_recovery_1_yards <- rep(NA_integer_, n)
  fumble_recovery_1_player_id <- rep(NA_character_, n)
  fumble_recovery_1_player_name <- rep(NA_character_, n)
  fumble_recovery_2_team <- rep(NA_character_, n)
  fumble_recovery_2_yards <- rep(NA_integer_, n)
  fumble_recovery_2_player_id <- rep(NA_character_, n)
  fumble_recovery_2_player_name <- rep(NA_character_, n)
  td_team <- rep(NA_character_, n)
  return_team <- rep(NA_character_, n)
  timeout_team <- rep(NA_character_, n)
  yards_gained <- rep(0, n)
  return_yards <- rep(0, n)
  air_yards <- rep(NA_integer_, n)
  yards_after_catch <- rep(NA_integer_, n)
  penalty_team <- rep(NA_character_, n)
  penalty_player_id <- rep(NA_character_, n)
  penalty_player_name <- rep(NA_character_, n)
  penalty_yards <- rep(NA_integer_, n)
  kick_distance <- rep(NA_integer_, n)
  defensive_two_point_attempt <- rep(0, n)
  defensive_two_point_conv <- rep(0, n)
  defensive_extra_point_attempt <- rep(0, n)
  defensive_extra_point_conv <- rep(0, n)
  penalty_fix <- rep(0, n)
  return_penalty_fix <- rep(0, n)
  rushing_yards <- rep(NA_integer_, n)
  lateral_rushing_yards <- rep(NA_integer_, n)
  passing_yards <- rep(NA_integer_, n)
  receiving_yards <- rep(NA_integer_, n)
  lateral_receiving_yards <- rep(NA_integer_, n)
  td_player_id <- rep(NA_character_, n)
  td_player_name <- rep(NA_character_, n)
  sack_player_id <- rep(NA_character_, n)
  sack_player_name <- rep(NA_character_, n)
  half_sack_1_player_id <- rep(NA_character_, n)
  half_sack_1_player_name <- rep(NA_character_, n)
  half_sack_2_player_id <- rep(NA_character_, n)
  half_sack_2_player_name <- rep(NA_character_, n)
  safety_player_name <- rep(NA_character_, n)
  safety_player_id <- rep(NA_character_, n)

  # switch() picks the branch by position, which is much faster than matching
  # the stat id against the branch names. So the switch has one branch per row
  # of stat_ids, in the same order, and the names are only labels. Stat ids
  # that aren't in stat_ids give NA and are skipped.
  s_branch <- match(s_stat, nflfastR::stat_ids$stat_id)

  for (index in seq_along(s_stat)) {
    i <- play_index[index]
    switch(
      s_branch[index],
      "1" = NULL,
      "2" = {
        punt_blocked[i] <- 1
        punt_attempt[i] <- 1
        kick_distance[i] <- s_yards[index]
        punter_player_id[i] <- s_id[index]
        punter_player_name[i] <- s_name[index]
      },
      "3" = {
        first_down_rush[i] <- 1
      },
      "4" = {
        first_down_pass[i] <- 1
      },
      "5" = {
        first_down_penalty[i] <- 1
      },
      "6" = {
        third_down_converted[i] <- 1
      },
      "7" = {
        third_down_failed[i] <- 1
      },
      "8" = {
        fourth_down_converted[i] <- 1
      },
      "9" = {
        fourth_down_failed[i] <- 1
      },
      "10" = {
        rush_attempt[i] <- 1
        rusher_player_id[i] <- s_id[index]
        rusher_player_name[i] <- s_name[index]
        yards_gained[i] <- s_yards[index]
        rushing_yards[i] <- s_yards[index]
      },
      "11" = {
        rush_attempt[i] <- 1
        touchdown[i] <- 1
        first_down_rush[i] <- 1
        rush_touchdown[i] <- 1
        rusher_player_id[i] <- s_id[index]
        rusher_player_name[i] <- s_name[index]
        yards_gained[i] <- s_yards[index]
        rushing_yards[i] <- s_yards[index]
        td_team[i] <- s_team[index]
        td_player_id[i] <- s_id[index]
        td_player_name[i] <- s_name[index]
      },
      "12" = {
        rush_attempt[i] <- 1
        lateral_rush[i] <- 1
        lateral_rusher_player_id[i] <- s_id[index]
        lateral_rusher_player_name[i] <- s_name[index]
        yards_gained[i] <- s_yards[index]
        lateral_rushing_yards[i] <- s_yards[index]
      },
      "13" = {
        rush_attempt[i] <- 1
        touchdown[i] <- 1
        rush_touchdown[i] <- 1
        lateral_rush[i] <- 1
        lateral_rusher_player_id[i] <- s_id[index]
        lateral_rusher_player_name[i] <- s_name[index]
        td_team[i] <- s_team[index]
        td_player_id[i] <- s_id[index]
        td_player_name[i] <- s_name[index]
        yards_gained[i] <- s_yards[index]
        lateral_rushing_yards[i] <- s_yards[index]
      },
      "14" = {
        incomplete_pass[i] <- 1
        pass_attempt[i] <- 1
        passer_player_id[i] <- s_id[index]
        passer_player_name[i] <- s_name[index]
      },
      "15" = {
        pass_attempt[i] <- 1
        complete_pass[i] <- 1
        passer_player_id[i] <- s_id[index]
        passer_player_name[i] <- s_name[index]
        yards_gained[i] <- s_yards[index]
        passing_yards[i] <- s_yards[index]
      },
      "16" = {
        pass_attempt[i] <- 1
        touchdown[i] <- 1
        pass_touchdown[i] <- 1
        complete_pass[i] <- 1
        passer_player_id[i] <- s_id[index]
        passer_player_name[i] <- s_name[index]
        yards_gained[i] <- s_yards[index]
        passing_yards[i] <- s_yards[index]
        td_team[i] <- s_team[index]
        td_player_id[i] <- s_id[index]
        td_player_name[i] <- s_name[index]
      },
      "17" = NULL,
      "18" = NULL,
      "19" = {
        interception[i] <- 1
        pass_attempt[i] <- 1
        passer_player_id[i] <- s_id[index]
        passer_player_name[i] <- s_name[index]
      },
      "20" = {
        pass_attempt[i] <- 1
        sack[i] <- 1
        passer_player_id[i] <- s_id[index]
        passer_player_name[i] <- s_name[index]
        yards_gained[i] <- s_yards[index]
      },
      "21" = {
        pass_attempt[i] <- 1
        complete_pass[i] <- 1
        receiver_player_id[i] <- s_id[index]
        receiver_player_name[i] <- s_name[index]
        yards_gained[i] <- s_yards[index]
        receiving_yards[i] <- s_yards[index]
      },
      "22" = {
        pass_attempt[i] <- 1
        touchdown[i] <- 1
        pass_touchdown[i] <- 1
        complete_pass[i] <- 1
        receiver_player_id[i] <- s_id[index]
        receiver_player_name[i] <- s_name[index]
        td_team[i] <- s_team[index]
        td_player_id[i] <- s_id[index]
        td_player_name[i] <- s_name[index]
        yards_gained[i] <- s_yards[index]
        receiving_yards[i] <- s_yards[index]
      },
      "23" = {
        pass_attempt[i] <- 1
        complete_pass[i] <- 1
        lateral_reception[i] <- 1
        lateral_receiver_player_id[i] <- s_id[index]
        lateral_receiver_player_name[i] <- s_name[index]
        yards_gained[i] <- s_yards[index]
        lateral_receiving_yards[i] <- s_yards[index]
      },
      "24" = {
        pass_attempt[i] <- 1
        touchdown[i] <- 1
        pass_touchdown[i] <- 1
        complete_pass[i] <- 1
        lateral_reception[i] <- 1
        lateral_receiver_player_id[i] <- s_id[index]
        lateral_receiver_player_name[i] <- s_name[index]
        td_team[i] <- s_team[index]
        td_player_id[i] <- s_id[index]
        td_player_name[i] <- s_name[index]
        yards_gained[i] <- s_yards[index]
        lateral_receiving_yards[i] <- s_yards[index]
      },
      "25" = {
        pass_attempt[i] <- 1
        interception_player_id[i] <- s_id[index]
        interception_player_name[i] <- s_name[index]
        return_team[i] <- s_team[index]
        return_yards[i] <- s_yards[index]
        return_penalty_fix[i] <- 1
      },
      "26" = {
        pass_attempt[i] <- 1
        touchdown[i] <- 1
        return_touchdown[i] <- 1
        interception_player_id[i] <- s_id[index]
        interception_player_name[i] <- s_name[index]
        td_team[i] <- s_team[index]
        td_player_id[i] <- s_id[index]
        td_player_name[i] <- s_name[index]
        return_team[i] <- s_team[index]
        return_yards[i] <- s_yards[index]
        return_penalty_fix[i] <- 1
      },
      "27" = {
        pass_attempt[i] <- 1
        lateral_return[i] <- 1
        lateral_interception_player_id[i] <- s_id[index]
        lateral_interception_player_name[i] <- s_name[index]
        return_yards[i] <- s_yards[index]
        return_penalty_fix[i] <- 1
      },
      "28" = {
        pass_attempt[i] <- 1
        touchdown[i] <- 1
        return_touchdown[i] <- 1
        lateral_return[i] <- 1
        lateral_interception_player_id[i] <- s_id[index]
        lateral_interception_player_name[i] <- s_name[index]
        td_team[i] <- s_team[index]
        td_player_id[i] <- s_id[index]
        td_player_name[i] <- s_name[index]
        return_yards[i] <- s_yards[index]
        return_penalty_fix[i] <- 1
      },
      "29" = {
        punt_attempt[i] <- 1
        punter_player_id[i] <- s_id[index]
        punter_player_name[i] <- s_name[index]
        kick_distance[i] <- s_yards[index]
      },
      "30" = {
        # yards always zero for stat_id 30 (punt inside 20) so we don't write kick_distance here
        punt_inside_twenty[i] <- 1
        punt_attempt[i] <- 1
        punter_player_id[i] <- s_id[index]
        punter_player_name[i] <- s_name[index]
      },
      "31" = {
        punt_in_endzone[i] <- 1
        punt_attempt[i] <- 1
        punter_player_id[i] <- s_id[index]
        punter_player_name[i] <- s_name[index]
        kick_distance[i] <- s_yards[index]
      },
      "32" = {
        punt_attempt[i] <- 1
        kick_distance[i] <- s_yards[index]
        punter_player_id[i] <- s_id[index]
        punter_player_name[i] <- s_name[index]
      },
      "33" = {
        punt_attempt[i] <- 1
        punt_returner_player_id[i] <- s_id[index]
        punt_returner_player_name[i] <- s_name[index]
        return_yards[i] <- s_yards[index]
        return_team[i] <- s_team[index]
        return_penalty_fix[i] <- 1
      },
      "34" = {
        touchdown[i] <- 1
        return_touchdown[i] <- 1
        punt_attempt[i] <- 1
        punt_returner_player_id[i] <- s_id[index]
        punt_returner_player_name[i] <- s_name[index]
        td_team[i] <- s_team[index]
        td_player_id[i] <- s_id[index]
        td_player_name[i] <- s_name[index]
        return_team[i] <- s_team[index]
        return_yards[i] <- s_yards[index]
        return_penalty_fix[i] <- 1
      },
      "35" = {
        punt_attempt[i] <- 1
        lateral_return[i] <- 1
        lateral_punt_returner_player_id[i] <- s_id[index]
        lateral_punt_returner_player_name[i] <- s_name[index]
        return_yards[i] <- s_yards[index]
        return_penalty_fix[i] <- 1
      },
      "36" = {
        touchdown[i] <- 1
        return_touchdown[i] <- 1
        punt_attempt[i] <- 1
        lateral_return[i] <- 1
        lateral_punt_returner_player_id[i] <- s_id[index]
        lateral_punt_returner_player_name[i] <- s_name[index]
        td_team[i] <- s_team[index]
        td_player_id[i] <- s_id[index]
        td_player_name[i] <- s_name[index]
        return_yards[i] <- s_yards[index]
        return_team[i] <- s_team[index]
        return_penalty_fix[i] <- 1
      },
      "37" = {
        punt_out_of_bounds[i] <- 1
        punt_attempt[i] <- 1
        return_yards[i] <- 0
        return_team[i] <- s_team[index]
      },
      "38" = {
        punt_downed[i] <- 1
        punt_attempt[i] <- 1
        return_team[i] <- s_team[index]
      },
      "39" = {
        punt_fair_catch[i] <- 1
        punt_attempt[i] <- 1
        punt_returner_player_id[i] <- s_id[index]
        punt_returner_player_name[i] <- s_name[index]
        return_team[i] <- s_team[index]
      },
      "40" = {
        punt_attempt[i] <- 1
        return_team[i] <- s_team[index]
      },
      "41" = {
        kickoff_attempt[i] <- 1
        kicker_player_id[i] <- s_id[index]
        kicker_player_name[i] <- s_name[index]
        kick_distance[i] <- s_yards[index]
      },
      "42" = {
        # yards always zero for stat_id 42 so we don't write kick_distance here
        kickoff_inside_twenty[i] <- 1
        kickoff_attempt[i] <- 1
        kicker_player_id[i] <- s_id[index]
        kicker_player_name[i] <- s_name[index]
      },
      "43" = {
        kickoff_in_endzone[i] <- 1
        kickoff_attempt[i] <- 1
        kick_distance[i] <- s_yards[index]
        kicker_player_id[i] <- s_id[index]
        kicker_player_name[i] <- s_name[index]
      },
      "44" = {
        kickoff_attempt[i] <- 1
        kick_distance[i] <- s_yards[index]
        kicker_player_id[i] <- s_id[index]
        kicker_player_name[i] <- s_name[index]
      },
      "45" = {
        kickoff_attempt[i] <- 1
        kickoff_returner_player_id[i] <- s_id[index]
        kickoff_returner_player_name[i] <- s_name[index]
        return_yards[i] <- s_yards[index]
        return_team[i] <- s_team[index]
        return_penalty_fix[i] <- 1
      },
      "46" = {
        touchdown[i] <- 1
        return_touchdown[i] <- 1
        kickoff_attempt[i] <- 1
        kickoff_returner_player_id[i] <- s_id[index]
        kickoff_returner_player_name[i] <- s_name[index]
        td_team[i] <- s_team[index]
        td_player_id[i] <- s_id[index]
        td_player_name[i] <- s_name[index]
        return_yards[i] <- s_yards[index]
        return_team[i] <- s_team[index]
        return_penalty_fix[i] <- 1
      },
      "47" = {
        kickoff_attempt[i] <- 1
        lateral_return[i] <- 1
        lateral_kickoff_returner_player_id[i] <- s_id[index]
        lateral_kickoff_returner_player_name[i] <- s_name[index]
        return_yards[i] <- s_yards[index]
        return_team[i] <- s_team[index]
        return_penalty_fix[i] <- 1
      },
      "48" = {
        touchdown[i] <- 1
        return_touchdown[i] <- 1
        kickoff_attempt[i] <- 1
        lateral_return[i] <- 1
        lateral_kickoff_returner_player_id[i] <- s_id[index]
        lateral_kickoff_returner_player_name[i] <- s_name[index]
        td_team[i] <- s_team[index]
        td_player_id[i] <- s_id[index]
        td_player_name[i] <- s_name[index]
        return_yards[i] <- s_yards[index]
        return_team[i] <- s_team[index]
        return_penalty_fix[i] <- 1
      },
      "49" = {
        kickoff_out_of_bounds[i] <- 1
        kickoff_attempt[i] <- 1
        return_team[i] <- s_team[index]
      },
      "50" = {
        kickoff_fair_catch[i] <- 1
        kickoff_attempt[i] <- 1
        kickoff_returner_player_id[i] <- s_id[index]
        kickoff_returner_player_name[i] <- s_name[index]
        return_team[i] <- s_team[index]
      },
      "51" = {
        kickoff_attempt[i] <- 1
        return_team[i] <- s_team[index]
      },
      "52" = {
        fumble_forced[i] <- 1
        fumble[i] <- 1
        fumbled_1_player_id[i] <-
          scalar_if_else(
            is.na(fumbled_1_player_id[i]),
            s_id[index],
            fumbled_1_player_id[i]
          )
        fumbled_1_player_name[i] <-
          scalar_if_else(
            is.na(fumbled_1_player_name[i]),
            s_name[index],
            fumbled_1_player_name[i]
          )
        fumbled_1_team[i] <-
          scalar_if_else(
            is.na(fumbled_1_team[i]),
            s_team[index],
            fumbled_1_team[i]
          )
        fumbled_2_player_id[i] <-
          scalar_if_else(
            is.na(fumbled_2_player_id[i]) &
              fumbled_1_player_id[i] != s_id[index],
            s_id[index],
            fumbled_2_player_id[i]
          )
        fumbled_2_player_name[i] <-
          scalar_if_else(
            is.na(fumbled_2_player_name[i]) &
              fumbled_1_player_name[i] != s_name[index],
            s_name[index],
            fumbled_2_player_name[i]
          )
        fumbled_2_team[i] <-
          scalar_if_else(
            is.na(fumbled_2_team[i]) &
              fumbled_1_player_name[i] != s_name[index],
            # fumbled_1_team[i] != s_team[index], # can't use team here because multiple players of the same team are possible
            s_team[index],
            fumbled_2_team[i]
          )
      },
      "53" = {
        fumble_not_forced[i] <- 1
        fumble[i] <- 1
        fumbled_1_player_id[i] <-
          scalar_if_else(
            is.na(fumbled_1_player_id[i]),
            s_id[index],
            fumbled_1_player_id[i]
          )
        fumbled_1_player_name[i] <-
          scalar_if_else(
            is.na(fumbled_1_player_name[i]),
            s_name[index],
            fumbled_1_player_name[i]
          )
        fumbled_1_team[i] <-
          scalar_if_else(
            is.na(fumbled_1_team[i]),
            s_team[index],
            fumbled_1_team[i]
          )
        fumbled_2_player_id[i] <-
          scalar_if_else(
            is.na(fumbled_2_player_id[i]) &
              fumbled_1_player_id[i] != s_id[index],
            s_id[index],
            fumbled_2_player_id[i]
          )
        fumbled_2_player_name[i] <-
          scalar_if_else(
            is.na(fumbled_2_player_name[i]) &
              fumbled_1_player_name[i] != s_name[index],
            s_name[index],
            fumbled_2_player_name[i]
          )
        fumbled_2_team[i] <-
          scalar_if_else(
            is.na(fumbled_2_team[i]) &
              fumbled_1_player_name[i] != s_name[index],
            # fumbled_1_team[i] != s_team[index],
            s_team[index],
            fumbled_2_team[i]
          )
      },
      "54" = {
        fumble_out_of_bounds[i] <- 1
        fumble[i] <- 1
        fumbled_1_player_id[i] <-
          scalar_if_else(
            is.na(fumbled_1_player_id[i]),
            s_id[index],
            fumbled_1_player_id[i]
          )
        fumbled_1_player_name[i] <-
          scalar_if_else(
            is.na(fumbled_1_player_name[i]),
            s_name[index],
            fumbled_1_player_name[i]
          )
        fumbled_1_team[i] <-
          scalar_if_else(
            is.na(fumbled_1_team[i]),
            s_team[index],
            fumbled_1_team[i]
          )
        fumbled_2_player_id[i] <-
          scalar_if_else(
            is.na(fumbled_2_player_id[i]) &
              fumbled_1_player_id[i] != s_id[index],
            s_id[index],
            fumbled_2_player_id[i]
          )
        fumbled_2_player_name[i] <-
          scalar_if_else(
            is.na(fumbled_2_player_name[i]) &
              fumbled_1_player_name[i] != s_name[index],
            s_name[index],
            fumbled_2_player_name[i]
          )
        fumbled_2_team[i] <-
          scalar_if_else(
            is.na(fumbled_2_team[i]) &
              fumbled_1_player_name[i] != s_name[index],
            # fumbled_1_team[i] != s_team[index],
            s_team[index],
            fumbled_2_team[i]
          )
      },
      "55" = {
        fumble[i] <- 1
        fumble_recovery_1_player_id[i] <-
          scalar_if_else(
            is.na(fumble_recovery_1_player_id[i]),
            s_id[index],
            fumble_recovery_1_player_id[i]
          )
        fumble_recovery_1_player_name[i] <-
          scalar_if_else(
            is.na(fumble_recovery_1_player_name[i]),
            s_name[index],
            fumble_recovery_1_player_name[i]
          )
        fumble_recovery_1_team[i] <-
          scalar_if_else(
            is.na(fumble_recovery_1_team[i]),
            s_team[index],
            fumble_recovery_1_team[i]
          )
        fumble_recovery_1_yards[i] <-
          scalar_if_else(
            is.na(fumble_recovery_1_yards[i]),
            s_yards[index],
            fumble_recovery_1_yards[i]
          )
        fumble_recovery_2_player_id[i] <-
          scalar_if_else(
            is.na(fumble_recovery_2_player_id[i]) &
              fumble_recovery_1_player_id[i] != s_id[index],
            s_id[index],
            fumble_recovery_2_player_id[i]
          )
        fumble_recovery_2_player_name[i] <-
          scalar_if_else(
            is.na(fumble_recovery_2_player_name[i]) &
              fumble_recovery_1_player_name[i] != s_name[index],
            s_name[index],
            fumble_recovery_2_player_name[i]
          )
        fumble_recovery_2_team[i] <-
          scalar_if_else(
            is.na(fumble_recovery_2_team[i]) &
              fumble_recovery_1_player_name[i] != s_name[index],
            # fumble_recovery_1_team[i] != s_team[index],
            s_team[index],
            fumble_recovery_2_team[i]
          )
        fumble_recovery_2_yards[i] <-
          scalar_if_else(
            is.na(fumble_recovery_2_yards[i]) &
              fumble_recovery_1_player_name[i] != s_name[index],
            # fumble_recovery_1_yards[i] != s_yards[index],
            s_yards[index],
            fumble_recovery_2_yards[i]
          )
      },
      "56" = {
        touchdown[i] <- 1
        fumble[i] <- 1
        td_team[i] <- s_team[index]
        td_player_id[i] <- s_id[index]
        td_player_name[i] <- s_name[index]
        fumble_recovery_1_player_id[i] <-
          scalar_if_else(
            is.na(fumble_recovery_1_player_id[i]),
            s_id[index],
            fumble_recovery_1_player_id[i]
          )
        fumble_recovery_1_player_name[i] <-
          scalar_if_else(
            is.na(fumble_recovery_1_player_name[i]),
            s_name[index],
            fumble_recovery_1_player_name[i]
          )
        fumble_recovery_1_team[i] <-
          scalar_if_else(
            is.na(fumble_recovery_1_team[i]),
            s_team[index],
            fumble_recovery_1_team[i]
          )
        fumble_recovery_1_yards[i] <-
          scalar_if_else(
            is.na(fumble_recovery_1_yards[i]),
            s_yards[index],
            fumble_recovery_1_yards[i]
          )
        fumble_recovery_2_player_id[i] <-
          scalar_if_else(
            is.na(fumble_recovery_2_player_id[i]) &
              fumble_recovery_1_player_id[i] != s_id[index],
            s_id[index],
            fumble_recovery_2_player_id[i]
          )
        fumble_recovery_2_player_name[i] <-
          scalar_if_else(
            is.na(fumble_recovery_2_player_name[i]) &
              fumble_recovery_1_player_name[i] != s_name[index],
            s_name[index],
            fumble_recovery_2_player_name[i]
          )
        fumble_recovery_2_team[i] <-
          scalar_if_else(
            is.na(fumble_recovery_2_team[i]) &
              fumble_recovery_1_player_name[i] != s_name[index],
            # fumble_recovery_1_team[i] != s_team[index],
            s_team[index],
            fumble_recovery_2_team[i]
          )
        fumble_recovery_2_yards[i] <-
          scalar_if_else(
            is.na(fumble_recovery_2_yards[i]) &
              fumble_recovery_1_player_name[i] != s_name[index],
            # fumble_recovery_1_yards[i] != s_yards[index],
            s_yards[index],
            fumble_recovery_2_yards[i]
          )
      },
      "57" = {
        fumble[i] <- 1
        lateral_recovery[i] <- 1
      },
      "58" = {
        touchdown[i] <- 1
        td_team[i] <- s_team[index]
        td_player_id[i] <- s_id[index]
        td_player_name[i] <- s_name[index]
        fumble[i] <- 1
        lateral_recovery[i] <- 1
      },
      "59" = {
        fumble[i] <- 1
        fumble_recovery_1_player_id[i] <-
          scalar_if_else(
            is.na(fumble_recovery_1_player_id[i]),
            s_id[index],
            fumble_recovery_1_player_id[i]
          )
        fumble_recovery_1_player_name[i] <-
          scalar_if_else(
            is.na(fumble_recovery_1_player_name[i]),
            s_name[index],
            fumble_recovery_1_player_name[i]
          )
        fumble_recovery_1_team[i] <-
          scalar_if_else(
            is.na(fumble_recovery_1_team[i]),
            s_team[index],
            fumble_recovery_1_team[i]
          )
        fumble_recovery_1_yards[i] <-
          scalar_if_else(
            is.na(fumble_recovery_1_yards[i]),
            s_yards[index],
            fumble_recovery_1_yards[i]
          )
        fumble_recovery_2_player_id[i] <-
          scalar_if_else(
            is.na(fumble_recovery_2_player_id[i]) &
              fumble_recovery_1_player_id[i] != s_id[index],
            s_id[index],
            fumble_recovery_2_player_id[i]
          )
        fumble_recovery_2_player_name[i] <-
          scalar_if_else(
            is.na(fumble_recovery_2_player_name[i]) &
              fumble_recovery_1_player_name[i] != s_name[index],
            s_name[index],
            fumble_recovery_2_player_name[i]
          )
        fumble_recovery_2_team[i] <-
          scalar_if_else(
            is.na(fumble_recovery_2_team[i]) &
              fumble_recovery_1_player_name[i] != s_name[index],
            # fumble_recovery_1_team[i] != s_team[index],
            s_team[index],
            fumble_recovery_2_team[i]
          )
        fumble_recovery_2_yards[i] <-
          scalar_if_else(
            is.na(fumble_recovery_2_yards[i]) &
              fumble_recovery_1_player_name[i] != s_name[index],
            # fumble_recovery_1_yards[i] != s_yards[index],
            s_yards[index],
            fumble_recovery_2_yards[i]
          )
      },
      "60" = {
        touchdown[i] <- 1
        return_touchdown[i] <- 1
        td_team[i] <- s_team[index]
        td_player_id[i] <- s_id[index]
        td_player_name[i] <- s_name[index]
        fumble[i] <- 1
        fumble_recovery_1_player_id[i] <-
          scalar_if_else(
            is.na(fumble_recovery_1_player_id[i]),
            s_id[index],
            fumble_recovery_1_player_id[i]
          )
        fumble_recovery_1_player_name[i] <-
          scalar_if_else(
            is.na(fumble_recovery_1_player_name[i]),
            s_name[index],
            fumble_recovery_1_player_name[i]
          )
        fumble_recovery_1_team[i] <-
          scalar_if_else(
            is.na(fumble_recovery_1_team[i]),
            s_team[index],
            fumble_recovery_1_team[i]
          )
        fumble_recovery_1_yards[i] <-
          scalar_if_else(
            is.na(fumble_recovery_1_yards[i]),
            s_yards[index],
            fumble_recovery_1_yards[i]
          )
        fumble_recovery_2_player_id[i] <-
          scalar_if_else(
            is.na(fumble_recovery_2_player_id[i]) &
              fumble_recovery_1_player_id[i] != s_id[index],
            s_id[index],
            fumble_recovery_2_player_id[i]
          )
        fumble_recovery_2_player_name[i] <-
          scalar_if_else(
            is.na(fumble_recovery_2_player_name[i]) &
              fumble_recovery_1_player_name[i] != s_name[index],
            s_name[index],
            fumble_recovery_2_player_name[i]
          )
        fumble_recovery_2_team[i] <-
          scalar_if_else(
            is.na(fumble_recovery_2_team[i]) &
              fumble_recovery_1_player_name[i] != s_name[index],
            # fumble_recovery_1_team[i] != s_team[index],
            s_team[index],
            fumble_recovery_2_team[i]
          )
        fumble_recovery_2_yards[i] <-
          scalar_if_else(
            is.na(fumble_recovery_2_yards[i]) &
              fumble_recovery_1_player_name[i] != s_name[index],
            # fumble_recovery_1_yards[i] != s_yards[index],
            s_yards[index],
            fumble_recovery_2_yards[i]
          )
      },
      "61" = {
        fumble[i] <- 1
        lateral_recovery[i] <- 1
      },
      "62" = {
        touchdown[i] <- 1
        return_touchdown[i] <- 1
        td_team[i] <- s_team[index]
        td_player_id[i] <- s_id[index]
        td_player_name[i] <- s_name[index]
        fumble[i] <- 1
        lateral_recovery[i] <- 1
      },
      "63" = {
        NULL
      },
      "64" = {
        touchdown[i] <- 1
        td_team[i] <- s_team[index]
        td_player_id[i] <- s_id[index]
        td_player_name[i] <- s_name[index]
      },
      "68" = {
        timeout[i] <- 1
        timeout_team[i] <- s_team[index]
      },
      "69" = {
        field_goal_missed[i] <- 1
        field_goal_attempt[i] <- 1
        kicker_player_id[i] <- s_id[index]
        kicker_player_name[i] <- s_name[index]
        kick_distance[i] <- s_yards[index]
      },
      "70" = {
        field_goal_made[i] <- 1
        field_goal_attempt[i] <- 1
        kicker_player_id[i] <- s_id[index]
        kicker_player_name[i] <- s_name[index]
        kick_distance[i] <- s_yards[index]
      },
      "71" = {
        field_goal_blocked[i] <- 1
        field_goal_attempt[i] <- 1
        kicker_player_id[i] <- s_id[index]
        kicker_player_name[i] <- s_name[index]
        kick_distance[i] <- s_yards[index]
      },
      "72" = {
        extra_point_good[i] <- 1
        extra_point_attempt[i] <- 1
        kicker_player_id[i] <- s_id[index]
        kicker_player_name[i] <- s_name[index]
      },
      "73" = {
        extra_point_failed[i] <- 1
        extra_point_attempt[i] <- 1
        kicker_player_id[i] <- s_id[index]
        kicker_player_name[i] <- s_name[index]
      },
      "74" = {
        extra_point_blocked[i] <- 1
        extra_point_attempt[i] <- 1
        kicker_player_id[i] <- s_id[index]
        kicker_player_name[i] <- s_name[index]
      },
      "75" = {
        two_point_rush_good[i] <- 1
        rush_attempt[i] <- 1
        two_point_attempt[i] <- 1
        rusher_player_id[i] <- s_id[index]
        rusher_player_name[i] <- s_name[index]
      },
      "76" = {
        two_point_rush_failed[i] <- 1
        rush_attempt[i] <- 1
        two_point_attempt[i] <- 1
        rusher_player_id[i] <- s_id[index]
        rusher_player_name[i] <- s_name[index]
      },
      "77" = {
        two_point_pass_good[i] <- 1
        pass_attempt[i] <- 1
        two_point_attempt[i] <- 1
        passer_player_id[i] <- s_id[index]
        passer_player_name[i] <- s_name[index]
      },
      "78" = {
        two_point_pass_failed[i] <- 1
        pass_attempt[i] <- 1
        two_point_attempt[i] <- 1
        passer_player_id[i] <- s_id[index]
        passer_player_name[i] <- s_name[index]
      },
      "79" = {
        solo_tackle[i] <- 1
        solo_tackle_1_player_id[i] <-
          scalar_if_else(
            is.na(solo_tackle_1_player_id[i]),
            s_id[index],
            solo_tackle_1_player_id[i]
          )
        solo_tackle_1_player_name[i] <-
          scalar_if_else(
            is.na(solo_tackle_1_player_name[i]),
            s_name[index],
            solo_tackle_1_player_name[i]
          )
        solo_tackle_1_team[i] <-
          scalar_if_else(
            is.na(solo_tackle_1_team[i]),
            s_team[index],
            solo_tackle_1_team[i]
          )
        solo_tackle_2_player_id[i] <-
          scalar_if_else(
            is.na(solo_tackle_2_player_id[i]) &
              solo_tackle_1_player_id[i] != s_id[index],
            s_id[index],
            solo_tackle_2_player_id[i]
          )
        solo_tackle_2_player_name[i] <-
          scalar_if_else(
            is.na(solo_tackle_2_player_name[i]) &
              solo_tackle_1_player_name[i] != s_name[index],
            s_name[index],
            solo_tackle_2_player_name[i]
          )
        solo_tackle_2_team[i] <-
          scalar_if_else(
            is.na(solo_tackle_2_team[i]) &
              solo_tackle_1_player_name[i] != s_name[index],
            # solo_tackle_1_team[i] != s_team[index],
            s_team[index],
            solo_tackle_2_team[i]
          )
      },
      "80" = {
        tackle_with_assist[i] <- 1
        tackle_with_assist_1_player_id[i] <-
          scalar_if_else(
            is.na(tackle_with_assist_1_player_id[i]),
            s_id[index],
            tackle_with_assist_1_player_id[i]
          )
        tackle_with_assist_1_player_name[i] <-
          scalar_if_else(
            is.na(tackle_with_assist_1_player_name[i]),
            s_name[index],
            tackle_with_assist_1_player_name[i]
          )
        tackle_with_assist_1_team[i] <-
          scalar_if_else(
            is.na(tackle_with_assist_1_team[i]),
            s_team[index],
            tackle_with_assist_1_team[i]
          )
        tackle_with_assist_2_player_id[i] <-
          scalar_if_else(
            is.na(tackle_with_assist_2_player_id[i]) &
              tackle_with_assist_1_player_id[i] != s_id[index],
            s_id[index],
            tackle_with_assist_2_player_id[i]
          )
        tackle_with_assist_2_player_name[i] <-
          scalar_if_else(
            is.na(tackle_with_assist_2_player_name[i]) &
              tackle_with_assist_1_player_name[i] != s_name[index],
            s_name[index],
            tackle_with_assist_2_player_name[i]
          )
        tackle_with_assist_2_team[i] <-
          scalar_if_else(
            is.na(tackle_with_assist_2_team[i]) &
              tackle_with_assist_1_player_name[i] != s_name[index],
            # tackle_with_assist_1_team[i] != s_team[index],
            s_team[index],
            tackle_with_assist_2_team[i]
          )
      },
      "81" = NULL,
      "82" = {
        # =81
        assist_tackle[i] <- 1
        assist_tackle_1_player_id[i] <-
          scalar_if_else(
            is.na(assist_tackle_1_player_id[i]),
            s_id[index],
            assist_tackle_1_player_id[i]
          )
        assist_tackle_1_player_name[i] <-
          scalar_if_else(
            is.na(assist_tackle_1_player_name[i]),
            s_name[index],
            assist_tackle_1_player_name[i]
          )
        assist_tackle_1_team[i] <-
          scalar_if_else(
            is.na(assist_tackle_1_team[i]),
            s_team[index],
            assist_tackle_1_team[i]
          )
        assist_tackle_2_player_id[i] <-
          scalar_if_else(
            is.na(assist_tackle_2_player_id[i]) &
              assist_tackle_1_player_id[i] != s_id[index],
            s_id[index],
            assist_tackle_2_player_id[i]
          )
        assist_tackle_2_player_name[i] <-
          scalar_if_else(
            is.na(assist_tackle_2_player_name[i]) &
              assist_tackle_1_player_name[i] != s_name[index],
            s_name[index],
            assist_tackle_2_player_name[i]
          )
        assist_tackle_2_team[i] <-
          scalar_if_else(
            is.na(assist_tackle_2_team[i]) &
              assist_tackle_1_player_name[i] != s_name[index],
            # assist_tackle_1_team[i] != s_team[index],
            s_team[index],
            assist_tackle_2_team[i]
          )
        assist_tackle_3_player_id[i] <-
          scalar_if_else(
            (is.na(assist_tackle_3_player_id[i]) &
              assist_tackle_1_player_id[i] != s_id[index] &
              assist_tackle_2_player_id[i] != s_id[index]),
            s_id[index],
            assist_tackle_3_player_id[i]
          )
        assist_tackle_3_player_name[i] <-
          scalar_if_else(
            (is.na(assist_tackle_3_player_name[i]) &
              assist_tackle_1_player_name[i] != s_name[index] &
              assist_tackle_2_player_name[i] != s_name[index]),
            s_name[index],
            assist_tackle_3_player_name[i]
          )
        assist_tackle_3_team[i] <-
          scalar_if_else(
            (is.na(assist_tackle_3_team[i]) &
              assist_tackle_1_player_name[i] != s_name[index] &
              assist_tackle_2_player_name[i] != s_name[index]),
            # assist_tackle_1_team[i] != s_team[index] &
            #  assist_tackle_2_team[i] != s_team[index]),
            s_team[index],
            assist_tackle_3_team[i]
          )
        assist_tackle_4_player_id[i] <-
          scalar_if_else(
            (is.na(assist_tackle_4_player_id[i]) &
              assist_tackle_1_player_id[i] != s_id[index] &
              assist_tackle_2_player_id[i] != s_id[index] &
              assist_tackle_3_player_id[i] != s_id[index]),
            s_id[index],
            assist_tackle_4_player_id[i]
          )
        assist_tackle_4_player_name[i] <-
          scalar_if_else(
            (is.na(assist_tackle_4_player_name[i]) &
              assist_tackle_1_player_name[i] != s_name[index] &
              assist_tackle_2_player_name[i] != s_name[index] &
              assist_tackle_3_player_name[i] != s_name[index]),
            s_name[index],
            assist_tackle_4_player_name[i]
          )
        assist_tackle_4_team[i] <-
          scalar_if_else(
            (is.na(assist_tackle_4_team[i]) &
              assist_tackle_1_player_name[i] != s_name[index] &
              assist_tackle_2_player_name[i] != s_name[index] &
              assist_tackle_3_player_name[i] != s_name[index]),
            # assist_tackle_1_team[i] != s_team[index] &
            #  assist_tackle_2_team[i] != s_team[index] &
            #  assist_tackle_3_team[i] != s_team[index]),
            s_team[index],
            assist_tackle_4_team[i]
          )
      },
      "83" = {
        sack[i] <- 1
        sack_player_id[i] <- s_id[index]
        sack_player_name[i] <- s_name[index]
      },
      "84" = {
        sack[i] <- 1
        assist_tackle[i] <- 1
        half_sack_1_player_id[i] <-
          scalar_if_else(
            is.na(half_sack_1_player_id[i]),
            s_id[index],
            half_sack_1_player_id[i]
          )
        half_sack_1_player_name[i] <-
          scalar_if_else(
            is.na(half_sack_1_player_name[i]),
            s_name[index],
            half_sack_1_player_name[i]
          )
        half_sack_2_player_id[i] <-
          scalar_if_else(
            is.na(half_sack_2_player_id[i]) &
              half_sack_1_player_id[i] != s_id[index],
            s_id[index],
            half_sack_2_player_id[i]
          )
        half_sack_2_player_name[i] <-
          scalar_if_else(
            is.na(half_sack_2_player_name[i]) &
              half_sack_1_player_name[i] != s_name[index],
            s_name[index],
            half_sack_2_player_name[i]
          )
      },
      "85" = {
        pass_defense_1_player_id[i] <-
          scalar_if_else(
            is.na(pass_defense_1_player_id[i]),
            s_id[index],
            pass_defense_1_player_id[i]
          )
        pass_defense_1_player_name[i] <-
          scalar_if_else(
            is.na(pass_defense_1_player_name[i]),
            s_name[index],
            pass_defense_1_player_name[i]
          )
        pass_defense_2_player_id[i] <-
          scalar_if_else(
            is.na(pass_defense_2_player_id[i]) &
              pass_defense_1_player_id[i] != s_id[index],
            s_id[index],
            pass_defense_2_player_id[i]
          )
        pass_defense_2_player_name[i] <-
          scalar_if_else(
            is.na(pass_defense_2_player_name[i]) &
              pass_defense_1_player_name[i] != s_name[index],
            s_name[index],
            pass_defense_2_player_name[i]
          )
      },
      "86" = {
        punt_attempt[i] <- 1
        blocked_player_id[i] <- s_id[index]
        blocked_player_name[i] <- s_name[index]
      },
      "87" = {
        blocked_player_id[i] <- s_id[index]
        blocked_player_name[i] <- s_name[index]
      },
      "88" = {
        field_goal_attempt[i] <- 1
        blocked_player_id[i] <- s_id[index]
        blocked_player_name[i] <- s_name[index]
      },
      "89" = {
        safety[i] <- 1
        safety_player_id[i] <- s_id[index]
        safety_player_name[i] <- s_name[index]
      },
      "90" = NULL,
      "91" = {
        fumble[i] <- 1
        forced_fumble_player_1_player_id[i] <-
          scalar_if_else(
            is.na(forced_fumble_player_1_player_id[i]),
            s_id[index],
            forced_fumble_player_1_player_id[i]
          )
        forced_fumble_player_1_player_name[i] <-
          scalar_if_else(
            is.na(forced_fumble_player_1_player_name[i]),
            s_name[index],
            forced_fumble_player_1_player_name[i]
          )
        forced_fumble_player_1_team[i] <-
          scalar_if_else(
            is.na(forced_fumble_player_1_team[i]),
            s_team[index],
            forced_fumble_player_1_team[i]
          )
        forced_fumble_player_2_player_id[i] <-
          scalar_if_else(
            is.na(forced_fumble_player_2_player_id[i]) &
              forced_fumble_player_1_player_id[i] != s_id[index],
            s_id[index],
            forced_fumble_player_2_player_id[i]
          )
        forced_fumble_player_2_player_name[i] <-
          scalar_if_else(
            is.na(forced_fumble_player_2_player_name[i]) &
              forced_fumble_player_1_player_name[i] != s_name[index],
            s_name[index],
            forced_fumble_player_2_player_name[i]
          )
        forced_fumble_player_2_team[i] <-
          scalar_if_else(
            is.na(forced_fumble_player_2_team[i]) &
              forced_fumble_player_1_player_name[i] != s_name[index],
            # forced_fumble_player_1_team[i] != s_team[index],
            s_team[index],
            forced_fumble_player_2_team[i]
          )
      },
      "93" = {
        penalty[i] <- 1
        penalty_player_id[i] <- s_id[index]
        penalty_player_name[i] <- s_name[index]
        penalty_team[i] <- s_team[index]
        penalty_yards[i] <- s_yards[index]
      },
      "95" = {
        tackled_for_loss[i] <- 1
      },
      "96" = {
        extra_point_safety[i] <- 1
        extra_point_attempt[i] <- 1
      },
      "99" = {
        two_point_rush_safety[i] <- 1
        rush_attempt[i] <- 1
        two_point_attempt[i] <- 1
        rusher_player_id[i] <- s_id[index]
        rusher_player_name[i] <- s_name[index]
      },
      "100" = {
        two_point_pass_safety[i] <- 1
        pass_attempt[i] <- 1
        two_point_attempt[i] <- 1
        passer_player_id[i] <- s_id[index]
        passer_player_name[i] <- s_name[index]
      },
      "102" = {
        kickoff_downed[i] <- 1
        kickoff_attempt[i] <- 1
      },
      "103" = {
        lateral_sack_player_id[i] <- s_id[index]
        lateral_sack_player_name[i] <- s_name[index]
      },
      "104" = {
        two_point_pass_reception_good[i] <- 1
        pass_attempt[i] <- 1
        two_point_attempt[i] <- 1
        receiver_player_id[i] <- s_id[index]
        receiver_player_name[i] <- s_name[index]
      },
      "105" = {
        two_point_pass_reception_failed[i] <- 1
        pass_attempt[i] <- 1
        two_point_attempt[i] <- 1
        receiver_player_id[i] <- s_id[index]
        receiver_player_name[i] <- s_name[index]
      },
      "106" = {
        fumble_lost[i] <- 1
        fumble[i] <- 1
        fumbled_1_player_id[i] <-
          scalar_if_else(
            is.na(fumbled_1_player_id[i]),
            s_id[index],
            fumbled_1_player_id[i]
          )
        fumbled_1_player_name[i] <-
          scalar_if_else(
            is.na(fumbled_1_player_name[i]),
            s_name[index],
            fumbled_1_player_name[i]
          )
        fumbled_1_team[i] <-
          scalar_if_else(
            is.na(fumbled_1_team[i]),
            s_team[index],
            fumbled_1_team[i]
          )
        fumbled_2_player_id[i] <-
          scalar_if_else(
            is.na(fumbled_2_player_id[i]) &
              fumbled_1_player_id[i] != s_id[index],
            s_id[index],
            fumbled_2_player_id[i]
          )
        fumbled_2_player_name[i] <-
          scalar_if_else(
            is.na(fumbled_2_player_name[i]) &
              fumbled_1_player_name[i] != s_name[index],
            s_name[index],
            fumbled_2_player_name[i]
          )
        fumbled_2_team[i] <-
          scalar_if_else(
            is.na(fumbled_2_team[i]) &
              fumbled_1_player_name[i] != s_name[index],
            # fumbled_1_team[i] != s_team[index],
            s_team[index],
            fumbled_2_team[i]
          )
      },
      "107" = {
        own_kickoff_recovery[i] <- 1
        kickoff_attempt[i] <- 1
        own_kickoff_recovery_player_id[i] <- s_id[index]
        own_kickoff_recovery_player_name[i] <- s_name[index]
      },
      "108" = {
        own_kickoff_recovery_td[i] <- 1
        touchdown[i] <- 1
        td_team[i] <- s_team[index]
        td_player_id[i] <- s_id[index]
        td_player_name[i] <- s_name[index]
        kickoff_attempt[i] <- 1
        own_kickoff_recovery_player_id[i] <- s_id[index]
        own_kickoff_recovery_player_name[i] <- s_name[index]
      },
      "110" = {
        qb_hit[i] <- 1
        qb_hit_1_player_id[i] <-
          scalar_if_else(
            is.na(qb_hit_1_player_id[i]),
            s_id[index],
            qb_hit_1_player_id[i]
          )
        qb_hit_1_player_name[i] <-
          scalar_if_else(
            is.na(qb_hit_1_player_name[i]),
            s_name[index],
            qb_hit_1_player_name[i]
          )
        qb_hit_2_player_id[i] <-
          scalar_if_else(
            is.na(qb_hit_2_player_id[i]) &
              qb_hit_1_player_id[i] != s_id[index],
            s_id[index],
            qb_hit_2_player_id[i]
          )
        qb_hit_2_player_name[i] <-
          scalar_if_else(
            is.na(qb_hit_2_player_name[i]) &
              qb_hit_1_player_name[i] != s_name[index],
            s_name[index],
            qb_hit_2_player_name[i]
          )
      },
      "111" = {
        pass_attempt[i] <- 1
        complete_pass[i] <- 1
        passer_player_id[i] <- s_id[index]
        passer_player_name[i] <- s_name[index]
        air_yards[i] <- s_yards[index]
      },
      "112" = {
        pass_attempt[i] <- 1
        passer_player_id[i] <- s_id[index]
        passer_player_name[i] <- s_name[index]
        air_yards[i] <- s_yards[index]
      },
      "113" = {
        pass_attempt[i] <- 1
        complete_pass[i] <- 1
        if (is.na(receiver_player_id[i])) {
          receiver_player_id[i] <- s_id[index]
          receiver_player_name[i] <- s_name[index]
        }
        if (is.na(yards_after_catch[i])) {
          yards_after_catch[i] <- s_yards[index]
        }
      },
      "115" = {
        pass_attempt[i] <- 1
        receiver_player_id[i] <- s_id[index]
        receiver_player_name[i] <- s_name[index]
      },
      "120" = {
        tackle_for_loss_1_player_id[i] <-
          scalar_if_else(
            is.na(tackle_for_loss_1_player_id[i]),
            s_id[index],
            tackle_for_loss_1_player_id[i]
          )
        tackle_for_loss_1_player_name[i] <-
          scalar_if_else(
            is.na(tackle_for_loss_1_player_name[i]),
            s_name[index],
            tackle_for_loss_1_player_name[i]
          )
        tackle_for_loss_2_player_id[i] <-
          scalar_if_else(
            is.na(tackle_for_loss_2_player_id[i]) &
              tackle_for_loss_1_player_id[i] != s_id[index],
            s_id[index],
            tackle_for_loss_2_player_id[i]
          )
        tackle_for_loss_2_player_name[i] <-
          scalar_if_else(
            is.na(tackle_for_loss_2_player_name[i]) &
              tackle_for_loss_1_player_name[i] != s_name[index],
            s_name[index],
            tackle_for_loss_2_player_name[i]
          )
      },
      "201" = NULL,
      "211" = NULL,
      "212" = NULL,
      "213" = NULL,
      "301" = {
        extra_point_aborted[i] <- 1
        extra_point_attempt[i] <- 1
      },
      "401" = NULL,
      "402" = {
        # tackle for loss player information is recorded in stat id 120
        NULL
      },
      "403" = {
        defensive_two_point_attempt[i] <- 1
      },
      "404" = {
        defensive_two_point_conv[i] <- 1
      },
      "405" = {
        defensive_extra_point_attempt[i] <- 1
      },
      "406" = {
        defensive_extra_point_conv[i] <- 1
      },
      "410" = {
        kickoff_attempt[i] <- 1
        kicker_player_id[i] <- s_id[index]
        kicker_player_name[i] <- s_name[index]
      },
      "420" = {
        two_point_return[i] <- 1
        two_point_attempt[i] <- 1
      }
    )
  }

  out <- list(
    play_id = as.integer(play_ids),
    punt_blocked = punt_blocked,
    first_down_rush = first_down_rush,
    first_down_pass = first_down_pass,
    first_down_penalty = first_down_penalty,
    third_down_converted = third_down_converted,
    third_down_failed = third_down_failed,
    fourth_down_converted = fourth_down_converted,
    fourth_down_failed = fourth_down_failed,
    incomplete_pass = incomplete_pass,
    interception = interception,
    punt_inside_twenty = punt_inside_twenty,
    punt_in_endzone = punt_in_endzone,
    punt_out_of_bounds = punt_out_of_bounds,
    punt_downed = punt_downed,
    punt_fair_catch = punt_fair_catch,
    kickoff_inside_twenty = kickoff_inside_twenty,
    kickoff_in_endzone = kickoff_in_endzone,
    kickoff_out_of_bounds = kickoff_out_of_bounds,
    kickoff_fair_catch = kickoff_fair_catch,
    fumble_forced = fumble_forced,
    fumble_not_forced = fumble_not_forced,
    fumble_out_of_bounds = fumble_out_of_bounds,
    timeout = timeout,
    field_goal_missed = field_goal_missed,
    field_goal_made = field_goal_made,
    field_goal_blocked = field_goal_blocked,
    extra_point_good = extra_point_good,
    extra_point_failed = extra_point_failed,
    extra_point_blocked = extra_point_blocked,
    two_point_rush_good = two_point_rush_good,
    two_point_rush_failed = two_point_rush_failed,
    two_point_pass_good = two_point_pass_good,
    two_point_pass_failed = two_point_pass_failed,
    solo_tackle = solo_tackle,
    safety = safety,
    penalty = penalty,
    tackled_for_loss = tackled_for_loss,
    extra_point_safety = extra_point_safety,
    two_point_rush_safety = two_point_rush_safety,
    two_point_pass_safety = two_point_pass_safety,
    kickoff_downed = kickoff_downed,
    two_point_pass_reception_good = two_point_pass_reception_good,
    two_point_pass_reception_failed = two_point_pass_reception_failed,
    fumble_lost = fumble_lost,
    own_kickoff_recovery = own_kickoff_recovery,
    own_kickoff_recovery_td = own_kickoff_recovery_td,
    qb_hit = qb_hit,
    extra_point_aborted = extra_point_aborted,
    two_point_return = two_point_return,
    rush_attempt = rush_attempt,
    pass_attempt = pass_attempt,
    sack = sack,
    touchdown = touchdown,
    pass_touchdown = pass_touchdown,
    rush_touchdown = rush_touchdown,
    return_touchdown = return_touchdown,
    extra_point_attempt = extra_point_attempt,
    two_point_attempt = two_point_attempt,
    field_goal_attempt = field_goal_attempt,
    kickoff_attempt = kickoff_attempt,
    punt_attempt = punt_attempt,
    fumble = fumble,
    complete_pass = complete_pass,
    assist_tackle = assist_tackle,
    lateral_reception = lateral_reception,
    lateral_rush = lateral_rush,
    lateral_return = lateral_return,
    lateral_recovery = lateral_recovery,
    passer_player_id = passer_player_id,
    passer_player_name = passer_player_name,
    receiver_player_id = receiver_player_id,
    receiver_player_name = receiver_player_name,
    rusher_player_id = rusher_player_id,
    rusher_player_name = rusher_player_name,
    lateral_receiver_player_id = lateral_receiver_player_id,
    lateral_receiver_player_name = lateral_receiver_player_name,
    lateral_rusher_player_id = lateral_rusher_player_id,
    lateral_rusher_player_name = lateral_rusher_player_name,
    lateral_sack_player_id = lateral_sack_player_id,
    lateral_sack_player_name = lateral_sack_player_name,
    interception_player_id = interception_player_id,
    interception_player_name = interception_player_name,
    lateral_interception_player_id = lateral_interception_player_id,
    lateral_interception_player_name = lateral_interception_player_name,
    punt_returner_player_id = punt_returner_player_id,
    punt_returner_player_name = punt_returner_player_name,
    lateral_punt_returner_player_id = lateral_punt_returner_player_id,
    lateral_punt_returner_player_name = lateral_punt_returner_player_name,
    kickoff_returner_player_name = kickoff_returner_player_name,
    kickoff_returner_player_id = kickoff_returner_player_id,
    lateral_kickoff_returner_player_id = lateral_kickoff_returner_player_id,
    lateral_kickoff_returner_player_name = lateral_kickoff_returner_player_name,
    punter_player_id = punter_player_id,
    punter_player_name = punter_player_name,
    kicker_player_name = kicker_player_name,
    kicker_player_id = kicker_player_id,
    own_kickoff_recovery_player_id = own_kickoff_recovery_player_id,
    own_kickoff_recovery_player_name = own_kickoff_recovery_player_name,
    blocked_player_id = blocked_player_id,
    blocked_player_name = blocked_player_name,
    tackle_for_loss_1_player_id = tackle_for_loss_1_player_id,
    tackle_for_loss_1_player_name = tackle_for_loss_1_player_name,
    tackle_for_loss_2_player_id = tackle_for_loss_2_player_id,
    tackle_for_loss_2_player_name = tackle_for_loss_2_player_name,
    qb_hit_1_player_id = qb_hit_1_player_id,
    qb_hit_1_player_name = qb_hit_1_player_name,
    qb_hit_2_player_id = qb_hit_2_player_id,
    qb_hit_2_player_name = qb_hit_2_player_name,
    forced_fumble_player_1_team = forced_fumble_player_1_team,
    forced_fumble_player_1_player_id = forced_fumble_player_1_player_id,
    forced_fumble_player_1_player_name = forced_fumble_player_1_player_name,
    forced_fumble_player_2_team = forced_fumble_player_2_team,
    forced_fumble_player_2_player_id = forced_fumble_player_2_player_id,
    forced_fumble_player_2_player_name = forced_fumble_player_2_player_name,
    solo_tackle_1_team = solo_tackle_1_team,
    solo_tackle_2_team = solo_tackle_2_team,
    solo_tackle_1_player_id = solo_tackle_1_player_id,
    solo_tackle_2_player_id = solo_tackle_2_player_id,
    solo_tackle_1_player_name = solo_tackle_1_player_name,
    solo_tackle_2_player_name = solo_tackle_2_player_name,
    assist_tackle_1_player_id = assist_tackle_1_player_id,
    assist_tackle_1_player_name = assist_tackle_1_player_name,
    assist_tackle_1_team = assist_tackle_1_team,
    assist_tackle_2_player_id = assist_tackle_2_player_id,
    assist_tackle_2_player_name = assist_tackle_2_player_name,
    assist_tackle_2_team = assist_tackle_2_team,
    assist_tackle_3_player_id = assist_tackle_3_player_id,
    assist_tackle_3_player_name = assist_tackle_3_player_name,
    assist_tackle_3_team = assist_tackle_3_team,
    assist_tackle_4_player_id = assist_tackle_4_player_id,
    assist_tackle_4_player_name = assist_tackle_4_player_name,
    assist_tackle_4_team = assist_tackle_4_team,
    tackle_with_assist = tackle_with_assist,
    tackle_with_assist_1_player_id = tackle_with_assist_1_player_id,
    tackle_with_assist_1_player_name = tackle_with_assist_1_player_name,
    tackle_with_assist_1_team = tackle_with_assist_1_team,
    tackle_with_assist_2_player_id = tackle_with_assist_2_player_id,
    tackle_with_assist_2_player_name = tackle_with_assist_2_player_name,
    tackle_with_assist_2_team = tackle_with_assist_2_team,
    pass_defense_1_player_id = pass_defense_1_player_id,
    pass_defense_1_player_name = pass_defense_1_player_name,
    pass_defense_2_player_id = pass_defense_2_player_id,
    pass_defense_2_player_name = pass_defense_2_player_name,
    fumbled_1_team = fumbled_1_team,
    fumbled_1_player_id = fumbled_1_player_id,
    fumbled_1_player_name = fumbled_1_player_name,
    fumbled_2_player_id = fumbled_2_player_id,
    fumbled_2_player_name = fumbled_2_player_name,
    fumbled_2_team = fumbled_2_team,
    fumble_recovery_1_team = fumble_recovery_1_team,
    fumble_recovery_1_yards = fumble_recovery_1_yards,
    fumble_recovery_1_player_id = fumble_recovery_1_player_id,
    fumble_recovery_1_player_name = fumble_recovery_1_player_name,
    fumble_recovery_2_team = fumble_recovery_2_team,
    fumble_recovery_2_yards = fumble_recovery_2_yards,
    fumble_recovery_2_player_id = fumble_recovery_2_player_id,
    fumble_recovery_2_player_name = fumble_recovery_2_player_name,
    td_team = td_team,
    return_team = return_team,
    timeout_team = timeout_team,
    yards_gained = yards_gained,
    return_yards = return_yards,
    air_yards = air_yards,
    yards_after_catch = yards_after_catch,
    penalty_team = penalty_team,
    penalty_player_id = penalty_player_id,
    penalty_player_name = penalty_player_name,
    penalty_yards = penalty_yards,
    kick_distance = kick_distance,
    defensive_two_point_attempt = defensive_two_point_attempt,
    defensive_two_point_conv = defensive_two_point_conv,
    defensive_extra_point_attempt = defensive_extra_point_attempt,
    defensive_extra_point_conv = defensive_extra_point_conv,
    penalty_fix = penalty_fix,
    return_penalty_fix = return_penalty_fix,
    rushing_yards = rushing_yards,
    lateral_rushing_yards = lateral_rushing_yards,
    passing_yards = passing_yards,
    receiving_yards = receiving_yards,
    lateral_receiving_yards = lateral_receiving_yards,
    td_player_id = td_player_id,
    td_player_name = td_player_name,
    sack_player_id = sack_player_id,
    sack_player_name = sack_player_name,
    half_sack_1_player_id = half_sack_1_player_id,
    half_sack_1_player_name = half_sack_1_player_name,
    half_sack_2_player_id = half_sack_2_player_id,
    half_sack_2_player_name = half_sack_2_player_name,
    safety_player_name = safety_player_name,
    safety_player_id = safety_player_id
  )
  data.table::setDT(out)
  out
}

# Scalar version of dplyr::if_else(): a missing condition returns NA
scalar_if_else <- function(condition, true, false) {
  if (is.na(condition)) {
    NA
  } else if (condition) {
    true
  } else {
    false
  }
}
