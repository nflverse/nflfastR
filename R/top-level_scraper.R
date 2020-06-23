################################################################################
# Author: Sebastian Carl
# Purpose: Top-Level functions which will be made availabe through the package
# Code Style Guide: styler::tidyverse_style()
################################################################################

#' Get NFL Play by Play Data
#'
#' @param game_ids Vector of character ids (see details for further information)
#' @param source Character - must now be \code{nfl} or unspecified (see details for further information)
#' @param pp Logical - either \code{TRUE} or \code{FALSE} (see details for further information)
#' @details To load valid game_ids please use the package function \code{\link{fast_scraper_schedules}}.
#'
#' The \code{source} parameter controls from which source the data is being
#' scraped. The old parameters \code{rs} as well as \code{gc}
#' are not valid anymore. Please use \code{nfl} or leave unspecified.
#' The \code{pp} parameter controls if the scraper should use parallel processing.
#' Please note that the initiating process takes a few seconds which means it
#' may be better to set \code{pp = FALSE} if you are scraping just a few games.
#' @return Data frame where each individual row represents a single play for
#' all passed game_ids containing the following
#' detailed information (description partly extracted from nflscrapR):
#' \describe{
#' \item{play_id}{Numeric play id that when used with game_id and drive provides the unique identifier for a single play.}
#' \item{game_id}{Ten digit identifier for NFL game.}
#' \item{home_team}{String abbreviation for the home team.}
#' \item{away_team}{String abbreviation for the away team.}
#' \item{season_type}{'REG' or 'POST' indicating if the game belongs to regular or post season.}
#' \item{week}{Season week.}
#' \item{posteam}{String abbreviation for the team with possession.}
#' \item{posteam_type}{String indicating whether the posteam team is home or away.}
#' \item{defteam}{String abbreviation for the team on defense.}
#' \item{side_of_field}{String abbreviation for which team's side of the field the team with possession is currently on.}
#' \item{yardline_100}{Numeric distance in the number of yards from the opponent's endzone for the posteam.}
#' \item{game_date}{Date of the game.}
#' \item{quarter_seconds_remaining}{Numeric seconds remaining in the quarter.}
#' \item{half_seconds_remaining}{Numeric seconds remaining in the half.}
#' \item{game_seconds_remaining}{Numeric seconds remaining in the game.}
#' \item{game_half}{String indicating which half the play is in, either Half1, Half2, or Overtime.}
#' \item{quarter_end}{Binary indicator for whether or not the row of the data is marking the end of a quarter.}
#' \item{drive}{Numeric drive number in the game.}
#' \item{sp}{Binary indicator for whether or not a score occurred on the play.}
#' \item{qtr}{Quarter of the game (5 is overtime).}
#' \item{down}{The down for the given play.}
#' \item{goal_to_go}{Binary indicator for whether or not the posteam is in a goal down situation.}
#' \item{time}{Time at start of play provided in string format as minutes:seconds remaining in the quarter.}
#' \item{yrdln}{String indicating the current field position for a given play.}
#' \item{ydstogo}{Numeric yards in distance from either the first down marker or the endzone in goal down situations.}
#' \item{ydsnet}{Numeric value for total yards gained on the given drive.}
#' \item{desc}{Detailed string description for the given play.}
#' \item{play_type}{String indicating the type of play: pass (includes sacks), run (includes scrambles), punt, field_goal, kickoff, extra_point, qb_kneel, qb_spike, no_play (timeouts and penalties), and missing for rows indicating end of play.}
#' \item{yards_gained}{Numeric yards gained (or lost) for the given play.}
#' \item{shotgun}{Binary indicator for whether or not the play was in shotgun formation.}
#' \item{no_huddle}{Binary indicator for whether or not the play was in no_huddle formation.}
#' \item{qb_dropback}{Binary indicator for whether or not the QB dropped back on the play (pass attempt, sack, or scrambled).}
#' \item{qb_kneel}{Binary indicator for whether or not the QB took a knee.}
#' \item{qb_spike}{Binary indicator for whether or not the QB spiked the ball.}
#' \item{qb_scramble}{Binary indicator for whether or not the QB scrambled.}
#' \item{pass_length}{String indicator for pass length: short or deep.}
#' \item{pass_location}{String indicator for pass location: left, middle, or right.}
#' \item{air_yards}{Numeric value for distance in yards perpendicular to the line of scrimmage at where the targeted receiver either caught or didn't catch the ball.}
#' \item{yards_after_catch}{Numeric value for distance in yards perpendicular to the yard line where the receiver made the reception to where the play ended.}
#' \item{run_location}{String indicator for location of run: left, middle, or right.}
#' \item{run_gap}{String indicator for line gap of run: end, guard, or tackle}
#' \item{field_goal_result}{String indicator for result of field goal attempt: made, missed, or blocked.}
#' \item{kick_distance}{Numeric distance in yards for kickoffs, field goals, and punts.}
#' \item{extra_point_result}{String indicator for the result of the extra point attempt: good, failed, blocked, safety (touchback in defensive endzone is 1 point apparently), or aborted.}
#' \item{two_point_conv_result}{String indicator for result of two point conversion attempt: success, failure, safety (touchback in defensive endzone is 1 point apparently), or return.}
#' \item{home_timeouts_remaining}{Numeric timeouts remaining in the half for the home team.}
#' \item{away_timeouts_remaining}{Numeric timeouts remaining in the half for the away team.}
#' \item{timeout}{Binary indicator for whether or not a timeout was called.}
#' \item{timeout_team}{String abbreviation for which team called the timeout.}
#' \item{td_team}{String abbreviation for which team scored the touchdown.}
#' \item{posteam_timeouts_remaining}{Number of timeouts remaining for the possession team.}
#' \item{defteam_timeouts_remaining}{Number of timeouts remaining for the team on defense.}
#' \item{total_home_score}{Score for the home team at the start of the play.}
#' \item{total_away_score}{Score for the away team at the start of the play.}
#' \item{posteam_score}{Score the posteam at the start of the play.}
#' \item{defteam_score}{Score the defteam at the start of the play.}
#' \item{score_differential}{Score differential between the posteam and defteam at the start of the play.}
#' \item{posteam_score_post}{Score for the posteam at the end of the play.}
#' \item{defteam_score_post}{Score for the defteam at the end of the play.}
#' \item{score_differential_post}{Score differential between the posteam and defteam at the end of the play.}
#' \item{no_score_prob}{Predicted probability of no score occurring for the rest of the half based on the expected points model.}
#' \item{opp_fg_prob}{Predicted probability of the defteam scoring a FG next.}
#' \item{opp_safety_prob}{Predicted probability of the defteam scoring a safety next.}
#' \item{opp_td_prob}{Predicted probability of the defteam scoring a TD next.}
#' \item{fg_prob}{Predicted probability of the posteam scoring a FG next.}
#' \item{safety_prob}{Predicted probability of the posteam scoring a safety next.}
#' \item{td_prob}{Predicted probability of the posteam scoring a TD next.}
#' \item{extra_point_prob}{Predicted probability of the posteam scoring an extra point.}
#' \item{two_point_conversion_prob}{Predicted probability of the posteam scoring the two point conversion.}
#' \item{ep}{Using the scoring event probabilities, the estimated expected points with respect to the possession team for the given play.}
#' \item{epa}{Expected points added (EPA) by the posteam for the given play.}
#' \item{total_home_epa}{Cumulative total EPA for the home team in the game so far.}
#' \item{total_away_epa}{Cumulative total EPA for the away team in the game so far.}
#' \item{total_home_rush_epa}{Cumulative total rushing EPA for the home team in the game so far.}
#' \item{total_away_rush_epa}{Cumulative total rushing EPA for the away team in the game so far.}
#' \item{total_home_pass_epa}{Cumulative total passing EPA for the home team in the game so far.}
#' \item{total_away_pass_epa}{Cumulative total passing EPA for the away team in the game so far.}
#' \item{air_epa}{EPA from the air yards alone. For completions this represents the actual value provided through the air. For incompletions this represents the hypothetical value that could've been added through the air if the pass was completed.}
#' \item{yac_epa}{EPA from the yards after catch alone. For completions this represents the actual value provided after the catch. For incompletions this represents the difference between the hypothetical air_epa and the play's raw observed EPA (how much the incomplete pass cost the posteam).}
#' \item{comp_air_epa}{EPA from the air yards alone only for completions.}
#' \item{comp_yac_epa}{EPA from the yards after catch alone only for completions.}
#' \item{total_home_comp_air_epa}{Cumulative total completions air EPA for the home team in the game so far.}
#' \item{total_away_comp_air_epa}{Cumulative total completions air EPA for the away team in the game so far.}
#' \item{total_home_comp_yac_epa}{Cumulative total completions yac EPA for the home team in the game so far.}
#' \item{total_away_comp_yac_epa}{Cumulative total completions yac EPA for the away team in the game so far.}
#' \item{total_home_raw_air_epa}{Cumulative total raw air EPA for the home team in the game so far.}
#' \item{total_away_raw_air_epa}{Cumulative total raw air EPA for the away team in the game so far.}
#' \item{total_home_raw_yac_epa}{Cumulative total raw yac EPA for the home team in the game so far.}
#' \item{total_away_raw_yac_epa}{Cumulative total raw yac EPA for the away team in the game so far.}
#' \item{wp}{Estimated win probabiity for the posteam given the current situation at the start of the given play.}
#' \item{def_wp}{Estimated win probability for the defteam.}
#' \item{home_wp}{Estimated win probability for the home team.}
#' \item{away_wp}{Estimated win probability for the away team.}
#' \item{wpa}{Win probability added (WPA) for the posteam.}
#' \item{home_wp_post}{Estimated win probability for the home team at the start of the play.}
#' \item{away_wp_post}{Estimated win probability for the away team at the start of the play.}
#' \item{vegas_wp}{Estimated win probabiity for the posteam given the current situation at the start of the given play, incorporating pre-game Vegas line.}
#' \item{vegas_home_wp}{Estimated win probability for the home team incorporating pre-game Vegas line.}
#' \item{total_home_rush_wpa}{Cumulative total rushing WPA for the home team in the game so far.}
#' \item{total_away_rush_wpa}{Cumulative total rushing WPA for the away team in the game so far.}
#' \item{total_home_pass_wpa}{Cumulative total passing WPA for the home team in the game so far.}
#' \item{total_away_pass_wpa}{Cumulative total passing WPA for the away team in the game so far.}
#' \item{air_wpa}{WPA through the air (same logic as air_epa).}
#' \item{yac_wpa}{WPA from yards after the catch (same logic as yac_epa).}
#' \item{comp_air_wpa}{The air_wpa for completions only.}
#' \item{comp_yac_wpa}{The yac_wpa for completions only.}
#' \item{total_home_comp_air_wpa}{Cumulative total completions air WPA for the home team in the game so far.}
#' \item{total_away_comp_air_wpa}{Cumulative total completions air WPA for the away team in the game so far.}
#' \item{total_home_comp_yac_wpa}{Cumulative total completions yac WPA for the home team in the game so far.}
#' \item{total_away_comp_yac_wpa}{Cumulative total completions yac WPA for the away team in the game so far.}
#' \item{total_home_raw_air_wpa}{Cumulative total raw air WPA for the home team in the game so far.}
#' \item{total_away_raw_air_wpa}{Cumulative total raw air WPA for the away team in the game so far.}
#' \item{total_home_raw_yac_wpa}{Cumulative total raw yac WPA for the home team in the game so far.}
#' \item{total_away_raw_yac_wpa}{Cumulative total raw yac WPA for the away team in the game so far.}
#' \item{punt_blocked}{Binary indicator for if the punt was blocked.}
#' \item{first_down_rush}{Binary indicator for if a running play converted the first down.}
#' \item{first_down_pass}{Binary indicator for if a passing play converted the first down.}
#' \item{first_down_penalty}{Binary indicator for if a penalty converted the first down.}
#' \item{third_down_converted}{Binary indicator for if the first down was converted on third down.}
#' \item{third_down_failed}{Binary indicator for if the posteam failed to convert first down on third down.}
#' \item{fourth_down_converted}{Binary indicator for if the first down was converted on fourth down.}
#' \item{fourth_down_failed}{Binary indicator for if the posteam failed to convert first down on fourth down.}
#' \item{incomplete_pass}{Binary indicator for if the pass was incomplete.}
#' \item{touchback}{Binary indicator for if a touchback occurred on the play.}
#' \item{interception}{Binary indicator for if the pass was intercepted.}
#' \item{punt_inside_twenty}{Binary indicator for if the punt ended inside the twenty yard line.}
#' \item{punt_in_endzone}{Binary indicator for if the punt was in the endzone.}
#' \item{punt_out_of_bounds}{Binary indicator for if the punt went out of bounds.}
#' \item{punt_downed}{Binary indicator for if the punt was downed.}
#' \item{punt_fair_catch}{Binary indicator for if the punt was caught with a fair catch.}
#' \item{kickoff_inside_twenty}{Binary indicator for if the kickoff ended inside the twenty yard line.}
#' \item{kickoff_in_endzone}{Binary indicator for if the kickoff was in the endzone.}
#' \item{kickoff_out_of_bounds}{Binary indicator for if the kickoff went out of bounds.}
#' \item{kickoff_downed}{Binary indicator for if the kickoff was downed.}
#' \item{kickoff_fair_catch}{Binary indicator for if the kickoff was caught with a fair catch.}
#' \item{fumble_forced}{Binary indicator for if the fumble was forced.}
#' \item{fumble_not_forced}{Binary indicator for if the fumble was not forced.}
#' \item{fumble_out_of_bounds}{Binary indicator for if the fumble went out of bounds.}
#' \item{solo_tackle}{Binary indicator if the play had a solo tackle (could be multiple due to fumbles).}
#' \item{safety}{Binary indicator for whether or not a safety occurred.}
#' \item{penalty}{Binary indicator for whether or not a penalty occurred.}
#' \item{tackled_for_loss}{Binary indicator for whether or not a tackle for loss occurred.}
#' \item{fumble_lost}{Binary indicator for if the fumble was lost.}
#' \item{own_kickoff_recovery}{Binary indicator for if the kicking team recovered the kickoff.}
#' \item{own_kickoff_recovery_td}{Binary indicator for if the kicking team recovered the kickoff and scored a TD.}
#' \item{qb_hit}{Binary indicator if the QB was hit on the play.}
#' \item{rush_attempt}{Binary indicator for if the play was a run.}
#' \item{pass_attempt}{Binary indicator for if the play was a pass attempt (includes sacks).}
#' \item{sack}{Binary indicator for if the play ended in a sack.}
#' \item{touchdown}{Binary indicator for if the play resulted in a TD.}
#' \item{pass_touchdown}{Binary indicator for if the play resulted in a passing TD.}
#' \item{rush_touchdown}{Binary indicator for if the play resulted in a rushing TD.}
#' \item{return_touchdown}{Binary indicator for if the play resulted in a return TD.}
#' \item{extra_point_attempt}{Binary indicator for extra point attempt.}
#' \item{two_point_attempt}{Binary indicator for two point conversion attempt.}
#' \item{field_goal_attempt}{Binary indicator for field goal attempt.}
#' \item{kickoff_attempt}{Binary indicator for kickoff.}
#' \item{punt_attempt}{Binary indicator for punts.}
#' \item{fumble}{Binary indicator for if a fumble occurred.}
#' \item{complete_pass}{Binary indicator for if the pass was completed.}
#' \item{assist_tackle}{Binary indicator for if an assist tackle occurred.}
#' \item{lateral_reception}{Binary indicator for if a lateral occurred on the reception.}
#' \item{lateral_rush}{Binary indicator for if a lateral occurred on a run.}
#' \item{lateral_return}{Binary indicator for if a lateral occurred on a return.}
#' \item{lateral_recovery}{Binary indicator for if a lateral occurred on a fumble recovery.}
#' \item{passer_player_id}{Unique identifier for the player that attempted the pass.}
#' \item{passer_player_name}{String name for the player that attempted the pass.}
#' \item{receiver_player_id}{Unique identifier for the receiver that was targeted on the pass.}
#' \item{receiver_player_name}{String name for the targeted receiver.}
#' \item{rusher_player_id}{Unique identifier for the player that attempted the run.}
#' \item{rusher_player_name}{String name for the player that attempted the run.}
#' \item{lateral_receiver_player_id}{Unique identifier for the player that received the lateral on a reception.}
#' \item{lateral_receiver_player_name}{String name for the player that received the lateral on a reception.}
#' \item{lateral_rusher_player_id}{Unique identifier for the player that received the lateral on a run.}
#' \item{lateral_rusher_player_name}{String name for the player that received the lateral on a run.}
#' \item{lateral_sack_player_id}{Unique identifier for the player that received the lateral on a sack.}
#' \item{lateral_sack_player_name}{String name for the player that received the lateral on a sack.}
#' \item{interception_player_id}{Unique identifier for the player that intercepted the pass.}
#' \item{interception_player_name}{String name for the player that intercepted the pass.}
#' \item{lateral_interception_player_id}{Unique indentifier for the player that received the lateral on an interception.}
#' \item{lateral_interception_player_name}{String name for the player that received the lateral on an interception.}
#' \item{punt_returner_player_id}{Unique identifier for the punt returner.}
#' \item{punt_returner_player_name}{String name for the punt returner.}
#' \item{lateral_punt_returner_player_id}{Unique identifier for the player that received the lateral on a punt return.}
#' \item{lateral_punt_returner_player_name}{String name for the player that received the lateral on a punt return.}
#' \item{kickoff_returner_player_name}{String name for the kickoff returner.}
#' \item{kickoff_returner_player_id}{Unique identifier for the kickoff returner.}
#' \item{lateral_kickoff_returner_player_id}{Unique identifier for the player that received the lateral on a kickoff return.}
#' \item{lateral_kickoff_returner_player_name}{String name for the player that received the lateral on a kickoff return.}
#' \item{punter_player_id}{Unique identifier for the punter.}
#' \item{punter_player_name}{String name for the punter.}
#' \item{kicker_player_name}{String name for the kicker on FG or kickoff.}
#' \item{kicker_player_id}{Unique identifier for the kicker on FG or kickoff.}
#' \item{own_kickoff_recovery_player_id}{Unique identifier for the player that recovered their own kickoff.}
#' \item{own_kickoff_recovery_player_name}{String name for the player that recovered their own kickoff.}
#' \item{blocked_player_id}{Unique identifier for the player that blocked the punt or FG.}
#' \item{blocked_player_name}{String name for the player that blocked the punt or FG.}
#' \item{tackle_for_loss_1_player_id}{Unique identifier for one of the potential players with the tackle for loss.}
#' \item{tackle_for_loss_1_player_name}{String name for one of the potential players with the tackle for loss.}
#' \item{tackle_for_loss_2_player_id}{Unique identifier for one of the potential players with the tackle for loss.}
#' \item{tackle_for_loss_2_player_name}{String name for one of the potential players with the tackle for loss.}
#' \item{qb_hit_1_player_id}{Unique identifier for one of the potential players that hit the QB.}
#' \item{qb_hit_1_player_name}{String name for one of the potential players that hit the QB.}
#' \item{qb_hit_2_player_id}{Unique identifier for one of the potential players that hit the QB.}
#' \item{qb_hit_2_player_name}{String name for one of the potential players that hit the QB.}
#' \item{forced_fumble_player_1_team}{Team of one of the players with a forced fumble.}
#' \item{forced_fumble_player_1_player_id}{Unique identifier of one of the players with a forced fumble.}
#' \item{forced_fumble_player_1_player_name}{String name of one of the players with a forced fumble.}
#' \item{forced_fumble_player_2_team}{Team of one of the players with a forced fumble.}
#' \item{forced_fumble_player_2_player_id}{Unique identifier of one of the players with a forced fumble.}
#' \item{forced_fumble_player_2_player_name}{String name of one of the players with a forced fumble.}
#' \item{solo_tackle_1_team}{Team of one of the players with a solo tackle.}
#' \item{solo_tackle_2_team}{Team of one of the players with a solo tackle.}
#' \item{solo_tackle_1_player_id}{Unique identifier of one of the players with a solo tackle.}
#' \item{solo_tackle_2_player_id}{Unique identifier of one of the players with a solo tackle.}
#' \item{solo_tackle_1_player_name}{String name of one of the players with a solo tackle.}
#' \item{solo_tackle_2_player_name}{String name of one of the players with a solo tackle.}
#' \item{assist_tackle_1_player_id}{Unique identifier of one of the players with a tackle assist.}
#' \item{assist_tackle_1_player_name}{String name of one of the players with a tackle assist.}
#' \item{assist_tackle_1_team}{Team of one of the players with a tackle assist.}
#' \item{assist_tackle_2_player_id}{Unique identifier of one of the players with a tackle assist.}
#' \item{assist_tackle_2_player_name}{String name of one of the players with a tackle assist.}
#' \item{assist_tackle_2_team}{Team of one of the players with a tackle assist.}
#' \item{assist_tackle_3_player_id}{Unique identifier of one of the players with a tackle assist.}
#' \item{assist_tackle_3_player_name}{String name of one of the players with a tackle assist.}
#' \item{assist_tackle_3_team}{Team of one of the players with a tackle assist.}
#' \item{assist_tackle_4_player_id}{Unique identifier of one of the players with a tackle assist.}
#' \item{assist_tackle_4_player_name}{String name of one of the players with a tackle assist.}
#' \item{assist_tackle_4_team}{Team of one of the players with a tackle assist.}
#' \item{pass_defense_1_player_id}{Unique identifier of one of the players with a pass defense.}
#' \item{pass_defense_1_player_name}{String name of one of the players with a pass defense.}
#' \item{pass_defense_2_player_id}{Unique identifier of one of the players with a pass defense.}
#' \item{pass_defense_2_player_name}{String name of one of the players with a pass defense.}
#' \item{fumbled_1_team}{Team of one of the players with a fumble.}
#' \item{fumbled_1_player_id}{Unique identifier of one of the players with a fumble.}
#' \item{fumbled_1_player_name}{String name of one of the players with a fumble.}
#' \item{fumbled_2_player_id}{Unique identifier of one of the players with a fumble.}
#' \item{fumbled_2_player_name}{String name of one of the players with a fumble.}
#' \item{fumbled_2_team}{Team of one of the players with a fumble.}
#' \item{fumble_recovery_1_team}{Team of one of the players with a fumble recovery.}
#' \item{fumble_recovery_1_yards}{Yards gained by one of the players with a fumble recovery.}
#' \item{fumble_recovery_1_player_id}{Unique identifier of one of the players with a fumble recovery.}
#' \item{fumble_recovery_1_player_name}{String name of one of the players with a fumble recovery.}
#' \item{fumble_recovery_2_team}{Team of one of the players with a fumble recovery.}
#' \item{fumble_recovery_2_yards}{Yards gained by one of the players with a fumble recovery.}
#' \item{fumble_recovery_2_player_id}{Unique identifier of one of the players with a fumble recovery.}
#' \item{fumble_recovery_2_player_name}{String name of one of the players with a fumble recovery.}
#' \item{return_team}{String abbreviation of the return team.}
#' \item{return_yards}{Yards gained by the return team.}
#' \item{penalty_team}{String abbreviation of the team with the penalty.}
#' \item{penalty_player_id}{Unique identifier for the player with the penalty.}
#' \item{penalty_player_name}{String name for the player with the penalty.}
#' \item{penalty_yards}{Yards gained (or lost) by the posteam from the penalty.}
#' \item{replay_or_challenge}{Binary indicator for whether or not a replay or challenge.}
#' \item{replay_or_challenge_result}{String indicating the result of the replay or challenge.}
#' \item{penalty_type}{String indicating the penalty type.}
#' \item{defensive_two_point_attempt}{Binary indicator whether or not the defense was able to have an attempt on a two point conversion, this results following a turnover.}
#' \item{defensive_two_point_conv}{Binary indicator whether or not the defense successfully scored on the two point conversion.}
#' \item{defensive_extra_point_attempt}{Binary indicator whether or not the defense was able to have an attempt on an extra point attempt, this results following a blocked attempt that the defense recovers the ball.}
#' \item{defensive_extra_point_conv}{Binary indicator whether or not the defense successfully scored on an extra point attempt.}
#' \item{season}{4 digit number indicating to which season the game belongs to.}
#' \item{cp}{Numeric value indicating the probability for a complete pass based on comparable game situations.}
#' \item{cpoe}{For a single pass play this is 1 - cp when the pass was completed or 0 - cp when the pass was incomplete. Analyzed for a whole game or season an indicator for the passer how much over or under expectation his completion percentage was.}
#' \item{series}{Starts at 1, each new first down increments, numbers shared across both teams NA: kickoffs, extra point/two point conversion attempts, non-plays, no posteam}
#' \item{series_success}{1: scored touchdown, gained enough yards for first down 0: punt, interception, fumble lost, turnover on downs, FG attempt NA: series is NA, series contains QB spike/kneel}
#' \item{start_time}{Kickoff time in eastern time zone.}
#' \item{stadium}{Game site name.}
#' \item{weather}{String describing the weather including temperature, humidity and wind (direction and speed). Doesn't change during the game!}
#' \item{nfl_api_id}{UUID of the game in the new NFL API.}
#' \item{play_clock}{Time on the playclock when the ball was snapped.}
#' \item{play_deleted}{Binary indicator for deleted plays.}
#' \item{play_type_nfl}{Play type as listed in the NFL source. Slightly different to the regular play_type variable.}
#' \item{end_clock_time}{Game time at the end of a given play.}
#' \item{end_yard_line}{String indicating the yardline at the end of the given play consisting of team half and yard line number.}
#' \item{drive_real_start_time}{Local day time when the drive started (currently not used by the NFL and therefore mostly 'NA').}
#' \item{drive_play_count}{Numeric value of how many regular plays happened in a given drive.}
#' \item{drive_time_of_possession}{Time of possession in a given drive.}
#' \item{drive_first_downs}{Number of forst downs in a given drive.}
#' \item{drive_inside20}{Binary indicator if the offense was able to get inside the opponents 20 yard line.}
#' \item{drive_ended_with_score}{Binary indicator the drive ended with a score.}
#' \item{drive_quarter_start}{Numeric value indicating in which quarter the given drive has started.}
#' \item{drive_quarter_end}{Numeric value indicating in which quarter the given drive has ended.}
#' \item{drive_yards_penalized}{Numeric value of how many yards the offense gained or lost through penalties in the given drive.}
#' \item{drive_start_transition}{String indicating how the offense got the ball.}
#' \item{drive_end_transition}{String indicating how the offense lost the ball.}
#' \item{drive_game_clock_start}{Game time at the beginning of a given drive.}
#' \item{drive_game_clock_end}{Game time at the end of a given drive.}
#' \item{drive_start_yard_line}{String indicating where a given drive started consisting of team half and yard line number.}
#' \item{drive_end_yard_line}{String indicating where a given drive ended consisting of team half and yard line number.}
#' \item{drive_play_id_started}{Play_id of the first play in the given drive.}
#' \item{drive_play_id_ended}{Play_id of the last play in the given drive.}
#' \item{away_score}{Total points scored by the away team.}
#' \item{home_score}{Total points scored by the home team.}
#' \item{location}{Either 'Home' o 'Neutral' indicating if the home team played at home or at a neutral site. }
#' \item{result}{Equals home_score - away_score and means the game outcome from the perspective of the home team.}
#' \item{total}{Equals home_score + away_score and means the total points scored in the given game.}
#' \item{spread_line}{The closing spread line for the game. A positive number means the home team was favored by that many points, a negative number means the away team was favored by that many points. (Source: Pro-Football-Reference)}
#' \item{total_line}{The closing total line for the game. (Source: Pro-Football-Reference)}
#' \item{div_game}{Binary indicator for if the given game was a division game.}
#' \item{roof}{One of 'dome', 'outdoors', 'closed', 'open' indicating indicating the roof status of the stadium the game was played in. (Source: Pro-Football-Reference)}
#' \item{surface}{What type of ground the game was played on. (Source: Pro-Football-Reference)}
#' \item{temp}{The temperature at the stadium only for 'roof' = 'outdoors' or 'open'.(Source: Pro-Football-Reference)}
#' \item{wind}{The speed of the wind in miles/hour only for 'roof' = 'outdoors' or 'open'. (Source: Pro-Football-Reference)}
#' \item{home_coach}{First and last name of the home team coach. (Source: Pro-Football-Reference)}
#' \item{away_coach}{First and last name of the away team coach. (Source: Pro-Football-Reference)}
#' \item{stadium_id}{ID of the stadium the game was played in. (Source: Pro-Football-Reference)}
#' \item{game_stadium}{Name of the stadium the game was played in. (Source: Pro-Football-Reference)}
#' }
#' @export
#' @examples
#' \dontrun{
#' # Get pbp data for two games using parallel processing
#' game_ids <- c("2019_01_GB_CHI", "2013_21_SEA_DEN")
#' pbp <- fast_scraper(game_ids, pp = TRUE)
#' }
fast_scraper <- function(game_ids, source = "nfl", pp = FALSE) {

  # Error handling to correct source type
  if (source != "nfl") {
    stop("You tried to specify a source that isn't the new NFL web page. Please remove source from your request or use source = 'nfl'. The 'source' option will soon be deprecated.")
  }

  # No parallel processing demanded -> use purrr
  if (pp == FALSE) {
    suppressWarnings({
      progressr::with_progress({
        p <- progressr::progressor(along = game_ids)
        pbp <- purrr::map_dfr(game_ids, function(x){
          if (substr(x, 1, 4) < 2011) {
            plays <- get_pbp_gc(x)
          } else {
            plays <- get_pbp_nfl(x)
          }
          p(sprintf("x=%s", as.character(x)))
          return(plays)
        })
      })

      if(purrr::is_empty(pbp) == FALSE) {
        message("Download finished. Adding variables...")
        pbp <- pbp  %>%
          add_game_data() %>%
          add_nflscrapr_mutations() %>%
          add_ep() %>%
          add_air_yac_ep() %>%
          add_wp() %>%
          add_air_yac_wp() %>%
          add_cp() %>%
          add_series_data() %>%
          select_variables()
      }
    })
  }

  # User wants parallel processing: check if the required package is installed.
  # Stop and Error when missing
  else if (pp == TRUE & !requireNamespace("furrr", quietly = TRUE)) {
    stop("Package \"furrr\" needed for parallel processing. Please install/load it.")
  }
  else {
    if (length(game_ids)<=4){
      message(glue::glue("You have passed only {length(game_ids)} GameIDs to parallel processing.\nPlease note that the initiating process takes a few seconds\nand consider using pp=FALSE for a small number of games."))
    }
    suppressWarnings({
      progressr::with_progress({
        p <- progressr::progressor(along = game_ids)
        future::plan("multiprocess")
        pbp <- furrr::future_map_dfr(game_ids,  function(x){
          if (substr(x, 1, 4) < 2011) {
            plays <- get_pbp_gc(x)
          } else {
            plays <- get_pbp_nfl(x)
          }
          p(sprintf("x=%s", as.character(x)))
          return(plays)
        })
      })

      if(purrr::is_empty(pbp) == FALSE) {
        message("Download finished. Adding variables...")
        pbp <- pbp %>%
          add_game_data() %>%
          add_nflscrapr_mutations() %>%
          add_ep() %>%
          add_air_yac_ep() %>%
          add_wp() %>%
          add_air_yac_wp() %>%
          add_cp() %>%
          add_series_data() %>%
          select_variables()
      }
    })
  }
  message("Procedure completed.")
  return(pbp)
}


#' Get NFL Play by Play Highlight Clips
#'
#' @param game_ids Vector of numeric or character ids
#' @param pp Logical - either \code{TRUE} or \code{FALSE} (see details for further information)
#' @details To load valid game_ids please use the package function \code{\link{fast_scraper_schedules}}.
#' The \code{pp} parameter controls if the scraper should use parallel processing.
#' Please note that the initiating process takes a few seconds which means it
#' may be better to set \code{pp = FALSE} if you are scraping just a few games.
#' @return Data frame containing game_id, play_id for all plays with available
#' highlightclip and the clip url
# @export
#' @noRd
#' @examples
#'
#' # Get highlight clips for two 2019 games using parallel processing
#' # game_ids <- c("2019090804", "2019101700")
#' # clips <- fast_scraper_clips(game_ids, pp = TRUE)
fast_scraper_clips <- function(game_ids, pp = FALSE) {
  stop("The NFL removed the public available data feed. We are working on a new solution.\n Meanwhile please check https://github.com/guga31bb/nflfastR-data/tree/master/legacy-data for data of the seasons 2000-2019")

  scraper_func <- get_pbp_highlights

  # No parallel processing demanded -> use purrr
  if (pp == FALSE) {
    suppressWarnings(
      clips <- purrr::map_dfr(game_ids, scraper_func)
    )
  }

  # User wants parallel processing: check if the required package is installed.
  # Stop and Error when missing
  else if (pp == TRUE & !requireNamespace("furrr", quietly = TRUE)) {
    stop("Package \"furrr\" needed for parallel processing. Please install/load it.")
  }
  else {
    if (length(game_ids)<=4){
      message(glue::glue("You have passed only {length(game_ids)} GameIDs to parallel processing.\nPlease note that the initiating process takes a few seconds\nand consider using pp=FALSE for a small number of games."))
    }
    suppressWarnings({
      future::plan("multiprocess")
      clips <- furrr::future_map_dfr(game_ids, scraper_func, .progress = TRUE)
    })
  }
  return(clips)
}

#' Get team rosters for multiple seasons and teams
#'
#' Given team_ids and years, return a dataset with each
#' player the NFL has listed as part of the roster.
#'
#' @param team_ids A string vector containing the IDs for NFL Team(s)
#' (see details for more information)
#' @param seasons A string vector of 4-digit years associated with given NFL seasons
#' @param pp Logical - either \code{TRUE} or \code{FALSE} (see details for further information)
#' @details To find team associated Team IDs use the \code{\link{teams_colors_logos}}
#' dataset stored in this package!
#' The \code{pp} parameter controls if the scraper should use parallel processing.
#' Please note that the initiating process takes a few seconds which means it
#' may be better to set \code{pp = FALSE} if you are scraping just a few teams/seasons.
#' @return Data frame where each individual row represents a player in
#' the roster of the given team and season listed by the NFL
#' containing the following information:
#' \itemize{
#' \item{team.season}
#' \item{teamPlayers.displayName}
#' \item{teamPlayers.firstName}
#' \item{teamPlayers.middleName}
#' \item{teamPlayers.lastName}
#' \item{teamPlayers.suffix}
#' \item{teamPlayers.status}
#' \item{teamPlayers.position}
#' \item{teamPlayers.positionGroup}
#' \item{teamPlayers.nflId}
#' \item{teamPlayers.esbId}
#' \item{teamPlayers.gsisId}
#' \item{teamPlayers.birthDate}
#' \item{teamPlayers.homeTown}
#' \item{teamPlayers.collegeId}
#' \item{teamPlayers.collegeName}
#' \item{teamPlayers.jerseyNumber}
#' \item{teamPlayers.height}
#' \item{teamPlayers.weight}
# \item{teamPlayers.yearsOfExperience}
# \item{teamPlayers.teamAbbr}
# \item{teamPlayers.teamSeq}
# \item{teamPlayers.teamId}
# \item{teamPlayers.teamFullName}
#' \item{team.teamId}
#' \item{team.abbr}
#' \item{team.cityState}
#' \item{team.fullName}
#' \item{team.nick}
# \item{team.teamType}
#' \item{team.conferenceAbbr}
#' \item{team.divisionAbbr}
#' \item{teamPlayers.headshot_url}
#' \item{teamPlayers.profile_url}
#' }
#' @examples
#' \dontrun{
#' # Roster of Steelers in 2018, no parallel processing
#' rosters <- fast_scraper_roster("3900", 2018, pp = FALSE)
#'
#' # Roster of Steelers and Seahawks in 2016 & 2019 using parallel processing
#' rosters <- fast_scraper_roster(c("3900", "4600"), c("2016", "2019"), pp = TRUE)
#' }
# @export
#' @noRd
fast_scraper_roster <- function(team_ids, seasons, pp = FALSE) {
  stop("The NFL removed the public available data feed. We are working on a new solution.\n Meanwhile please check https://github.com/guga31bb/nflfastR-data/tree/master/roster-data for data of the seasons 2000-2019")

  # No parallel processing demanded -> use purrr
  if (pp == FALSE) {
    suppressWarnings(
      rosters <-
        purrr::pmap_df(
          # pmap needs a list of lists. It is generated as all combinations of
          # team_ids and seasons by cross2 but needs to be transposed for pmap
          purrr::transpose(purrr::cross2(unique(team_ids), unique(seasons))),
          function(teamId, season) {
            grab_roster(teamId, season)
          }
        ) %>%
        add_roster_mutations()
    )
  }

  # User wants parallel processing: check if the required package is installed.
  # Stop and Error when missing
  else if (pp == TRUE & !requireNamespace("furrr", quietly = TRUE)) {
    stop("Package \"furrr\" needed for parallel processing. Please install/load it.")
  }
  else {
    suppressWarnings({
      future::plan("multiprocess")
      rosters <-
        furrr::future_pmap_dfr(
          # pmap needs a list of lists. It is generated as all combinations of
          # team_ids and seasons by cross2 but needs to be transposed for pmap
          purrr::transpose(purrr::cross2(unique(team_ids), unique(seasons))),
          function(teamId, season) {
            grab_roster(teamId, season)
          },
          .progress = TRUE
        ) %>%
        add_roster_mutations()
    })
  }
  return(rosters)
}

#' Get NFL Season Schedules
#'
#' @param seasons Vector of numeric or character 4 digit seasons
#' @param pp Logical - either \code{TRUE} or \code{FALSE} (see details for further information)
#' @details This functions now incorporates the games file provided and maintained
#' by Lee Sharpe.
#'
#' The \code{pp} parameter controls if the scraper should use parallel processing.
#' Please note that the initiating process takes a few seconds which means it
#' may be better to set \code{pp = FALSE} if you are scraping less than 10 seasons.
#' @return Data frame containing the following detailed game information:
#' \describe{
#' \item{game_id}{Character identifier including season, week, away team and home team}
#' \item{season}{4 digit season year.}
#' \item{game_type}{One of 'REG', 'WC', 'DIV', 'CON', 'SB' indicating if a game was a regular season game or one of the playoff rounds.}
#' \item{week}{Numeric week number.}
#' \item{gameday}{Game date in format yyyy/mm/dd.}
#' \item{weekday}{The day of the week on which the game occcured.}
#' \item{gametime}{The kickoff time of the game. This is represented in 24-hour time and the Eastern time zone, regardless of what time zone the game was being played in.}
#' \item{away_team}{Away team abbreviation.}
#' \item{home_team}{Home team abbreviation.}
#' \item{away_score}{The number of points the away team scored. Is 'NA' for games which haven't yet been played.}
#' \item{home_score}{The number of points the home team scored. Is 'NA' for games which haven't yet been played.}
#' \item{home_result}{Equals home_score - away_score and means the game outcome from the perspective of the home team.}
#' \item{stadium}{Name of the stadium the game was or will be played in. (Source: Pro-Football-Reference)}
#' \item{location}{Either 'Home' o 'Neutral' indicating if the home team played at home or at a neutral site. }
#' \item{roof}{One of 'dome', 'outdoors', 'closed', 'open' indicating indicating the roof status of the stadium the game was played in. (Source: Pro-Football-Reference)}
#' \item{surface}{What type of ground the game was played on. (Source: Pro-Football-Reference)}
#' \item{old_game_id}{Unique game identifier of the old NFL API.}
#' }
#' @export
#' @examples
#'\dontrun{
#' # Get schedules for the whole 2015 - 2018 seasons
#' seasons <- 2015:2018
#' schedules <- fast_scraper_schedules(seasons)
#' }
fast_scraper_schedules <- function(seasons, pp = FALSE) {

  # No parallel processing demanded -> use purrr
  if (pp == FALSE) {
    suppressWarnings(
      progressr::with_progress({
        p <- progressr::progressor(along = seasons)
        schedules <- purrr::map_dfr(seasons, function(x){
          sched <- get_season_schedule(x)
          p(sprintf("x=%s", as.integer(x)))
          return(sched)
        })
      })
    )
  }

  # User wants parallel processing: check if the required package is installed.
  # Stop and Error when missing
  else if (pp == TRUE & !requireNamespace("furrr", quietly = TRUE)) {
    stop("Package \"furrr\" needed for parallel processing. Please install/load it.")
  }
  else {
    if (length(seasons)<=10){
      message(glue::glue("You have passed only {length(seasons)} season(s) to parallel processing.\nPlease note that the initiating process takes a few seconds\nand consider using pp=FALSE for a small number of seasons."))
    }
    suppressWarnings({
      progressr::with_progress({
        p <- progressr::progressor(along = seasons)
        future::plan("multiprocess")
        schedules <- furrr::future_map_dfr(seasons, function(x){
          sched <- get_season_schedule(x)
          p(sprintf("x=%s", as.integer(x)))
          return(sched)
        })
      })
    })
  }
  return(schedules)
}
