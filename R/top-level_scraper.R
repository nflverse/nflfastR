################################################################################
# Author: Sebastian Carl
# Purpose: Top-Level functions which will be made available through the package
# Code Style Guide: styler::tidyverse_style()
################################################################################

#' Get NFL Play by Play Data
#'
#' @description Load and parse NFL play-by-play data and add all of the original
#'   nflfastR variables. As nflfastR now provides multiple functions which add
#'   information to the output of this function, it is recommended to use
#'   \code{\link{build_nflfastR_pbp}} instead.
#'
#' @param game_ids Vector of character ids or a data frame including the variable
#' `game_id` (see details for further information).
#' @param source `r lifecycle::badge("deprecated")` has no effect and will be
#'   removed in a future release.
#' @param pp `r lifecycle::badge("deprecated")` has no effect and will be
#'   removed in a future release.
#' @param ... Additional arguments passed to the scraping functions (for internal use)
#' @param in_builder If \code{TRUE}, the final message will be suppressed (for usage inside of \code{\link{build_nflfastR_pbp}}).
#' @details To load valid game_ids please use the package function
#' \code{\link{fast_scraper_schedules}} (the function can directly handle the
#' output of that function)
#' @seealso For information on parallel processing and progress updates please
#' see [nflfastR].
#' @return Data frame where each individual row represents a single play for
#' all passed game_ids containing the following
#' detailed information (description partly extracted from nflscrapR):
#' \describe{
#' \item{play_id}{Numeric play id that when used with game_id and drive provides the unique identifier for a single play.}
#' \item{game_id}{Ten digit identifier for NFL game.}
#' \item{old_game_id}{Legacy NFL game ID.}
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
#' \item{yards_gained}{Numeric yards gained (or lost) by the possessing team, excluding yards gained via fumble recoveries and laterals.}
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
#' \item{timeout}{Binary indicator for whether or not a timeout was called by either team.}
#' \item{timeout_team}{String abbreviation for which team called the timeout.}
#' \item{td_team}{String abbreviation for which team scored the touchdown.}
#' \item{td_player_name}{String name of the player who scored a touchdown.}
#' \item{td_player_id}{Unique identifier of the player who scored a touchdown.}
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
#' \item{vegas_wpa}{Win probability added (WPA) for the posteam: spread_adjusted model.}
#' \item{vegas_home_wpa}{Win probability added (WPA) for the home team: spread_adjusted model.}
#' \item{home_wp_post}{Estimated win probability for the home team at the end of the play.}
#' \item{away_wp_post}{Estimated win probability for the away team at the end of the play.}
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
#' \item{tackled_for_loss}{Binary indicator for whether or not a tackle for loss on a run play occurred.}
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
#' \item{passing_yards}{Numeric yards by the passer_player_name, including yards gained in pass plays with laterals.
#' This should equal official passing statistics.}
#' \item{receiver_player_id}{Unique identifier for the receiver that was targeted on the pass.}
#' \item{receiver_player_name}{String name for the targeted receiver.}
#' \item{receiving_yards}{Numeric yards by the receiver_player_name, excluding yards gained in pass plays with laterals.
#' This should equal official receiving statistics but could miss yards gained in pass plays with laterals.
#' Please see the description of `lateral_receiver_player_name` for further information.}
#' \item{rusher_player_id}{Unique identifier for the player that attempted the run.}
#' \item{rusher_player_name}{String name for the player that attempted the run.}
#' \item{rushing_yards}{Numeric yards by the rusher_player_name, excluding yards gained in rush plays with laterals.
#' This should equal official rushing statistics but could miss yards gained in rush plays with laterals.
#' Please see the description of `lateral_rusher_player_name` for further information.}
#' \item{lateral_receiver_player_id}{Unique identifier for the player that received the last(!) lateral on a pass play.}
#' \item{lateral_receiver_player_name}{String name for the player that received the last(!) lateral on a pass play.
#' If there were multiple laterals in the same play, this will only be the last player who received a lateral.
#' Please see \url{https://github.com/mrcaseb/nfl-data/tree/master/data/lateral_yards}
#' for a list of plays where multiple players recorded lateral receiving yards.}
#' \item{lateral_receiving_yards}{Numeric yards by the `lateral_receiver_player_name` in pass plays with laterals.
#' Please see the description of `lateral_receiver_player_name` for further information.}
#' \item{lateral_rusher_player_id}{Unique identifier for the player that received the last(!) lateral on a run play.}
#' \item{lateral_rusher_player_name}{String name for the player that received the last(!) lateral on a run play.
#' If there were multiple laterals in the same play, this will only be the last player who received a lateral.
#' Please see \url{https://github.com/mrcaseb/nfl-data/tree/master/data/lateral_yards}
#' for a list of plays where multiple players recorded lateral rushing yards.}
#' \item{lateral_rushing_yards}{Numeric yards by the `lateral_rusher_player_name` in run plays with laterals.
#' Please see the description of `lateral_rusher_player_name` for further information.}
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
#' \item{qb_hit_1_player_id}{Unique identifier for one of the potential players that hit the QB. No sack as the QB was not the ball carrier. For sacks please see `sack_player` or `half_sack_*_player`.}
#' \item{qb_hit_1_player_name}{String name for one of the potential players that hit the QB. No sack as the QB was not the ball carrier. For sacks please see `sack_player` or `half_sack_*_player`.}
#' \item{qb_hit_2_player_id}{Unique identifier for one of the potential players that hit the QB. No sack as the QB was not the ball carrier. For sacks please see `sack_player` or `half_sack_*_player`.}
#' \item{qb_hit_2_player_name}{String name for one of the potential players that hit the QB. No sack as the QB was not the ball carrier. For sacks please see `sack_player` or `half_sack_*_player`.}
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
#' \item{tackle_with_assist}{Binary indicator for if there has been a tackle with assist.}
#' \item{tackle_with_assist_1_player_id}{Unique identifier of one of the players with a tackle with assist.}
#' \item{tackle_with_assist_1_player_name}{String name of one of the players with a tackle with assist.}
#' \item{tackle_with_assist_1_team}{Team of one of the players with a tackle with assist.}
#' \item{tackle_with_assist_2_player_id}{Unique identifier of one of the players with a tackle with assist.}
#' \item{tackle_with_assist_2_player_name}{String name of one of the players with a tackle with assist.}
#' \item{tackle_with_assist_2_team}{Team of one of the players with a tackle with assist.}
#' \item{pass_defense_1_player_id}{Unique identifier of one of the players with a pass defense.}
#' \item{pass_defense_1_player_name}{String name of one of the players with a pass defense.}
#' \item{pass_defense_2_player_id}{Unique identifier of one of the players with a pass defense.}
#' \item{pass_defense_2_player_name}{String name of one of the players with a pass defense.}
#' \item{fumbled_1_team}{Team of one of the first player with a fumble.}
#' \item{fumbled_1_player_id}{Unique identifier of the first player who fumbled on the play.}
#' \item{fumbled_1_player_name}{String name of one of the first player who fumbled on the play.}
#' \item{fumbled_2_player_id}{Unique identifier of the second player who fumbled on the play.}
#' \item{fumbled_2_player_name}{String name of one of the second player who fumbled on the play.}
#' \item{fumbled_2_team}{Team of one of the second player with a fumble.}
#' \item{fumble_recovery_1_team}{Team of one of the players with a fumble recovery.}
#' \item{fumble_recovery_1_yards}{Yards gained by one of the players with a fumble recovery.}
#' \item{fumble_recovery_1_player_id}{Unique identifier of one of the players with a fumble recovery.}
#' \item{fumble_recovery_1_player_name}{String name of one of the players with a fumble recovery.}
#' \item{fumble_recovery_2_team}{Team of one of the players with a fumble recovery.}
#' \item{fumble_recovery_2_yards}{Yards gained by one of the players with a fumble recovery.}
#' \item{fumble_recovery_2_player_id}{Unique identifier of one of the players with a fumble recovery.}
#' \item{fumble_recovery_2_player_name}{String name of one of the players with a fumble recovery.}
#' \item{sack_player_id}{Unique identifier of the player who recorded a solo sack.}
#' \item{sack_player_name}{String name of the player who recorded a solo sack.}
#' \item{half_sack_1_player_id}{Unique identifier of the first player who recorded half a sack.}
#' \item{half_sack_1_player_name}{String name of the first player who recorded half a sack.}
#' \item{half_sack_2_player_id}{Unique identifier of the second player who recorded half a sack.}
#' \item{half_sack_2_player_name}{String name of the second player who recorded half a sack.}
#' \item{return_team}{String abbreviation of the return team.}
#' \item{return_yards}{Yards gained by the return team.}
#' \item{penalty_team}{String abbreviation of the team with the penalty.}
#' \item{penalty_player_id}{Unique identifier for the player with the penalty.}
#' \item{penalty_player_name}{String name for the player with the penalty.}
#' \item{penalty_yards}{Yards gained (or lost) by the posteam from the penalty.}
#' \item{replay_or_challenge}{Binary indicator for whether or not a replay or challenge.}
#' \item{replay_or_challenge_result}{String indicating the result of the replay or challenge.}
#' \item{penalty_type}{String indicating the penalty type of the first penalty in the given play. Will be `NA` if `desc` is missing the type.}
#' \item{defensive_two_point_attempt}{Binary indicator whether or not the defense was able to have an attempt on a two point conversion, this results following a turnover.}
#' \item{defensive_two_point_conv}{Binary indicator whether or not the defense successfully scored on the two point conversion.}
#' \item{defensive_extra_point_attempt}{Binary indicator whether or not the defense was able to have an attempt on an extra point attempt, this results following a blocked attempt that the defense recovers the ball.}
#' \item{defensive_extra_point_conv}{Binary indicator whether or not the defense successfully scored on an extra point attempt.}
#' \item{safety_player_name}{String name for the player who scored a safety.}
#' \item{safety_player_id}{Unique identifier for the player who scored a safety.}
#' \item{season}{4 digit number indicating to which season the game belongs to.}
#' \item{cp}{Numeric value indicating the probability for a complete pass based on comparable game situations.}
#' \item{cpoe}{For a single pass play this is 1 - cp when the pass was completed or 0 - cp when the pass was incomplete. Analyzed for a whole game or season an indicator for the passer how much over or under expectation his completion percentage was.}
#' \item{series}{Starts at 1, each new first down increments, numbers shared across both teams NA: kickoffs, extra point/two point conversion attempts, non-plays, no posteam}
#' \item{series_success}{1: scored touchdown, gained enough yards for first down.}
#' \item{series_result}{Possible values: First down, Touchdown, Opp touchdown, Field goal, Missed field goal, Safety, Turnover, Punt, Turnover on downs, QB kneel, End of half}
#' \item{start_time}{Kickoff time in eastern time zone.}
#' \item{order_sequence}{Column provided by NFL to fix out-of-order plays. Available 2011 and beyond with source "nfl".}
#' \item{time_of_day}{Time of day of play in UTC "HH:MM:SS" format. Available 2011 and beyond with source "nfl".}
#' \item{stadium}{Game site name.}
#' \item{weather}{String describing the weather including temperature, humidity and wind (direction and speed). Doesn't change during the game!}
#' \item{nfl_api_id}{UUID of the game in the new NFL API.}
#' \item{play_clock}{Time on the playclock when the ball was snapped.}
#' \item{play_deleted}{Binary indicator for deleted plays.}
#' \item{play_type_nfl}{Play type as listed in the NFL source. Slightly different to the regular play_type variable.}
#' \item{special_teams_play}{Binary indicator for whether play is special teams play from NFL source. Available 2011 and beyond with source "nfl".}
#' \item{st_play_type}{Type of special teams play from NFL source. Available 2011 and beyond with source "nfl".}
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
#' \item{fixed_drive}{Manually created drive number in a game.}
#' \item{fixed_drive_result}{Manually created drive result.}
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
#' \donttest{
#' # Get pbp data for two games
#' fast_scraper(c("2019_01_GB_CHI", "2013_21_SEA_DEN"))
#'
#' # It is also possible to directly use the
#' # output of `fast_scraper_schedules` as input
#' library(dplyr, warn.conflicts = FALSE)
#' fast_scraper_schedules(2020) %>%
#'   tail(3) %>%
#'   fast_scraper()
#'
#' \dontshow{
#' # Close open connections for R CMD Check
#' future::plan("sequential")
#' }
#' }
fast_scraper <- function(game_ids,
                         source = lifecycle::deprecated(),
                         pp = lifecycle::deprecated(),
                         ...,
                         in_builder = FALSE) {
  if (lifecycle::is_present(source)) {
    lifecycle::deprecate_warn(
      when = "4.0.0",
      what = "fast_scraper(source = )",
      details = "The source argument isn't used anymore and will be dropped in a future release."
    )
  }

  if (lifecycle::is_present(pp)) {
    lifecycle::deprecate_warn(
      when = "4.0.0",
      what = "fast_scraper(pp = )",
      details = cli::cli_text(c(
        "We have dropped the in-package activation of parallel processing as ",
        "this is considered bad practice.\n",
        "Please choose an appropriate plan before calling the function, e.g. ",
        "{.code future::plan(\"multisession\")}"
      ))
    )
  }

  if (!is.vector(game_ids) && is.data.frame(game_ids)) game_ids <- game_ids$game_id

  if (!is.vector(game_ids)) cli::cli_abort("Param {.code game_ids} is not a valid vector!")

  if (length(game_ids) > 1 && is_sequential()) {
    cli::cli_alert_info(
      c(
        "It is recommended to use parallel processing when trying to load multiple games.",
        "Please consider running {.code future::plan(\"multisession\")}! ",
        "Will go on sequentially..."
      )
    )
  }

  suppressWarnings({
    p <- progressr::progressor(along = game_ids)
    pbp <- furrr::future_map_dfr(game_ids, function(x, p, ...) {
      if (substr(x, 1, 4) < 2001) {
        plays <- get_pbp_gc(x, ...)
      } else {
        plays <- get_pbp_nfl(x, ...)
      }
      p(sprintf("ID=%s", as.character(x)))
      return(plays)
    }, p, ...)

    if (length(pbp) != 0) {
      user_message("Download finished. Adding variables...", "done")
      pbp <- pbp %>%
        add_game_data() %>%
        add_nflscrapr_mutations() %>%
        add_ep() %>%
        add_air_yac_ep() %>%
        add_wp() %>%
        add_air_yac_wp() %>%
        add_cp() %>%
        add_drive_results() %>%
        add_series_data() %>%
        select_variables()
    }
  })

  if (!in_builder) {
    str <- paste0(my_time(), " | Procedure completed.")
    cli::cli_alert_success("{.field {str}}")
  }
  return(pbp)
}

#' Get team rosters for multiple seasons
#'
#' @description Given years return a dataset with each player listed as part of the roster.
#'
#' @param seasons A vector of 4-digit years associated with given NFL seasons
#' @param pp `r lifecycle::badge("deprecated")` has no effect and will be
#'   removed in a future release.
#' @details The roster data is accessed via the free to use Sleeper API.
#' @seealso For information on parallel processing and progress updates please
#' see [nflfastR].
#' @return Data frame where each individual row represents a player in
#' the roster of the given team and season containing the following information:
#' \describe{
#' \item{season}{4 digit season year.}
#' \item{team}{Team abbreviation.}
#' \item{position}{Abbreviation of the player's position (e.g. "QB", "WR", "RB", "CB"...).}
#' \item{depth_chart_position}{Starting with the 2020 season: the abbreviation of the players depth_chart_position.}
#' \item{jersey_number}{The player's 2 digit jersey number.}
#' \item{status}{String indicating the status of the player (e.g. "Active", "Inactive", "Injured Reserve"...) at the update time \code{update_dt} (see below)}
#' \item{full_name}{Full name of the player.}
#' \item{first_name}{First name of the player.}
#' \item{last_name}{Last name of the player.}
#' \item{birth_date}{Birth date of the player.}
#' \item{height}{Height of the player.}
#' \item{weight}{Weight of the player.}
#' \item{college}{Name of the college the player has attended.}
#' \item{high_school}{Name of the High School the player has attended (only non-NA for players who were listed in the 2020 season).}
#' \item{gsis_id}{The player's NFL GSIS ID, which can be used to link the player to play-by-play data.}
#' \item{espn_id}{The player's ESPN ID (only non-NA for players who were listed in the 2020 season).}
#' \item{sportradar_id}{The player's Sportradar ID (only non-NA for players who were listed in the 2020 season).}
#' \item{yahoo_id}{The player's Yahoo Sports ID (only non-NA for players who were listed in the 2020 season).}
#' \item{rotowire_id}{The player's Rotowire ID (only non-NA for players who were listed in the 2020 season).}
#' \item{update_dt}{Date and time when the current entry was last updated (starting with the 2020 season).}
#' \item{headshot_url}{URL to a player image (starting in the 2020 season on ESPN servers).}
#' }
#' @examples
#' \donttest{
#' # Roster of the 2019 and 2020 seasons
#' fast_scraper_roster(2019:2020)
#' \dontshow{
#' # Close open connections for R CMD Check
#' future::plan("sequential")
#' }
#' }
#' @export
fast_scraper_roster <- function(seasons, pp = lifecycle::deprecated()) {
  if (lifecycle::is_present(pp)) {
    lifecycle::deprecate_warn(
      when = "4.0.0",
      what = "fast_scraper_roster(pp = )",
      details = cli::cli_text(c(
        "We have dropped the in-package activation of parallel processing as ",
        "this is considered bad practice.\n",
        "Please choose an appropriate plan before calling the function, e.g.",
        "{.code future::plan(\"multisession\")}"
      ))
    )
  }

  if (length(seasons) > 1 && is_sequential()) {
    cli::cli_alert_info(
      c(
        "It is recommended to use parallel processing when trying to load multiple seasons. ",
        "Please consider running {.code future::plan(\"multisession\")}! ",
        "Will go on sequentially..."
      )
    )
  }

  suppressWarnings({
    p <- progressr::progressor(along = seasons)
    ret <- furrr::future_map_dfr(seasons, function(x, p) {
      out <- get_scheds_and_rosters(x, "roster")
      p(sprintf("season=%s", as.integer(x)))
      return(out)
    }, p)
  })
  return(ret)
}

#' Get NFL Season Schedules
#'
#' @param seasons Vector of numeric or character 4 digit seasons
#' @param pp `r lifecycle::badge("deprecated")` has no effect and will be
#'   removed in a future release.
#' @details This functions now incorporates the games file provided and maintained
#' by Lee Sharpe.
#' @seealso For information on parallel processing and progress updates please
#' see [nflfastR].
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
#'\donttest{
#' # Get schedules for the whole 2015 - 2018 seasons
#' fast_scraper_schedules(2015:2018)
#' \dontshow{
#' # Close open connections for R CMD Check
#' future::plan("sequential")
#' }
#' }
fast_scraper_schedules <- function(seasons, pp = lifecycle::deprecated()) {
  if (lifecycle::is_present(pp)) {
    lifecycle::deprecate_warn(
      when = "4.0.0",
      what = "fast_scraper_schedules(pp = )",
      details = cli::cli_text(c(
        "We have dropped the in-package activation of parallel processing as ",
        "this is considered bad practice.\n",
        "Please choose an appropriate plan before calling the function, e.g.",
        "{.code future::plan(\"multisession\")}"
      ))
    )
  }
  if (length(seasons) > 1 && is_sequential()) {
    cli::cli_alert_info(
      c(
        "It is recommended to use parallel processing when trying to load multiple seasons. ",
        "Please consider running {.code future::plan(\"multisession\")}! ",
        "Will go on sequentially..."
      )
    )
  }

  suppressWarnings({
    p <- progressr::progressor(along = seasons)
    ret <- furrr::future_map_dfr(seasons, function(x, p) {
      out <- get_scheds_and_rosters(x, "schedule")
      p(sprintf("season=%s", as.integer(x)))
      return(out)
    }, p)
  })
  return(ret)
}
