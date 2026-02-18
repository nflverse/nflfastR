# Get NFL Play by Play Data

Load and parse NFL play-by-play data and add all of the original
nflfastR variables. As nflfastR now provides multiple functions which
add information to the output of this function, it is recommended to use
[`build_nflfastR_pbp`](https://nflfastr.com/reference/build_nflfastR_pbp.md)
instead.

## Usage

``` r
fast_scraper(
  game_ids,
  dir = getOption("nflfastR.raw_directory", default = NULL),
  ...,
  in_builder = FALSE
)
```

## Arguments

- game_ids:

  Vector of character ids or a data frame including the variable
  `game_id` (see details for further information).

- dir:

  Path to local directory (defaults to option "nflfastR.raw_directory")
  where nflfastR searches for raw game play-by-play data. See
  [`save_raw_pbp()`](https://nflfastr.com/reference/save_raw_pbp.md) for
  additional information.

- ...:

  Additional arguments passed to the scraping functions (for internal
  use)

- in_builder:

  If `TRUE`, the final message will be suppressed (for usage inside of
  [`build_nflfastR_pbp`](https://nflfastr.com/reference/build_nflfastR_pbp.md)).

## Value

Data frame where each individual row represents a single play for all
passed game_ids containing the following detailed information
(description partly extracted from nflscrapR):

- play_id:

  Numeric play id that when used with game_id and drive provides the
  unique identifier for a single play.

- game_id:

  Ten digit identifier for NFL game.

- old_game_id:

  Legacy NFL game ID.

- home_team:

  String abbreviation for the home team.

- away_team:

  String abbreviation for the away team.

- season_type:

  'REG' or 'POST' indicating if the game belongs to regular or post
  season.

- week:

  Season week.

- posteam:

  String abbreviation for the team with possession.

- posteam_type:

  String indicating whether the posteam team is home or away.

- defteam:

  String abbreviation for the team on defense.

- side_of_field:

  String abbreviation for which team's side of the field the team with
  possession is currently on.

- yardline_100:

  Numeric distance in the number of yards from the opponent's endzone
  for the posteam.

- game_date:

  Date of the game.

- quarter_seconds_remaining:

  Numeric seconds remaining in the quarter.

- half_seconds_remaining:

  Numeric seconds remaining in the half.

- game_seconds_remaining:

  Numeric seconds remaining in the game.

- game_half:

  String indicating which half the play is in, either Half1, Half2, or
  Overtime.

- quarter_end:

  Binary indicator for whether or not the row of the data is marking the
  end of a quarter.

- drive:

  Numeric drive number in the game.

- sp:

  Binary indicator for whether or not a score occurred on the play.

- qtr:

  Quarter of the game (5 is overtime).

- down:

  The down for the given play.

- goal_to_go:

  Binary indicator for whether or not the posteam is in a goal down
  situation.

- time:

  Time at start of play provided in string format as minutes:seconds
  remaining in the quarter.

- yrdln:

  String indicating the current field position for a given play.

- ydstogo:

  Numeric yards in distance from either the first down marker or the
  endzone in goal down situations.

- ydsnet:

  Numeric value for total yards gained on the given drive.

- desc:

  Detailed string description for the given play.

- play_type:

  String indicating the type of play: pass (includes sacks), run
  (includes scrambles), punt, field_goal, kickoff, extra_point,
  qb_kneel, qb_spike, no_play (timeouts and penalties), and missing for
  rows indicating end of play.

- yards_gained:

  Numeric yards gained (or lost) by the possessing team, excluding yards
  gained via fumble recoveries and laterals.

- shotgun:

  Binary indicator for whether or not the play was in shotgun formation.

- no_huddle:

  Binary indicator for whether or not the play was in no_huddle
  formation.

- qb_dropback:

  Binary indicator for whether or not the QB dropped back on the play
  (pass attempt, sack, or scrambled).

- qb_kneel:

  Binary indicator for whether or not the QB took a knee.

- qb_spike:

  Binary indicator for whether or not the QB spiked the ball.

- qb_scramble:

  Binary indicator for whether or not the QB scrambled.

- pass_length:

  String indicator for pass length: short or deep.

- pass_location:

  String indicator for pass location: left, middle, or right.

- air_yards:

  Numeric value for distance in yards perpendicular to the line of
  scrimmage at where the targeted receiver either caught or didn't catch
  the ball.

- yards_after_catch:

  Numeric value for distance in yards perpendicular to the yard line
  where the receiver made the reception to where the play ended.

- run_location:

  String indicator for location of run: left, middle, or right.

- run_gap:

  String indicator for line gap of run: end, guard, or tackle

- field_goal_result:

  String indicator for result of field goal attempt: made, missed, or
  blocked.

- kick_distance:

  Numeric distance in yards for kickoffs, field goals, and punts.

- extra_point_result:

  String indicator for the result of the extra point attempt: good,
  failed, blocked, safety (touchback in defensive endzone is 1 point
  apparently), or aborted.

- two_point_conv_result:

  String indicator for result of two point conversion attempt: success,
  failure, safety (touchback in defensive endzone is 1 point
  apparently), or return.

- home_timeouts_remaining:

  Numeric timeouts remaining in the half for the home team.

- away_timeouts_remaining:

  Numeric timeouts remaining in the half for the away team.

- timeout:

  Binary indicator for whether or not a timeout was called by either
  team.

- timeout_team:

  String abbreviation for which team called the timeout.

- td_team:

  String abbreviation for which team scored the touchdown.

- td_player_name:

  String name of the player who scored a touchdown.

- td_player_id:

  Unique identifier of the player who scored a touchdown.

- posteam_timeouts_remaining:

  Number of timeouts remaining for the possession team.

- defteam_timeouts_remaining:

  Number of timeouts remaining for the team on defense.

- total_home_score:

  Score for the home team at the end of the play.

- total_away_score:

  Score for the away team at the end of the play.

- posteam_score:

  Score the posteam at the start of the play.

- defteam_score:

  Score the defteam at the start of the play.

- score_differential:

  Score differential between the posteam and defteam at the start of the
  play.

- posteam_score_post:

  Score for the posteam at the end of the play.

- defteam_score_post:

  Score for the defteam at the end of the play.

- score_differential_post:

  Score differential between the posteam and defteam at the end of the
  play.

- no_score_prob:

  Predicted probability of no score occurring for the rest of the half
  based on the expected points model.

- opp_fg_prob:

  Predicted probability of the defteam scoring a FG next.

- opp_safety_prob:

  Predicted probability of the defteam scoring a safety next.

- opp_td_prob:

  Predicted probability of the defteam scoring a TD next.

- fg_prob:

  Predicted probability of the posteam scoring a FG next.

- safety_prob:

  Predicted probability of the posteam scoring a safety next.

- td_prob:

  Predicted probability of the posteam scoring a TD next.

- extra_point_prob:

  Predicted probability of the posteam scoring an extra point.

- two_point_conversion_prob:

  Predicted probability of the posteam scoring the two point conversion.

- ep:

  Using the scoring event probabilities, the estimated expected points
  with respect to the possession team for the given play.

- epa:

  Expected points added (EPA) by the posteam for the given play.

- total_home_epa:

  Cumulative total EPA for the home team in the game so far.

- total_away_epa:

  Cumulative total EPA for the away team in the game so far.

- total_home_rush_epa:

  Cumulative total rushing EPA for the home team in the game so far.

- total_away_rush_epa:

  Cumulative total rushing EPA for the away team in the game so far.

- total_home_pass_epa:

  Cumulative total passing EPA for the home team in the game so far.

- total_away_pass_epa:

  Cumulative total passing EPA for the away team in the game so far.

- air_epa:

  EPA from the air yards alone. For completions this represents the
  actual value provided through the air. For incompletions this
  represents the hypothetical value that could've been added through the
  air if the pass was completed.

- yac_epa:

  EPA from the yards after catch alone. For completions this represents
  the actual value provided after the catch. For incompletions this
  represents the difference between the hypothetical air_epa and the
  play's raw observed EPA (how much the incomplete pass cost the
  posteam).

- comp_air_epa:

  EPA from the air yards alone only for completions.

- comp_yac_epa:

  EPA from the yards after catch alone only for completions.

- total_home_comp_air_epa:

  Cumulative total completions air EPA for the home team in the game so
  far.

- total_away_comp_air_epa:

  Cumulative total completions air EPA for the away team in the game so
  far.

- total_home_comp_yac_epa:

  Cumulative total completions yac EPA for the home team in the game so
  far.

- total_away_comp_yac_epa:

  Cumulative total completions yac EPA for the away team in the game so
  far.

- total_home_raw_air_epa:

  Cumulative total raw air EPA for the home team in the game so far.

- total_away_raw_air_epa:

  Cumulative total raw air EPA for the away team in the game so far.

- total_home_raw_yac_epa:

  Cumulative total raw yac EPA for the home team in the game so far.

- total_away_raw_yac_epa:

  Cumulative total raw yac EPA for the away team in the game so far.

- wp:

  Estimated win probabiity for the posteam given the current situation
  at the start of the given play.

- def_wp:

  Estimated win probability for the defteam.

- home_wp:

  Estimated win probability for the home team.

- away_wp:

  Estimated win probability for the away team.

- wpa:

  Win probability added (WPA) for the posteam.

- vegas_wpa:

  Win probability added (WPA) for the posteam: spread_adjusted model.

- vegas_home_wpa:

  Win probability added (WPA) for the home team: spread_adjusted model.

- home_wp_post:

  Estimated win probability for the home team at the end of the play.

- away_wp_post:

  Estimated win probability for the away team at the end of the play.

- vegas_wp:

  Estimated win probabiity for the posteam given the current situation
  at the start of the given play, incorporating pre-game Vegas line.

- vegas_home_wp:

  Estimated win probability for the home team incorporating pre-game
  Vegas line.

- total_home_rush_wpa:

  Cumulative total rushing WPA for the home team in the game so far.

- total_away_rush_wpa:

  Cumulative total rushing WPA for the away team in the game so far.

- total_home_pass_wpa:

  Cumulative total passing WPA for the home team in the game so far.

- total_away_pass_wpa:

  Cumulative total passing WPA for the away team in the game so far.

- air_wpa:

  WPA through the air (same logic as air_epa).

- yac_wpa:

  WPA from yards after the catch (same logic as yac_epa).

- comp_air_wpa:

  The air_wpa for completions only.

- comp_yac_wpa:

  The yac_wpa for completions only.

- total_home_comp_air_wpa:

  Cumulative total completions air WPA for the home team in the game so
  far.

- total_away_comp_air_wpa:

  Cumulative total completions air WPA for the away team in the game so
  far.

- total_home_comp_yac_wpa:

  Cumulative total completions yac WPA for the home team in the game so
  far.

- total_away_comp_yac_wpa:

  Cumulative total completions yac WPA for the away team in the game so
  far.

- total_home_raw_air_wpa:

  Cumulative total raw air WPA for the home team in the game so far.

- total_away_raw_air_wpa:

  Cumulative total raw air WPA for the away team in the game so far.

- total_home_raw_yac_wpa:

  Cumulative total raw yac WPA for the home team in the game so far.

- total_away_raw_yac_wpa:

  Cumulative total raw yac WPA for the away team in the game so far.

- punt_blocked:

  Binary indicator for if the punt was blocked.

- first_down_rush:

  Binary indicator for if a running play converted the first down.

- first_down_pass:

  Binary indicator for if a passing play converted the first down.

- first_down_penalty:

  Binary indicator for if a penalty converted the first down.

- third_down_converted:

  Binary indicator for if the first down was converted on third down.

- third_down_failed:

  Binary indicator for if the posteam failed to convert first down on
  third down.

- fourth_down_converted:

  Binary indicator for if the first down was converted on fourth down.

- fourth_down_failed:

  Binary indicator for if the posteam failed to convert first down on
  fourth down.

- incomplete_pass:

  Binary indicator for if the pass was incomplete.

- touchback:

  Binary indicator for if a touchback occurred on the play.

- interception:

  Binary indicator for if the pass was intercepted.

- punt_inside_twenty:

  Binary indicator for if the punt ended inside the twenty yard line.

- punt_in_endzone:

  Binary indicator for if the punt was in the endzone.

- punt_out_of_bounds:

  Binary indicator for if the punt went out of bounds.

- punt_downed:

  Binary indicator for if the punt was downed.

- punt_fair_catch:

  Binary indicator for if the punt was caught with a fair catch.

- kickoff_inside_twenty:

  Binary indicator for if the kickoff ended inside the twenty yard line.

- kickoff_in_endzone:

  Binary indicator for if the kickoff was in the endzone.

- kickoff_out_of_bounds:

  Binary indicator for if the kickoff went out of bounds.

- kickoff_downed:

  Binary indicator for if the kickoff was downed.

- kickoff_fair_catch:

  Binary indicator for if the kickoff was caught with a fair catch.

- fumble_forced:

  Binary indicator for if the fumble was forced.

- fumble_not_forced:

  Binary indicator for if the fumble was not forced.

- fumble_out_of_bounds:

  Binary indicator for if the fumble went out of bounds.

- solo_tackle:

  Binary indicator if the play had a solo tackle (could be multiple due
  to fumbles).

- safety:

  Binary indicator for whether or not a safety occurred.

- penalty:

  Binary indicator for whether or not a penalty occurred.

- tackled_for_loss:

  Binary indicator for whether or not a tackle for loss on a run play
  occurred.

- fumble_lost:

  Binary indicator for if the fumble was lost.

- own_kickoff_recovery:

  Binary indicator for if the kicking team recovered the kickoff.

- own_kickoff_recovery_td:

  Binary indicator for if the kicking team recovered the kickoff and
  scored a TD.

- qb_hit:

  Binary indicator if the QB was hit on the play.

- rush_attempt:

  Binary indicator for if the play was a run.

- pass_attempt:

  Binary indicator for if the play was a pass attempt (includes sacks).

- sack:

  Binary indicator for if the play ended in a sack.

- touchdown:

  Binary indicator for if the play resulted in a TD.

- pass_touchdown:

  Binary indicator for if the play resulted in a passing TD.

- rush_touchdown:

  Binary indicator for if the play resulted in a rushing TD.

- return_touchdown:

  Binary indicator for if the play resulted in a return TD.

- extra_point_attempt:

  Binary indicator for extra point attempt.

- two_point_attempt:

  Binary indicator for two point conversion attempt.

- field_goal_attempt:

  Binary indicator for field goal attempt.

- kickoff_attempt:

  Binary indicator for kickoff.

- punt_attempt:

  Binary indicator for punts.

- fumble:

  Binary indicator for if a fumble occurred.

- complete_pass:

  Binary indicator for if the pass was completed.

- assist_tackle:

  Binary indicator for if an assist tackle occurred.

- lateral_reception:

  Binary indicator for if a lateral occurred on the reception.

- lateral_rush:

  Binary indicator for if a lateral occurred on a run.

- lateral_return:

  Binary indicator for if a lateral occurred on a return.

- lateral_recovery:

  Binary indicator for if a lateral occurred on a fumble recovery.

- passer_player_id:

  Unique identifier for the player that attempted the pass.

- passer_player_name:

  String name for the player that attempted the pass.

- passing_yards:

  Numeric yards by the passer_player_name, including yards gained in
  pass plays with laterals. This should equal official passing
  statistics.

- receiver_player_id:

  Unique identifier for the receiver that was targeted on the pass.

- receiver_player_name:

  String name for the targeted receiver.

- receiving_yards:

  Numeric yards by the receiver_player_name, excluding yards gained in
  pass plays with laterals. This should equal official receiving
  statistics but could miss yards gained in pass plays with laterals.
  Please see the description of `lateral_receiver_player_name` for
  further information.

- rusher_player_id:

  Unique identifier for the player that attempted the run.

- rusher_player_name:

  String name for the player that attempted the run.

- rushing_yards:

  Numeric yards by the rusher_player_name, excluding yards gained in
  rush plays with laterals. This should equal official rushing
  statistics but could miss yards gained in rush plays with laterals.
  Please see the description of `lateral_rusher_player_name` for further
  information.

- lateral_receiver_player_id:

  Unique identifier for the player that received the last(!) lateral on
  a pass play.

- lateral_receiver_player_name:

  String name for the player that received the last(!) lateral on a pass
  play. If there were multiple laterals in the same play, this will only
  be the last player who received a lateral. Please see
  <https://github.com/mrcaseb/nfl-data/tree/master/data/lateral_yards>
  for a list of plays where multiple players recorded lateral receiving
  yards.

- lateral_receiving_yards:

  Numeric yards by the `lateral_receiver_player_name` in pass plays with
  laterals. Please see the description of `lateral_receiver_player_name`
  for further information.

- lateral_rusher_player_id:

  Unique identifier for the player that received the last(!) lateral on
  a run play.

- lateral_rusher_player_name:

  String name for the player that received the last(!) lateral on a run
  play. If there were multiple laterals in the same play, this will only
  be the last player who received a lateral. Please see
  <https://github.com/mrcaseb/nfl-data/tree/master/data/lateral_yards>
  for a list of plays where multiple players recorded lateral rushing
  yards.

- lateral_rushing_yards:

  Numeric yards by the `lateral_rusher_player_name` in run plays with
  laterals. Please see the description of `lateral_rusher_player_name`
  for further information.

- lateral_sack_player_id:

  Unique identifier for the player that received the lateral on a sack.

- lateral_sack_player_name:

  String name for the player that received the lateral on a sack.

- interception_player_id:

  Unique identifier for the player that intercepted the pass.

- interception_player_name:

  String name for the player that intercepted the pass.

- lateral_interception_player_id:

  Unique indentifier for the player that received the lateral on an
  interception.

- lateral_interception_player_name:

  String name for the player that received the lateral on an
  interception.

- punt_returner_player_id:

  Unique identifier for the punt returner.

- punt_returner_player_name:

  String name for the punt returner.

- lateral_punt_returner_player_id:

  Unique identifier for the player that received the lateral on a punt
  return.

- lateral_punt_returner_player_name:

  String name for the player that received the lateral on a punt return.

- kickoff_returner_player_name:

  String name for the kickoff returner.

- kickoff_returner_player_id:

  Unique identifier for the kickoff returner.

- lateral_kickoff_returner_player_id:

  Unique identifier for the player that received the lateral on a
  kickoff return.

- lateral_kickoff_returner_player_name:

  String name for the player that received the lateral on a kickoff
  return.

- punter_player_id:

  Unique identifier for the punter.

- punter_player_name:

  String name for the punter.

- kicker_player_name:

  String name for the kicker on FG or kickoff.

- kicker_player_id:

  Unique identifier for the kicker on FG or kickoff.

- own_kickoff_recovery_player_id:

  Unique identifier for the player that recovered their own kickoff.

- own_kickoff_recovery_player_name:

  String name for the player that recovered their own kickoff.

- blocked_player_id:

  Unique identifier for the player that blocked the punt or FG.

- blocked_player_name:

  String name for the player that blocked the punt or FG.

- tackle_for_loss_1_player_id:

  Unique identifier for one of the potential players with the tackle for
  loss.

- tackle_for_loss_1_player_name:

  String name for one of the potential players with the tackle for loss.

- tackle_for_loss_2_player_id:

  Unique identifier for one of the potential players with the tackle for
  loss.

- tackle_for_loss_2_player_name:

  String name for one of the potential players with the tackle for loss.

- qb_hit_1_player_id:

  Unique identifier for one of the potential players that hit the QB. No
  sack as the QB was not the ball carrier. For sacks please see
  `sack_player` or `half_sack_*_player`.

- qb_hit_1_player_name:

  String name for one of the potential players that hit the QB. No sack
  as the QB was not the ball carrier. For sacks please see `sack_player`
  or `half_sack_*_player`.

- qb_hit_2_player_id:

  Unique identifier for one of the potential players that hit the QB. No
  sack as the QB was not the ball carrier. For sacks please see
  `sack_player` or `half_sack_*_player`.

- qb_hit_2_player_name:

  String name for one of the potential players that hit the QB. No sack
  as the QB was not the ball carrier. For sacks please see `sack_player`
  or `half_sack_*_player`.

- forced_fumble_player_1_team:

  Team of one of the players with a forced fumble.

- forced_fumble_player_1_player_id:

  Unique identifier of one of the players with a forced fumble.

- forced_fumble_player_1_player_name:

  String name of one of the players with a forced fumble.

- forced_fumble_player_2_team:

  Team of one of the players with a forced fumble.

- forced_fumble_player_2_player_id:

  Unique identifier of one of the players with a forced fumble.

- forced_fumble_player_2_player_name:

  String name of one of the players with a forced fumble.

- solo_tackle_1_team:

  Team of one of the players with a solo tackle.

- solo_tackle_2_team:

  Team of one of the players with a solo tackle.

- solo_tackle_1_player_id:

  Unique identifier of one of the players with a solo tackle.

- solo_tackle_2_player_id:

  Unique identifier of one of the players with a solo tackle.

- solo_tackle_1_player_name:

  String name of one of the players with a solo tackle.

- solo_tackle_2_player_name:

  String name of one of the players with a solo tackle.

- assist_tackle_1_player_id:

  Unique identifier of one of the players with a tackle assist.

- assist_tackle_1_player_name:

  String name of one of the players with a tackle assist.

- assist_tackle_1_team:

  Team of one of the players with a tackle assist.

- assist_tackle_2_player_id:

  Unique identifier of one of the players with a tackle assist.

- assist_tackle_2_player_name:

  String name of one of the players with a tackle assist.

- assist_tackle_2_team:

  Team of one of the players with a tackle assist.

- assist_tackle_3_player_id:

  Unique identifier of one of the players with a tackle assist.

- assist_tackle_3_player_name:

  String name of one of the players with a tackle assist.

- assist_tackle_3_team:

  Team of one of the players with a tackle assist.

- assist_tackle_4_player_id:

  Unique identifier of one of the players with a tackle assist.

- assist_tackle_4_player_name:

  String name of one of the players with a tackle assist.

- assist_tackle_4_team:

  Team of one of the players with a tackle assist.

- tackle_with_assist:

  Binary indicator for if there has been a tackle with assist.

- tackle_with_assist_1_player_id:

  Unique identifier of one of the players with a tackle with assist.

- tackle_with_assist_1_player_name:

  String name of one of the players with a tackle with assist.

- tackle_with_assist_1_team:

  Team of one of the players with a tackle with assist.

- tackle_with_assist_2_player_id:

  Unique identifier of one of the players with a tackle with assist.

- tackle_with_assist_2_player_name:

  String name of one of the players with a tackle with assist.

- tackle_with_assist_2_team:

  Team of one of the players with a tackle with assist.

- pass_defense_1_player_id:

  Unique identifier of one of the players with a pass defense.

- pass_defense_1_player_name:

  String name of one of the players with a pass defense.

- pass_defense_2_player_id:

  Unique identifier of one of the players with a pass defense.

- pass_defense_2_player_name:

  String name of one of the players with a pass defense.

- fumbled_1_team:

  Team of one of the first player with a fumble.

- fumbled_1_player_id:

  Unique identifier of the first player who fumbled on the play.

- fumbled_1_player_name:

  String name of one of the first player who fumbled on the play.

- fumbled_2_player_id:

  Unique identifier of the second player who fumbled on the play.

- fumbled_2_player_name:

  String name of one of the second player who fumbled on the play.

- fumbled_2_team:

  Team of one of the second player with a fumble.

- fumble_recovery_1_team:

  Team of one of the players with a fumble recovery.

- fumble_recovery_1_yards:

  Yards gained by one of the players with a fumble recovery.

- fumble_recovery_1_player_id:

  Unique identifier of one of the players with a fumble recovery.

- fumble_recovery_1_player_name:

  String name of one of the players with a fumble recovery.

- fumble_recovery_2_team:

  Team of one of the players with a fumble recovery.

- fumble_recovery_2_yards:

  Yards gained by one of the players with a fumble recovery.

- fumble_recovery_2_player_id:

  Unique identifier of one of the players with a fumble recovery.

- fumble_recovery_2_player_name:

  String name of one of the players with a fumble recovery.

- sack_player_id:

  Unique identifier of the player who recorded a solo sack.

- sack_player_name:

  String name of the player who recorded a solo sack.

- half_sack_1_player_id:

  Unique identifier of the first player who recorded half a sack.

- half_sack_1_player_name:

  String name of the first player who recorded half a sack.

- half_sack_2_player_id:

  Unique identifier of the second player who recorded half a sack.

- half_sack_2_player_name:

  String name of the second player who recorded half a sack.

- return_team:

  String abbreviation of the return team.

- return_yards:

  Yards gained by the return team.

- penalty_team:

  String abbreviation of the team with the penalty.

- penalty_player_id:

  Unique identifier for the player with the penalty.

- penalty_player_name:

  String name for the player with the penalty.

- penalty_yards:

  Yards gained (or lost) by the posteam from the penalty.

- replay_or_challenge:

  Binary indicator for whether or not a replay or challenge.

- replay_or_challenge_result:

  String indicating the result of the replay or challenge.

- penalty_type:

  String indicating the penalty type of the first penalty in the given
  play. Will be `NA` if `desc` is missing the type.

- defensive_two_point_attempt:

  Binary indicator whether or not the defense was able to have an
  attempt on a two point conversion, this results following a turnover.

- defensive_two_point_conv:

  Binary indicator whether or not the defense successfully scored on the
  two point conversion.

- defensive_extra_point_attempt:

  Binary indicator whether or not the defense was able to have an
  attempt on an extra point attempt, this results following a blocked
  attempt that the defense recovers the ball.

- defensive_extra_point_conv:

  Binary indicator whether or not the defense successfully scored on an
  extra point attempt.

- safety_player_name:

  String name for the player who scored a safety.

- safety_player_id:

  Unique identifier for the player who scored a safety.

- season:

  4 digit number indicating to which season the game belongs to.

- cp:

  Numeric value indicating the probability for a complete pass based on
  comparable game situations.

- cpoe:

  For a single pass play this is 1 - cp when the pass was completed or
  0 - cp when the pass was incomplete. Analyzed for a whole game or
  season an indicator for the passer how much over or under expectation
  his completion percentage was.

- series:

  Starts at 1, each new first down increments, numbers shared across
  both teams NA: kickoffs, extra point/two point conversion attempts,
  non-plays, no posteam

- series_success:

  1: scored touchdown, gained enough yards for first down.

- series_result:

  Possible values: First down, Touchdown, Opp touchdown, Field goal,
  Missed field goal, Safety, Turnover, Punt, Turnover on downs, QB
  kneel, End of half

- start_time:

  Kickoff time in eastern time zone.

- order_sequence:

  Column provided by NFL to fix out-of-order plays. Available 2011 and
  beyond with source "nfl".

- time_of_day:

  Time of day of play in UTC "HH:MM:SS" format. Available 2011 and
  beyond with source "nfl".

- stadium:

  Game site name.

- weather:

  String describing the weather including temperature, humidity and wind
  (direction and speed). Doesn't change during the game!

- nfl_api_id:

  UUID of the game in the new NFL API.

- play_clock:

  Time on the playclock when the ball was snapped.

- play_deleted:

  Binary indicator for deleted plays.

- play_type_nfl:

  Play type as listed in the NFL source. Slightly different to the
  regular play_type variable.

- special_teams_play:

  Binary indicator for whether play is special teams play from NFL
  source. Available 2011 and beyond with source "nfl".

- st_play_type:

  Type of special teams play from NFL source. Available 2011 and beyond
  with source "nfl".

- end_clock_time:

  Game time at the end of a given play.

- end_yard_line:

  String indicating the yardline at the end of the given play consisting
  of team half and yard line number.

- drive_real_start_time:

  Local day time when the drive started (currently not used by the NFL
  and therefore mostly 'NA').

- drive_play_count:

  Numeric value of how many regular plays happened in a given drive.

- drive_time_of_possession:

  Time of possession in a given drive.

- drive_first_downs:

  Number of first downs in a given drive.

- drive_inside20:

  Binary indicator if the offense was able to get inside the opponents
  20 yard line.

- drive_ended_with_score:

  Binary indicator the drive ended with a score.

- drive_quarter_start:

  Numeric value indicating in which quarter the given drive has started.

- drive_quarter_end:

  Numeric value indicating in which quarter the given drive has ended.

- drive_yards_penalized:

  Numeric value of how many yards the offense gained or lost through
  penalties in the given drive.

- drive_start_transition:

  String indicating how the offense got the ball.

- drive_end_transition:

  String indicating how the offense lost the ball.

- drive_game_clock_start:

  Game time at the beginning of a given drive.

- drive_game_clock_end:

  Game time at the end of a given drive.

- drive_start_yard_line:

  String indicating where a given drive started consisting of team half
  and yard line number.

- drive_end_yard_line:

  String indicating where a given drive ended consisting of team half
  and yard line number.

- drive_play_id_started:

  Play_id of the first play in the given drive.

- drive_play_id_ended:

  Play_id of the last play in the given drive.

- fixed_drive:

  Manually created drive number in a game.

- fixed_drive_result:

  Manually created drive result.

- away_score:

  Total points scored by the away team.

- home_score:

  Total points scored by the home team.

- location:

  Either 'Home' o 'Neutral' indicating if the home team played at home
  or at a neutral site.

- result:

  Equals home_score - away_score and means the game outcome from the
  perspective of the home team.

- total:

  Equals home_score + away_score and means the total points scored in
  the given game.

- spread_line:

  The closing spread line for the game. A positive number means the home
  team was favored by that many points, a negative number means the away
  team was favored by that many points. (Source: Pro-Football-Reference)

- total_line:

  The closing total line for the game. (Source: Pro-Football-Reference)

- div_game:

  Binary indicator for if the given game was a division game.

- roof:

  One of 'dome', 'outdoors', 'closed', 'open' indicating indicating the
  roof status of the stadium the game was played in. (Source:
  Pro-Football-Reference)

- surface:

  What type of ground the game was played on. (Source:
  Pro-Football-Reference)

- temp:

  The temperature at the stadium only for 'roof' = 'outdoors' or
  'open'.(Source: Pro-Football-Reference)

- wind:

  The speed of the wind in miles/hour only for 'roof' = 'outdoors' or
  'open'. (Source: Pro-Football-Reference)

- home_coach:

  First and last name of the home team coach. (Source:
  Pro-Football-Reference)

- away_coach:

  First and last name of the away team coach. (Source:
  Pro-Football-Reference)

- stadium_id:

  ID of the stadium the game was played in. (Source:
  Pro-Football-Reference)

- game_stadium:

  Name of the stadium the game was played in. (Source:
  Pro-Football-Reference)

## Details

To load valid game_ids please use the package function
[`fast_scraper_schedules`](https://nflfastr.com/reference/fast_scraper_schedules.md)
(the function can directly handle the output of that function)

## See also

For information on parallel processing and progress updates please see
[nflfastR](https://nflfastr.com/reference/nflfastR-package.md).

[`build_nflfastR_pbp()`](https://nflfastr.com/reference/build_nflfastR_pbp.md),
[`save_raw_pbp()`](https://nflfastr.com/reference/save_raw_pbp.md)

## Examples

``` r
# \donttest{
# Get pbp data for two games
try({# to avoid CRAN test problems
fast_scraper(c("2019_01_GB_CHI", "2013_21_SEA_DEN"))
})
#> ℹ It is recommended to use parallel processing when trying to load multiple games.Please consider running `future::plan("multisession")`! Will go on sequentially...
#> ✔ 07:36:10 | Download finished. Adding variables...
#> ✔ 07:36:10 | added game variables
#> ✔ 07:36:10 | added nflscrapR variables
#> ✔ 07:36:10 | added ep variables
#> ✔ 07:36:11 | added air_yac_ep variables
#> ✔ 07:36:11 | added wp variables
#> ✔ 07:36:11 | added air_yac_wp variables
#> ✔ 07:36:11 | added cp and cpoe
#> ✔ 07:36:11 | added fixed drive variables
#> ✔ 07:36:11 | added series variables
#> ✔ 07:36:11 | Procedure completed.
#> ── nflverse play by play ───────────────────────────────────────────────────────
#> ℹ Data updated: 2026-02-18 07:36:11 UTC
#> # A tibble: 337 × 339
#>    play_id game_id     old_game_id home_team away_team season_type  week posteam
#>      <dbl> <chr>       <chr>       <chr>     <chr>     <chr>       <int> <chr>  
#>  1       1 2013_21_SE… 2014020200  DEN       SEA       POST           21 NA     
#>  2      37 2013_21_SE… 2014020200  DEN       SEA       POST           21 DEN    
#>  3      61 2013_21_SE… 2014020200  DEN       SEA       POST           21 DEN    
#>  4      83 2013_21_SE… 2014020200  DEN       SEA       POST           21 DEN    
#>  5     119 2013_21_SE… 2014020200  DEN       SEA       POST           21 SEA    
#>  6     141 2013_21_SE… 2014020200  DEN       SEA       POST           21 SEA    
#>  7     162 2013_21_SE… 2014020200  DEN       SEA       POST           21 SEA    
#>  8     183 2013_21_SE… 2014020200  DEN       SEA       POST           21 SEA    
#>  9     210 2013_21_SE… 2014020200  DEN       SEA       POST           21 SEA    
#> 10     232 2013_21_SE… 2014020200  DEN       SEA       POST           21 SEA    
#> # ℹ 327 more rows
#> # ℹ 331 more variables: posteam_type <chr>, defteam <chr>, side_of_field <chr>,
#> #   yardline_100 <dbl>, game_date <chr>, quarter_seconds_remaining <dbl>,
#> #   half_seconds_remaining <dbl>, game_seconds_remaining <dbl>,
#> #   game_half <chr>, quarter_end <dbl>, drive <dbl>, sp <dbl>, qtr <dbl>,
#> #   down <dbl>, goal_to_go <dbl>, time <chr>, yrdln <chr>, ydstogo <dbl>,
#> #   ydsnet <dbl>, desc <chr>, play_type <chr>, yards_gained <dbl>, …


# It is also possible to directly use the
# output of `fast_scraper_schedules` as input
try({# to avoid CRAN test problems
library(dplyr, warn.conflicts = FALSE)
fast_scraper_schedules(2020) |>
  slice_tail(n = 3) |>
  fast_scraper()
})
#> Warning: `fast_scraper_schedules()` was deprecated in nflfastR 5.2.0.
#> ℹ Please use `nflreadr::load_schedules()` instead.
#> ℹ It is recommended to use parallel processing when trying to load multiple games.Please consider running `future::plan("multisession")`! Will go on sequentially...
#> ✔ 07:36:14 | Download finished. Adding variables...
#> ✔ 07:36:14 | added game variables
#> ✔ 07:36:14 | added nflscrapR variables
#> ✔ 07:36:15 | added ep variables
#> ✔ 07:36:15 | added air_yac_ep variables
#> ✔ 07:36:15 | added wp variables
#> ✔ 07:36:15 | added air_yac_wp variables
#> ✔ 07:36:15 | added cp and cpoe
#> ✔ 07:36:15 | added fixed drive variables
#> ✔ 07:36:15 | added series variables
#> ✔ 07:36:15 | Procedure completed.
#> ── nflverse play by play ───────────────────────────────────────────────────────
#> ℹ Data updated: 2026-02-18 07:36:15 UTC
#> # A tibble: 539 × 339
#>    play_id game_id     old_game_id home_team away_team season_type  week posteam
#>      <dbl> <chr>       <chr>       <chr>     <chr>     <chr>       <int> <chr>  
#>  1       1 2020_20_BU… 2021012401  KC        BUF       POST           20 NA     
#>  2      42 2020_20_BU… 2021012401  KC        BUF       POST           20 BUF    
#>  3      57 2020_20_BU… 2021012401  KC        BUF       POST           20 BUF    
#>  4      78 2020_20_BU… 2021012401  KC        BUF       POST           20 BUF    
#>  5     102 2020_20_BU… 2021012401  KC        BUF       POST           20 BUF    
#>  6     123 2020_20_BU… 2021012401  KC        BUF       POST           20 BUF    
#>  7     145 2020_20_BU… 2021012401  KC        BUF       POST           20 BUF    
#>  8     174 2020_20_BU… 2021012401  KC        BUF       POST           20 BUF    
#>  9     207 2020_20_BU… 2021012401  KC        BUF       POST           20 BUF    
#> 10     236 2020_20_BU… 2021012401  KC        BUF       POST           20 BUF    
#> # ℹ 529 more rows
#> # ℹ 331 more variables: posteam_type <chr>, defteam <chr>, side_of_field <chr>,
#> #   yardline_100 <dbl>, game_date <chr>, quarter_seconds_remaining <dbl>,
#> #   half_seconds_remaining <dbl>, game_seconds_remaining <dbl>,
#> #   game_half <chr>, quarter_end <dbl>, drive <dbl>, sp <dbl>, qtr <dbl>,
#> #   down <dbl>, goal_to_go <dbl>, time <chr>, yrdln <chr>, ydstogo <dbl>,
#> #   ydsnet <dbl>, desc <chr>, play_type <chr>, yards_gained <dbl>, …

# }
```
