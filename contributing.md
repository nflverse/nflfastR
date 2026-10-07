Contributing to nflfastR
================

Thanks for your interest in contributing! There are many ways to make a
valuable contribution to an open-source project, such as issues or pull
requests.

This guide focuses specifically on how users (or AI agents) can make
specific data corrections if the raw data contains minor errors.

Basically, there are two possible reasons why data in nflfastR might be
incorrect:

1.  The parser has a bug and does not handle a specific situation
    correctly
2.  The raw data is incorrect

In the first case, please submit issues or pull requests that explain
the problem and, if necessary, propose changes to the parser.

For the second case, nflfastR provides a mechanism for manually
overwriting the data before the parser applies its usual modifications
to the raw data.

## Contribute manual data corrections

Manual corrections should be the last resort when we have no other way
to fix an error. Alternatively, we can also use them if a fix in the
parser would be very time-consuming or potentially prone to errors. A
typical example is rare errors in the raw data, particularly in the raw
data from older seasons. So if there are only a handful of incorrect
values, this might be the right approach.

Here’s how to submit manual corrections:

1.  fork the repo
2.  create new branch (we have no naming conventions)
3.  add patch data to `"inst/patch_pbp.json"` and save the file
4.  run `devtools::load_all()`
5.  run `.patch_clean_data()` interactively to ensure correct sorting
    and to remove duplicates
6.  run `devtools::test(filter = "patch_pbp")`. This will run tests
    aimed at the file format of the patch file in
    `"inst/patch_pbp.json"`.
7.  run `build_nflfastR_pbp()` with the `game_id`s affected by your
    correction to confirm that your patch works as intended
8.  commit and push
9.  create the pull request

A patch in the `"inst/patch_pbp.json"` file looks like this

``` json
[
  {
    "game_id": "2015_01_IND_BUF",
    "play_id": 1207,
    "column": "quarter",
    "value": [2]
  },
  {
    "game_id": "2014_07_ATL_BAL",
    "play_id": 99999,
    "column": "start_time",
    "value": ["10/19/14, 13:02:00"]
  }
]
```

Some notes:

- The first entry overwrites the `qtr` variable (special name, see
  table 1) in the play-by-play data of `game_id 2015_01_IND_BUF` and
  `play_id 1207` with the integer `2`.
- The second entry overwrites the `start_time` variable in the
  play-by-play data of the complete `game_id 2014_07_ATL_BAL` with the
  string `"10/19/14, 13:02:00"`.
- Please note the `[]` brackets in the value field. They allow us to
  save `value` in different classes, i.e. `integer`, `numeric`,
  `character`. We apply validation to ensure that this value is a list
  of length one.
- Integers and numerics don’t need quotes. We want to preserve their
  classes.
- The field `play_id` is also unquoted and should be an integer.
- The special `play_id 99999` is a placeholder and tells nflfastR to
  apply the patch to the complete column, i.e. all `play_id`s of the
  specified `game_id`.
- Only `value` is allowed to be `NA`, which means you would like to
  overwrite specifically with `"NA"`.
- Valid column names to be patched are listed in table 2 below.
- There are five more or less special column names that are renamed in
  the parsing process (that’s a legacy thing we inherited from nflscrapR
  for backwards compatibility). This means that five column names are
  different in the patch process than in the final pbp. The following
  Table 1 shows the column names used in the final pbp and how they must
  be named in the patch file.

**Table 1: Column names in final `pbp` matched to their name in the
`column` field of `"inst/patch_pbp.json"`**

| pbp name      | patch file name  |
|:--------------|:-----------------|
| qtr           | quarter          |
| yrdln         | yardline         |
| ydstogo       | yards_to_go      |
| side_of_field | yardline_side    |
| desc          | play_description |

Package tests are designed to enforce the above described format
criteria.

**Table 2: Column names allowed in the `column` field of
`"inst/patch_pbp.json"`**

|  |  |  |  |
|:---|:---|:---|:---|
| nfl_api_id | kickoff_fair_catch | lateral_sack_player_id | tackle_with_assist_2_team |
| weather | fumble_forced | lateral_sack_player_name | pass_defense_1_player_id |
| stadium | fumble_not_forced | interception_player_id | pass_defense_1_player_name |
| start_time | fumble_out_of_bounds | interception_player_name | pass_defense_2_player_id |
| time | timeout | lateral_interception_player_id | pass_defense_2_player_name |
| down | field_goal_missed | lateral_interception_player_name | fumbled_1_team |
| drive | field_goal_made | punt_returner_player_id | fumbled_1_player_id |
| end_clock_time | field_goal_blocked | punt_returner_player_name | fumbled_1_player_name |
| end_yard_line | extra_point_good | lateral_punt_returner_player_id | fumbled_2_player_id |
| first_down | extra_point_failed | lateral_punt_returner_player_name | fumbled_2_player_name |
| goal_to_go | extra_point_blocked | kickoff_returner_player_name | fumbled_2_team |
| order_sequence | two_point_rush_good | kickoff_returner_player_id | fumble_recovery_1_team |
| play_clock | two_point_rush_failed | lateral_kickoff_returner_player_id | fumble_recovery_1_yards |
| play_description | two_point_pass_good | lateral_kickoff_returner_player_name | fumble_recovery_1_player_id |
| play_id | two_point_pass_failed | punter_player_id | fumble_recovery_1_player_name |
| play_type_nfl | solo_tackle | punter_player_name | fumble_recovery_2_team |
| quarter | safety | kicker_player_name | fumble_recovery_2_yards |
| sp | penalty | kicker_player_id | fumble_recovery_2_player_id |
| special_teams_play | tackled_for_loss | own_kickoff_recovery_player_id | fumble_recovery_2_player_name |
| st_play_type | extra_point_safety | own_kickoff_recovery_player_name | td_team |
| time_of_day | two_point_rush_safety | blocked_player_id | return_team |
| yardline | two_point_pass_safety | blocked_player_name | timeout_team |
| yards_to_go | kickoff_downed | tackle_for_loss_1_player_id | yards_gained |
| posteam | two_point_pass_reception_good | tackle_for_loss_1_player_name | return_yards |
| drive_quarter_start | two_point_pass_reception_failed | tackle_for_loss_2_player_id | air_yards |
| drive_end_transition | fumble_lost | tackle_for_loss_2_player_name | yards_after_catch |
| drive_end_yard_line | own_kickoff_recovery | qb_hit_1_player_id | penalty_team |
| drive_ended_with_score | own_kickoff_recovery_td | qb_hit_1_player_name | penalty_player_id |
| drive_first_downs | qb_hit | qb_hit_2_player_id | penalty_player_name |
| drive_game_clock_end | extra_point_aborted | qb_hit_2_player_name | penalty_yards |
| drive_game_clock_start | two_point_return | forced_fumble_player_1_team | kick_distance |
| drive_inside20 | rush_attempt | forced_fumble_player_1_player_id | defensive_two_point_attempt |
| drive_play_count | pass_attempt | forced_fumble_player_1_player_name | defensive_two_point_conv |
| drive_play_id_ended | sack | forced_fumble_player_2_team | defensive_extra_point_attempt |
| drive_play_id_started | touchdown | forced_fumble_player_2_player_id | defensive_extra_point_conv |
| drive_quarter_end | pass_touchdown | forced_fumble_player_2_player_name | rushing_yards |
| drive_real_start_time | rush_touchdown | solo_tackle_1_team | lateral_rushing_yards |
| drive_start_transition | return_touchdown | solo_tackle_2_team | passing_yards |
| drive_start_yard_line | extra_point_attempt | solo_tackle_1_player_id | receiving_yards |
| drive_time_of_possession | two_point_attempt | solo_tackle_2_player_id | lateral_receiving_yards |
| drive_yards_penalized | field_goal_attempt | solo_tackle_1_player_name | td_player_id |
| ydsnet | kickoff_attempt | solo_tackle_2_player_name | td_player_name |
| punt_blocked | punt_attempt | assist_tackle_1_player_id | sack_player_id |
| first_down_rush | fumble | assist_tackle_1_player_name | sack_player_name |
| first_down_pass | complete_pass | assist_tackle_1_team | half_sack_1_player_id |
| first_down_penalty | assist_tackle | assist_tackle_2_player_id | half_sack_1_player_name |
| third_down_converted | lateral_reception | assist_tackle_2_player_name | half_sack_2_player_id |
| third_down_failed | lateral_rush | assist_tackle_2_team | half_sack_2_player_name |
| fourth_down_converted | lateral_return | assist_tackle_3_player_id | safety_player_name |
| fourth_down_failed | lateral_recovery | assist_tackle_3_player_name | safety_player_id |
| incomplete_pass | passer_player_id | assist_tackle_3_team | yardline_side |
| interception | passer_player_name | assist_tackle_4_player_id | yardline_number |
| punt_inside_twenty | receiver_player_id | assist_tackle_4_player_name | quarter_end |
| punt_in_endzone | receiver_player_name | assist_tackle_4_team | safety_team |
| punt_out_of_bounds | rusher_player_id | tackle_with_assist | — |
| punt_downed | rusher_player_name | tackle_with_assist_1_player_id | — |
| punt_fair_catch | lateral_receiver_player_id | tackle_with_assist_1_player_name | — |
| kickoff_inside_twenty | lateral_receiver_player_name | tackle_with_assist_1_team | — |
| kickoff_in_endzone | lateral_rusher_player_id | tackle_with_assist_2_player_id | — |
| kickoff_out_of_bounds | lateral_rusher_player_name | tackle_with_assist_2_player_name | — |
