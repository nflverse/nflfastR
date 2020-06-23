# nflfastR 2.0.5

* Fix yet another bug caused by NFL providing plays out of order
* Fix bugs related to penalties on PATs and kickoffs
* Fix bugs related to NFL providing wrong scoring team on defensive touchdowns in older games involving the Jaguars
* Fix some minor issues related to wrong `first_down_rush` and `return_touchdown`
* Improved error handling of `fast_scraper()` for not yet played games
* Improved variable documentation and prepared for new website
* Improved performance for dplyr v1.0.0

# nflfastR 2.0.4

* Fix another bug with out of order plays
* Fix bug affecting cumulative totals for WPA, air_WPA and yac_WPA 
* Fix bug affecting cumulative totals for air_EPA and yac_EPA

# nflfastR 2.0.3

* Fix for NFL providing plays out of order
* Fix for series not incrementing following defensive TD

# nflfastR 2.0.2

* Fixed a bug in the series and series success calculations caused by timeouts
following a possession change
* Fixed win probability on PATs

# nflfastR 2.0.1

* Added minimum version requirement on `xgboost` (>= 1.1) as the recent `xgboost` update 
caused a breaking change leading to failure in adding model results to data

# nflfastR 2.0.0

### Models
* Added new models for Expected Points, Win Probability and Completion Probability 
and removed `nflscrapR` dependency. This is a **major** change as we are stepping away 
from the well established `nflscrapR` models. But we believe it is a good step forward.
See `data-raw/MODEL-README.md` for detailed model information.

* Added internal functions for `EPA` and `WPA` to `helper_add_ep_wp.R`.

* Added new function `calculate_expected_points()` usable for the enduser.

### Functions
* Completely overhauled `fast_scraper()` to make it work with the NFL's new server 
backend. The option `source` is still available but will be deprecated since there
is only one source now. There are some changes in the output as well (please see below).

* `fast_scraper()` now adds game data to the play by play data set courtesy of Lee Sharpe. 
Game data include:
away_score, home_score, location, result, total, spread_line, total_line, div_game, 
roof, surface, temp, wind, home_coach, away_coach, stadium, stadium_id, gameday

* `fastcraper_schedules()` now incorporates Lee Sharpe's `games.rds`.

* The functions `fast_scraper_clips()` and `fast_scraper_roster()` are deactivated 
due to the missing data source. They might be reactivated or completely dropped 
in future versions.

* The function `fix_fumbles()` has been renamed to `add_qb_epa()` as the new name
much better describes what the function is actually doing.

### Miscellaneous

* Added progress information using the `progressr`package and removed the 
`furrr` progress bars.

* `clean_pbp()` now adds the column `ìd` which is the id of the player in the column `name`. 
Because we have to piece together different data to cover the full span of years,
**player IDs are not consistent between the early (1999-2010) and recent (2011 onward)
periods**.

* Added a `NEWS.md` file to track changes to the package.

* Fixed several bugs inhereted from `nflscrapR`, including one where EPA was missing 
when a play was followed by two timeouts (for example, a two-minute warning followed by a timeout),
and another where `play_type` was incorrect on plays with declined penalties.

* Fixed a bug, where `receiver_player_name` and `receiver` didn't name the correct
players on plays with lateral passes.

### Play-by-Play Output
The output has changed a little bit. 

#### The following variables were dropped

| Dropped Variables          | Description                                                                                                                                                                       |
|----------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| game_key                   | RS feed game identifier.                                                                                                                                                          |
| game_time_local            | Kickoff time in local time zone.                                                                                                                                                  |
| iso_time                   | Kickoff time according ISO 8601.                                                                                                                                                  |
| game_type                  | One of 'REG', 'WC', 'DIV', 'CON', 'SB' indicating if a game was a regular season game or one of the playoff rounds.                                                               |
| site_id                    | RS feed id for game site.                                                                                                                                                         |
| site_city                  | Game site city.                                                                                                                                                                   |
| site_state                 | Game site state.                                                                                                                                                                  |
| drive_possession_team_abbr | Abbreviation of the possession team in a given drive.                                                                                                                             |
| scoring_team_abbr          | Abbreviation of the scoring team if the play was a scoring play.                                                                                                                  |
| scoring_type               | String indicating the scoring type. One of 'FG', 'TD', 'PAT', 'SFTY', 'PAT2'.                                                                                                     |
| alert_play_type            | String describing the play type of a play the NFL has listed as alert play. For most of those plays there are highlight clips available through fast_scraper_clips. |
| time_of_day                | Local time at the beginning of the play.                                                                                                                                          |
| yards                      | Analogue yards_gained but with the kicking team being the possession team (which means that there are many yards gained through kickoffs and punts).                              |
| end_yardline_number        | Yardline number within the above given side at the end of the given play.                                                                                                         |
| end_yardline_side          | String indicating the side of the field at the end of the given play.                                                                                                             |

#### The following variables were renamed

| Renamed Variables                             | Description                                                                                                                                               |
|-----------------------------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------|
| game_time_eastern -> start_time               | Kickoff time in eastern time zone.                                                                                                                        |
| site_fullname -> stadium                      | Game site name.                                                                                                                                           |
| drive_how_started -> drive_start_transition   | String indicating how the offense got the ball.                                                                                                           |
| drive_how_ended -> drive_end_transition       | String indicating how the offense lost the ball.                                                                                                          |
| drive_start_time -> drive_game_clock_start    | Game time at the beginning of a given drive.                                                                                                              |
| drive_end_time -> drive_game_clock_end        | Game time at the end of a given drive.                                                                                                                    |
| drive_start_yardline -> drive_start_yard_line | String indicating where a given drive started consisting of team half and yard line number.                                                               |
| drive_end_yardline -> drive_end_yard_line     | String indicating where a given drive ended consisting of team half and yard line number.                                                                 |
| roof_type -> roof                             | One of 'dome', 'outdoors', 'closed', 'open' indicating indicating the roof status of the stadium the game was played in. (Source: Pro-Football-Reference) |

#### The following variables were added

| Added Variables        | Description                                                                                                                                                                                                          |
|------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| vegas_wp               | Estimated win probabiity for the posteam given the current situation at the start of the given play, incorporating pre-game Vegas line.                                                                              |
| vegas_home_wp          | Estimated win probability for the home team incorporating pre-game Vegas line.                                                                                                                                       |
| weather                | String describing the weather including temperature, humidity and wind (direction and speed). Doesn't change during the game!                                                                                        |
| nfl_api_id             | UUID of the game in the new NFL API.                                                                                                                                                                                 |
| play_clock             | Time on the playclock when the ball was snapped.                                                                                                                                                                     |
| play_deleted           | Binary indicator for deleted plays.                                                                                                                                                                                  |
| end_clock_time         | Game time at the end of a given play.                                                                                                                                                                                |
| end_yard_line          | String indicating the yardline at the end of the given play consisting of team half and yard line number.                                                                                                            |
| drive_real_start_time  | Local day time when the drive started (currently not used by the NFL and therefore mostly 'NA').                                                                                                                     |
| drive_ended_with_score | Binary indicator the drive ended with a score.                                                                                                                                                                       |
| drive_quarter_start    | Numeric value indicating in which quarter the given drive has started.                                                                                                                                               |
| drive_quarter_end      | Numeric value indicating in which quarter the given drive has ended.                                                                                                                                                 |
| drive_play_id_started  | Play_id of the first play in the given drive.                                                                                                                                                                        |
| drive_play_id_ended    | Play_id of the last play in the given drive.                                                                                                                                                                         |
| away_score             | Total points scored by the away team.                                                                                                                                                                                |
| home_score             | Total points scored by the home team.                                                                                                                                                                                |
| location               | Either 'Home' o 'Neutral' indicating if the home team played at home or at a neutral site.                                                                                                                           |
| result                 | Equals home_score - away_score and means the game outcome from the perspective of the home team.                                                                                                                     |
| total                  | Equals home_score + away_score and means the total points scored in the given game.                                                                                                                                  |
| spread_line            | The closing spread line for the game. A positive number means the home team was favored by that many points, a negative number means the away team was favored by that many points. (Source: Pro-Football-Reference) |
| total_line             | The closing total line for the game. (Source: Pro-Football-Reference)                                                                                                                                                |
| div_game               | Binary indicator for if the given game was a division game.                                                                                                                                                          |
| roof                   | One of 'dome', 'outdoors', 'closed', 'open' indicating indicating the roof status of the stadium the game was played in. (Source: Pro-Football-Reference)                                                            |
| surface                | What type of ground the game was played on. (Source: Pro-Football-Reference)                                                                                                                                         |
| temp                   | The temperature at the stadium only for 'roof' = 'outdoors' or 'open'.(Source: Pro-Football-Reference)                                                                                                               |
| wind                   | The speed of the wind in miles/hour only for 'roof' = 'outdoors' or 'open'. (Source: Pro-Football-Reference)                                                                                                         |
| home_coach             | First and last name of the home team coach. (Source: Pro-Football-Reference)                                                                                                                                         |
| away_coach             | First and last name of the away team coach. (Source: Pro-Football-Reference)                                                                                                                                         |
| stadium_id             | ID of the stadium the game was played in. (Source: Pro-Football-Reference)                                                                                                                                           |
| game_stadium           | Name of the stadium the game was played in. (Source: Pro-Football-Reference)                                                                                                                                         |
