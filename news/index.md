# Changelog

## nflfastR (development version)

- Added new function
  [`update_pbp_db()`](https://nflfastr.com/reference/update_pbp_db.md),
  a fresh approach to the database helper.
  ([\#544](https://github.com/nflverse/nflfastR/issues/544))

## nflfastR 5.2.0

CRAN release: 2026-02-07

- Bump required fastrmodels version to 2.0 for better compatibility with
  xgboost.
- Fixed an issue with duplicated play IDs in some 2000 games.
  ([\#521](https://github.com/nflverse/nflfastR/issues/521))
- Added the argument `pbp` to
  [`calculate_stats()`](https://nflfastr.com/reference/calculate_stats.md)
  to allow stats calculation based on subsets of nflverse play-by-play
  data. ([\#524](https://github.com/nflverse/nflfastR/issues/524))
- Fixed a bug where
  [`calculate_stats()`](https://nflfastr.com/reference/calculate_stats.md)
  didn’t count 60 yard field goal attempts in `"fg_made_60_"` and
  `"fg_missed_60_"`.
  ([\#531](https://github.com/nflverse/nflfastR/issues/531))
- Fixed a bug where
  [`clean_pbp()`](https://nflfastr.com/reference/clean_pbp.md) did not
  provide a passer on plays where scrambles where manually adjusted
  based on data from Aaron Schatz.
  ([\#536](https://github.com/nflverse/nflfastR/issues/536))
- nflfastR now directly reexports nflreadr’s
  [`load_pbp()`](https://nflreadr.nflverse.com/reference/load_pbp.html),
  [`load_player_stats()`](https://nflreadr.nflverse.com/reference/load_player_stats.html),
  and
  [`load_team_stats()`](https://nflreadr.nflverse.com/reference/load_team_stats.html).
  This means that the functions can be called normally via nflfastR, but
  are no longer available in the documentation (whether in the R Help or
  on the pkgdown website). Instead, only links to nflreadr are included.
  This ensures that the documentation is always up to date.
  ([\#538](https://github.com/nflverse/nflfastR/issues/538))
- [`fast_scraper_roster()`](https://nflfastr.com/reference/fast_scraper_roster.md)
  and
  [`fast_scraper_schedules()`](https://nflfastr.com/reference/fast_scraper_schedules.md)
  are officially deprecated and will be removed in a future update.
  Please use
  [`load_rosters()`](https://nflreadr.nflverse.com/reference/load_rosters.html)
  and
  [`load_schedules()`](https://nflreadr.nflverse.com/reference/load_schedules.html).
  ([\#539](https://github.com/nflverse/nflfastR/issues/539))
- [`report()`](https://nflfastr.com/reference/report.md) is deprecated
  and will be removed in a future update. Please use
  [`nflverse_sitrep()`](https://nflreadr.nflverse.com/reference/sitrep.html).
  ([\#540](https://github.com/nflverse/nflfastR/issues/540))
- Fixed incompatibility with xgboost v3 model outputs.
  ([\#553](https://github.com/nflverse/nflfastR/issues/553))
- Added `"Kickoff Out of Bounds"` (introduced in the 2024 season) to the
  `penalty_type` variable in play-by-play.
  ([\#560](https://github.com/nflverse/nflfastR/issues/560))

Thank you to [@Doug-Analytics](https://github.com/Doug-Analytics),
[@isaactpetersen](https://github.com/isaactpetersen),
[@jeleff1000](https://github.com/jeleff1000),
[@JoeMarino2021](https://github.com/JoeMarino2021),
[@kbannon77](https://github.com/kbannon77),
[@lancejames35](https://github.com/lancejames35),
[@LinkedInMindset](https://github.com/LinkedInMindset),
[@manbradcalf](https://github.com/manbradcalf),
[@mrcaseb](https://github.com/mrcaseb),
[@thedfszone](https://github.com/thedfszone),
[@TheMathNinja](https://github.com/TheMathNinja), and
[@zaynpatel](https://github.com/zaynpatel) for their questions,
feedback, and contributions towards this release.

## nflfastR 5.1.0

CRAN release: 2025-05-14

- The function
  [`calculate_standings()`](https://nflfastr.com/reference/calculate_standings.md)
  has been deprecated. Please use
  [`nflseedR::nfl_standings()`](https://nflseedr.com/reference/nfl_standings.html)
  in nflseedR v2.0 instead.
  ([\#510](https://github.com/nflverse/nflfastR/issues/510))
- nflfastR now requires R 4.1 to allow the package to use R’s native
  pipe `|>` operator. This follows the [Tidyverse R version support
  rules](https://tidyverse.org/blog/2019/04/r-version-support/).
  ([\#511](https://github.com/nflverse/nflfastR/issues/511))
- Fixed a bug where
  [`calculate_stats()`](https://nflfastr.com/reference/calculate_stats.md)
  incorrectly counted `receiving_air_yards`.
  ([\#500](https://github.com/nflverse/nflfastR/issues/500))
- Fixed a bug where `vegas_wp` variables were broken when `spread_line`
  data was missing.
  ([\#503](https://github.com/nflverse/nflfastR/issues/503))
- Fixed a bug where
  [`calculate_stats()`](https://nflfastr.com/reference/calculate_stats.md)
  incorrectly calculated `target_share` and `air_yards_share` when
  `summary_level = "season"`.
  ([\#505](https://github.com/nflverse/nflfastR/issues/505))
- Fixed a bug where
  [`calculate_stats()`](https://nflfastr.com/reference/calculate_stats.md)
  incorrectly counted `fumbles`.
  ([\#514](https://github.com/nflverse/nflfastR/issues/514))
- Compatibility improvements with xgboost.
  ([\#517](https://github.com/nflverse/nflfastR/issues/517))

Thank you to [@ak47twq](https://github.com/ak47twq),
[@isaactpetersen](https://github.com/isaactpetersen),
[@jacobakaye](https://github.com/jacobakaye),
[@johnpholden](https://github.com/johnpholden),
[@marvin3FF](https://github.com/marvin3FF),
[@mrcaseb](https://github.com/mrcaseb), and
[@tanho63](https://github.com/tanho63) for their questions, feedback,
and contributions towards this release.

## nflfastR 5.0.0

CRAN release: 2024-11-26

### Major Changes

- Added new function
  [`calculate_stats()`](https://nflfastr.com/reference/calculate_stats.md)
  that combines the output of all `calculate_player_stats*()` functions
  with a more robust and faster approach. The
  `calculate_player_stats*()` function will be deprecated in a future
  release. ([\#470](https://github.com/nflverse/nflfastR/issues/470))
- Added new exported dataframe `nfl_stats_variables`. It lists and
  explains all variables returned by
  [`calculate_stats()`](https://nflfastr.com/reference/calculate_stats.md).
  A searchable table is available at
  <https://nflfastr.com/articles/stats_variables.html>.
  ([\#470](https://github.com/nflverse/nflfastR/issues/470))

### Bug Fixes and Minor Changes

- Drop [crayon](https://r-lib.github.io/crayon/),
  [DT](https://github.com/rstudio/DT), [httr](https://httr.r-lib.org/),
  [jsonlite](https://jeroen.r-universe.dev/jsonlite),
  [qs](https://github.com/qsbase/qs) dependencies.
  ([\#453](https://github.com/nflverse/nflfastR/issues/453))
- The function `calculate_player_stats_def` now returns `season_type` if
  argument `weekly` is set to `TRUE` for consistency with the other
  player stats functions.
  ([\#455](https://github.com/nflverse/nflfastR/issues/455))
- The function
  [`missing_raw_pbp()`](https://nflfastr.com/reference/missing_raw_pbp.md)
  now allows filtering by season.
  ([\#457](https://github.com/nflverse/nflfastR/issues/457))
- More robust handling of player IDs in
  [`decode_player_ids()`](https://nflfastr.com/reference/decode_player_ids.md).
  ([\#458](https://github.com/nflverse/nflfastR/issues/458))
- Fixed rare cases where the value of the `yrdln` variable didn’t equal
  `"MID 50"` at midfield.
  ([\#459](https://github.com/nflverse/nflfastR/issues/459))
- Fixed rare cases where `drive_start_yard_line` missed the blank space
  between team name and yard line number.
  ([\#459](https://github.com/nflverse/nflfastR/issues/459))
- Fixed play description in some 1999 and 2000 games where the string
  “D.Holland” replaced the kick distance.
  ([\#459](https://github.com/nflverse/nflfastR/issues/459))
- Fixed a problem where the `goal_to_go` variable was `FALSE` in actual
  goal to go situations.
  ([\#460](https://github.com/nflverse/nflfastR/issues/460))
- Fixed a bug in `fixed_drive` and `fixed_drive_result` where the second
  weather delay in `2023_13_ARI_PIT` wasn’t identified correctly.
  ([\#461](https://github.com/nflverse/nflfastR/issues/461))
- `punter_player_id`, and `punter_player_name` are filled for blocked
  punt attempts.
  ([\#463](https://github.com/nflverse/nflfastR/issues/463))
- Fixed an issue affecting scores of 2022 games involving a return
  touchdown ([\#466](https://github.com/nflverse/nflfastR/issues/466))
- Added identification of scrambles from 1999 through 2004 with thank to
  Aaron Schatz
  ([\#468](https://github.com/nflverse/nflfastR/issues/468),
  [\#489](https://github.com/nflverse/nflfastR/issues/489))
- Updated the dataframe `stat_ids` with some IDs that were previously
  missing. ([\#470](https://github.com/nflverse/nflfastR/issues/470))
- nflfastR tried to fix bugs in the underlying pbp data of JAX home
  games prior to the 2016 season. An update of the raw pbp data resolved
  those bugs so nflfastR needs to remove the hard coded adjustments.
  This means that nflfastR \<= v4.6.1 will return incorrect pbp data for
  all Jacksonville home games prior to the 2016 season!
  ([\#478](https://github.com/nflverse/nflfastR/issues/478))
- Fixed a problem where
  [`clean_pbp()`](https://nflfastr.com/reference/clean_pbp.md) returned
  `pass = 1` in actual rush plays in very rare cases.
  ([\#479](https://github.com/nflverse/nflfastR/issues/479))
- Removed extra lines for injury timeouts that were breaking
  `fixed_drive`
  ([\#482](https://github.com/nflverse/nflfastR/issues/482))
- The variable `penalty_type` now correctly lists the penalty “Kickoff
  Short of Landing Zone” introduced in the 2024 season.
  ([\#486](https://github.com/nflverse/nflfastR/issues/486))
- Fixed a bug where `ep` was incorrect on PAT attempts preceded by a
  timeout and then a penalty (extremely rare). This bug also caused the
  variables `total_home_epa` and `total_away_epa` to be incorrect for
  all subsequent plays in the same game.
  ([\#493](https://github.com/nflverse/nflfastR/issues/493))

Thank you to [@ahmed-cheema](https://github.com/ahmed-cheema),
[@andrewtek](https://github.com/andrewtek),
[@guga31bb](https://github.com/guga31bb),
[@isaactpetersen](https://github.com/isaactpetersen),
[@JoeMarino2021](https://github.com/JoeMarino2021),
[@john-b-edwards](https://github.com/john-b-edwards),
[@marcusSasser](https://github.com/marcusSasser),
[@mlounsberry](https://github.com/mlounsberry),
[@morganandrew](https://github.com/morganandrew),
[@mrcaseb](https://github.com/mrcaseb),
[@mscoop16](https://github.com/mscoop16),
[@parsnipz](https://github.com/parsnipz),
[@rjthompson2](https://github.com/rjthompson2), and
[@Useight](https://github.com/Useight) for their questions, feedback,
and contributions towards this release.

## nflfastR 4.6.1

CRAN release: 2024-01-09

- The function
  [`calculate_series_conversion_rates()`](https://nflfastr.com/reference/calculate_series_conversion_rates.md)
  now correctly aggregates season level conversion rates. Performance
  has also been improved.
  ([\#440](https://github.com/nflverse/nflfastR/issues/440))
- Adjusted test behavior at CRAN’s request.

Thank you to [@andrewtek](https://github.com/andrewtek),
[@gregalvi86](https://github.com/gregalvi86),
[@Ic4ru5Wing](https://github.com/Ic4ru5Wing),
[@JoeMarino2021](https://github.com/JoeMarino2021),
[@jreddy1990](https://github.com/jreddy1990),
[@marvin3FF](https://github.com/marvin3FF),
[@mrcaseb](https://github.com/mrcaseb),
[@RicShern](https://github.com/RicShern),
[@SPNE](https://github.com/SPNE), and
[@trivialfis](https://github.com/trivialfis) for their questions,
feedback, and contributions towards this release.

## nflfastR 4.6.0

CRAN release: 2023-10-20

### New Features

- nflfastR now fully supports loading raw pbp data from local file
  system. The best way to use this feature is to set
  `options("nflfastR.raw_directory" = {"your/local/directory"})`.
  Alternatively, both
  [`build_nflfastR_pbp()`](https://nflfastr.com/reference/build_nflfastR_pbp.md)
  and [`fast_scraper()`](https://nflfastr.com/reference/fast_scraper.md)
  support the argument `dir` which defaults to the above option.
  ([\#423](https://github.com/nflverse/nflfastR/issues/423))
- Added the new function
  [`save_raw_pbp()`](https://nflfastr.com/reference/save_raw_pbp.md)
  which efficiently downloads raw play-by-play data and saves it to the
  local file system. This serves as a helper to setup the system for
  faster play-by-play parsing via the above functionality.
  ([\#423](https://github.com/nflverse/nflfastR/issues/423))
- Added the new function
  [`missing_raw_pbp()`](https://nflfastr.com/reference/missing_raw_pbp.md)
  that computes a vector of game IDs missing in the local raw
  play-by-play directory.
  ([\#423](https://github.com/nflverse/nflfastR/issues/423))

### Minor Improvements and Bugfixes

- The internal function `get_pbp_nfl()` now uses
  [`ifelse()`](https://rdrr.io/r/base/ifelse.html) instead of
  [`dplyr::if_else()`](https://dplyr.tidyverse.org/reference/if_else.html)
  to handle some null-checking, fixes bug found in `2022_21_CIN_KC`
  match.
- The function
  [`calculate_player_stats()`](https://nflfastr.com/reference/calculate_player_stats.md)
  now summarises target share and air yards share correctly when called
  with argument `weekly = FALSE`
  ([\#413](https://github.com/nflverse/nflfastR/issues/413))
- The function
  [`calculate_player_stats()`](https://nflfastr.com/reference/calculate_player_stats.md)
  now returns the opponent team when called with argument
  `weekly = TRUE`
  ([\#414](https://github.com/nflverse/nflfastR/issues/414))
- The function
  [`calculate_player_stats_def()`](https://nflfastr.com/reference/calculate_player_stats_def.md)
  no longer errors when small subsets of pbp data are missing stats.
  ([\#415](https://github.com/nflverse/nflfastR/issues/415))
- The function
  [`calculate_series_conversion_rates()`](https://nflfastr.com/reference/calculate_series_conversion_rates.md)
  no longer returns `NA` values if a small subset of pbp data is missing
  series on offense or defense.
  ([\#417](https://github.com/nflverse/nflfastR/issues/417))
- `fixed_drive` now correctly increments on plays where posteam lost a
  fumble but remains posteam because defteam also lost a fumble during
  the same play.
  ([\#419](https://github.com/nflverse/nflfastR/issues/419))
- nflfastR now fixes missing drive number counts in raw pbp data in
  order to provide accurate drive information.
  ([\#420](https://github.com/nflverse/nflfastR/issues/420))
- nflfastR now returns correct `kick_distance` on all punts and
  kickoffs. ([\#422](https://github.com/nflverse/nflfastR/issues/422))
- Decode player IDs in 2023 pbp.
  ([\#425](https://github.com/nflverse/nflfastR/issues/425))
- Drop the pseudo plays TV Timeout and Two-Minute Warning.
  ([\#426](https://github.com/nflverse/nflfastR/issues/426))
- Fix posteam on kickoffs and PATs following a defensive TD in 2023+
  pbp. ([\#427](https://github.com/nflverse/nflfastR/issues/427))
- [`calculate_player_stats()`](https://nflfastr.com/reference/calculate_player_stats.md)
  no more counts lost fumbles on plays where a player fumbles, a team
  mate recovers and then loses a fumble to the defense.
  ([\#431](https://github.com/nflverse/nflfastR/issues/431))
- The variables `passer`, `receiver`, and `rusher` no more return `NA`
  on “abnormal” plays - like direct snaps, aborted snaps, laterals
  etc. - that resulted in a penalty.
  ([\#435](https://github.com/nflverse/nflfastR/issues/435))

Thank you to [@903124](https://github.com/903124),
[@ak47twq](https://github.com/ak47twq),
[@andrewtek](https://github.com/andrewtek),
[@darkhark](https://github.com/darkhark),
[@dennisbrookner](https://github.com/dennisbrookner),
[@marvin3FF](https://github.com/marvin3FF),
[@mistakia](https://github.com/mistakia),
[@mrcaseb](https://github.com/mrcaseb),
[@nicholasmendoza22](https://github.com/nicholasmendoza22),
[@rickstarblazer](https://github.com/rickstarblazer),
[@RileyJohnson22](https://github.com/RileyJohnson22), and
[@tanho63](https://github.com/tanho63) for their questions, feedback,
and contributions towards this release.

## nflfastR 4.5.1

CRAN release: 2022-12-22

- New implementation of tests to be able to identify breaking changes in
  reverse dependencies
  ([\#396](https://github.com/nflverse/nflfastR/issues/396),
  [\#406](https://github.com/nflverse/nflfastR/issues/406))
- [`calculate_standings()`](https://nflfastr.com/reference/calculate_standings.md)
  no more freezes when computing standings from schedules where some
  games are missing results, i.e. upcoming games.
- Bug fix that caused problems with upcoming dplyr and tidyselect
  updates that weren’t reverse compatible.
- Significant performance improvements of internal functions.
  ([\#402](https://github.com/nflverse/nflfastR/issues/402))
- Wrap examples in [`try()`](https://rdrr.io/r/base/try.html) to avoid
  CRAN problems.
  ([\#404](https://github.com/nflverse/nflfastR/issues/404))
- Fixed a bug where
  [`calculate_standings()`](https://nflfastr.com/reference/calculate_standings.md)
  wasn’t able to handle nflverse pbp data.
  ([\#404](https://github.com/nflverse/nflfastR/issues/404))

## nflfastR 4.5.0

CRAN release: 2022-11-05

### New (experimental) functions

- Added new function
  [`calculate_player_stats_def()`](https://nflfastr.com/reference/calculate_player_stats_def.md)
  that aggregates defensive player stats either at game level or
  overall. ([\#288](https://github.com/nflverse/nflfastR/issues/288))
- The situation report `nflverse_sitrep` which is an alias of the
  already available
  [`report()`](https://nflfastr.com/reference/report.md)
- Added new function
  [`calculate_player_stats_kicking()`](https://nflfastr.com/reference/calculate_player_stats_kicking.md)
  that aggregates player stats for field goals and extra points at game
  level or overall.
  ([\#381](https://github.com/nflverse/nflfastR/issues/381))
- Added new function
  [`calculate_series_conversion_rates()`](https://nflfastr.com/reference/calculate_series_conversion_rates.md)
  that computes series conversion and series result rates at a game
  level or season level.
  ([\#393](https://github.com/nflverse/nflfastR/issues/393))

### Bugfixes and Minor Improvements

- Internal change to
  [`calculate_player_stats()`](https://nflfastr.com/reference/calculate_player_stats.md)
  that reflects new nflverse data infrastructure.
- [`calculate_player_stats()`](https://nflfastr.com/reference/calculate_player_stats.md)
  now unifies player names and joins the following player information
  via
  [`nflreadr::load_players()`](https://nflreadr.nflverse.com/reference/load_players.html):
  - `player_display_name` - Full name of the player
  - `position` - Position of the player
  - `position_group` - Position group of the player
  - `headshot_url` - URL to a player headshot image
- Make data work in 2022 (hopefully)
- Fix Amon-Ra St. Brown breaking the name parser
- Add gsis_id patch to
  [`clean_pbp()`](https://nflfastr.com/reference/clean_pbp.md).
- [`calculate_player_stats_def()`](https://nflfastr.com/reference/calculate_player_stats_def.md)
  failed in situations where play-by-play data is missing certain stats.
  ([\#382](https://github.com/nflverse/nflfastR/issues/382))
- Spot-fixing
  [`calculate_player_stats()`](https://nflfastr.com/reference/calculate_player_stats.md)
  for `NA` names.

## nflfastR 4.4.0

CRAN release: 2022-08-06

### New Functions, Options, Data

- Added new function
  [`calculate_standings()`](https://nflfastr.com/reference/calculate_standings.md)
  that computes regular season division standings and playoff seeds from
  nflverse data.
- The database function
  [`update_db()`](https://nflfastr.com/reference/update_db.md) now
  supports the option “nflfastR.dbdirectory” which can be used to set
  the directory of the nflfastR pbp database globally and independent of
  any project structure or working directories.
- The embedded data frame
  [`?teams_colors_logos`](https://nflfastr.com/reference/teams_colors_logos.md)
  has been updated to reflect the most recent team color themes and
  gained additional variables for conference and division as well as
  logo urls to the conference and league logos.
  ([\#290](https://github.com/nflverse/nflfastR/issues/290))
- The embedded data frame
  [`?teams_colors_logos`](https://nflfastr.com/reference/teams_colors_logos.md)
  has been updated with the Washington Commanders.
  ([\#312](https://github.com/nflverse/nflfastR/issues/312))

### Deprecation

- The argument `qs` in the functions
  [`load_pbp()`](https://nflreadr.nflverse.com/reference/load_pbp.html)
  and
  [`load_player_stats()`](https://nflreadr.nflverse.com/reference/load_player_stats.html)
  has been deprecated as of nflfastR 4.3.0. This release removes the
  argument entirely.

### Bugfixes and Minor Improvements

- Fixed bug where a player could be duplicated in
  [`calculate_player_stats()`](https://nflfastr.com/reference/calculate_player_stats.md)
  in very rare cases caused by plays with laterals.
  ([\#289](https://github.com/nflverse/nflfastR/issues/289))
- Fixed a bug where the function
  [`add_xpass()`](https://nflfastr.com/reference/add_xpass.md) failed
  when called with an empty data frame.
  ([\#296](https://github.com/nflverse/nflfastR/issues/296))
- Fixed a bug where `play_type` showed `no_play` on plays with penalties
  that don’t result in a replay of the down.
  ([\#277](https://github.com/nflverse/nflfastR/issues/277),
  [\#281](https://github.com/nflverse/nflfastR/issues/281))
- Fixed a bug in the variable descriptions of `total_home_score` and
  `total_away_score`.
  ([\#300](https://github.com/nflverse/nflfastR/issues/300))
- `fast_scraper_rosters()` and
  [`fast_scraper_schedules()`](https://nflfastr.com/reference/fast_scraper_schedules.md)
  now call
  [`nflreadr::load_rosters()`](https://nflreadr.nflverse.com/reference/load_rosters.html)
  and
  [`nflreadr::load_schedules()`](https://nflreadr.nflverse.com/reference/load_schedules.html)
  under the hood
  ([\#304](https://github.com/nflverse/nflfastR/issues/304))
- Fixed a bug causing missing EPA on game-ending turnovers in overtime
- Bump minimum nflreadr version to 1.2.0 for data repository change
- Fix a bug affecting yardline for a very small number of plays in the
  2000 season ([\#323](https://github.com/nflverse/nflfastR/issues/323))
- [`update_db()`](https://nflfastr.com/reference/update_db.md) now uses
  a default play to predefine column types for all db drivers.
  ([\#324](https://github.com/nflverse/nflfastR/issues/324))
- Fix a bug that resulted in incorrect `xyac_mean_yardage` on 4th downs
  ([\#327](https://github.com/nflverse/nflfastR/issues/327))
- Fix a bug that resulted in missing `xyac` information for plays
  involving J.O’Shaughnessy
  ([\#329](https://github.com/nflverse/nflfastR/issues/329))
- Fix a bug that resulted in missing `epa` on the last play of some
  games involving NE and BUF
  ([\#331](https://github.com/nflverse/nflfastR/issues/331))
- [`fast_scraper()`](https://nflfastr.com/reference/fast_scraper.md) and
  [`build_nflfastR_pbp()`](https://nflfastr.com/reference/build_nflfastR_pbp.md)
  now return data frames of class `nflverse_data` to be consistent with
  `nflreadr`.
- Fix behavior of EP model in neutral site games (treats both teams as
  away teams)

## nflfastR 4.3.0

CRAN release: 2021-10-06

### Minor Changes

- Add [nflreadr](https://nflreadr.nflverse.com/) to dependecies and drop
  lubridate and magrittr dependency
- The functions
  [`load_pbp()`](https://nflreadr.nflverse.com/reference/load_pbp.html)
  and
  [`load_player_stats()`](https://nflreadr.nflverse.com/reference/load_player_stats.html)
  now call
  [`nflreadr::load_pbp()`](https://nflreadr.nflverse.com/reference/load_pbp.html)
  and
  [`nflreadr::load_player_stats()`](https://nflreadr.nflverse.com/reference/load_player_stats.html)
  respectively. Therefore the argument `qs` has been deprecated in both
  functions. It will be removed in a future release. Running
  [`load_player_stats()`](https://nflreadr.nflverse.com/reference/load_player_stats.html)
  without any argument will now return player stats of the current
  season only (the default in `nflreadr`).
- The deprecated arguments `source` and `pp` in the functions
  `fast_scraper_*()` and
  [`build_nflfastR_pbp()`](https://nflfastr.com/reference/build_nflfastR_pbp.md)
  have been removed
- Added the variables `racr` (“Receiver Air Conversion Ratio”),
  `target_share`, `air_yards_share`, `wopr` (“Weighted Opportunity
  Rating”) and `pacr` (“Passing Air Conversion Ratio”) to the output of
  [`calculate_player_stats()`](https://nflfastr.com/reference/calculate_player_stats.md)
- Added the function
  [`report()`](https://nflfastr.com/reference/report.md) which will be
  used by the maintainers to help users debug their problems
  ([\#274](https://github.com/nflverse/nflfastR/issues/274)).

### Bug Fixes

- Fixed a minor bug in the console output of
  [`update_db()`](https://nflfastr.com/reference/update_db.md)
- Fix for a handful of missing `receiver` names
  ([\#270](https://github.com/nflverse/nflfastR/issues/270))
- Fixed bug with missing `return_team` on interception return touchdowns
  ([\#275](https://github.com/nflverse/nflfastR/issues/275))
- Fixed a rare bug where an internal object wasn’t predefined
  ([\#272](https://github.com/nflverse/nflfastR/issues/272))

## nflfastR 4.2.0

CRAN release: 2021-08-03

- All `wpa` variables are `NA` on end game line
- All `wp` variables are 0, 0.5, 1, or `NA` on end game line
- Fix bug where win prob on PATs assumed a PAT placed at 15 yard line,
  even in older seasons
- The function
  [`decode_player_ids()`](https://nflfastr.com/reference/decode_player_ids.md)
  now really decodes the new variable `fantasy_id`
  ([\#229](https://github.com/nflverse/nflfastR/issues/229))
- Fixed a bug that caused slightly differing `wp` values depending on
  the first game in the data set
  ([\#183](https://github.com/nflverse/nflfastR/issues/183))
- Edited GitHub references to point to nflverse
- Added the variables `sack_yards`, `sack_fumbles`, `rushing_fumbles`
  and `receiving_fumbles` to the output of the function
  [`calculate_player_stats()`](https://nflfastr.com/reference/calculate_player_stats.md),
  thanks to Mike Filicicchia
  ([@TheMathNinja](https://github.com/TheMathNinja)).
  ([\#239](https://github.com/nflverse/nflfastR/issues/239))
- Fixed a bug where
  [`calculate_player_stats()`](https://nflfastr.com/reference/calculate_player_stats.md)
  falsely counted lost fumbles on aborted snaps
  ([\#238](https://github.com/nflverse/nflfastR/issues/238))
- Added the variable `season_type` to the output of
  [`calculate_player_stats()`](https://nflfastr.com/reference/calculate_player_stats.md)
  and
  [`load_player_stats()`](https://nflreadr.nflverse.com/reference/load_player_stats.html)
  in preparation of the extended Regular Season starting in 2021
  ([\#240](https://github.com/nflverse/nflfastR/issues/240))
- Updated `season_type` definitions in preparation of the extended
  Regular Season starting in 2021
  ([\#242](https://github.com/nflverse/nflfastR/issues/242))
- Fix for `fixed_drive` where it wasn’t incrementing when there was a
  muffed punt followed by timeout
  ([\#244](https://github.com/nflverse/nflfastR/issues/244))
- Fix for `fixed_drive` where it wasn’t incrementing following an
  interception with the intercepting player then losing a fumble
  ([\#247](https://github.com/nflverse/nflfastR/issues/247))
- Fix for more issues with missing play info in 2018_01_ATL_PHI
  ([\#246](https://github.com/nflverse/nflfastR/issues/246))
- Added the variables `safety_player_name` and `safety_player_id` to the
  play-by-play data
  ([\#252](https://github.com/nflverse/nflfastR/issues/252))
- Dropped the dependency `usethis`

## nflfastR 4.1.0

CRAN release: 2021-03-23

### Breaking changes

#### Functions

- Added the function
  [`calculate_player_stats()`](https://nflfastr.com/reference/calculate_player_stats.md)
  that aggregates official passing, rushing, and receiving stats either
  at game level or overall
- Added the function
  [`load_player_stats()`](https://nflreadr.nflverse.com/reference/load_player_stats.html)
  that loads weekly player stats from 1999 to the most recent season
- The performance of the functions
  [`add_xyac()`](https://nflfastr.com/reference/add_xyac.md) and
  [`clean_pbp()`](https://nflfastr.com/reference/clean_pbp.md) has been
  significantly improved

#### New Variables

- Added the new columns `td_player_name` and `td_player_id` to clearly
  identify the player who scored a touchdown (this is especially helpful
  for plays with multiple fumbles or laterals resulting in a touchdown)
- The function
  [`calculate_player_stats()`](https://nflfastr.com/reference/calculate_player_stats.md)
  now adds the variable `dakota`, the `epa` + `cpoe` composite, for
  players with minimum 5 pass attempts.
- Added column `home_opening_kickoff` to
  [`clean_pbp()`](https://nflfastr.com/reference/clean_pbp.md)
- Added the variables `sack_player_id`, `sack_player_name`,
  `half_sack_1_player_id`, `half_sack_1_player_name`,
  `half_sack_2_player_id` and `half_sack_2_player_name` who identify
  players that recorded sacks (or half sacks). Also updated the
  description of the variables `qb_hit_1_player_id`,
  `qb_hit_1_player_name`, `qb_hit_2_player_id` and
  `qb_hit_2_player_name` to make more clear that they did not record a
  sack. ([\#180](https://github.com/nflverse/nflfastR/issues/180))

### Minor improvements and fixes

- The variable `qb_scramble` was incomplete for the 2005 season because
  of missing scramble indicators in the play description. This has been
  mostly fixed courtesy of charting data from Football Outsiders (with
  thanks to Aaron Schatz!). Some notes on this fix: Weeks 1-16 are based
  on charting. Weeks 17-21 are guesses (basically every QB run except
  those that were a) a loss, b) no gain, or c) on 3/4 down with 1-2 to
  go). Plays nullified by penalty are not included.
- Change `name`, `id`, `rusher`, and `rusher_id` to be the player
  charged with the fumble on aborted snaps when the QB is unable to make
  a play (i.e. pass, sack, or scramble)
  ([\#162](https://github.com/nflverse/nflfastR/issues/162))
- The function
  [`clean_pbp()`](https://nflfastr.com/reference/clean_pbp.md) now
  standardizes the team name columns `tackle_with_assist_*_team`
- Fix bug in `drive` that was causing incorrect overtime win
  probabilities
  ([\#194](https://github.com/nflverse/nflfastR/issues/194))
- Fixed a bug where `posteam` was not `NA` on end of quarter 2 (or end
  of quarter 4 in overtime games) causing wrong values for
  `fixed_drive`, `fixed_drive_result`, `series` and `series_result`
- Fixed a bug where `fixed_drive` and `series` were falsely incrementing
  on kickoffs recovered by the kicking team or on defensive touchdowns
  followed by timeouts
- Fixed a bug where `fixed_drive` and `series` were falsely incrementing
  on muffed punts recovered by the punting team for a touchdown
- Fixed a bug where
  [`add_xpass()`](https://nflfastr.com/reference/add_xpass.md) crashed
  when ran with data already including xpass variables.
- Fixed a bug in `epa` when a safety is scored by the team beginning the
  play in possession of the ball
  ([\#186](https://github.com/nflverse/nflfastR/issues/186))
- Fix some bugs related to David and Duke Johnson on the Texans in 2020
  ([\#163](https://github.com/nflverse/nflfastR/issues/163))
- Fix yet another bug related to correctly identifying possession team
  on kickoffs nullified by penalty
  ([\#199](https://github.com/nflverse/nflfastR/issues/199))
- Fixed a bug where
  [`calculate_player_stats()`](https://nflfastr.com/reference/calculate_player_stats.md)
  forgot to clean player names by using their IDs
- Fixed a bug where special teams touchdowns were missing in the output
  of
  [`calculate_player_stats()`](https://nflfastr.com/reference/calculate_player_stats.md)
  ([\#203](https://github.com/nflverse/nflfastR/issues/203))
- Fixed for some old Jaguars games where the wrong team was awarded
  points for safeties and kickoff return TDs
  ([\#209](https://github.com/nflverse/nflfastR/issues/209))
- The function
  [`update_db()`](https://nflfastr.com/reference/update_db.md) no more
  falsely closes a database connection provided by the argument
  `db_connection`
  ([\#210](https://github.com/nflverse/nflfastR/issues/210))
- Fixed a bug where `yards_gained` was missing yardage on plays with
  laterals. ([\#216](https://github.com/nflverse/nflfastR/issues/216))
- Fixed a bug where there were stats wrongly given on a play with
  penalty ([\#218](https://github.com/nflverse/nflfastR/issues/218))
- `fixed_drive` now increments properly on onside kick recoveries
  ([\#215](https://github.com/nflverse/nflfastR/issues/215))
- `fixed_drive` no longer counts a muffed kickoff as a one-play drive on
  its own ([\#217](https://github.com/nflverse/nflfastR/issues/217))
- `fixed_drive` now properly increments after a safety
  ([\#219](https://github.com/nflverse/nflfastR/issues/219))
- Improved parser for `penalty_type` and updated the description of the
  variable to make more clear it’s the first penalty that happened on a
  play. ([\#223](https://github.com/nflverse/nflfastR/issues/223))

## nflfastR 4.0.0

CRAN release: 2021-02-15

### Breaking changes

#### Changed Functions

- Deprecated the arguments `source` and `pp` all across the package.
  Using them will cause a warning. Parallel processing has to be
  activated by choosing an appropriate
  [`future::plan()`](https://future.futureverse.org/reference/plan.html)
  before calling the relevant functions. For more information please see
  [the package
  documentation](https://nflfastr.com/reference/nflfastR-package.html).
- The function
  [`build_nflfastR_pbp()`](https://nflfastr.com/reference/build_nflfastR_pbp.md)
  will now run
  [`decode_player_ids()`](https://nflfastr.com/reference/decode_player_ids.md)
  by default (can be deactivated with the argument `decode = FALSE`).
- The function
  [`build_nflfastR_pbp()`](https://nflfastr.com/reference/build_nflfastR_pbp.md)
  will now run
  [`add_xpass()`](https://nflfastr.com/reference/add_xpass.md) by
  default and add the new variables `xpass` and `pass_oe`.
- The functions
  [`fast_scraper()`](https://nflfastr.com/reference/fast_scraper.md) and
  [`build_nflfastR_pbp()`](https://nflfastr.com/reference/build_nflfastR_pbp.md)
  now allow the output of
  [`fast_scraper_schedules()`](https://nflfastr.com/reference/fast_scraper_schedules.md)
  directly as input so it’s not necessary anymore to pull the `game_id`
  first.

#### New Functions and Variables

- Added the new function
  [`load_pbp()`](https://nflreadr.nflverse.com/reference/load_pbp.html)
  that loads complete seasons into memory for fast access of the
  play-by-play data.
- Added the new variables `rushing_yards`, `lateral_rushing_yards`,
  `passing_yards`, `receiving_yards`, `lateral_receiving_yards` to fix
  an old bug where `yards_gained` gets overwritten on plays with
  laterals ([\#115](https://github.com/nflverse/nflfastR/issues/115)).
- Added columns `vegas_wpa` and `vegas_home_wpa` which contain Win
  Probability Added from the spread-adjusted WP model
- Added column `out_of_bounds`
- Added columns `fantasy`, `fantasy_id`, `fantasy_player_name`, and
  `fantasy_player_id` that indicate the rusher or receiver on the play
- Added columns `tackle_with_assist`, `tackle_with_assist_1_player_id`,
  `tackle_with_assist_1_player_name`, `tackle_with_assist_1_team`,
  `tackle_with_assist_2_player_id`, `tackle_with_assist_2_player_name`,
  `tackle_with_assist_2_team`

#### Models and Miscellaneous

- Tuned spread-adjusted win probability model one final (?) time.
  Expected points is now no longer required for
  [`calculate_win_probability()`](https://nflfastr.com/reference/calculate_win_probability.md)
- Added field descriptions
  [`vignette("field_descriptions")`](https://nflfastr.com/articles/field_descriptions.md)
  with a searchable list of all nflfastR variables
- Switched data source for 2001-2010 to what is used for 2011 and on
- All models have been moved to the
  [fastrmodels](https://cran.r-project.org/package=fastrmodels) package
- Added the data frames
  [`?field_descriptions`](https://nflfastr.com/reference/field_descriptions.md)
  and [`?stat_ids`](https://nflfastr.com/reference/stat_ids.md) to the
  package

### Minor improvements and fixes

- Fix bug where `fixed_drive` and `series` weren’t updating after muffed
  punt ([\#144](https://github.com/nflverse/nflfastR/issues/144))
- Fix bug induced by fixing the above
  ([\#149](https://github.com/nflverse/nflfastR/issues/149))
- Fix bug where some special teams plays were incorrectly being labeled
  as pass plays
  ([\#125](https://github.com/nflverse/nflfastR/issues/125))
- Fix bug where points for safeties were given to the `defteam` instead
  of the `posteam`
  ([\#152](https://github.com/nflverse/nflfastR/issues/152))
- Fix bug where a muffed punt TD was given to the wrong team in a 2011
  Jaguars game
  ([\#154](https://github.com/nflverse/nflfastR/issues/154))
- Win probability is now calculated prior to PAT attempts rather than
  using WP on the ensuing kickoff
- Improved performance of internal functions that speed up the
  rebuilding process in
  [`update_db()`](https://nflfastr.com/reference/update_db.md) (added
  `qs` and `curl` to dependencies)
- Fixed a bug where
  [`calculate_expected_points()`](https://nflfastr.com/reference/calculate_expected_points.md)
  and
  [`calculate_win_probability()`](https://nflfastr.com/reference/calculate_win_probability.md)
  duplicated some existing variables instead of replacing them
  ([\#170](https://github.com/nflverse/nflfastR/issues/170))
- Fixed a bug where `penalty_type` wasn’t `"no_play"` although it should
  have been ([\#172](https://github.com/nflverse/nflfastR/issues/172))
- Fixed a bug where `penalty_team` could be incorrect in games of the
  Jaguars in the seasons 2011 - 2015
  ([\#174](https://github.com/nflverse/nflfastR/issues/174))
- Fixed a bug related to the calculation of `epa` on plays before a
  failed pass interference challenge in a few 2019 games
  ([\#175](https://github.com/nflverse/nflfastR/issues/175))
- Fixed a bug related to lots of fields with `NA` on offsetting
  penalties ([\#44](https://github.com/nflverse/nflfastR/issues/44))
- Fixed a bug in `epa` when possession team changes at end of 1st or 3rd
  quarter ([\#182](https://github.com/nflverse/nflfastR/issues/182))
- Fixed a bug where various functions have left open connections
- `vegas_wp` is now `NA` on final line since there is no possession team

## nflfastR 3.2.0

CRAN release: 2020-11-20

### Models

- Performance update for win probability model with point spread
  (`vegas_wp`)
- Added `yardline_100` as an input to both win probability models (not
  having it included was an oversight)

### Minor improvements and fixes

- Fixed a bug where `series` was increased on PATs
- Fixed a bug affecting the week 10 Raiders-Broncos game
- Added the column `team_wordmark` - which contains URLs to the team’s
  wordmarks - to the included data frame
  [`?teams_colors_logos`](https://nflfastr.com/reference/teams_colors_logos.md)

## nflfastR 3.1.1

CRAN release: 2020-10-22

### New features

#### Database Function `update_db()`

- The argument `force_rebuild` of the function
  [`update_db()`](https://nflfastr.com/reference/update_db.md) is now of
  hybrid type. It can rebuild the play by play data table either for the
  whole nflfastR era (with `force_rebuild = TRUE`) or just for specified
  seasons (e.g. `force_rebuild = 2019:2020`). The latter is intended to
  be used for running seasons because the NFL fixes bugs in the play by
  play data during the week and we recommend to rebuild the current
  season every Thursday.
- Fixed a bug where
  [`update_db()`](https://nflfastr.com/reference/update_db.md)
  disconnected the connection to a database provided by the argument
  `db_connection`
  ([\#102](https://github.com/nflverse/nflfastR/issues/102))
- Fixed a bug where
  [`update_db()`](https://nflfastr.com/reference/update_db.md) didn’t
  build a fresh database without providing the argument `force_rebuild`
- [`update_db()`](https://nflfastr.com/reference/update_db.md) no longer
  removes the complete data table when a numeric argument
  `force_rebuild` is passed but only removes the rows within the table
  ([\#109](https://github.com/nflverse/nflfastR/issues/109))

#### New Functions

- Added the new function
  [`build_nflfastR_pbp()`](https://nflfastr.com/reference/build_nflfastR_pbp.md),
  a convenient wrapper around multiple nflfastR functions for an easy
  creation of the nflfastR play-by-play data set
- Added a function that applies our experimental expected pass model,
  [`add_xpass()`](https://nflfastr.com/reference/add_xpass.md), that
  creates columns `xpass` and `pass_oe`

### Minor improvements and fixes

- More fixes for `fixed_drive` which was not incrementing properly on
  drives that began following a timeout
- Fixed more bugs in EPA and win probability on PATs and kickoffs with
  penalties
- Fixed a bug where scoring probabilities weren’t adding to 1 on field
  goal attempts near the end of a half
- Messages to the user are now created with the new dependency `usethis`
- Fixed bug where plays with “backward pass” in play description were
  counted as pass plays (`pass` = 1)
- Fixed missing kick distance on touchbacks and blocked punts
  ([\#53](https://github.com/nflverse/nflfastR/issues/53))
- Added the option `fast` (either `TRUE` or `FALSE`) to the function
  [`decode_player_ids()`](https://nflfastr.com/reference/decode_player_ids.md)
  to activate the high efficient C++ decoder of the package
  [`gsisdecoder`](https://cran.r-project.org/package=gsisdecoder)

## nflfastR 3.0.0

CRAN release: 2020-09-24

### Breaking changes

- [`fast_scraper_roster()`](https://nflfastr.com/reference/fast_scraper_roster.md)
  is finally back! It loads NFL roster of a given season.
- Added the function
  [`decode_player_ids()`](https://nflfastr.com/reference/decode_player_ids.md)
  to decode all player IDs to the commonly known GSIS ID format
  (00-00xxxxx)

### New features

- Add option `source = "old"` to
  [`fast_scraper()`](https://nflfastr.com/reference/fast_scraper.md) to
  enable scraping of old source. This is mostly useless as it doesn’t
  work for 2020 and provides less info
- Added new option `db_connection` to
  [`update_db()`](https://nflfastr.com/reference/update_db.md) to allow
  advanced users to use other DBI drivers, such as
  `RMariaDB::MariaDB()`, `RPostgres::Postgres()` or `odbc::odbc()`
  (please see
  [dbplyr](https://dbplyr.tidyverse.org/articles/dbplyr.html) for more
  information)

### Minor improvements and fixes

- [`clean_pbp()`](https://nflfastr.com/reference/clean_pbp.md) now fixes
  some bugs in jersey numbers
- [`clean_pbp()`](https://nflfastr.com/reference/clean_pbp.md),
  [`add_qb_epa()`](https://nflfastr.com/reference/add_qb_epa.md) and
  [`add_xyac()`](https://nflfastr.com/reference/add_xyac.md) can now
  handle empty data frames
- Fix empty line causing
  [`fast_scraper()`](https://nflfastr.com/reference/fast_scraper.md) to
  fail (affects multiple games of the 2020 season)
- Fix bug in `fixed_drive` that counted PAT after defensive TD as its
  own drive
- Fixed a bug which caused too high number of tackles in special cases
- Fixed a bug where CPOE was NA when targeting players with apostrophe
  in last name

## nflfastR 2.2.1

CRAN release: 2020-09-01

- Fix [`add_xyac()`](https://nflfastr.com/reference/add_xyac.md)
  breaking with some old packages
- Fix [`add_xyac()`](https://nflfastr.com/reference/add_xyac.md) and
  [`add_qb_epa()`](https://nflfastr.com/reference/add_qb_epa.md)
  calculations being wrong for some failed 4th downs
- Updated Readme with ep and cp model plots
- Updated `vignette("examples")` with the new
  [`add_xyac()`](https://nflfastr.com/reference/add_xyac.md) function
- Added xYAC model to `vignette("nflfastR-models")`
- Added variables `fixed_drive` and `fixed_drive_result` to the output
  of [`fast_scraper()`](https://nflfastr.com/reference/fast_scraper.md)
  because the NFL-provided drive info is extremely buggy
- Added variable `series_result`
- [`clean_pbp()`](https://nflfastr.com/reference/clean_pbp.md) now adds
  4 new variables `passer_jersey_number`, `rusher_jersey_number`,
  `receiver_jersey_number` and `jersey_number`. These can be used to
  join rosters.
- Fixed incorrect `timeout_team`, `return_team`,
  `fumble_recovery_1_team` for JAX games from 2011-2015
- Re-trained EPA model with `fixed_drive` and corrections to
  `timeout_team`

## nflfastR 2.2.0

- New function
  [`add_xyac()`](https://nflfastr.com/reference/add_xyac.md) which adds
  the following columns associated with expected yards after the catch
  (xYAC): `xyac_epa`, `xyac_success`, `xyac_fd`, `xyac_mean_yardage`,
  `xyac_median_yardage`

## nflfastR 2.1.3

- Fixed a bug in `series_success` caused by bad `drive` information
  provided by NFL

## nflfastR 2.1.2

- Added the following columns that are available 2011 and later:
  `special_teams_play`, `st_play_type`, `time_of_day`, and
  `order_sequence`
- Added `old_game_id` column (useful for merging to external data that
  still uses this ID: format is YYYYMMDDxx)
- The [`clean_pbp()`](https://nflfastr.com/reference/clean_pbp.md)
  function now adds an `aborted_play` column
- Fixed a bug where pass plays with a penalty at end of play were
  classified as `play_type` = `no_play` rather than `pass`
- Fixed bug where EPA on defensive 2 point return was -0.95 instead of
  -2.95
- Fixed some remaining failed challenge plays that incorrectly had 0 for
  EPA
- Updated the included dataframe `teams_colors_logos` for the interim
  name of the ‘Washington Football Team’ and the corresponding logo
  urls.
- Some internal code improvements causing the required `tidyselect`
  version to be \>= 1.1.0

## nflfastR 2.1.1

#### Functions

- [`clean_pbp()`](https://nflfastr.com/reference/clean_pbp.md) now
  standardizes player IDs across the old (1999-2010) and new (2011+)
  data sources. Player IDs once again uniquely identify players, and
  each unique player has one unique ID (as they did before the NFL data
  source change):
  - For players whose careers finished before 2011, their IDs remain the
    same
  - For players who played in both eras or only in the new era, their ID
    is the new ID
  - For example, Akili Smith (ID: 00-0015082) and Alex Smith (ID:
    32013030-2d30-3032-3334-3336b638d37d) are both abbreviated as
    “A.Smith” but can be distinguished by their IDs, with Akili showing
    what the old format ID looks like, and Smith the new one
  - Standardization is realized by using an ID map available in the data
    repo
- [`clean_pbp()`](https://nflfastr.com/reference/clean_pbp.md) now
  removes all variables it is about to create to make sure nothing
  unexpected can happen

#### Miscellaneous

- Added minimum version requirements to some package dependencies
  because installation broke for some users with outdated packages

- Made a minor bug fix to catch more out-of-order plays and fixed a bug
  where some plays were being incorrectly dropped in older seasons

- Standardized team names (e.g. `SD` –\> `LAC`) in some columns we had
  missed

## nflfastR 2.1.0

#### Models

- Removed `week` from Expected Points models along with an update of
  `vignette("nflfastR-models")` and `vignette("examples")`

#### Functions

- Added function
  [`update_db()`](https://nflfastr.com/reference/update_db.md) which
  adds all completed games to a SQLite database
- Added function
  [`calculate_win_probability()`](https://nflfastr.com/reference/calculate_win_probability.md)
- Added new examples to `vignette("examples")` demonstrating the usage
  of the above mentioned functions

#### Bugs

- Fixed a problem with inconsistent data types of the variable
  `drive_real_start_time` pre and post 2011
- Fixed a problem where some `game_id`s were overwritten during the play
  by play parsing
- Fix some more WP bugs on kickoffs with penalties and rare play
  description

#### Miscellaneous

- [`fast_scraper()`](https://nflfastr.com/reference/fast_scraper.md) now
  loads the raw game data from a separate raw data repo
- Completely overhauled the entire code base to directly implement [tidy
  evaluation](https://dplyr.tidyverse.org/articles/programming.html)
  using `.data` from the [rlang](https://rlang.r-lib.org/) package (this
  is a major code change that takes some getting used to but we need it
  in preparation of a future release)

## nflfastR 2.0.6

- Fixed a problem where defensive two point conversions were not counted
- Kneels on kickoffs are no longer counted as qb kneels
- Variable `yards_gained` more precisely defined
- Bugfixes for more games with out of order of plays
- Fix bug related to EPA on plays with a failed pass interference
  challenge
- Added new example to `vignette("examples")` to demonstrate Expected
  Points calculator
  [`calculate_expected_points()`](https://nflfastr.com/reference/calculate_expected_points.md)
- Fix for WP on 2-pt conversion negated by penalty
- Add more variables (containing team names) to team standardization in
  [`clean_pbp()`](https://nflfastr.com/reference/clean_pbp.md)
- Fix WP for onside kicks

## nflfastR 2.0.5

- Fix yet another bug caused by NFL providing plays out of order
- Fix bugs related to penalties on PATs and kickoffs
- Fix bugs related to NFL providing wrong scoring team on defensive
  touchdowns in older games involving the Jaguars
- Fix some minor issues related to wrong `first_down_rush` and
  `return_touchdown`
- Improved error handling of
  [`fast_scraper()`](https://nflfastr.com/reference/fast_scraper.md) for
  not yet played games
- Improved variable documentation and prepared for new website
- Improved performance for dplyr v1.0.0
- Rebuilt EP and WP models due to bugfixes in the underlying data in the
  versions 2.0.3, 2.0.4 and 2.0.5

## nflfastR 2.0.4

- Fix another bug with out of order plays
- Fix bug affecting cumulative totals for WPA, air_WPA and yac_WPA
- Fix bug affecting cumulative totals for air_EPA and yac_EPA

## nflfastR 2.0.3

- Fix for NFL providing plays out of order
- Fix for series not incrementing following defensive TD

## nflfastR 2.0.2

- Fixed a bug in the series and series success calculations caused by
  timeouts following a possession change
- Fixed win probability on PATs

## nflfastR 2.0.1

- Added minimum version requirement on `xgboost` (\>= 1.1) as the recent
  `xgboost` update caused a breaking change leading to failure in adding
  model results to data

## nflfastR 2.0.0

#### Models

- Added new models for Expected Points, Win Probability and Completion
  Probability and removed `nflscrapR` dependency. This is a **major**
  change as we are stepping away from the well established `nflscrapR`
  models. But we believe it is a good step forward. See
  `data-raw/MODEL-README.md` for detailed model information.

- Added internal functions for `EPA` and `WPA` to `helper_add_ep_wp.R`.

- Added new function
  [`calculate_expected_points()`](https://nflfastr.com/reference/calculate_expected_points.md)
  usable for the enduser.

#### Functions

- Completely overhauled
  [`fast_scraper()`](https://nflfastr.com/reference/fast_scraper.md) to
  make it work with the NFL’s new server backend. The option `source` is
  still available but will be deprecated since there is only one source
  now. There are some changes in the output as well (please see below).

- [`fast_scraper()`](https://nflfastr.com/reference/fast_scraper.md) now
  adds game data to the play by play data set courtesy of Lee Sharpe.
  Game data include: away_score, home_score, location, result, total,
  spread_line, total_line, div_game, roof, surface, temp, wind,
  home_coach, away_coach, stadium, stadium_id, gameday

- `fastcraper_schedules()` now incorporates Lee Sharpe’s `games.rds`.

- The functions `fast_scraper_clips()` and
  [`fast_scraper_roster()`](https://nflfastr.com/reference/fast_scraper_roster.md)
  are deactivated due to the missing data source. They might be
  reactivated or completely dropped in future versions.

- The function `fix_fumbles()` has been renamed to
  [`add_qb_epa()`](https://nflfastr.com/reference/add_qb_epa.md) as the
  new name much better describes what the function is actually doing.

#### Miscellaneous

- Added progress information using the `progressr`package and removed
  the `furrr` progress bars.

- [`clean_pbp()`](https://nflfastr.com/reference/clean_pbp.md) now adds
  the column `ìd` which is the id of the player in the column `name`.
  Because we have to piece together different data to cover the full
  span of years, **player IDs are not consistent between the early
  (1999-2010) and recent (2011 onward) periods**.

- Added a `NEWS.md` file to track changes to the package.

- Fixed several bugs inhereted from `nflscrapR`, including one where EPA
  was missing when a play was followed by two timeouts (for example, a
  two-minute warning followed by a timeout), and another where
  `play_type` was incorrect on plays with declined penalties.

- Fixed a bug, where `receiver_player_name` and `receiver` didn’t name
  the correct players on plays with lateral passes.

#### Play-by-Play Output

The output has changed a little bit.

##### The following variables were dropped

| Dropped Variables          | Description                                                                                                                                                         |
|----------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| game_key                   | RS feed game identifier.                                                                                                                                            |
| game_time_local            | Kickoff time in local time zone.                                                                                                                                    |
| iso_time                   | Kickoff time according ISO 8601.                                                                                                                                    |
| game_type                  | One of ‘REG’, ‘WC’, ‘DIV’, ‘CON’, ‘SB’ indicating if a game was a regular season game or one of the playoff rounds.                                                 |
| site_id                    | RS feed id for game site.                                                                                                                                           |
| site_city                  | Game site city.                                                                                                                                                     |
| site_state                 | Game site state.                                                                                                                                                    |
| drive_possession_team_abbr | Abbreviation of the possession team in a given drive.                                                                                                               |
| scoring_team_abbr          | Abbreviation of the scoring team if the play was a scoring play.                                                                                                    |
| scoring_type               | String indicating the scoring type. One of ‘FG’, ‘TD’, ‘PAT’, ‘SFTY’, ‘PAT2’.                                                                                       |
| alert_play_type            | String describing the play type of a play the NFL has listed as alert play. For most of those plays there are highlight clips available through fast_scraper_clips. |
| time_of_day                | Local time at the beginning of the play.                                                                                                                            |
| yards                      | Analogue yards_gained but with the kicking team being the possession team (which means that there are many yards gained through kickoffs and punts).                |
| end_yardline_number        | Yardline number within the above given side at the end of the given play.                                                                                           |
| end_yardline_side          | String indicating the side of the field at the end of the given play.                                                                                               |

##### The following variables were renamed

| Renamed Variables                              | Description                                                                                                                                               |
|------------------------------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------|
| game_time_eastern -\> start_time               | Kickoff time in eastern time zone.                                                                                                                        |
| site_fullname -\> stadium                      | Game site name.                                                                                                                                           |
| drive_how_started -\> drive_start_transition   | String indicating how the offense got the ball.                                                                                                           |
| drive_how_ended -\> drive_end_transition       | String indicating how the offense lost the ball.                                                                                                          |
| drive_start_time -\> drive_game_clock_start    | Game time at the beginning of a given drive.                                                                                                              |
| drive_end_time -\> drive_game_clock_end        | Game time at the end of a given drive.                                                                                                                    |
| drive_start_yardline -\> drive_start_yard_line | String indicating where a given drive started consisting of team half and yard line number.                                                               |
| drive_end_yardline -\> drive_end_yard_line     | String indicating where a given drive ended consisting of team half and yard line number.                                                                 |
| roof_type -\> roof                             | One of ‘dome’, ‘outdoors’, ‘closed’, ‘open’ indicating indicating the roof status of the stadium the game was played in. (Source: Pro-Football-Reference) |

##### The following variables were added

| Added Variables        | Description                                                                                                                                                                                                          |
|------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| vegas_wp               | Estimated win probabiity for the posteam given the current situation at the start of the given play, incorporating pre-game Vegas line.                                                                              |
| vegas_home_wp          | Estimated win probability for the home team incorporating pre-game Vegas line.                                                                                                                                       |
| weather                | String describing the weather including temperature, humidity and wind (direction and speed). Doesn’t change during the game!                                                                                        |
| nfl_api_id             | UUID of the game in the new NFL API.                                                                                                                                                                                 |
| play_clock             | Time on the playclock when the ball was snapped.                                                                                                                                                                     |
| play_deleted           | Binary indicator for deleted plays.                                                                                                                                                                                  |
| end_clock_time         | Game time at the end of a given play.                                                                                                                                                                                |
| end_yard_line          | String indicating the yardline at the end of the given play consisting of team half and yard line number.                                                                                                            |
| drive_real_start_time  | Local day time when the drive started (currently not used by the NFL and therefore mostly ‘NA’).                                                                                                                     |
| drive_ended_with_score | Binary indicator the drive ended with a score.                                                                                                                                                                       |
| drive_quarter_start    | Numeric value indicating in which quarter the given drive has started.                                                                                                                                               |
| drive_quarter_end      | Numeric value indicating in which quarter the given drive has ended.                                                                                                                                                 |
| drive_play_id_started  | Play_id of the first play in the given drive.                                                                                                                                                                        |
| drive_play_id_ended    | Play_id of the last play in the given drive.                                                                                                                                                                         |
| away_score             | Total points scored by the away team.                                                                                                                                                                                |
| home_score             | Total points scored by the home team.                                                                                                                                                                                |
| location               | Either ‘Home’ o ‘Neutral’ indicating if the home team played at home or at a neutral site.                                                                                                                           |
| result                 | Equals home_score - away_score and means the game outcome from the perspective of the home team.                                                                                                                     |
| total                  | Equals home_score + away_score and means the total points scored in the given game.                                                                                                                                  |
| spread_line            | The closing spread line for the game. A positive number means the home team was favored by that many points, a negative number means the away team was favored by that many points. (Source: Pro-Football-Reference) |
| total_line             | The closing total line for the game. (Source: Pro-Football-Reference)                                                                                                                                                |
| div_game               | Binary indicator for if the given game was a division game.                                                                                                                                                          |
| roof                   | One of ‘dome’, ‘outdoors’, ‘closed’, ‘open’ indicating indicating the roof status of the stadium the game was played in. (Source: Pro-Football-Reference)                                                            |
| surface                | What type of ground the game was played on. (Source: Pro-Football-Reference)                                                                                                                                         |
| temp                   | The temperature at the stadium only for ‘roof’ = ‘outdoors’ or ‘open’.(Source: Pro-Football-Reference)                                                                                                               |
| wind                   | The speed of the wind in miles/hour only for ‘roof’ = ‘outdoors’ or ‘open’. (Source: Pro-Football-Reference)                                                                                                         |
| home_coach             | First and last name of the home team coach. (Source: Pro-Football-Reference)                                                                                                                                         |
| away_coach             | First and last name of the away team coach. (Source: Pro-Football-Reference)                                                                                                                                         |
| stadium_id             | ID of the stadium the game was played in. (Source: Pro-Football-Reference)                                                                                                                                           |
| game_stadium           | Name of the stadium the game was played in. (Source: Pro-Football-Reference)                                                                                                                                         |
