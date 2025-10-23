# Package index

## Main Functions

- [`build_nflfastR_pbp()`](https://nflfastr.com/reference/build_nflfastR_pbp.md)
  : Build a Complete nflfastR Data Set
- [`update_db()`](https://nflfastr.com/reference/update_db.md) : Update
  or Create a nflfastR Play-by-Play Database

## Load Functions

These functions access precomputed data using the nflreadr package. See
<https://nflreadr.nflverse.com> for info and more data load functions.

- [`reexports`](https://nflfastr.com/reference/reexports.md)
  [`load_pbp`](https://nflfastr.com/reference/reexports.md)
  [`load_player_stats`](https://nflfastr.com/reference/reexports.md)
  [`load_team_stats`](https://nflfastr.com/reference/reexports.md)
  [`load_schedules`](https://nflfastr.com/reference/reexports.md)
  [`load_rosters`](https://nflfastr.com/reference/reexports.md)
  [`nflverse_sitrep`](https://nflfastr.com/reference/reexports.md) :
  Objects exported from other packages

## Utility Functions

- [`save_raw_pbp()`](https://nflfastr.com/reference/save_raw_pbp.md) :
  Download Raw PBP Data to Local Filesystem
- [`missing_raw_pbp()`](https://nflfastr.com/reference/missing_raw_pbp.md)
  : Compute Missing Raw PBP Data on Local Filesystem
- [`calculate_expected_points()`](https://nflfastr.com/reference/calculate_expected_points.md)
  : Compute expected points
- [`calculate_series_conversion_rates()`](https://nflfastr.com/reference/calculate_series_conversion_rates.md)
  : Compute Series Conversion Information from Play by Play
- [`calculate_stats()`](https://nflfastr.com/reference/calculate_stats.md)
  : Calculate NFL Stats
- [`calculate_win_probability()`](https://nflfastr.com/reference/calculate_win_probability.md)
  : Compute win probability

## Documentation

- [`nflfastR`](https://nflfastr.com/reference/nflfastR-package.md)
  [`nflfastR-package`](https://nflfastr.com/reference/nflfastR-package.md)
  : nflfastR: Functions to Efficiently Access NFL Play by Play Data
- [`teams_colors_logos`](https://nflfastr.com/reference/teams_colors_logos.md)
  : NFL Team names, colors and logo urls.
- [`field_descriptions`](https://nflfastr.com/reference/field_descriptions.md)
  : nflfastR Field Descriptions
- [`stat_ids`](https://nflfastr.com/reference/stat_ids.md) : NFL Stat
  IDs and their Meanings
- [`nfl_stats_variables`](https://nflfastr.com/reference/nfl_stats_variables.md)
  : NFL Stats Variables

## Lower Level Functions

These functions are wrapped in the above listed main functions and
typically not used by the enduser.

- [`fast_scraper()`](https://nflfastr.com/reference/fast_scraper.md) :
  Get NFL Play by Play Data
- [`add_qb_epa()`](https://nflfastr.com/reference/add_qb_epa.md) :
  Compute QB epa
- [`add_xpass()`](https://nflfastr.com/reference/add_xpass.md) : Add
  expected pass columns
- [`add_xyac()`](https://nflfastr.com/reference/add_xyac.md) : Add
  expected yards after completion (xyac) variables
- [`clean_pbp()`](https://nflfastr.com/reference/clean_pbp.md) : Clean
  Play by Play Data
- [`decode_player_ids()`](https://nflfastr.com/reference/decode_player_ids.md)
  : Decode the player IDs in nflfastR play-by-play data

## Deprecated

These functions are no longer recommended for use, see nflreadr for
latest versions.

- [`fast_scraper_roster()`](https://nflfastr.com/reference/fast_scraper_roster.md)
  **\[deprecated\]** : Load Team Rosters for Multiple Seasons
- [`fast_scraper_schedules()`](https://nflfastr.com/reference/fast_scraper_schedules.md)
  **\[deprecated\]** : Load NFL Season Schedules
- [`report()`](https://nflfastr.com/reference/report.md)
  **\[deprecated\]** : Get a Situation Report on System, nflverse
  Package Versions and Dependencies
