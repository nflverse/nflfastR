# Speeding up `calculate_stats()`

This is a work plan for an agent (or a human) who rewrites `calculate_stats()` with data.table and removes its dplyr code, the same way as `build_nflfastR_pbp()` (see `data-raw/parser_performance/PLAN.md`). The test setup in step 0 comes first and must be finished before any line of the computation changes. The rules at the end are not optional.

Seasons 1999 and 2000 are deprecated. They are not used anywhere in this plan: not in the test games, not in the full-season comparisons and not in benchmarks.

## Baseline

2023 season (49.7k plays, 193k playstats rows), data already in memory, Apple Silicon Mac:

| `summary_level` / `stat_type` | Seconds |
|---|---|
| season / player | 4.9 |
| season / team | 3.5 |
| week / player | 8–17 (large variance between runs) |
| week / team | 4.0 |

Downloads come on top (`load_pbp()` ~44 s, `load_playstats()` 1.6 s, `load_players()` timed out after 60 s once).

Where the time goes (Rprof, week / player):

- ~85% is the one `summarise()` with ~130 expressions. dplyr evaluates every expression once per group in R: 18.6k groups for week / player, ~2.4M small R calls, plus `.data$` pronoun and `dplyr::first()` overhead.
- ~10% is `has_id()`: `;`-collapsed strings per player, team and play (`more_stats`, `team_stats`, `all_stats`) and a regex per row.
- The rest: grouped `mutate()`s, `left_join()`s and named-vector lookups with `paste(game_id, play_id)` keys.

A prototype with plain data.table on the same data: grouped flags 0.37 s, 110 row-level contribution columns 0.21 s, one GForce `lapply(.SD, sum)` by group 0.03 s (week and season), non-sum extras 0.05 s. The target is well under 1 s for every combination.

## Step 0: test setup

Goal: a test that fails on any change of any value of `calculate_stats()` output, runs offline after one cached download, is pinned against upstream data changes, and reaches every line and every data case of the current code. All of step 0 is done with the current, unchanged computation.

### 0.1 Mockable data loaders

`calculate_stats()` calls three loaders. Wrap the two external ones in package functions so tests can mock them with `local_mocked_bindings()` without `.package` (see AGENTS.md):

- `nflreadr::load_pbp()` → `stats_load_pbp()`
- `nflreadr::load_players()` → `stats_load_players()`
- `load_playstats()` is already a package function.

This is the only change to `R/calculate_stats.R` in step 0. Check that the output for the 2023 season is `identical()` before and after.

Status: done. The output for the 2023 season is `identical()` to master for all 12 combinations.

### 0.2 Case checklist

Line coverage alone is not enough: the `summarise()` call is one expression, so every stat line counts as covered whether or not any row matches. The test data must also reach every data case. An item is covered when the expected outputs (or, for row items, the inputs) of the test data contain it.

The checklist is implemented in `checklist.R`: `checklist_outputs()` runs the current `calculate_stats()` on given pbp, playstats and players (loaders mocked, no download) for the 8 distinct combinations (week summaries ignore `season_type`), `checklist_items()` returns one row per item with `covered` and the `games` that reach it (`NULL` for items that can't be attributed to single games, e.g. season summaries), and `checklist_report()` lists the gaps. 0.3 uses `games` for the selection, 0.6 uses `checklist_report()` on the pinned data.

Stat columns:

- Every numeric output column is non-zero at least once in each of the four outputs that summarize all data (week / player, week / team, season / player / REG+POST, season / team / REG+POST) in which it exists.
- Every numeric column of week / player is zero or `NA` at least once. Not required for the other outputs: a team's season total is rarely zero.
- The REG and POST season summaries only add the `season_type` filter, so they only need rows.
- Every character output column has at least one non-`NA` value in each of the four outputs. The list columns (`fg_made_list`, `fg_missed_list`, `fg_blocked_list`, `gwfg_distance_list`) also have at least one `NA` (from `""`) in each output, and at least one value with more than one entry in any output.

`has_id()` flags, each `TRUE` and `FALSE` at least once for a row that the stat counts:

- player: fumble (52:54) and fumble lost (106) on a sack (20), on a rush (10:11) and on a reception (21:22)
- team: first down 4 on completions (15:16) and receptions (21:22), first down 3 on rushes (10:11)
- play: 37, 38, 39, 33:34 and 34/36 on punts (2, 29, 31, 32)

Special cases:

- `passing_cpoe`: a group with all `cpoe` `NA` (the `NA_real_` branch) and one with values
- `fg_pct` and `pat_pct` with `fg_att == 0` / `pat_att == 0` and `> 0`
- `fg_long`, `pt_long`: the `%0%` branch to `NA` and the non-zero branch
- `fg_made_*` and `fg_missed_*` distance buckets, including 60+
- `pacr`: `NaN` → `NA`, `passing_air_yards <= 0` → 0, regular value
- `racr`: `NaN` → `NA`, `receiving_air_yards == 0` → 0, negative air yards for a player not in `racr_ids` → 0, negative air yards for an RB/FB (stays negative), regular value
- `is_gwfg_attempt`: made, missed and blocked game winning field goal attempts; a field goal in the final drive that doesn't count (defteam scored afterwards, or score differential outside -2..0)
- `special_teams_tds` and `def_tds`, including a defensive TD on a special teams play (counted in `special_teams_tds`, not `def_tds`)
- `def_fumbles` with `team == def` and with `team != def`
- `timeouts` (team stats only)
- A player who changed teams during the season: `recent_team` (last team), `target_share` and `air_yards_share` (first row's `team_targets` / `team_air_yards`)
- A player with more than one `player_name` spelling in playstats (`custom_mode()`), including a tie
- A player whose `gsis_player_id` is `NA` (team stats rows such as timeouts and team penalties), and a player id that's missing from `load_players()`
- Week summaries with a regular season and a postseason week; season summaries where `season_type` collapses to `"REG+POST"`
- A pbp play without playstats and playstats rows without a pbp play (`semi_join()`)
- Plays of two games in the same week with the same `play_id` (the `more_stats` / `team_stats` grouping by `season`, `week`, `play_id`)

Result on the full 2023 season: 769 of 783 items covered. The gaps:

- Rare events: a missed field goal under 20 yards (`fg_missed_0_19`) and a blocked game winning field goal attempt (`gwfg_blocked`), in all four outputs. The selection in 0.3 searches all seasons since 2001 for them.
- `fg_made_list` / `fg_missed_list` `NA` in season / team: no team goes a full season without a made or missed field goal. Reachable with a subset of a team's games.
- A player with two name spellings in one week, and a tie between spellings in one week (both covered for seasons).
- A player missing from `load_players()` and play stats without a pbp play. 0.4 builds both on purpose (remove players from the pinned players, keep the play stats of a play that's removed from the pinned pbp).

Found along the way: `calculate_stats()` uses `posteam_type` and `defteam_score` for the game winning field goals, but `stats_validate_pbp()` doesn't require them, so custom pbp without these columns fails with a dplyr error instead of the list of missing columns. The pinned pbp in 0.4 needs these two columns too (22 columns). Fix the validation in step 4 (snapshot changes; show the user).

Code paths (line coverage):

- `pbp = NULL` (download path, mocked) and custom `pbp` (`custom_pbp` attribute)
- `season_type` filter for `"REG"` and `"POST"`, and the filter that leaves 0 rows (warning + empty tibble)
- all four `grp_id` values
- `stats_validate_pbp()` error
- `load_playstats(TRUE)` and the `stopifnot()` failure

### 0.3 Game selection

`data-raw/calculate_stats_performance/select_games.R`:

1. Run the current `calculate_stats()` (`summary_level = "week"`, both stat types) for every season from 2001 to the most recent complete season, and record per game which checklist items it covers. Items that depend on season summaries (team changes, `REG+POST`) are recorded per player-season and need all games of that player-season in the selection.
2. Pick games greedily like `data-raw/test_game_selection/select_games.R`: add the game that covers the most uncovered items; ties go to the game with fewer playstats rows. Stop when everything is covered, then cover every item a second time while staying under ~60 games.
3. Write `selected_games.csv` (game, gain, coverage) and `gaps.csv` (items no game covers). Every gap must either be covered by a hand-picked game or be explained in this file as unreachable (dead code candidate for the rewrite).

### 0.4 Pinned inputs and expected output

`data-raw/calculate_stats_performance/update_stats_regression.R` (with `--dry-run`, like `update_regression_test.R`) builds:

- `stats_inputs_<hash>.rds`: a list with the pbp of the selected games (only the 20 columns of `stats_validate_pbp()` plus `posteam_type` and `defteam_score`, see 0.2), their playstats and the `load_players()` rows of all players in those playstats. Remove some players on purpose for the "missing from `load_players()`" case.
- `stats_expected_<hash>.rds`: the output of the current `calculate_stats()` on these inputs for all 12 combinations of `summary_level`, `stat_type` and `season_type`, plus the `pbp = NULL` path with mocked loaders.

The assets go to the `regression_tests` pre-release next to the pbp assets and get entries in `tests/testthat/regression_data.dcf` (`stats_inputs`, `stats_inputs_md5`, `stats_expected`, `stats_expected_md5`). The upload is public: run the script without `--dry-run` only after the user explicitly asks.

### 0.5 Tests

In `tests/testthat/test-calculate_stats.R`, next to the existing tests (which stay, they check the live data):

- "calculate_stats output is unchanged for the regression games": download the assets with `regression_data()`, mock `load_playstats()`, `stats_load_players()` and `stats_load_pbp()`, run all 12 combinations and compare with `expect_stats_unchanged()`.
- The same comparison for the input variants of one combination: shuffled pbp rows, shuffled playstats rows, a grouped tibble, a data.table and an `nflverse_data` object. The output must be the same as for the plain input.
- Snapshots (`expect_snapshot()`) of the empty-filter warning, the `stats_validate_pbp()` error and the `load_playstats()` input check.
- The `pbp = NULL` path has no `custom_pbp` attribute; custom `pbp` has it.

`expect_stats_unchanged()` goes into `tests/testthat/helpers.R`, modeled on `expect_pbp_unchanged()`. It matches rows by the grouping variables and reports missing or new columns, changed column order, changed classes and attributes, missing, new or duplicated rows, type changes and changed values: non-doubles exactly, doubles with a relative tolerance of 1e-9 (sums and means can change in the last bits when the summation order changes, e.g. GForce `mean()` vs. base `mean()`). It writes all differences to `stats_diff.csv` in `tools::R_user_dir("nflfastR", which = "cache")`.

### 0.6 Check the test setup

Step 0 is done when:

1. `devtools::test_coverage_active_file("R/calculate_stats.R")` reports 100% for the regression test alone.
2. `select_games.R` reports no checklist gaps (other than documented unreachable ones).
3. Mutation spot checks fail the test, each on its own: change `15:16` to `15` in `completions`, drop `has_id(106, more_stats)` from `rushing_fumbles_lost`, swap `dplyr::first()` and `dplyr::last()` for `recent_team`, change the `racr` RB rule, change the `pt_touchback * 20L`, remove the `na_if()` step. Revert after each.
4. `devtools::test(filter = "calculate_stats")` passes.

### 0.7 Full-season comparison

The regression test uses ~60 games. The rewrite is also checked on full seasons, outside the test suite:

- `calculate_stats_old.R`: verbatim copy of the current `calculate_stats()` and its helpers with the suffix `_old`, sourced after `devtools::load_all()`.
- `compare.R`: for the seasons 2001, 2012, 2023 and the most recent complete season, all 12 combinations, runs old and new on the same locally cached pbp, playstats and players and reports `identical()` per combination; if not identical, it prints the differences with the logic of `expect_stats_unchanged()`. Doubles that differ only within the tolerance are listed for review, not silently accepted.
- `benchmark.R`: times all four combinations on 2023 with data in memory (median of 3 warmed-up runs), old vs. new, and writes a table for this file.

## Step 1: slim input and play information

- Keep only the required columns of `pbp` as a data.table (`as_pbp_dt()` on the selected columns, never a full copy of ~370 columns), then filter `season_type`.
- One play table keyed by `game_id` / `play_id` with `off`, `def` (through `team_name_fn()`), `special`, `season_type` and `is_gwfg_attempt` (grouped `max(fixed_drive)` by `game_id`, `posteam`). Add it to playstats with one update join. This replaces `playinfo`, `s_type_vctr`, `gwfg_vctr` and `%ifna%`.
- Playstats: `setDT()`, filter with a join on the pbp keys instead of `semi_join()`, `setnames()`.

Check: regression test, `compare.R`.

## Step 2: flags instead of collapsed strings

Replace `more_stats`, `team_stats`, `all_stats` and `has_id()` by logical flag columns computed by group (same grouping keys as now), e.g. `p_fumble`, `p_fumble_lost`, `t_first_down_pass`, `t_first_down_rush`, `a_punt_oob`, `a_punt_downed`, `a_punt_fair_catch`, `a_punt_returned`, `a_punt_return_td`. `team_play_air_yards`, `play_punt_return_yards`, `team_targets` and `team_air_yards` become grouped `:=` columns.

Check: regression test, `compare.R`.

## Step 3: aggregation

- Every stat that is a `sum()` becomes a contribution column computed once on the whole vectors, with the same expression as now (e.g. `(stat_id %in% 15:16) * yards`), then one `dt[, lapply(.SD, sum), by = grp_vctr]` (GForce). Keep integer results integer.
- `fg_long` and `pt_long`: GForce `max()`, then the 0 → `NA` replacement on the column.
- Stats that aren't sums in one grouped call: `player_name` (most common, ties as in `custom_mode()`), `season_type` collapse, `game_id`, `team` (first), `recent_team` (last), `opponent_team`, `games` (`uniqueN()`), the `fg_*_list` columns, first `team_targets` / `team_air_yards`.
- Derived stats on the aggregated columns: `passing_yards_after_catch`, `pacr`, `racr`, `target_share`, `air_yards_share`, `wopr`, `def_sacks`, `def_sack_yards`, `fg_pct`, `pat_pct`, `pt_net_yards`, fantasy points. `fifelse()` / `fcase()`, `NaN` handling as now.

Check: regression test, `compare.R`, `benchmark.R`.

## Step 4: pbp stats, player information, output

- `passing_epa`, `passing_cpoe`, `rushing_epa`, `receiving_epa`: grouped aggregations on the slim pbp, added with update joins.
- `""` → `NA` in character columns with `set()`.
- Column order with `setcolorder()`, sort with `setorderv()` (C locale, like `arrange()`).
- Player information: join with data.table. Keep the semantics of `left_join()`: `NA` keys match `NA`, a duplicated `gsis_id` duplicates rows.
- Output: `setDF()` and the class `c("tbl_df", "tbl", "data.frame")`, like `make_nflverse_data()` does without importing tibble. The output must stay `identical()` in class and attributes.
- `load_players()` failures: fail with a clear error instead of "Column `gsis_id` doesn't exist" (new snapshot test).
- The empty-filter warning prints `{seasons}`, which is the default season for custom pbp. Print the seasons in pbp instead (snapshot changes; show the user).
- Column names used in `[.data.table` go into `utils::globalVariables()` at the end of `R/utils.R`; remove the separate call in `R/calculate_stats.R`.

Check: regression test, `compare.R`, `benchmark.R`, `devtools::check()`.

## Step 5: wrap-up

- Remove dplyr, rlang data masks (`data_syms()`, `set_names()`, `.data`, `.env`) and tibble from `R/calculate_stats.R`. `rlang::arg_match()` and `rlang::check_installed()` stay.
- `air format .`, `jarl check R/calculate_stats.R`.
- NEWS bullet with the benchmark numbers (and the `load_players()` error).
- Update this file with the results.

## Step 6 (separate effort): dependencies

Rewriting `calculate_stats()` alone doesn't drop dplyr. Remaining users: `calculate_standings()` (23 calls), `calculate_series_conversion_rates()` (21), `R/helper_database_functions.R` (17), `R/ep_wp_calculators.R` (9), `build_playstats()` (13, plus tidyr, janitor and purrr), `R/helper_scrape_gc.R` (35, deprecated) and single calls in `R/utils.R`, `R/save_raw_pbp.R`, `R/helper_scrape_nfl.R`, `R/build_nflfastR_pbp.R` and `R/helper_tidy_play_stats.R`. The parser also uses `janitor::make_clean_names()`. Without dplyr, tidyr, tibble and janitor, the recursive dependencies drop from 44 to 27 packages (cpp11, dplyr, generics, hms, janitor, lubridate, pillar, pkgconfig, R6, snakecase, stringr, tibble, tidyr, tidyselect, timechange, utf8, withr). `build_playstats()` is internal and only builds release data; it could move to `data-raw/` or the nflverse-pbp repo instead of being rewritten. Each function needs its own test setup like step 0 before it changes.

## Rules

- Don't change the computation before step 0 is complete and checked (0.6).
- Never use seasons 1999 or 2000.
- After every step: the regression test passes and `compare.R` reports `identical()` for all seasons and combinations. A difference is a bug in the rewrite until the user decides otherwise.
- Never update the expected stats output or loosen the tolerance to make the test pass. Show the user the differences and let them decide.
- Never upload to the `regression_tests` release without the user's explicit request; use `--dry-run`.
- Follow the data.table rules in AGENTS.md (`dt[cond, col := value]`, `as.numeric(cond)`, `%chin%`, `fifelse()` / `fcase()`, `globalVariables()`).
- Skip the `calculate_stats` tests only when working on other parts of the package; here they are the point.
- Commit only when the user asks.
