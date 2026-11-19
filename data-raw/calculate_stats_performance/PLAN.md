# Speeding up `calculate_stats()`

This is a work plan for an agent (or a human) who rewrites `calculate_stats()` with data.table and removes its dplyr code, the same way as `build_nflfastR_pbp()` (see `data-raw/parser_performance/PLAN.md`). The test setup in step 0 comes first and must be finished before any line of the computation changes. The rules at the end are not optional.

Seasons 1999 and 2000 are deprecated. They are not used anywhere in this plan: not in the test games, not in the full-season comparisons and not in benchmarks.

## Status and how to resume

Steps 0–5 were done on branch `calculate-stats-performance` (step 0 in #630, steps 1–5 in #631). Done: steps 0–5. The rewrite of `calculate_stats()` is complete. Step 6 (dependencies) is a separate effort.

After the rewrite, the scripts that were only needed for it were removed: the game selection (`season_hits.R`, `run_all.sh`, `select_games.R`) and the old-vs-new comparison with a frozen copy of the old code (`calculate_stats_old.R`, `compare.R`, `benchmark.R`). The steps below still name them; they are in the git history (e.g. commit c1359e8). `compare_versions.R` replaces `compare.R` and `benchmark.R`: it compares two versions of the package (e.g. a git worktree of master and the current checkout), so it stays correct after intended output changes.

Files in this folder:

| File | Purpose |
|---|---|
| `PLAN.md` | This plan. Update the status here after every step. |
| `checklist.R` | Case checklist (0.2): `checklist_pbp_columns`, `checklist_outputs()`, `checklist_items()`, `checklist_report()`. Source after `devtools::load_all()`. |
| `selected_games.csv` | The 60 selected games (committed; the input of 0.4). |
| `update_stats_regression.R` | Builds and uploads the pinned inputs and expected output (0.4). Uses `checklist.R`. |
| `compare_versions.R` | Two versions of the package on full seasons: all 12 combinations compared, the four `summary_level` / `stat_type` combinations timed. |
| `hits/` | Git-ignored. Downloaded data per season and the results of `compare_versions.R`. `compare_versions.R` downloads missing seasons. |

Compare a branch with master:

```sh
git worktree add ../nflfastR-master master
Rscript data-raw/calculate_stats_performance/compare_versions.R ../nflfastR-master . 2023
```

The third argument takes comma separated seasons (default 2001, 2012, 2023 and the most recent complete season), the fourth the number of timed runs (default 3, 0 skips the timing). Each version runs in a fresh R process with `pkgload::load_all()`. Both versions need the loader wrappers `stats_load_from_url()` and `stats_load_players()` (since #630).

Practical notes from earlier sessions:

- Use `Rscript` from the package root. `checklist_outputs()` mocks `load_playstats()` and `stats_load_players()` with `testthat::local_mocked_bindings(.package = "nflfastR")`, so nothing is downloaded during a checklist run.
- `readRDS()` of a URL (and with it `nflreadr::load_pbp()`, `load_players()` and `load_playstats()`) times out after 60 s on slow connections. Download with `curl::curl_download()` to a file, like `download_rds()` in `compare_versions.R`.
- `return()` inside `j` of a data.table call at the top level of a script fails ("no function to return from"). Put the code in a function.
- Grouped data.table calls whose `j` is `if (...) .(...)` return a table without the `j` columns when no group matches. Check `nrow()` before using the result.
- Rscript reads a script while it runs it. Don't edit or `air format` a script while it's running, or the rest is read from shifted offsets (parse error near a random word).
- R messages on this machine are in German (`Fehler` = error, `Ausführung angehalten` = execution halted). Don't filter them away when you grep the output.
- A full season of the checklist takes ~45 s (8 calls of the current `calculate_stats()`).
- The `calculate_stats` tests need the network and take long; skip them (`devtools::test(filter = "calculate_stats", invert = TRUE)`) only when working on other parts of the package, not here.

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

Status: done. Implementation:

- `season_hits.R <season>` downloads the season once (pbp with `checklist_pbp_columns`, playstats, and one `load_players()` snapshot for all seasons; with `curl::curl_download()`, because `readRDS()` of a URL times out after 60 s on slow connections) into `hits/` (git-ignored), runs the checklist on the full season and builds candidate "units" for items that need more than one game: a player with targets for two teams (first game with targets per team), two name spellings of a player in a season and a tie between them (pairs of games), a regular season and a postseason game of a team, and two games of a week with the same `play_id`. `run_all.sh [last_season] [jobs]` runs all seasons since 2001 in parallel and skips finished ones (~45 s per season plus the download).
- `select_games.R [n_games]` picks single games and units greedily (ties go to fewer new play stats rows) and then runs the checklist on the data of the selected games only. That run is the real check, because items of season summaries can't be attributed to single games.

Result for 2001–2025 with a budget of 60 games: 15 games (5 single games, 5 units) cover everything in round 1, the rest of the budget covers every game-attributable item at least twice. The 60 games come from 17 seasons and have 37k play stats rows. On the selected data, 778 of 783 items are covered (more than any full season: 2023 has 769). Remaining gaps:

- `fg_missed_0_19 != 0` in all four outputs: no season since 2001 has a missed field goal under 20 yards (the kick would have to be snapped from inside the 2 yard line). The bucket is always 0. Unreachable; keep the column in the rewrite, it's part of the output.
- `player id missing from load_players()`: built in 0.4.

"Play stats without pbp play" turned out to occur in the real data, so 0.4 doesn't need to build it.

Found along the way: the playstats lack stat IDs in early seasons, so `calculate_stats()` reports 0 for the related stats there. This is a property of the source data, not of `calculate_stats()`, and doesn't change the rewrite.

| Stat ID | Meaning | Missing in |
|---|---|---|
| 111, 113 | air yards / yards after catch of completions | 2001–2005 |
| 112 | air yards of incompletions | 2003–2005 (2001–2002 have ~570 rows instead of ~7000) |
| 115 | targets | 2003–2008 |
| 110 | QB hits | 2003–2005 |
| 402 | tackles for loss | 2003–2011 |

### 0.4 Pinned inputs and expected output

`data-raw/calculate_stats_performance/update_stats_regression.R` (with `--dry-run`, like `update_regression_test.R`) builds:

- `stats_inputs_<hash>.rds`: a list with the pbp of the selected games (only the 20 columns of `stats_validate_pbp()` plus `posteam_type` and `defteam_score`, see 0.2), their playstats and the `load_players()` rows of all players in those playstats. Remove some players on purpose for the "missing from `load_players()`" case.
- `stats_expected_<hash>.rds`: the output of the current `calculate_stats()` on these inputs for all 12 combinations of `summary_level`, `stat_type` and `season_type`, plus the `pbp = NULL` path with mocked loaders.

The assets go to the `regression_tests` pre-release next to the pbp assets and get entries in `tests/testthat/regression_data.dcf` (`stats_inputs`, `stats_inputs_md5`, `stats_expected`, `stats_expected_md5`). The upload is public: run the script without `--dry-run` only after the user explicitly asks.

Details for the implementation:

- Inputs: the games in `selected_games.csv`. Download the pbp and playstats of their seasons with `curl::curl_download()` (see the notes above) and keep only the rows of the selected games. Keep the pbp columns of `checklist_pbp_columns`, all playstats columns, and the players columns `calculate_stats()` uses (`gsis_id`, `display_name`, `position`, `position_group`, `headshot`) for the `gsis_id`s in the playstats.
- Remove one or two players with stats from the pinned players on purpose ("player id missing from `load_players()`"). Choose them deterministically in the script and say which in a comment. Prefer a player with receiving stats and negative receiving air yards that isn't an RB/FB: they also exercise the `racr_ids` lookup.
- Upstream data can change between the selection and the pinning. After building the inputs, the script runs `checklist_items()` on them and stops if anything other than `fg_missed_0_19 != 0` (unreachable, see 0.3) is uncovered.
- Expected output: run the current `calculate_stats()` for all 12 combinations (with `load_playstats()` and `stats_load_players()` mocked to the pinned inputs) and for `pbp = NULL` (also `stats_load_pbp()` mocked). Store them in a named list, e.g. `"season/player/REG"`.
- Reuse the manifest helpers in `tests/testthat/helper-regression.R` and the upload code of `data-raw/test_game_selection/update_regression_test.R` (asset names with md5, `gh release upload`). Watch out: `write_regression_manifest()` writes only the fields of `regression_kinds` (`raw`, `games`, `expected`), and `regression_data()` downloads all of them, including the large raw tarball. Extend both so they handle the stats kinds and keep each other's fields: updating the pbp regression data must not drop the stats entries and vice versa, and the stats test should download only the stats assets. The README in `data-raw/test_game_selection/` describes the manifest; add the stats assets there.
- Show the user the asset sizes and the dry-run output before asking for the upload.

Status: done. The assets are uploaded and in `regression_data.dcf`. `--new-inputs` downloads into `hits/pin_download` and reuses it. The dry run result:

- Removed from the pinned players: `00-0025944` (Steven Hauschka, K, with negative receiving air yards) and `00-0021182` (Maurice Morris, RB).
- Checklist on the pinned inputs: 779 of 783 items, the gaps are the 4 unreachable `fg_missed_0_19 != 0`.
- Expected output: 12 combinations plus `pbp_null/season/player/REG`, from 60 games.
- Assets: `regression_stats_inputs_4dbcca1781.rds` (340 kB), `regression_stats_expected_b1c7f2f016.rds` (360 kB). The checksums are the same on a second run.
- `helper-regression.R`: `regression_kinds` is now a list of the sets `pbp` and `stats`, `regression_data(set = "stats")` downloads only the stats assets, and `write_regression_manifest()` writes the fields of both sets that are in the manifest. `update_regression_test.R` already keeps the other entries of the old manifest.

### 0.5 Tests

In `tests/testthat/test-calculate_stats.R`, next to the existing tests (which stay, they check the live data):

- "calculate_stats output is unchanged for the regression games": download the assets with `regression_data()`, mock `load_playstats()`, `stats_load_players()` and `stats_load_pbp()`, run all 12 combinations and compare with `expect_stats_unchanged()`.
- The same comparison for the input variants of one combination: shuffled pbp rows, shuffled playstats rows, a grouped tibble, a data.table and an `nflverse_data` object. The output must be the same as for the plain input.

Shuffled play stats are not part of the test: the output of the current `calculate_stats()` depends on their row order, and the real play stats aren't sorted by any key (neither `game_id`/`play_id` nor season, week, game). Order dependent are `recent_team` (last row), `team`, `game_id` and `opponent_team` of week summaries (first row), `target_share` / `air_yards_share` / `wopr` of players with two teams (first row's team values), `player_name` ties in `custom_mode()`, the `season_type` collapse (`"REG+POST"` or `"POST+REG"`) and the order of the entries in the `fg_*_list` columns. The user decided (2026-11-18): the rewrite keeps exactly this order dependence (process play stats in their given order; the regression test pins it). An order-independent output would be a separate, deliberate output change.
- Snapshots (`expect_snapshot()`) of the empty-filter warning, the `stats_validate_pbp()` error and the `load_playstats()` input check.
- The `pbp = NULL` path has no `custom_pbp` attribute; custom `pbp` has it.

`expect_stats_unchanged()` goes into `tests/testthat/helpers.R`, modeled on `expect_pbp_unchanged()`. It matches rows by the grouping variables and reports missing or new columns, changed column order, changed classes and attributes, missing, new or duplicated rows, type changes and changed values: non-doubles exactly, doubles with a relative tolerance of 1e-9 (sums and means can change in the last bits when the summation order changes, e.g. GForce `mean()` vs. base `mean()`). It writes all differences to `stats_diff.csv` in `tools::R_user_dir("nflfastR", which = "cache")`.

Status: done. `local_stats_regression()` and `stats_regression_run()` in `helper-regression.R` download the assets, mock the three loaders for the calling test and run the outputs named like the expected list. `expect_stats_unchanged()` compares named lists of outputs and writes all value differences of one call to one CSV (column `output` names the output). It was checked on mutated outputs: value changes of 1e-7, `NA` → `NaN`, integer → double, class, `custom_pbp` attribute, row and column order, missing and duplicated rows and character and list column values are reported; a relative change of 1e-12 isn't. The snapshot of the empty-filter warning passes `seasons = 2023` with pbp of 2023, so it should survive the message change of step 4. `devtools::test(filter = "calculate_stats")` passes (30 expectations).

`local_stats_regression()` mocks `stats_load_from_url()` (added in 0.6), not `load_playstats()`, so the body of `load_playstats()` runs in the regression test.

### 0.6 Check the test setup

Step 0 is done when:

1. `devtools::test_coverage_active_file("R/calculate_stats.R")` reports 100% for the regression test alone.
2. `select_games.R` reports no checklist gaps (other than documented unreachable ones).
3. Mutation spot checks fail the test, each on its own: change `15:16` to `15` in `completions`, drop `has_id(106, more_stats)` from `rushing_fumbles_lost`, swap `dplyr::first()` and `dplyr::last()` for `recent_team`, change the `racr` RB rule, change the `pt_touchback * 20L`, remove the `na_if()` step. Revert after each.
4. `devtools::test(filter = "calculate_stats")` passes.

Status: done.

- The user approved one more change of `R/calculate_stats.R` in step 0: `load_playstats()` calls the new wrapper `stats_load_from_url()` instead of `nflreadr::load_from_url()`, so tests can mock the download and still run `load_playstats()`. A new test checks the URLs for given seasons and for `seasons = TRUE`.
- Coverage of `R/calculate_stats.R` from the offline tests alone (the regression tests and snapshots, without the two live tests): 99.45% of 544 lines. Uncovered are only the bodies of the three loader wrappers (`stats_load_pbp()`, `stats_load_players()`, `stats_load_from_url()`), which the offline tests mock and the live tests run.
- Checklist on the pinned inputs: 779 of 783 items; the gaps are the unreachable `fg_missed_0_19 != 0` (see 0.4).
- Mutation spot checks: each of the six mutations makes the regression test fail (completions `15:16` → `15`, no `has_id(106, more_stats)` in `rushing_fumbles_lost`, `recent_team` with `first()`, negated `racr` RB rule, `pt_touchback * 25L`, no `na_if()`).
- `devtools::test(filter = "calculate_stats")` passes (32 expectations).

### 0.7 Full-season comparison

The regression test uses ~60 games. The rewrite is also checked on full seasons, outside the test suite:

- `calculate_stats_old.R`: verbatim copy of the current `calculate_stats()` and its helpers with the suffix `_old`, sourced after `devtools::load_all()`.
- `compare.R`: for the seasons 2001, 2012, 2023 and the most recent complete season, all 12 combinations, runs old and new on the same locally cached pbp, playstats and players and reports `identical()` per combination; if not identical, it prints the differences with the logic of `expect_stats_unchanged()`. Doubles that differ only within the tolerance are listed for review, not silently accepted.
- `benchmark.R`: times all four combinations on 2023 with data in memory (median of 3 warmed-up runs), old vs. new, and writes a table for this file.

Status: done.

- `calculate_stats_old.R` was generated from commit `5f6c00d`: `R/calculate_stats.R` without roxygen and `globalVariables()`, plus `custom_mode()`, `team_name_fn()` and its patterns, all with the suffix `_old`. `source_calculate_stats_old()` evaluates it in an environment whose parent is the nflfastR namespace; the old code uses no object of the package.
- `compare.R` on 2001, 2012, 2023 and 2025 with the unchanged code: 48 of 48 combinations `identical()`. With `pt_touchback * 25L` it reports the changed `pt_net_yards` values, so it does find differences. The seasons can be passed as an argument (e.g. `compare.R 2023` for a quick check).
- `benchmark.R` before the rewrite (2023, median of 3 runs, seconds); old and new are the same code, so this is the baseline:

| `summary_level` / `stat_type` | Old | New |
|---|---|---|
| season / player | 5.07 | 5.19 |
| season / team | 3.54 | 3.56 |
| week / player | 17.25 | 16.99 |
| week / team | 3.88 | 3.91 |

## Step 1: slim input and play information

- Keep only the required columns of `pbp` as a data.table (`as_pbp_dt()` on the selected columns, never a full copy of ~370 columns), then filter `season_type`.
- One play table keyed by `game_id` / `play_id` with `off`, `def` (through `team_name_fn()`), `special`, `season_type` and `is_gwfg_attempt` (grouped `max(fixed_drive)` by `game_id`, `posteam`). Add it to playstats with one update join. This replaces `playinfo`, `s_type_vctr`, `gwfg_vctr` and `%ifna%`.
- Playstats: `setDT()`, filter with a join on the pbp keys instead of `semi_join()`, `setnames()`.

Check: regression test, `compare.R`.

Status: done. Regression test passes, `compare.R` reports 48 of 48 combinations `identical()`.

- `pbp` becomes `as_pbp_dt(.subset(pbp, stats_pbp_columns()))`. `.subset()` drops dplyr groups and avoids `[.data.table` (a character `i` would be a join). The code only adds columns (`last_drive`), so the user's pbp is never changed by reference (checked).
- `stats_pbp_columns()` is `stats_required_pbp_columns()` (the validated list) plus `defteam_score` and `posteam_type`, which the gwfg logic uses but `stats_validate_pbp()` never required. Adding them to the validation changes the snapshot, so it happens in step 4 (agreed with the user).
- Playstats use `as.data.table()` (a copy), not `setDT()`: `setDT()` would change the class of the object returned by the loader, which may be cached (memoise, test mocks).
- The filter keeps the row order of playstats (`sort(unique())` of the `which = TRUE` join). A plain `nomatch = NULL` join returns pbp order, which would change `first()`, `last()`, `custom_mode()` ties and the `fg_*_list` order.
- `is_gwfg_attempt` stays integer (`as.integer(fcoalesce(cond, FALSE))`), so the gwfg stats stay integer. `NA` conditions count as no attempt, like `case_when(... ~ 1L, TRUE ~ 0L)`.

## Step 2: flags instead of collapsed strings

Replace `more_stats`, `team_stats`, `all_stats` and `has_id()` by logical flag columns computed by group (same grouping keys as now), e.g. `p_fumble`, `p_fumble_lost`, `t_first_down_pass`, `t_first_down_rush`, `a_punt_oob`, `a_punt_downed`, `a_punt_fair_catch`, `a_punt_returned`, `a_punt_return_td`. `team_play_air_yards`, `play_punt_return_yards`, `team_targets` and `team_air_yards` become grouped `:=` columns.

Check: regression test, `compare.R`.

Status: done. Regression test passes, `compare.R 2023` reports 12 of 12 combinations `identical()`.

- `flag_groups()` sets each flag to `FALSE`, then to `TRUE` with a join on the unique keys of the rows with the stat IDs. No R code per group. Joins match `NA` keys like dplyr's `NA` group (e.g. `player_id` `NA` across games with the same `play_id` in a week), and an `NA` `stat_id` never sets a flag, like `has_id()` on the pasted `"NA"`.
- Same grouping keys as before, also where they look odd: the player and team flags group by `season`, `week`, `play_id` without `game_id`.
- `by` of a data.table call must be `c()`, a list or a variable: an `if` there errors, so the target share groups are in `share_by`.

## Step 3: aggregation

- Every stat that is a `sum()` becomes a contribution column computed once on the whole vectors, with the same expression as now (e.g. `(stat_id %in% 15:16) * yards`), then one `dt[, lapply(.SD, sum), by = grp_vctr]` (GForce). Keep integer results integer.
- `fg_long` and `pt_long`: GForce `max()`, then the 0 → `NA` replacement on the column.
- Stats that aren't sums in one grouped call: `player_name` (most common, ties as in `custom_mode()`), `season_type` collapse, `game_id`, `team` (first), `recent_team` (last), `opponent_team`, `games` (`uniqueN()`), the `fg_*_list` columns, first `team_targets` / `team_air_yards`.
- Derived stats on the aggregated columns: `passing_yards_after_catch`, `pacr`, `racr`, `target_share`, `air_yards_share`, `wopr`, `def_sacks`, `def_sack_yards`, `fg_pct`, `pat_pct`, `pt_net_yards`, fantasy points. `fifelse()` / `fcase()`, `NaN` handling as now.

Check: regression test, `compare.R` on all seasons, `benchmark.R`.

Status: done. Regression test passes, `compare.R` on all seasons reports 48 of 48 combinations `identical()`.

- One GForce call on a table of contributions: `c(lapply(.SD, sum), list(fg_long = max(...), pt_long = max(...)))` with `keyby`. Logical contributions stay logical; `gsum()` returns integer, like `sum()`.
- The other stats in one grouped call (`others`), also with `keyby`, so the rows match and `cbind()` joins them. data.table doesn't drop `NULL` items of a grouped `j` (it errors), so every per-group value is computed for all summaries; `stats_output_columns()` selects and orders the columns like the old `summarise()`. The first team is `first_team` (renamed to `team` for week / player) so it doesn't collide with the grouping column `team`.
- `custom_mode()` returns `character(0)` when all names are `NA` (the `NA` `player_id` group); dplyr made it `NA`, now `[1L]` does.
- `fg_pct` / `pat_pct` with `dt[att > 0, pct := ...]`; `fg_long` / `pt_long` 0 → `NA` with a subset assignment. `%0%` removed.
- `benchmark.R` (2023, median of 3 runs, seconds):

| `summary_level` / `stat_type` | Old | New | Speedup |
|---|---|---|---|
| season / player | 5.16 | 0.98 | 5.3x |
| season / team | 3.51 | 0.59 | 6.0x |
| week / player | 17.26 | 1.28 | 13.5x |
| week / team | 3.85 | 0.65 | 6.0x |

## Step 4: pbp stats, player information, output

- `passing_epa`, `passing_cpoe`, `rushing_epa`, `receiving_epa`: grouped aggregations on the slim pbp, added with update joins.
- `""` → `NA` in character columns with `set()`.
- Column order with `setcolorder()`, sort with `setorderv()` (C locale, like `arrange()`).
- Player information: join with data.table. Keep the semantics of `left_join()`: `NA` keys match `NA`, a duplicated `gsis_id` duplicates rows.
- Output: `setDF()` and the class `c("tbl_df", "tbl", "data.frame")`, like `make_nflverse_data()` does without importing tibble. The output must stay `identical()` in class and attributes.
- `load_players()` failures: fail with a clear error instead of "Column `gsis_id` doesn't exist" (new snapshot test).
- The empty-filter warning prints `{seasons}`, which is the default season for custom pbp. Print the seasons in pbp instead (snapshot changes; show the user).
- `stats_validate_pbp()`: also require `defteam_score` and `posteam_type` (used by the gwfg logic, see step 1), so `stats_pbp_columns()` and `stats_required_pbp_columns()` become one list. The "validates custom pbp" snapshot changes; show the user. Agreed with the user.
- Column names used in `[.data.table` go into `utils::globalVariables()` at the end of `R/utils.R`; remove the separate call in `R/calculate_stats.R`.

Check: regression test, `compare.R`, `benchmark.R`, `devtools::check()`.

Status: done. Regression test passes, `compare.R 2023` reports 12 of 12 combinations `identical()`. `devtools::check()`: 0 warnings, 0 notes; the only test failure was the changed validation snapshot (waiting for the user's review).

- `passing_cpoe` uses GForce `mean()` (`gmean()`), a one-pass mean that differs from base `mean()` by up to ~1e-13 relative. The user accepted this for performance (first done with `base::mean()` to stay identical, reverted in step 5). `compare.R` reports it as within tolerance. The sums with GForce `gsum()` are identical.
- EPA stats: one aggregation per stat on the slim pbp, added with update joins (`NA` keys match, like `left_join()`). The EPA columns are part of `stats_output_columns()`, which replaces the `select()` that moved them.
- Sort with `setorderv(na.last = TRUE)`: C locale and `NA` last, like `arrange()`.
- `pacr` / `racr` with `fcase()` (all conditions on the original values; `default` can be a vector). Fantasy points with `- 2 *` instead of `+ -2 *` (exactly the same in floating point; fixes the jarl warnings).
- Player information: `stats_player_info()` errors if `load_players()` returned data without the needed columns (new snapshot test). The join `player_info[stats, on = "player_id", allow.cartesian = TRUE]` keeps the order of stats and duplicates rows for duplicated `gsis_id`s like `left_join()`.
- The empty-filter warning prints `seasons_in_pbp` (the snapshot doesn't change: the test passes `seasons = 2023`).
- `stats_validate_pbp()` also requires `posteam_type` and `defteam_score` (`stats_pbp_columns()` is the one list).
- Output: `setDF()` and the class `c("tbl_df", "tbl", "data.frame")`. Only `tibble::tibble()` for the empty result remains (step 5).
- `benchmark.R` (2023, median of 3 runs, seconds):

| `summary_level` / `stat_type` | Old | New | Speedup |
|---|---|---|---|
| season / player | 4.93 | 0.66 | 7.5x |
| season / team | 3.50 | 0.56 | 6.3x |
| week / player | 16.89 | 1.21 | 14.0x |
| week / team | 3.78 | 0.58 | 6.5x |

## Step 5: wrap-up

- Remove dplyr, rlang data masks (`data_syms()`, `set_names()`, `.data`, `.env`) and tibble from `R/calculate_stats.R`. `rlang::arg_match()` and `rlang::check_installed()` stay.
- `air format .`, `jarl check R/calculate_stats.R`.
- NEWS bullet with the benchmark numbers (and the `load_players()` error).
- Update this file with the results.

Check: regression test, `compare.R` on all seasons, `benchmark.R`, `devtools::check()`.

Status: done. Regression test passes (18 expectations), `compare.R` on all seasons: 48 of 48 combinations identical or within tolerance (12 identical; the others differ only in `passing_cpoe`, at most 3.6e-13 relative, from GForce `mean()`). `devtools::check()`: 0 errors, 0 warnings, 0 notes. `jarl check R/calculate_stats.R`: all checks passed.

- `R/calculate_stats.R` uses no dplyr, tibble or rlang data masks any more. The empty result is `structure(data.frame(), class = c("tbl_df", "tbl", "data.frame"))`, `identical()` to `tibble::tibble()`. The roxygen example uses `str()` instead of `dplyr::glimpse()`.
- `compare.R` reports "identical", "within tolerance" or "DIFFERENT" and accepts the first two.
- Two NEWS bullets: the speedup, and the `load_players()` error, the two new required pbp columns and the seasons in the empty-filter warning.
- Final `benchmark.R` (2023, median of 3 runs, seconds):

| `summary_level` / `stat_type` | Old | New | Speedup |
|---|---|---|---|
| season / player | 4.89 | 0.64 | 7.6x |
| season / team | 3.47 | 0.55 | 6.3x |
| week / player | 16.74 | 1.18 | 14.1x |
| week / team | 3.75 | 0.57 | 6.5x |

## Review of #631: items 1 and 2

A review after step 5 found two remaining hotspots. Profile of week / player on 2023: the grouped `others` call took 56% of the time, because `custom_mode()`, `paste()`, `fg_list()` and `uniqueN()` kept GForce off for the whole call and ran R code for each of the ~18.6k groups.

- Item 1: the stats that aren't sums are split into separate calls, each only for the summaries that output it. First values per group (`season_type` and `game_id` of week summaries, `off`, `def`, the first `team`, `team_targets`, `team_air_yards`) come from one GForce call with `lapply(.SD, data.table::first)`. `x[.N]` isn't GForce, `data.table::last()` is; a grouping column in `j` turns GForce off, so `team` is only in `.SDcols` for player stats. `opponent_team` is a vectorized `fifelse()` on those columns. Season summaries: the `season_type` collapse uses `paste()` only for groups with more than one season type, `games` counts unique `game_id`s, `recent_team` uses `data.table::last()`. `player_name` (player stats only): counts per group and name, sorted by count with the stable `setorderv()` (`by` keeps the order of first appearance), so ties go to the first name like `custom_mode()`. The `fg_*_list` columns paste only the rows of the kicks and join; groups without kicks get `NA` from the join, so the `""` → `NA` loop and `fg_list()` are gone (the play stats of 2001–2025 have no `""` strings and no `NA` stat IDs).
- Item 2: `passing_air_yards`, `punt_return_yards` and `targets` are row columns of `playstats`, used by the grouped `:=` totals (`team_play_air_yards`, `play_punt_return_yards`, `team_targets`, `team_air_yards`) and as contributions. `sum(col)` with `:=` and `by` uses GForce, `sum(expression)` doesn't.

Check: regression test passes (18 expectations), `compare.R` on all seasons: 48 of 48 identical or within tolerance (12 identical, the others only `passing_cpoe` as before). `jarl check`: all passed. `benchmark.R` (2023, median of 3 runs, seconds):

| `summary_level` / `stat_type` | Old | New | Speedup |
|---|---|---|---|
| season / player | 4.96 | 0.55 | 9.1x |
| season / team | 3.53 | 0.40 | 8.8x |
| week / player | 16.87 | 0.53 | 31.9x |
| week / team | 3.81 | 0.40 | 9.5x |

## Review of #631: items 3–12

Small cleanups, no change of the output:

- `fg_long` and `pt_long` are the maximums of the contributions `fg_made_distance` and `pt_yards`; the duplicated `fg_long_yards` and `pt_long_yards` are gone. The GForce call sums `.SDcols = !grp_vctr`.
- The grouping variables come from the logicals `week` and `player` (`c("season", if (week) "week", if (player) "player_id" else "team")`) instead of the codes `"10"`–`"40"`; `stats_output_columns()` takes `player` and `week`.
- The play information is added with one update join, then `playstats[!is.na(is_gwfg_attempt)]` keeps the rows of plays in pbp in their order (`is_gwfg_attempt` is never `NA` for a play in pbp). Before, a `which = TRUE` join selected the rows first.
- `stats_player_info()` no longer orders its columns; the `setcolorder()` after the join does that.
- Columns are selected by reference (`set(value = NULL)` and `setcolorder()`) instead of copying the table with `[, ..out_cols]`. A comment explains that `setorderv()` only moves `NA` groups to the end, because `keyby` already sorted the rows.
- `stat_id %in% td_ids()` is computed once (`is_td`).
- `%chin%` for the `season_type` argument, `setdiff()` in `stats_validate_pbp()`, no temporary variable in `load_playstats()`.

Check: regression test passes, `compare.R` on all seasons: 48 of 48 identical or within tolerance (12 identical, as before). `benchmark.R` (2023, median of 3 runs, seconds):

| `summary_level` / `stat_type` | Old | New | Speedup |
|---|---|---|---|
| season / player | 4.95 | 0.50 | 9.8x |
| season / team | 3.52 | 0.38 | 9.2x |
| week / player | 16.54 | 0.50 | 33.1x |
| week / team | 3.76 | 0.38 | 10.0x |

## Step 6 (separate effort): dependencies

Rewriting `calculate_stats()` alone doesn't drop dplyr. Remaining users: `calculate_standings()` (23 calls), `calculate_series_conversion_rates()` (21), `R/helper_database_functions.R` (17), `R/ep_wp_calculators.R` (9), `build_playstats()` (13, plus tidyr, janitor and purrr), `R/helper_scrape_gc.R` (35, deprecated) and single calls in `R/utils.R`, `R/save_raw_pbp.R`, `R/helper_scrape_nfl.R`, `R/build_nflfastR_pbp.R` and `R/helper_tidy_play_stats.R`. The parser also uses `janitor::make_clean_names()`. Without dplyr, tidyr, tibble and janitor, the recursive dependencies drop from 44 to 27 packages (cpp11, dplyr, generics, hms, janitor, lubridate, pillar, pkgconfig, R6, snakecase, stringr, tibble, tidyr, tidyselect, timechange, utf8, withr). `build_playstats()` is internal and only builds release data; it could move to `data-raw/` or the nflverse-pbp repo instead of being rewritten. Each function needs its own test setup like step 0 before it changes.

## Rules

- Don't change the computation before step 0 is complete and checked (0.6).
- Never use seasons 1999 or 2000.
- After every step: the regression test passes and `compare.R 2023` reports `identical()` or "within tolerance" for all combinations. Doubles that differ only within the tolerance of the tests (relative 1e-6) are accepted: performance matters more than the last digits (decided by the user after step 4, about `passing_cpoe`). After step 3 and step 5, run `compare.R` on all seasons (no argument). The tests compare doubles with a tolerance; `compare.R` checks `identical()` on full seasons, which catches small changes of doubles, class and attributes, and row-order effects. A difference is a bug in the rewrite until the user decides otherwise. For changes after the rewrite, `compare_versions.R` against master does the same.
- Never update the expected stats output or loosen the tolerance to make the test pass. Show the user the differences and let them decide.
- Never upload to the `regression_tests` release without the user's explicit request; use `--dry-run`.
- Follow the data.table rules in AGENTS.md (`dt[cond, col := value]`, `as.numeric(cond)`, `%chin%`, `fifelse()` / `fcase()`, `globalVariables()`).
- Skip the `calculate_stats` tests only when working on other parts of the package; here they are the point.
- Commit only when the user asks.
