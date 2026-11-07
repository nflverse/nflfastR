# Speeding up `build_nflfastR_pbp()`

This is a work plan for an agent (or a human) who speeds up the play-by-play parser. It is based on profiling the 2023 season (285 games, ~50k plays) and single games on an Apple Silicon Mac. The rules at the end are not optional.

## Baseline

2023 season, single core (`future::plan("sequential")`):

| Step | Seconds | Share |
|---|---|---|
| `get_pbp_nfl()` (per-game parse, incl. reading raw files) | 115 | 66% |
| `add_xyac()` | 34 | 20% |
| `add_nflscrapr_mutations()` | 6.8 | 4% |
| `add_ep()` | 5.0 | 3% |
| `clean_pbp()` | 3.0 | 2% |
| `add_wp()` | 2.9 | 2% |
| `decode_player_ids()` | 1.8 | 1% |
| everything else | ~4.5 | 3% |
| **Total** | **173** | |

With `future::plan("multisession", workers = 8)` the full build takes 83 s. Everything after the download runs sequentially (~58 s).

Single game (`2023_05_KC_MIN`) without `add_xyac()`: 964 ms. `get_pbp_nfl()` 387 ms, `add_ep()` 112 ms, `add_wp()` 98 ms, `add_nflscrapr_mutations()` 70 ms, `clean_pbp()` 63 ms.

The CRAN and r-universe xgboost binaries for macOS are built without OpenMP, so all model predictions run on one core on macOS. Don't rely on xgboost threads.

## Status

Steps 1 to 6 are done on branch `parser-performance`. Every rewritten function was checked with `identical()` against the original on the 2023 season and the regression games, and the tests pass after every step.

| Step | Function | 2023 season before → after |
|---|---|---|
| 1 | `get_pbp_nfl()` (incl. `sum_play_stats()` → `tidy_play_stats()`) | 144 s → 16 s |
| 2 | `add_xyac()` | 34.4 s → 17.3 s |
| 3 | `add_nflscrapr_mutations()` | 7.1 s → 2.5 s |
| 4 | `add_ep()` + `add_wp()` | 7.8 s → 4.1 s |
| 5 | `decode_player_ids()` | 1.37 s → 0.22 s |
| 6 | `clean_pbp()` / `add_cp()` | 3.0 s → 2.5 s / 0.70 s → 0.29 s |

Ideas adopted from the `box_scores` repo (branch `static-box-scores`, which runs its own fast copies of nflfastR steps for single-game builds), again checked with `identical()`:

| Function | Change | 2023 season before → after |
|---|---|---|
| `add_drive_results()`, `add_series_data()` | Plain R vectors instead of grouped dplyr; lags within `game_id`/`game_half` through a "previous row of the same group" index, so games don't need to be sorted | 2.8 s → 0.27 s |
| `add_ep()` | The 3 EP predictions (all plays, missed FG, kickoff touchback) in one call of the model; feature matrix built directly | single game 0.48 s → 0.32 s (EP + air/yac EP + WP) |
| `add_cp()` | Model only for valid passes; receiver regex only where `receiver_player_id` is missing | 0.30 s → 0.06 s |
| `add_qb_epa()` | Skip the dplyr pipeline if there's no completed pass with a lost fumble | single game 18 ms → 3 ms |
| `get_pbp_nfl()` | Stats table with `rbindlist()` instead of `tidyr::unnest()`; works for games without any stats yet (they used to fail) | 42 → 37 ms per game |

Tried and dropped: cleaning text columns on their unique values (`box_scores`' `.fast_text_fix()`) was slower than our `team_name_fn()` fast path (31 ms vs 13 ms per game). The expression interpreter for `add_nflscrapr_mutations()` and `clean_pbp()` (`_fast_dplyr.R`) wasn't adopted: nflfastR owns that code and should rewrite it without dplyr instead.

`add_wp()`, `add_nflscrapr_mutations()` and `clean_pbp()` (and their helpers `prepare_wp_data()`, `make_model_mutations()`, `fix_scrambles()`, `translate_play_type_nfl()`, `restore_kickoff_attempt()` and the overtime WP helpers) were then rewritten without dplyr. They work on a plain list of column vectors. Helpers in `R/utils.R` keep dplyr's semantics: `if_else_vec()` and `case_when_vec()` (missing conditions, common result type), `lag_vec()`/`lead_vec()`, `group_key()`, `group_prev_index()`/`group_next_index()` for shifts within groups, `cumsum_by()`, `fill_up_by()`, `first_non_na_by()` and `mode_by()`. The `mutate()` blocks were translated mechanically (`name = expr` became `d$name <- expr`), so the expressions and comments are unchanged. Every function was checked with `identical()` on the 2023 season, the regression games, a single game and the season with shuffled rows. Single game: `add_wp()` 84 → 22 ms, `add_nflscrapr_mutations()` 63 → 14 ms, `clean_pbp()` 47 → 15 ms. Season times barely change because they are dominated by regexes.

After that, the base R versions of `if_else()`, `case_when()`, `lag()` and `lead()` were replaced by `data.table::fifelse()`, `data.table::fcase()` and `data.table::shift()` (10x faster per call on a season, 2x on a single game; `fcase()` refuses mixed integer/double outputs, so type mismatches fail loudly). The grouped helpers stay: grouped `[.data.table` calls cost ~0.2 ms each, ~10x more than the split-based helpers on a single game. stringr was replaced by stringi in the whole package (stringr functions are thin wrappers around stringi; the mappings follow stringr's own code, e.g. `\\1` becomes `$1` in replacements and `str_starts()` becomes `stri_detect_regex()` with `^(...)`), which drops stringr from the dependencies. A full `build_nflfastR_pbp()` of the 2023 season and of the regression games gives `identical()` output before and after these changes.

Finally, the rewritten steps were turned into idiomatic data.table code: each step converts its input once (`as_pbp_dt()`, a copy without extra attributes), changes columns by reference with `data.table::set()` and does all grouped work with `dt[, x := ..., by = ...]` (`cumsum()`, `shift()`, `nafill()`, the most common player id per group, the drive/series logic within game halves). Sorting, duplicates and renaming use `d[order(...)]`, `unique(by = )` and `setnames()`. That removed 18 own helpers (grouped lags, cumulative sums, fills, modes, ...); only `apply_where()`, `squish()` and `as_pbp_dt()` are left. Column names used in data.table calls are declared with `utils::globalVariables()`. Grouped `[.data.table` calls cost ~0.2 ms each, so a single game got ~15 ms slower.

After that, the whole pipeline works on one data.table by reference. `fast_scraper()` converts the downloaded table once with `setDT()` (no copy, the table is ours) and every step changes it by reference: `add_game_data()` with an update join, the EP/WP/air-yac/cp/xpass/xyac/qb_epa/decode steps with `set()` and grouped `:=`, `select_variables()` with `set(j = , value = NULL)` and `setcolorder()`. Model scenario data (missed field goal, kickoff touchback, PAT) are plain column lists, because `x$col <- value` on a data.table copies the whole table. `build_nflfastR_pbp()` converts back with `setDF()` before `make_nflverse_data()`. The exported steps (`clean_pbp()`, `add_qb_epa()`, `add_xyac()`, `add_xpass()`, `decode_player_ids()`) only work by reference inside the builder (`in_builder = TRUE` and a data.table, see `in_pipeline()`); called by users they work on a copy and return the type of the input, exactly as before (checked for tibble, `nflverse_data`, data.frame and data.table input; the input is never changed). `is.data.table()` alone isn't enough, because `nflverse_data` objects from `load_pbp()` inherit from data.table. The new option `nflfastR.verbose = FALSE` turns off the ~20 progress messages of a build (~1.5 ms each).

Whole pipeline, measured with `benchmark.R` on the same machine: 2023 season 170.1 s → 40.6 s (4.2x), single game `2023_05_KC_MIN` 1287 ms → 258 ms (5.0x), 224 ms (5.7x) with `options(nflfastR.verbose = FALSE)`.

What's left: ~14.5 s of `add_xyac()` is xgboost prediction, which is single-threaded on macOS. Splitting the prediction into chunks across `future` workers is the next lever. In single-game builds the biggest remaining steps are `add_xyac()` (~105 ms, mostly xgboost) and `get_pbp_nfl()` (31 ms).

`tidy_play_stats()` was generated mechanically from the branches of the old `sum_play_stats()` (`row$x <- v` became `x[i] <- v`), so the stat rules didn't change. It is still an R loop; a C++ port would save another ~2 s per season.

A cleanup pass after a code review removed leftovers of the mechanical translation, again checked with `identical()` on the regression games and the 2003 and 2023 seasons: cascades of `set()` calls on one column became one `fcase()` (two point, field goal and extra point results, points scored via `points_scored()`), `fcoalesce()` replaces hand-written coalescing in `clean_pbp()`, literal patterns use `stri_detect_fixed()`, regexes that ran several times on `desc` run once, `drive_posteam()` and the end of half are computed once for drives and series (helper columns of `add_drive_results()` that `add_series_data()` drops; both use `play_result()`), EP and WP team totals share `add_team_totals()`, `predict_rows()` only handles vector predictions, `tidy_play_stats()` uses `switch()` instead of 108 `else if` branches, and the remaining `dplyr` calls in `add_ep_variables()` and `decode_player_ids()` are gone. New tests check that the exported steps don't change their input and keep its type, and cover games without play stats and the parser helpers.

Memory was reduced next, again with `identical()` output. Peak memory of `build_nflfastR_pbp()` for the 2023 season went from 2.44 GB to 1.39 GB RSS (R heap 1.29 GB → 0.65 GB), the 100 regression games from 1.23 GB to 1.05 GB, at the same speed:

- `add_xyac()` processes passes in chunks of `xyac_chunk_size` (1000) passes, because every pass is expanded to 76 rows. The chunks are built in order of `half_seconds_remaining`: identical EP game states mostly come from passes at the same time of a half in different games, and `predict_ep()` predicts every unique state only once per chunk. Without that order, chunks lost the deduplication (845k instead of 690k EP predictions per season, +17% time). Peak 767 MB → 235 MB.
- `get_pbp_nfl()` removes duplicate plays per game and drops raw columns that no step uses and that aren't in the output (team ids, colors, logos, ...). `add_nflscrapr_mutations()` sorts by reference with `setorderv()` and copies the table only if there are empty rows to remove (there were two to three full copies before).
- `add_game_data()` adds the game columns with `match()` and `set()`. The update join with `[.data.table` allocated about twice the size of the table.
- Steps that work on a part of the plays (air/yac EP and WP, xpass, cp, xyac) copy only the columns they need (`subset_columns()`), not all ~450.

Measure memory with `gc(reset = TRUE)` before and the "max used" of `gc()` after a step (in a fresh process, models preloaded, because the first `load_model()` and `nflreadr::load_players()` count otherwise), and `/usr/bin/time -l` for the RSS of the whole process. `bench::bench_memory()` inflates the heap a lot. Rprof memory profiles attribute the allocations of lazily evaluated arguments (e.g. a dplyr pipe passed to a function) to the called function.

`%in%` on character vectors became data.table's `%chin%` (22 lines, mostly in the parser: `play_type`, `series_result`, `field_goal_result`, `season_type`, the `scramble_fix` lookup; also `calculate_stats()`, `calculate_series_conversion_rates()` and `completed_game_ids()`). On 50k plays it is about 4x faster with 4x less memory (0.2 ms vs 0.8 ms), so the effect on a full build is small. Results are the same, including `NA` handling. Checked with the test suite and the regression games test, not with `identical()` on full builds. Numeric and scalar `%in%` calls stay; `substr(id, 1, 4) %in% seq.int(...)` in `maybe_valid()` and `verify_game_ids()` compares character to numbers and must keep `%in%`.

`fast_scraper()` replaced the deprecated `furrr::future_map_dfr()` (which binds with `dplyr::bind_rows()`) with `furrr::future_map()` and `data.table::rbindlist(use.names = TRUE, fill = TRUE)`. `rbindlist()` returns a data.table, so the separate `setDT()` is gone. For the 2023 season (286 games, 50k plays, plus an empty `data.frame()` from a failed game) the bound table is `identical()` to `setDT(bind_rows(...))`; the bind is 43 ms instead of 74 ms, which is negligible next to the total run time. Checked with the regression games test. `get_pbp_gc()` (`R/helper_scrape_gc.R`) still uses `future_map_dfr()` but is deprecated, so it stays.

The raw plays have three big string columns: `playDescription`, `playDescriptionWithJerseyNumbers` and `shortDescription` (together about 40% of the plays table without `playStats`). Only the one with jersey numbers is used. Before, all three went through the joins and `normalize_raw_columns()`, `short_description` was dropped at the end of `get_pbp_nfl()`, and `play_description_with_jersey_numbers` was carried through every step until `select_variables()` dropped it. `get_pbp_nfl()` now drops `shortDescription` and replaces `playDescription` with the jersey number version right after reading the plays. `quarter_end` is now detected in the jersey number text (`"END QUARTER"` is the same in both). Checked with the test suite and the regression games test, not with `identical()` on full builds.

Earlier steps had only replaced the column-wise dplyr verbs in `get_pbp_nfl()`; the joins, `mutate()` calls and filters were still dplyr, as were `fix_posteams()` and `fix_bad_games()`. They are data.table code now and change one table by reference: `setDT()` on the raw plays and drives, the game info added with `:=`, the left joins of drives and play stats as joins with `which = TRUE` (row numbers of the match, NA without one) whose rows are added with one `:=`, and `normalize_raw_columns()`, `empty_strings_to_na()`, `fix_posteams()`, `fix_bad_games()` and the game-specific fixes with `:=`/`set()`. Rows are only subset (a copy) if there are plays to remove. An update join with `mget(paste0("i.", cols))` was 3x slower for the ~190 play stats columns (3.3 ms vs 1.0 ms per game). The scalars `season`, `week` and `id` go into `j` through `env`, so they can't be mistaken for columns. `get_pbp_nfl()` returns a data.table instead of a tibble, which `rbindlist()` in `fast_scraper()` binds the same way. The `td_team` fixes for old JAX games are in the table `td_team_fixes`. On the 100 regression games the output of every game is `identical()` to the old version (after `as_tibble()`), and so is the bound table; the per-game parse takes 2.7 s instead of 3.3 s for the 100 games. No raw file from 1999 to 2026 has the same home and away team or missing `driveSequenceNumber` on all plays, so those two branches never run with the current data. They were checked with `identical()` on 20 regression games whose raw data was changed to trigger both, and `fix_posteams()` and `fix_bad_games()` have unit tests.

`calculate_expected_points()` (used by `add_qb_epa()` for the fumble rows) was the last dplyr code in the pipeline. It now follows the semantics of the exported steps: it works on `as_pbp_dt()` of its input, builds the model input with `make_model_mutations()` on a list of columns and `model_matrix()`, adds the predictions with `:=`, puts them first with `setcolorder()` and returns `as_input_type()`. Its column list was renamed to `drop.cols.ep`: `drop.cols` of `clean_pbp()` (`R/helper_additional_functions.R`) is collated later and had overwritten it since nflfastR 2.x, so the function removed `success`, `passer`, `first_down` and the other player columns of its input instead of old EP columns (which ended up as duplicates `ep...8`, `ep...19`). It also always returned a plain data.frame, and with a data.table input the model helper columns leaked into the output. For the 17k plays of the regression games (without EP columns) all columns of the old output are `identical()` to the new output; the only differences are the 24 columns the old version wrongly removed and character row names. `ep_model_select()` is gone. `import(dplyr)` was removed from `NAMESPACE`: unqualified `intersect()`/`setdiff()` in the pipeline dispatched to dplyr's generics and now are base. `:=` is imported from data.table instead of rlang (rlang's two-argument `:=` made R CMD check complain about `` `:=`(a = ..., b = ...) `` calls). R CMD check without tests has no notes.

## Notes for the next agent

The paragraphs above are the history. This section describes the state of branch `parser-performance` (based on `regression-test-games`, pushed, no pull request yet) and what you need to continue.

### Current architecture

- `fast_scraper()` binds the parsed games with `data.table::rbindlist()` and passes one data.table through all steps. Internal steps (`add_game_data()`, `add_nflscrapr_mutations()`, `add_ep()`, `add_air_yac_ep()`, `add_wp()`, `add_air_yac_wp()`, `add_cp()`, `add_drive_results()`, `add_series_data()`, `restore_kickoff_attempt()`, `select_variables()`) expect a data.table and change it by reference. They don't accept tibbles anymore.
- The exported steps (`clean_pbp()`, `add_qb_epa()`, `add_xyac()`, `add_xpass()`, `decode_player_ids()`) start with `by_ref <- in_pipeline(pbp, ...)`. Inside the builder they change the table by reference; called by users they work on `as_pbp_dt(pbp)` (a copy) and return `as_input_type(d, pbp)` (`clean_pbp()` always returns a tibble, like before).
- `build_nflfastR_pbp()` and `fast_scraper()` call `data.table::setDF()` before `make_nflverse_data()`, so the final object is identical to the old one.
- Model inputs are built with `model_matrix(data, features)`; the feature vectors are `ep_model_features`, `wp_model_features`, `wp_spread_model_features`, `cp_model_features`, `xpass_model_features` and `xyac_model_features`.
- Remaining own helpers in `R/utils.R`: `apply_where()` (run a function only where a condition is TRUE, for expensive regexes), `squish()` (stringr's `str_squish()`), `as_pbp_dt()`, `in_pipeline()`, `as_input_type()`, `model_matrix()`, `is_verbose()`. `predict_rows()`/`expand_rows()` (`R/helper_add_ep_wp.R`) predict on a subset of rows and return a full-length result.
- No dplyr, tidyr or stringr in the pipeline anymore. dplyr is only used (always with `dplyr::`) by functions outside the pipeline such as `calculate_stats()` and `calculate_win_probability()`; the package no longer has `import(dplyr)`. stringr is no longer a dependency.

### How changes were verified

Every change was checked with `identical()` against the previous version, not only with the regression test (which has tolerances):

- Full pipeline: `build_nflfastR_pbp()` of the 2023 season (local raw files) and of the regression games, with the old code from a `git worktree` and the new code (`pkgload::load_all()` of each), compared with `identical()` after removing the `nflverse_timestamp` attribute.
- Single functions: old version from a copy of the `R/` files (`sys.source()` into an environment whose parent is the nflfastR namespace) against the new one, on the 2023 season, the regression games, a single game and the season with shuffled rows or games.
- Exported steps: tibble, `nflverse_data`, data.frame and data.table input, compared with the behavior from before all changes (commit `dc5912b2`), including that the input isn't modified.
- Tests: `devtools::test(filter = "calculate_stats", invert = TRUE)`. `tests/testthat/setup.R` runs all tests with `nflfastR.verbose = FALSE`.
- `jarl check` on changed files (config in `jarl.toml`). The only remaining finding in the touched files is the unused `get_pbp_gc()` (dead code from before this branch).
- R CMD check without tests must have no notes. Column names used in data.table calls go into the `utils::globalVariables()` call at the end of `R/utils.R`.

### Pitfalls we ran into

- `x$col <- value` on a data.table (`$<-.data.table`) copies the whole table. Use `data.table::set()`, or plain column lists (`as.list(dt)`, no copy) for scenario data.
- On a column list, `l$col <- 1` stores a single value; tibbles recycled it. Use `rep(1, n)`. This silently broke the WP of PATs once.
- `nflverse_data` objects (from `load_pbp()`, `make_nflverse_data()`) have class `data.table` without being valid data.tables. Never change user input by reference and don't call `as.data.table()` on them blindly (the schedule in `add_game_data()` is rebuilt from its columns).
- `data.table::fcase()` errors on mixed integer/double results; `fifelse()` converts them. `fifelse(..., na = x)` replaces `if_else(..., missing = x)`.
- In package code, `` `:=`(a = ..., b = ...) `` causes an R CMD check note because rlang's `:=` is imported; use `c("a", "b") := list(...)`. Use `list()` instead of `.()`.
- `dt[logical_vector]` filters rows, it doesn't select columns (this broke `clean_pbp()` on data.table input once). Drop columns with `set(dt, j = cols, value = NULL)`.
- Passing a data.table to mgcv can misbehave; `predict.bam()` gets `as.data.frame()`.
- `air format` formats `data.table::fcase()` calls as aligned tables by default. `air.toml` turns this off with `default-table = false`. Assign long conditions to named variables anyway, it keeps `fcase()` calls readable.
- xgboost on macOS (CRAN and r-universe binaries) has no OpenMP, so predictions are single-threaded there. On Linux they use all cores automatically (models are loaded with `nthread = 0`; limit with `OMP_NUM_THREADS`).
- The xgboost predictions of a row don't depend on other rows, so predicting only the rows that are needed (or unique rows) gives identical output. mgcv predictions on subsets were identical too.

### Benchmark workflow

`.github/workflows/benchmark.yaml` runs `data-raw/parser_performance/compare_benchmark.R` on `ubuntu-latest`: `build_nflfastR_pbp()` of the branch against master (or the `base` input) on the ~100 regression games and on one game (median of 10, after a warm-up), each measurement in a fresh R process, alternating the versions. It also compares the outputs of both versions. The result is in the job summary.

- It runs on pull requests that change the parser and, once the file is on the default branch, manually (`workflow_dispatch`). A temporary `push` trigger for this branch was removed after the pull request was opened, because it ran the benchmark twice for every push (against master and against the pull request base).
- Against master, the output check reports `xyac_median_yardage` (integer → double). That's the intended change of the base branch `regression-test-games`, not a bug.
- Locally (Apple Silicon, single-threaded xgboost): 100 regression games 55.2 s → 12.1 s, single game 1359 ms → 317 ms.
- On GitHub (`ubuntu-latest`, 4 cores, run 42393129509): 100 regression games 113.4 s → 17.5 s (6.5x), single game 2904 ms → 616 ms (4.7x).
- **Unresolved:** on Linux the output check found more differences than on the Mac: `xyac_epa` (2282 values), and `ep`, `epa` and the EP probabilities of 1-2 plays (plus the totals and `qb_epa` derived from them). On Apple Silicon both versions are `identical()`. Suspected causes (not verified): (1) the per-pass `sum()` in the grouped data.table summary of `summarize_xyac()` uses data.table's GForce sum, while base `sum()` (old code) accumulates in 80-bit `long double` on x86, which is the same as `double` on Apple Silicon; (2) mgcv predictions of the FG model on a subset of rows can differ in the last bits with a different BLAS. Next step: report the size of the differences in the benchmark (max absolute/relative difference) and, if they are only rounding, decide whether to restore bitwise identity (e.g. base `sum()` outside of GForce).

### Open items

- Find out why the Linux output differs slightly from master (see the benchmark workflow notes above).
- Pull request #622 is open against `regression-test-games` (#621), which should be merged first.
- `add_xyac()`: ~14.5 s per season are xgboost predictions. On macOS, chunks across `future` workers would help; on Linux, xgboost already uses all cores.
- `tidy_play_stats()` is an R loop; a C++ port would save ~2 s per season.
- The exported steps can't be used on a data.table by reference outside the builder by design (`in_pipeline()` needs `in_builder = TRUE`).

## Steps, in order

Do one step at a time. After each step, run the tests (see below) and compare timings with `data-raw/parser_performance/benchmark.R`.

### Step 1: per-game parse, `sum_play_stats()` and model caching

- **`get_pbp_nfl()`** (`R/helper_scrape_nfl.R`): about 300 ms of the 374 ms per game are overhead from column-wise dplyr verbs on ~283 columns: three `mutate_if()` calls for type conversion, `mutate_if(is.character, team_name_fn + squish)`, `janitor::clean_names()` (74 ms on its own) and `mutate_if(na_if(""))`. Replace them with a single `data.table::set()` loop over the columns, and replace `clean_names()` with a cached name mapping. Run `team_name_fn()` only on columns that contain one of its patterns; the output is identical because no replacement creates a match for a later pattern.
- **`sum_play_stats()`** (`R/helper_tidy_play_stats.R`): 25 ms per game. It subsets the stats table once per play (quadratic) and builds a ~190-field list per play. Rewrite it as one pass over the stats rows, sorted by play, that writes into preallocated column vectors. Order matters: later stats overwrite earlier ones and the `*_1`/`*_2` slots are filled in order.
- **`load_model()`** (`R/utils.R`): rebuilds the xgboost booster on every call (EP 16 ms × 6 calls, xyac 143 ms). Cache loaded boosters in a package environment.

### Step 2: `add_xyac()`

- ~14 s of 34 s are grouped dplyr work (`group_by(index)` over ~17k groups and ~900k rows: `case_when`, `lag`, `cumsum`, `summarise`). Rewrite with data.table or plain vectorized code (cumulative sums by group via `ave()`/`data.table`).
- ~19 s are the EP prediction on ~900k expanded rows. Only ~700k rows are unique: predict on unique rows and map back. Rows with `yardline_100 == 0` get `ep = 7` afterwards, so they don't need a prediction.
- `calculate_expected_points()` calls `make_model_mutations()` (with `as.factor()`) on all 900k rows; build the model matrix directly.

### Step 3: `add_nflscrapr_mutations()`

- `group_by(5 keys) |> slice(1)` takes 754 ms on a season; `!duplicated()` on the same keys takes 60 ms and keeps exactly the same rows. The following `arrange()` ends with the unique `index`, so the order doesn't change.
- The big `mutate()` runs grouped by `game_id`, so every regex and `if_else` runs once per game. Only `lead()` and `lag()` need the grouping. Run the rest ungrouped and use a by-game shift.
- `tolower(play_description)` is computed about 8 times. Compute it once.

### Step 4: EP and WP scenario predictions

- `add_ep_variables()` predicts the EP model 3 times on all rows (base, missed FG, kickoff touchback) and the FG model on all rows. The missed-FG and kickoff predictions are only read at FG and kickoff rows, and the FG model only at FG and PAT rows. Predict only on those subsets. Per-row predictions don't depend on other rows, so the output is identical.
- `add_wp_variables()` predicts 6 PAT scenarios and 2 kickoff scenarios on all rows but reads them only at PAT and kickoff rows. `make_pat_prob` is also passed to the overtime code, so it must stay full length.

### Step 5: `decode_player_ids()`

48 ID columns × 50k rows = 2.4M values, 282k non-NA, 1,947 unique. `case_when()` evaluates both decoders on every value of every column. Decode the unique IDs once and look them up with `match()`.

### Step 6: duplicate regex passes and redundant work in `clean_pbp()`

- `clean_pbp()` extracts each name twice: once for the name and again for the jersey number with a nearly identical pattern. One pass with capture groups gives both.
- `prepare_xyac_data()` and `add_cp()` both re-extract the raw receiver name from `desc`.
- `clean_pbp()` runs `team_name_fn()` again on team columns that `get_pbp_nfl()` already normalized. Keep it for standalone use of the exported function, but skip it in the builder (or make it cheap).

### Not worth it

- Porting the loop over game IDs to C++: the loop itself costs nothing, the body is the problem. It would also lose `future` parallelism.
- Faster tree inference: xgboost already runs at ~5 ns per tree per row.
- Faster regex engines: stringi already uses compiled ICU code. Fewer passes are the only lever.

## Rules

- The regression test must pass after every step. Never update the expected output or loosen tolerances to make it pass. Read `data-raw/test_game_selection/README.md` before working on a failure of that test.
- Never run `data-raw/test_game_selection/update_regression_test.R` without `--dry-run`.
- Skip the `calculate_stats` tests. They are slow and the parser doesn't affect them:

  ```r
  devtools::test(filter = "calculate_stats", invert = TRUE)
  ```

- Run `air format .` after changing code.
- Keep output columns, column order and column types unchanged.
- Changes don't have to be exact ports of the old code. Idiomatic approaches are welcome as long as the output stays identical. Prefer well-tested packages (data.table, stringi) over own implementations of generic functions; own helpers are harder to maintain.
- Internal refactorings don't get a `NEWS.md` bullet. Add one bullet for the overall speedup.
