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
- Internal refactorings don't get a `NEWS.md` bullet. Add one bullet for the overall speedup.
