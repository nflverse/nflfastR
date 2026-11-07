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

Whole pipeline, measured with `benchmark.R` on the same machine: 2023 season 170.1 s → 46.2 s (3.7x), single game `2023_05_KC_MIN` 1287 ms → 582 ms (2.2x).

What's left: ~14.5 s of `add_xyac()` is xgboost prediction, which is single-threaded on macOS. Splitting the prediction into chunks across `future` workers is the next lever. In single-game builds most of the remaining time is fixed dplyr overhead in `add_wp()`, `add_nflscrapr_mutations()` and `clean_pbp()`.

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
