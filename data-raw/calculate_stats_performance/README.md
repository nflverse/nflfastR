# Test data and comparisons of `calculate_stats()`

`calculate_stats()` was rewritten with data.table in #630 and #631 (9 to 33 times faster, output unchanged except floating point differences in `passing_cpoe`). The work plan of the rewrite is in the git history (`PLAN.md`, last version in commit 0fd591f). This folder keeps what is needed to change `calculate_stats()` safely: the regression test data and a comparison of two package versions on full seasons.

## Files

| File | Purpose |
|---|---|
| `checklist.R` | Case checklist: `checklist_pbp_columns`, `checklist_outputs()`, `checklist_items()`, `checklist_report()`. Source after `devtools::load_all()`. |
| `selected_games.csv` | The 60 regression games, picked greedily so that they reach every case of the checklist (most of them twice). |
| `update_stats_regression.R` | Builds and uploads the pinned inputs and the expected output of the regression test. |
| `compare_versions.R` | Compares two versions of the package on full seasons: all 12 combinations of `summary_level`, `stat_type` and `season_type`, and timings of the four `summary_level` / `stat_type` combinations. |
| `hits/` | Git-ignored. Downloaded data per season and the results of `compare_versions.R`. Missing data is downloaded again. |

## Checking a change

Run the `calculate_stats` tests and compare with master on a full season:

```sh
git worktree add ../nflfastR-master master
Rscript data-raw/calculate_stats_performance/compare_versions.R ../nflfastR-master . 2023
```

The third argument takes comma separated seasons (default 2001, 2012, 2023 and the most recent complete season), the fourth the number of timed runs (default 3, 0 skips the timing). Each version runs in a fresh R process with `pkgload::load_all()`. Both versions need the loader wrappers `stats_load_from_url()` and `stats_load_players()`. The script reports "identical", "within tolerance" (doubles within the relative tolerance of the tests, accepted) or "DIFFERENT". A difference is a bug until the user decides otherwise.

## Regression test

`test-calculate_stats.R` runs all 12 combinations on the pinned inputs of the 60 games and compares them with the expected output (`expect_stats_unchanged()` in `tests/testthat/helpers.R`). The assets `regression_stats_inputs_*.rds` and `regression_stats_expected_*.rds` are in the `regression_tests` release, see `data-raw/test_game_selection/README.md` for the manifest. `local_stats_regression()` in `helper-regression.R` mocks `stats_load_from_url()`, `stats_load_players()` and `stats_load_pbp()`, so the tests download nothing else.

The pinned inputs contain:

- the pbp of the selected games, only the columns of `stats_pbp_columns()`;
- their play stats;
- the `load_players()` rows of the players in the play stats, except two removed on purpose to cover players missing from `load_players()`: `00-0025944` (a kicker with negative receiving air yards, so the `racr` rule for non RB/FB players is reached) and `00-0021182` (an RB).

After an intended change of the output, show the user the differences first. Rebuild the expected output from the pinned inputs with `update_stats_regression.R --dry-run`; `--new-inputs` downloads the inputs again. Upload (without `--dry-run`) only when the user explicitly asks: the release is public.

## Non-obvious behavior

- **The output depends on the row order of the play stats.** The real play stats aren't sorted by any key. Order dependent are `recent_team` (last row), `team`, `game_id` and `opponent_team` of week summaries (first row), `target_share`, `air_yards_share` and `wopr` of players with two teams (first row's team values), `player_name` ties (first name wins), the `season_type` collapse (`"REG+POST"` or `"POST+REG"`) and the order of the entries in the `fg_*_list` columns. The user decided to keep this. The play stats are processed in their given order, and filters must keep it (a plain `nomatch = NULL` join returns pbp order). The test shuffles pbp, never the play stats.
- **`fg_missed_0_19` is always 0.** No season since 2001 has a missed field goal under 20 yards. The column stays, it's part of the output. The checklist on the pinned inputs covers 779 of 783 items; the 4 gaps are this bucket in the four outputs that summarize all data.
- **Early play stats lack some stat IDs**, so the related stats are 0 there. This is the source data, not `calculate_stats()`:

  | Stat ID | Meaning | Missing in |
  |---|---|---|
  | 111, 113 | air yards / yards after catch of completions | 2001–2005 |
  | 112 | air yards of incompletions | 2003–2005 (2001–2002 have ~570 rows instead of ~7000) |
  | 115 | targets | 2003–2008 |
  | 110 | QB hits | 2003–2005 |
  | 402 | tackles for loss | 2003–2011 |

- The player and team flags group by `season`, `week`, `play_id` without `game_id`, as the original code did. Joins match `NA` keys like dplyr's `NA` group.
- Play stats are copied with `as.data.table()`, not `setDT()`, because the loader result may be cached (memoise, test mocks). The user's pbp is only subset with `.subset()` and never changed by reference.
- GForce: `sum(col)` with `by` uses GForce, `sum(expression)` doesn't, so row level values are columns first. `data.table::first()` and `last()` are GForce, `x[.N]` isn't. A grouping column in `j` turns GForce off. `passing_cpoe` uses GForce `mean()`, which differs from base `mean()` by up to ~4e-13 relative (accepted).

## Practical notes

- Never use the deprecated seasons 1999 and 2000.
- Use `Rscript` from the package root. `checklist_outputs()` mocks the loaders with `testthat::local_mocked_bindings(.package = "nflfastR")`, so nothing is downloaded during a checklist run.
- `readRDS()` of a URL (and with it `nflreadr::load_pbp()`, `load_players()` and `load_playstats()`) times out after 60 s on slow connections. Download with `curl::curl_download()` to a file, like `download_rds()` in `compare_versions.R`.
- `return()` inside `j` of a data.table call at the top level of a script fails ("no function to return from"). Put the code in a function.
- Grouped data.table calls whose `j` is `if (...) .(...)` return a table without the `j` columns when no group matches. Check `nrow()` before using the result.
- Rscript reads a script while it runs it. Don't edit or `air format` a script while it's running, or the rest is read from shifted offsets (parse error near a random word).
- R messages on this machine may be in German (`Fehler` = error, `Ausführung angehalten` = execution halted). Don't filter them away when you grep the output.
- A full season of the checklist takes ~45 s.
