# Regression tests for `build_nflfastR_pbp()`

The regression test ("build_nflfastR_pbp output is unchanged for the regression games" in `tests/testthat/test-build_nflfastR_pbp.R`) rebuilds about 100 games and compares the output with a stored expected output, play by play and column by column. The games were picked so that, together, they reach every reachable condition in the parser. Any change to the pbp output of these games makes the test fail.

This document explains how the test works, what to do when it fails, and how to update the expected output. It is written for both human contributors and AI agents.

## How it works

### Test data

The test data is not stored in git. It lives as assets of the pre-release [`regression_tests`](https://github.com/nflverse/nflfastR/releases/tag/regression_tests) in the nflfastR GitHub repo:

| Asset | Content |
|---|---|
| `regression_raw_<hash>.tar` | Raw pbp of the regression games |
| `regression_games_<hash>.rds` | Pinned schedule rows of these games |
| `regression_expected_<hash>.rds` | Expected output of `build_nflfastR_pbp()` |

`tests/testthat/regression_data.dcf` (the manifest, a [DCF](https://stat.ethz.ch/R-manual/R-devel/library/base/html/dcf.html) file that base R reads with `read.dcf()`) is the only file in git. It names the release tag and the assets the current commit uses, together with their md5 checksums. Asset names contain the checksum, so assets are never overwritten and every commit keeps pointing to its own data.

Raw data and schedule are pinned, so upstream data changes can't change the test result. Only package code and dependencies can.

### The test

1. It skips on CRAN, when offline, and when gsisdecoder isn't installed.
2. It downloads the assets named in the manifest and checks their md5. Downloads are cached in `tools::R_user_dir("nflfastR", which = "cache")`, so later runs reuse them. Old versions stay there until you delete them.
3. It runs `build_nflfastR_pbp()` on the raw files with the pinned schedule.
4. It compares the result with the expected output using `expect_pbp_unchanged()` in `tests/testthat/helpers.R`.

`expect_pbp_unchanged()` matches plays by `game_id` and `play_id` and reports:

- missing or new columns, and changed column order
- missing, new or duplicated plays
- columns whose type changed, e.g. integer to double
- changed values, compared one by one:
  - non-doubles exactly
  - doubles with a relative tolerance of 1e-6
  - model output (see `regression_model_columns()`) with a relative tolerance of 1e-3, because model output differs slightly across platforms. That matches the 3 significant digits the old tests rounded to: 6 and 4 digits failed on some OSes.

Values close to zero also get an absolute tolerance of 1e-6.

The test takes about one minute and runs in `devtools::test()` and on every R-CMD-check CI job.

## When the test fails

### 1. Find out what changed

The failure message lists every changed column with the number of changed values and games, plus three example plays:

```
pbp differs from expected output:
- epa: 412 values changed in 37 games, e.g.
    2018_07_CLE_TB 1234 (expected -0.41, got -0.38)
    ...
- All value differences written to /tmp/.../nflfastR_regression_diff.csv
```

The CSV lists every changed value with the columns `column`, `game_id`, `play_id`, `expected` and `actual`. Where to find it:

- **Locally:** run `devtools::test(filter = "build_nflfastR_pbp")`. The CSV is `regression_diff.csv` in `tools::R_user_dir("nflfastR", which = "cache")`, and the failure message prints the full path. The download test with the two sample games writes `pbp_download_diff.csv` to the same folder.
- **On CI:** the summary page of the failed workflow run has an artifact named `pbp-diff-<os>-<r>` that contains the CSVs of all failed pbp comparisons.

### 2. Decide whether the change is intended

Compare the differences with what the PR is supposed to do.

- **Unintended:** the PR has a bug. Fix the code, push, and CI runs again.
- **Intended:** the expected output must be updated, see below.
- **Fails on one OS only, with tiny differences in model columns:** that's platform noise above the tolerance. Raise `model_tolerance` in `expect_pbp_unchanged()`. Don't update the expected output for this.
- **Fails on every PR at once, without related code changes:** a dependency changed its output (e.g. fastrmodels, xgboost, gsisdecoder or nflreadr). Update the expected output in a separate PR from `master`, not inside an unrelated PR.

### 3. Document the change

Before updating the expected output, post the failure message in the PR, or attach the CSV. After the update, the PR diff only shows a new hash in the manifest, so this comment is the only record of what changed.

## Updating the expected output

Updating needs the gh CLI with write access to `nflverse/nflfastR`, so only maintainers can do it. Contributors from forks can't upload. They ask a maintainer, who needs "Allow edits by maintainers" enabled on the PR to push to it.

From the package root, on the PR branch:

```sh
gh pr checkout <number>

# optional: build the files without uploading anything
Rscript data-raw/test_game_selection/update_regression_test.R --dry-run

# build, upload and rewrite the manifest
Rscript data-raw/test_game_selection/update_regression_test.R

git commit -m "update regression test expectation" tests/testthat/regression_data.dcf
git push
```

The script rebuilds the expected output with the code of the checked-out branch, using the raw data and schedule of the current manifest. It uploads only assets that changed and then rewrites `regression_data.dcf`. Commit the manifest together with the code change.

### The local CRAN test

The test "build_nflfastR_pbp works (local data)" in the same file runs only on CRAN, where the regression test is skipped, and needs no network access. It builds two games from raw files in `tests/testthat/` and compares them with `tests/testthat/expected_pbp.rds` using the same `expect_pbp_unchanged()`. The test "build_nflfastR_pbp works (outside CRAN)" builds the same two games from downloaded raw files and compares them with the same file, so an outdated `expected_pbp.rds` fails on CI before it reaches CRAN. An intentional output change usually breaks it. Anyone can update the file, no write access needed:

```r
devtools::load_all()
testthat::local_test_directory("tests/testthat")
source("helpers.R")
saveRDS(
  strip_nflverse_attributes(load_test_pbp()),
  "expected_pbp.rds",
  compress = "xz"
)
```

### Two PRs that both change the output

Both PRs edit the same lines of `regression_data.dcf`. The PR that merges second gets a merge conflict. Resolve it by merging `master` into that PR and running the update script again, so the expected output matches the combined code.

### Release assets

Assets of PRs that were never merged stay in the release. They're small and harmless. Never delete an asset that a commit on `master` refers to: older commits would no longer be testable.

## Changing the set of games

The games are listed in `selected_games.csv`. They were picked with condition coverage, not line coverage, because most of the parser is vectorized: a `case_when()` line runs for every game, whether or not any play matches.

1. `instrument.R` rewrites the parser source so that every condition records, per game, whether it was reached, TRUE for at least one value and FALSE for at least one value.
2. `run_all.sh` (one `run_season.R` process per season) runs every game since 2001 with the instrumented parser and writes `hits/hits_<season>.rds`. This needs a local directory of raw pbp files.
3. `select_games.R` greedily picks the games that cover the most conditions, then covers each condition a second time. It writes `selected_games.csv`, `coverage_curve.csv`, `gaps.csv` and `never_hit.csv` to `hits/` (ignored by git). Copy the new `selected_games.csv` to this folder.
4. Upload new raw files, schedule and expected output with

   ```sh
   Rscript data-raw/test_game_selection/update_regression_test.R --new-games <raw_dir>
   ```

   where `<raw_dir>` contains `<season>/<game_id>.rds`.

Rerun the selection when the parser gains new branches that the current games don't reach.

## Rules for AI agents

- Never run `update_regression_test.R` without `--dry-run` unless the user explicitly asks for it. It uploads files to a public GitHub release, which other people and older commits depend on.
- Never update the expected output just to make a failing test pass. First show the user which columns, games and plays changed and why, and let them decide whether the change is intended.
- Don't change tolerances in `expect_pbp_unchanged()` to hide differences. Only raise `model_tolerance` when the user agrees the differences are platform noise.
- Don't delete release assets.
