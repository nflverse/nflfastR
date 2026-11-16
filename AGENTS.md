## This package

<!-- Insert package-specific content here. use_tidy_agents() will preserve this section when updating the rest of the file. -->

### Regression test of `build_nflfastR_pbp()`

The regression test in `tests/testthat/test-build_nflfastR_pbp.R` compares the pbp of about 100 games with an expected output stored in a GitHub release. Before working on a failure of this test, read `data-raw/test_game_selection/README.md`. In short:

- Show the user which columns, games and plays changed (the failure message prints the path of a CSV with all differences) and let them decide whether the change is intended.
- Never run `data-raw/test_game_selection/update_regression_test.R` without `--dry-run` unless the user explicitly asks. It uploads to a public release.
- Never update the expected output or loosen tolerances just to make the test pass.

### data.table in the `build_nflfastR_pbp()` pipeline

The pipeline passes one data.table through all steps and changes it by reference. Write idiomatic data.table code, not dplyr code translated line by line:

- Update the rows of an existing column that meet a condition with `dt[cond, col := value]`, not `set(dt, j = "col", value = fifelse(cond, value, dt$col))` (the `mutate(col = if_else(...))` pattern). The subset assignment only writes the matching rows. It is only equivalent if `cond` can't be `NA` where `col` isn't `NA`, because `fifelse()` writes `NA` there and the subset assignment leaves the value unchanged. Keep `fifelse()` (and say why in a comment) where that `NA` is intended.
- Create a new column with values in some rows only (`NA` elsewhere) with `dt[cond, col := value]` as well. Don't build a full vector of `NA` and fill it.
- Write 0/1 indicators as `as.numeric(cond)`, not `fifelse(cond, 1, 0)`.
- Match character vectors with `%chin%` instead of `%in%` (everywhere in the package, also in dplyr code). Keep `%in%` for numbers and logicals, and for vectors that can be factors or `NULL` (e.g. user input), because `%chin%` errors on them.
- Use `fifelse()`/`fcase()` to compute a new column from two or more values, or when all conditions must be evaluated on the original values (e.g. swapping teams).
- Put column names used in `[.data.table` into the `utils::globalVariables()` call at the end of `R/utils.R`.

## Package development

### Key commands

(All these functions have been optimized for agentic use, so they can be called directly without other arguments.)

```R
# Executing code
devtools::load_all()
code

# Tests
devtools::test() # all tests
devtools::test(filter = "^{name}") # tests for files starting with {name}
devtools::test_active_file("R/{name}.R") # tests for R/{name}.R
devtools::test_active_file("R/{name}.R", desc = 'blah') # single test with exact description "blah" (no regexp)

# Test coverage
devtools::test_coverage() # all files
devtools::test_coverage_active_file("R/{name}.R") # coverage for R/{name}.R from tests in tests/testthat/test-{name}.R

# Documentation
devtools::document() # redocument package
pkgdown::check_pkgdown() # check website

# Run complete R CMD check
devtools::check()
```

### Running R

There are three possible ways to run code, listed in rough order of desirability:

- If you're running inside Posit Assistant or otherwise have an
  `executeCode()` tool available, use it to run code in a session that the
  user can also interact with.

- Otherwise, if an R REPL (e.g. `mcp__r__repl` or `btw::run_r`) is
  available, use that. Note that `mcp__r__repl` uses a sandbox that blocks
  network requests and reads/writes outside of the current directory.

- Otherwise, use `Rscript -e "code"`. On Windows, `Rscript -e` can segfault on
  multiline or complex code; in that case, write it to a temporary `.R` file
  and run `Rscript path/to/file.R`.

### Code style

- Follow the tidyverse style guide
- Always run `air format .` after generating code. (air is bundled with Positron so look there if you can't otherwise find it.)
- Run `jarl check` on the files you have created. (notify the user if it isn't available on the local machine)
- Use the base pipe operator (`|>`), not the magrittr pipe (`%>%`).
- Use `\() ...` for single-line anonymous functions. For all other cases, use `function() {...}`.

### Test style

- Tests for `R/{name}.R` go in `tests/testthat/test-{name}.R`.
- All new code should have an accompanying test.
- If there are existing tests, place new tests next to similar existing tests.
- Strive to keep your tests minimal with few comments.
- Never put code in a `test-{name}.R` file outside of a `test_that()` block. Instead, use `tests/testthat/helper.R` or `tests/testthat/helper-{name}.R`.
- Avoid `expect_true()` and `expect_false()` in favor of a specific expectation with a better failure message. A few expectations in newer releases that you might not know about are `expect_all_true()`, `expect_all_equal()`, and `expect_r6_class()`.
- When testing errors and warnings:
  - Only use `expect_error()` or `expect_warning()` if the error or warning has a known class.
  - Generally, prefer `expect_snapshot(error = TRUE)` for errors and `expect_snapshot()` for warnings because these allow the user to review the full text of the output.
- Avoid the `.package` argument to `local_mocked_bindings()`; this modifies the namespace of another package, which is not good practice. Instead create a mockable version of the function in the current package. See `?local_mocked_bindings` for more details.

### Documentation

- Every user-facing function should be exported and have roxygen2 documentation.
- Internal functions should not have roxygen documentation.
- Wrap roxygen2 comments to 80 characters.
- Whenever you add a new (non-internal) documentation topic, also add the topic to `_pkgdown.yml`.
- Always re-document the package after changing a roxygen2 comment.
- Use `pkgdown::check_pkgdown()` to check that all topics are included in the reference index.

### `NEWS.md`

- Every user-facing change should be given a bullet in `NEWS.md`.
- Changes that shouldn't get a bullet:
    - Small documentation changes.
    - Internal refactorings.
    - Fixes to bugs introduced in the current dev version.
- Each bullet should briefly describe the change to the end user and mention the related issue in parentheses.
- A bullet can consist of multiple sentences but should not contain any newlines (i.e. DO NOT line wrap).
- If the change is related to a function, put the name of the function early in the bullet.
- If the change is related to an issue, include the issue number in parentheses.
- Only include a GitHub username if the PR was created by someone who isn't an author.
- Order bullets alphabetically by function name. Put all bullets that don't mention function names at the beginning.

## Specialized skills

- Do you need to deprecate a function or argument? Read `usethis::learn_tidy_skill("deprecate")`.
- Are you adding input checking to an existing function or writing a new exported function? Read `usethis::learn_tidy_skill("arg-checking")`.
- Are you creating a new package? Read `usethis::learn_tidy_skill("package-setup")`.

## Git

- If the user asks you to commit, use markdown in the commit message, and don't line wrap.
- If the commit fixes an issue, include `Fixes #num.` on its own line.
- Only push when the user explicitly requests it.

## Writing

- Use sentence case for headings.
- Use US English.

### Proofreading

If the user asks you to proofread a file, act as an expert proofreader and editor with a deep understanding of clear, engaging, and well-structured writing.

Work paragraph by paragraph, always starting by making a TODO list that includes individual items for each top-level section.

Fix spelling, grammar, and other minor problems without asking the user. Label any unclear, confusing, or ambiguous sentences with a FIXME comment.

Only report what you have changed.
