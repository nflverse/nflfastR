test_that("build_nflfastR_pbp output is unchanged for the regression games", {
  # The regression games are ~100 games picked to cover (almost) every
  # condition in the parser, see data-raw/test_game_selection/. Their raw files
  # and expected output are downloaded from a GitHub release (see
  # helper-regression.R), so the test is skipped on CRAN and offline.
  # After an intentional output change, update the expected output with
  # data-raw/test_game_selection/update_regression_test.R
  skip_on_cran()
  skip_if_offline("github.com")
  skip_if_not_installed("gsisdecoder")

  reg <- regression_data(test_path("regression_data.json"))
  skip_if(is.null(reg), "download of regression test data failed")

  exp <- readRDS(reg$expected)
  pbp <- build_nflfastR_pbp(
    unique(exp$game_id),
    dir = reg$raw_dir,
    games = readRDS(reg$games),
    rules = FALSE
  ) |>
    suppressMessages()

  expect_pbp_unchanged(pbp, exp)
})
