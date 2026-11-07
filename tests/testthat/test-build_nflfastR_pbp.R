test_that("build_nflfastR_pbp works (local data)", {
  # Uses locally available raw data, so it can't break because of failed
  # downloads. It only runs on CRAN, where the regression games test below is
  # skipped. Everywhere else the regression games test covers it.
  skip_if_not(is_on_cran(), "runs only on CRAN")
  pbp <- load_test_pbp()
  expect_s3_class(pbp, "nflverse_data")
  expect_pbp_unchanged(pbp, load_expected_pbp(), diff_name = "pbp_local")
})

test_that("build_nflfastR_pbp works (outside CRAN)", {
  # Same as above, but with downloaded raw data
  skip_on_cran()
  skip_if_offline("github.com")

  pbp <- load_test_pbp(dir = NULL)
  expect_pbp_unchanged(pbp, load_expected_pbp(), diff_name = "pbp_download")
})

test_that("build_nflfastR_pbp is quiet with option nflfastR.verbose = FALSE", {
  skip_if_not_installed("gsisdecoder")
  withr::local_options(nflfastR.verbose = FALSE)
  g <- readRDS(file.path(test_dir, "games.rds"))
  expect_no_message(
    build_nflfastR_pbp(game_ids, dir = test_dir, games = g, rules = FALSE)
  )
})

test_that("default_play is synced with build_nflfastR_pbp", {
  # `default_play` is a table of 1 row that initializes the data table in pbp
  # DBs, so it must match the column names and types of build_nflfastR_pbp()
  pbp <- strip_nflverse_attributes(load_test_pbp())
  expect_identical(
    vapply(default_play, class, character(1)),
    vapply(pbp, class, character(1))
  )
})

test_that("build_nflfastR_pbp output is unchanged for the regression games", {
  # The regression games are ~100 games picked to cover (almost) every
  # condition in the parser, see data-raw/test_game_selection/. Their raw files
  # and expected output are downloaded from a GitHub release (see
  # helper-regression.R), so the test is skipped on CRAN and offline.
  # For what to do when this test fails and how to update the expected
  # output, see data-raw/test_game_selection/README.md
  skip_on_cran()
  skip_if_offline("github.com")
  skip_if_not_installed("gsisdecoder")

  reg <- regression_data(test_path("regression_data.dcf"))
  skip_if(is.null(reg), "download of regression test data failed")

  exp <- readRDS(reg$expected)
  pbp <- build_nflfastR_pbp(
    unique(exp$game_id),
    dir = reg$raw_dir,
    games = readRDS(reg$games),
    rules = FALSE
  ) |>
    suppressMessages()

  expect_pbp_unchanged(pbp, exp, diff_name = "regression")
})
