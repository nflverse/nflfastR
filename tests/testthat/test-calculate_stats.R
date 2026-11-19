test_that("calculate_stats works with live data", {
  # The regression tests below use pinned inputs. This test downloads the
  # current data, so it fails if the upstream data change their shape.
  skip_on_cran()
  skip_if_offline("github.com")

  reg <- regression_data(test_path("regression_data.dcf"), set = "stats")
  skip_if(is.null(reg), "download of regression test data failed")
  exp <- readRDS(reg$stats_expected)[["week/player/REG"]]

  # nflreadr warns and returns empty data if a download times out
  s <- withCallingHandlers(
    calculate_stats(2023, summary_level = "week", stat_type = "player"),
    warning = function(w) {
      if (grepl("Failed to readRDS", conditionMessage(w), fixed = TRUE)) {
        skip("download of live data failed")
      }
    }
  )

  expect_gt(nrow(s), 17500)
  expect_identical(
    vapply(s, typeof, character(1)),
    vapply(exp, typeof, character(1))
  )
})

test_that("calculate_stats output columns are documented", {
  skip_on_cran()
  skip_if_offline("github.com")

  reg <- local_stats_regression()
  cols <- unique(unlist(lapply(reg$expected, names)))

  expect_in(cols, nfl_stats_variables$variable)
})

test_that("calculate_stats output is unchanged for the regression games", {
  # The regression games are picked to cover every data case of
  # calculate_stats(), see data-raw/calculate_stats_performance/README.md. Their
  # inputs and the expected output are downloaded from a GitHub release (see
  # helper-regression.R).
  skip_on_cran()
  skip_if_offline("github.com")

  reg <- local_stats_regression()
  out <- stats_regression_run(names(reg$expected), reg$inputs$pbp)

  expect_stats_unchanged(out, reg$expected, diff_name = "stats")
})

test_that("calculate_stats output doesn't depend on the input type and order", {
  skip_on_cran()
  skip_if_offline("github.com")

  reg <- local_stats_regression()
  pbp <- reg$inputs$pbp
  name <- c("season/player/REG+POST", "week/player/REG+POST")
  exp <- reg$expected[name]

  # The output depends on the row order of the play stats (first and last
  # team, name ties, list order), so only the order of pbp is shuffled. See
  # data-raw/calculate_stats_performance/README.md.
  shuffled_pbp <- withr::with_seed(1, pbp[sample(nrow(pbp)), ])
  variants <- list(
    shuffled_pbp = shuffled_pbp,
    data.table = data.table::as.data.table(pbp),
    nflverse_data = make_nflverse_data(data.table::copy(pbp)),
    nflreadr_data = as_nflreadr_data(pbp)
  )
  for (v in names(variants)) {
    out <- stats_regression_run(name, variants[[v]])
    names(out) <- paste(v, name)
    names(exp) <- paste(v, name)
    expect_stats_unchanged(out, exp, diff_name = paste0("stats_", v))
  }
})

test_that("calculate_stats marks custom pbp", {
  skip_on_cran()
  skip_if_offline("github.com")

  reg <- local_stats_regression()
  out <- stats_regression_run(
    c("season/player/REG", "pbp_null/season/player/REG"),
    reg$inputs$pbp
  )

  expect_identical(attr(out[[1]], "custom_pbp"), TRUE)
  expect_null(attr(out[[2]], "custom_pbp"))
})

test_that("calculate_stats warns about an empty season_type filter", {
  pbp <- data.frame(season = 2023L, season_type = "REG")
  local_mocked_bindings(stats_validate_pbp = function(pbp) 2023L)

  expect_snapshot(
    out <- calculate_stats(2023, "season", "team", "POST", pbp = pbp)
  )
  expect_identical(out, test_tibble())
})

test_that("calculate_stats validates custom pbp", {
  pbp <- data.frame(season = 2023L, game_id = "2023_01_DET_KC", play_id = 1)

  expect_snapshot(calculate_stats(pbp = pbp), error = TRUE)
})

test_that("calculate_stats errors if load_players fails", {
  skip_on_cran()
  skip_if_offline("github.com")

  reg <- local_stats_regression()
  local_mocked_bindings(stats_load_players = function() data.frame())

  expect_snapshot(
    calculate_stats(stat_type = "player", pbp = reg$inputs$pbp),
    error = TRUE
  )
})

test_that("load_playstats checks seasons", {
  expect_snapshot(load_playstats(1998), error = TRUE)
  expect_snapshot(load_playstats("2023"), error = TRUE)
})

test_that("load_playstats loads the seasons of the play stats release", {
  local_mocked_bindings(stats_load_from_url = function(urls) urls)
  url <- "https://github.com/nflverse/nflverse-pbp/releases/download/playstats/play_stats_%d.rds"

  expect_identical(load_playstats(2022:2023), sprintf(url, 2022:2023))
  expect_identical(
    load_playstats(TRUE),
    sprintf(url, seq(1999, nflreadr::most_recent_season()))
  )
})
