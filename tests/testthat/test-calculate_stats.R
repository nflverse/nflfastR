test_that("calculate_stats works", {
  skip_on_cran()
  skip_if_offline("github.com")

  s1 <- calculate_stats(
    seasons = 2023,
    summary_level = "season",
    stat_type = "player"
  )
  s2 <- calculate_stats(
    seasons = 2023,
    summary_level = "week",
    stat_type = "player"
  )
  s3 <- calculate_stats(
    seasons = 2023,
    summary_level = "season",
    stat_type = "team"
  )
  s4 <- calculate_stats(
    seasons = 2023,
    summary_level = "week",
    stat_type = "team"
  )
  s5 <- calculate_stats(
    seasons = 2023,
    summary_level = "week",
    stat_type = "player",
    season_type = "POST"
  )

  names_and_types_s1 <- vapply(s1, class, FUN.VALUE = character(1L))
  names_and_types_s2 <- vapply(s2, class, FUN.VALUE = character(1L))
  names_and_types_s3 <- vapply(s3, class, FUN.VALUE = character(1L))
  names_and_types_s4 <- vapply(s4, class, FUN.VALUE = character(1L))
  names_and_types_s5 <- vapply(s5, class, FUN.VALUE = character(1L))

  var_names <- nflfastR::nfl_stats_variables$variable

  # Make sure variable names are listed in nflfastR::nfl_stats_variables$variable
  expect_in(names(names_and_types_s1), var_names)
  expect_in(names(names_and_types_s2), var_names)
  expect_in(names(names_and_types_s3), var_names)
  expect_in(names(names_and_types_s4), var_names)
  expect_in(names(names_and_types_s5), var_names)

  # Weak row number test
  expect_gt(nrow(s1), 1900)
  expect_gt(nrow(s2), 17500)
  expect_identical(nrow(s3), 32L)
  expect_gt(nrow(s4), 500)
  expect_gt(nrow(s5), 800)

  # Snapshot variable types and names
  expect_snapshot_value(names_and_types_s1, style = "json2", variant = "stats")
  expect_snapshot_value(names_and_types_s2, style = "json2", variant = "stats")
  expect_snapshot_value(names_and_types_s3, style = "json2", variant = "stats")
  expect_snapshot_value(names_and_types_s4, style = "json2", variant = "stats")
  expect_snapshot_value(names_and_types_s5, style = "json2", variant = "stats")
})

test_that("calculate_stats works with pbp subsets", {
  skip_on_cran()
  skip_if_offline("github.com")

  pbp <- load_pbp(2024) |>
    dplyr::filter(week <= 2, grepl("LAC", game_id))
  s <- calculate_stats(summary_level = "week", stat_type = "player", pbp = pbp)

  # Weak row number test
  expect_lt(nrow(s), 130)

  # week is filtered to <= 2 so stats should return only those weeks
  expect_in(unique(s$week), 1:2)

  # drop some required columns
  pbp_wrong <- pbp |> dplyr::mutate(qb_epa = NULL, play_type = NULL)
  expect_error(
    calculate_stats(pbp = pbp_wrong),
    regexp = 'missing the following required variables: "play_type" and "qb_epa"'
  )
})

test_that("calculate_stats output is unchanged for the regression games", {
  # The regression games are picked to cover every data case of
  # calculate_stats(), see data-raw/calculate_stats_performance/PLAN.md. Their
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
  # data-raw/calculate_stats_performance/PLAN.md.
  shuffled_pbp <- withr::with_seed(1, pbp[sample(nrow(pbp)), ])
  variants <- list(
    shuffled_pbp = shuffled_pbp,
    grouped = dplyr::group_by(pbp, game_id),
    data.table = data.table::as.data.table(pbp),
    nflverse_data = make_nflverse_data(data.table::as.data.table(pbp))
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
  expect_identical(out, tibble::tibble())
})

test_that("calculate_stats validates custom pbp", {
  pbp <- data.frame(season = 2023L, game_id = "2023_01_DET_KC", play_id = 1)

  expect_snapshot(calculate_stats(pbp = pbp), error = TRUE)
})

test_that("load_playstats checks seasons", {
  expect_snapshot(load_playstats(1998), error = TRUE)
  expect_snapshot(load_playstats("2023"), error = TRUE)
})
