test_that("calculate_stats works", {
  skip_on_cran()
  skip_if_offline("github.com")

  s1 <- calculate_stats(seasons = 2023, summary_level = "season", stat_type = "player")
  s2 <- calculate_stats(seasons = 2023, summary_level = "week",   stat_type = "player")
  s3 <- calculate_stats(seasons = 2023, summary_level = "season", stat_type = "team")
  s4 <- calculate_stats(seasons = 2023, summary_level = "week",   stat_type = "team")
  s5 <- calculate_stats(seasons = 2023, summary_level = "week",   stat_type = "player", season_type = "POST")

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
