test_that("calculate_stats works", {
  skip_on_cran()
  skip_if_offline("github.com")

  s1 <- calculate_stats(seasons = 2023, summary_level = "season", stat_type = "player")
  s2 <- calculate_stats(seasons = 2023, summary_level = "week",   stat_type = "player")
  s3 <- calculate_stats(seasons = 2023, summary_level = "season", stat_type = "team")
  s4 <- calculate_stats(seasons = 2023, summary_level = "week",   stat_type = "team")
  s5 <- calculate_stats(seasons = 2023, summary_level = "week",   stat_type = "player", season_type = "POST")

  n1 <- names(s1)
  n2 <- names(s2)
  n3 <- names(s3)
  n4 <- names(s4)
  n5 <- names(s5)

  var_names <- nflfastR::nfl_stats_variables$variable

  # Make sure variable names are listed in nflfastR::nfl_stats_variables$variable
  expect_in(n1, var_names)
  expect_in(n2, var_names)
  expect_in(n3, var_names)
  expect_in(n4, var_names)
  expect_in(n5, var_names)

  # Weak row number test
  expect_gt(nrow(s1), 1900)
  expect_gt(nrow(s2), 17500)
  expect_identical(nrow(s3), 32L)
  expect_gt(nrow(s4), 500)
  expect_gt(nrow(s5), 800)
})
