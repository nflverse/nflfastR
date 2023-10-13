test_that("calculate_expected_points works", {
  # This test used to run on CRAN but their changes to env vars which cause
  # check NOTES for multi-threading forced us to skip on cran.
  skip_on_cran()

  data <- tibble::tibble(
    "season" = 2018:2019,
    "home_team" = "SEA",
    "posteam" = "SEA",
    "roof" = "outdoors",
    "half_seconds_remaining" = 1800,
    "yardline_100" = 75,
    "down" = 1,
    "ydstogo" = 10,
    "posteam_timeouts_remaining" = 3,
    "defteam_timeouts_remaining" = 3
  )
  ep <- calculate_expected_points(data) %>% round_double_to_digits()
  exp <- load_expectation("ep")
  expect_equal(ep, exp)
})

test_that("calculate_expected_points works", {
  # This test used to run on CRAN but their changes to env vars which cause
  # check NOTES for multi-threading forced us to skip on cran.
  skip_on_cran()

  data <- tibble::tibble(
    "receive_2h_ko" = 0,
    "home_team" = "SEA",
    "posteam" = "SEA",
    "score_differential" = 0,
    "half_seconds_remaining" = 1800,
    "game_seconds_remaining" = 3600,
    "spread_line" = c(1, 3, 4, 7, 14),
    "down" = 1,
    "ydstogo" = 10,
    "yardline_100" = 75,
    "posteam_timeouts_remaining" = 3,
    "defteam_timeouts_remaining" = 3
  )
  wp <- calculate_win_probability(data) %>% round_double_to_digits()
  exp <- load_expectation("wp")
  expect_equal(wp, exp)
})
