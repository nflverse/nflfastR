test_that("calculate_expected_points works", {
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
  ep <- calculate_expected_points(data)
  expect_snapshot_file(save_test_object(ep), "ep.csv", cran = TRUE)
})

test_that("calculate_expected_points works", {
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
  wp <- calculate_win_probability(data)
  expect_snapshot_file(save_test_object(wp), "wp.csv", cran = TRUE)
})
