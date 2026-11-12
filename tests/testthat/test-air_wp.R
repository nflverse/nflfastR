air_pass <- function(
  air_yards,
  down = 2,
  ydstogo = 8,
  yardline_100 = 60,
  qtr = 2
) {
  g <- 1800 + 600
  tibble::tibble(
    game_id = "2025_01_KC_LAC",
    season = 2025,
    qtr = qtr,
    posteam = "KC",
    home_team = "LAC",
    away_team = "KC",
    play_type = "pass",
    air_yards = air_yards,
    down = down,
    ydstogo = ydstogo,
    yardline_100 = yardline_100,
    score_differential = 0,
    home = 0,
    receive_2h_ko = 1,
    half_seconds_remaining = 600,
    game_seconds_remaining = g,
    elapsed_share = (3600 - g) / 3600,
    posteam_timeouts_remaining = 3,
    defteam_timeouts_remaining = 3,
    wp = 0.5,
    wpa = 0,
    two_point_attempt = 0,
    penalty = 0,
    yards_after_catch = NA_real_,
    complete_pass = 0,
    fixed_drive = 1,
    kickoff_attempt = 0
  )
}

air_wp_of <- function(x) {
  out <- add_air_yac_wp_variables(x)
  out$air_wpa + out$wp
}

test_that("air WP moves the ball to the catch point", {
  # same snap, deeper targets are worth more
  w <- air_wp_of(air_pass(c(-3, 0, 5, 15, 30)))
  expect_true(all(diff(w) > 0))
  # a catch in the end zone is a touchdown (from the 60: 60+ air yards), worth
  # more than a catch at the 1
  td <- air_wp_of(air_pass(c(60, 70)))
  expect_equal(td[1], td[2])
  expect_gt(td[1], air_wp_of(air_pass(59)))
})

test_that("air WP on a 4th down throw short of the line to gain is a turnover", {
  short <- air_wp_of(air_pass(5, down = 4, ydstogo = 10, yardline_100 = 40))
  long <- air_wp_of(air_pass(12, down = 4, ydstogo = 10, yardline_100 = 40))
  # the other team takes over near midfield: worse than the snap, much worse
  # than converting
  expect_lt(short, 0.5)
  expect_gt(long, short + 0.05)
})
