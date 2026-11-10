vegas_state <- function(
  t = 3000,
  sd = 0,
  spread = 0,
  down = 1,
  ytg = 10,
  yl = 75,
  home = 1,
  season = 2025
) {
  data.frame(
    season = season,
    game_seconds_remaining = t,
    half_seconds_remaining = ifelse(t > 1800, t - 1800, t),
    score_differential = sd,
    posteam_spread = spread,
    home = home,
    receive_2h_ko = 0,
    down = down,
    ydstogo = ytg,
    yardline_100 = yl,
    posteam_timeouts_remaining = 3,
    defteam_timeouts_remaining = 3
  )
}

test_that("vegas_wp is smooth in the clock", {
  # nothing changes but the clock (first half, 5 second steps)
  t <- seq(3590, 1810, by = -5)
  for (spread in c(-7, 0, 7)) {
    for (sd in c(-7, 0, 7)) {
      p <- wp_vegas_predict(vegas_state(t, sd, spread))
      expect_true(all(p > 0 & p < 1))
      expect_lt(max(abs(diff(p))), 0.02)
      # total movement beyond the net change: the old model was around 1
      expect_lt(sum(abs(diff(p))) - abs(p[length(p)] - p[1]), 0.05)
    }
  }
})

test_that("vegas_wp is monotone in score differential and spread", {
  g <- expand.grid(
    t = c(3400, 2000, 900, 240),
    sd = -10:10,
    spread = c(-7, 0, 7),
    down = c(1, 3),
    yl = c(80, 45, 10)
  )
  base <- vegas_state(g$t, g$sd, g$spread, g$down, 10, g$yl)
  p <- wp_vegas_predict(base)
  up_sd <- wp_vegas_predict(vegas_state(
    g$t,
    g$sd + 1,
    g$spread,
    g$down,
    10,
    g$yl
  ))
  up_spread <- wp_vegas_predict(vegas_state(
    g$t,
    g$sd,
    g$spread + 1,
    g$down,
    10,
    g$yl
  ))
  expect_true(all(up_sd >= p - 1e-9))
  expect_true(all(up_spread >= p - 1e-9))
})

test_that("scoring a touchdown never lowers vegas_wp with more than 5 minutes left", {
  g <- expand.grid(
    t = c(3500, 2700, 1900, 1500, 600, 301),
    sd = c(-7, -3, 0, 3, 7),
    spread = c(-10, 0, 10),
    down = 1:3,
    yl = c(1, 3, 5)
  )
  x <- vegas_state(g$t, g$sd, g$spread, g$down, g$yl, g$yl)
  pre <- wp_vegas_predict(x)
  post <- 0.94 *
    (1 - wp_vegas_raw(wp_post_td_state(x, 7))) +
    0.06 * (1 - wp_vegas_raw(wp_post_td_state(x, 6)))
  expect_true(all(pre <= post + 1e-9))
})

test_that("kickoff start spots follow the kickoff rules", {
  # own 28 (kickoffs from the 30), own 22, own 25, own 30, own 31
  expect_equal(
    wp_kickoff_start(c(2005, 2013, 2019, 2024, 2026)),
    c(72, 78, 75, 70, 69)
  )
  # without a season the current rules are used
  expect_equal(wp_kickoff_start(NA), wp_kickoff_start(2100))
  expect_equal(wp_kickoff_start(NULL), wp_kickoff_start(2100))
})

test_that("the tree margin uses all trees when asked to", {
  k <- wp_vegas_model$constants
  b <- wp_vegas_booster()
  X <- wp_vegas_tree_matrix(vegas_state(c(3000, 1700, 100), c(0, 3, -4)), k)
  full <- as.numeric(stats::predict(
    b,
    xgboost::xgb.DMatrix(X),
    outputmargin = TRUE
  ))
  expect_equal(
    wp_vegas_tree_margin(b, X, k$n_trees, wp_vegas_inclusive_range(b)),
    full
  )
})

test_that("vegas_wp works without a season column", {
  x <- vegas_state(c(3000, 1000))
  x$season <- NULL
  p <- wp_vegas_predict(x)
  expect_length(p, 2)
  expect_true(all(p > 0 & p < 1))
})
