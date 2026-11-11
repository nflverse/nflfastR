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
      expect_lt(sum(abs(diff(p))) - abs(p[length(p)] - p[1]), 0.06)
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

test_that("field goal range keeps its value at the end of the 1st half", {
  # 1st and 10 at the opponent's 20 with 10 seconds left is close to a free
  # field goal, while at the own 5 the half is as good as over
  fg_range <- wp_vegas_predict(vegas_state(t = 1810, yl = 20))
  backed_up <- wp_vegas_predict(vegas_state(t = 1810, yl = 95))
  expect_gt(fg_range - backed_up, 0.1)
  # and it doesn't fade over the last two minutes
  two_min <- wp_vegas_predict(vegas_state(t = 1920, yl = 20))
  expect_lt(abs(two_min - fg_range), 0.01)
})

test_that("being backed up before halftime is worse than reaching halftime", {
  # with a minute left the other team can get the ball back in field goal range
  minute <- wp_vegas_predict(vegas_state(t = 1860, yl = 95))
  halftime <- wp_vegas_predict(vegas_state(t = 1801, yl = 95))
  expect_lt(minute, halftime)
})

test_that("vegas_wp is at least the value of kicking a field goal", {
  g <- expand.grid(
    t = c(3000, 1920, 1830, 1810, 1803, 600, 30, 3),
    yl = c(5, 20, 35, 45),
    down = c(1, 4)
  )
  x <- vegas_state(t = g$t, down = g$down, ytg = 8, yl = g$yl)
  fg <- wp_fg_value(x, wp_vegas_model)
  expect_true(all(wp_vegas_predict(x) >= fg - 1e-12))
  # 4th and 8 at the 20 at the end of the half is a field goal
  x <- vegas_state(t = 1810, down = 4, ytg = 8, yl = 20)
  expect_equal(
    wp_vegas_predict(x),
    wp_fg_value(x, wp_vegas_model)
  )
})

test_that("vegas_wp is smooth in the clock at the end of the game", {
  g <- expand.grid(
    t = 1:240,
    sd = c(-8, -3, 0, 3),
    down = c(1, 4),
    yl = c(5, 35, 60, 95)
  )
  x <- vegas_state(t = g$t, sd = g$sd, down = g$down, ytg = 5, yl = g$yl)
  x$wp <- wp_vegas_predict(x)
  x <- x[
    order(
      x$score_differential,
      x$down,
      x$yardline_100,
      x$game_seconds_remaining
    ),
  ]
  same <- c(FALSE, diff(x$game_seconds_remaining) == 1)
  step <- abs(diff(x$wp))[same[-1]]
  # one second never moves WP by more than a few points
  expect_lt(max(step), 0.04)
})

test_that("a field goal that ends the game is valued by the result", {
  # tied, 4th and 5 at the opponent's 35: a make wins, a miss goes to overtime
  x <- vegas_state(t = 1:5, down = 4, ytg = 5, yl = 35)
  p <- fg_make_prob(2025, NULL, 35)
  ot <- wp_ot_start(x)
  expect_true(all(wp_vegas_predict(x) >= p + (1 - p) * ot - 1e-9))
  # and the value changes smoothly once there is time left after the kick
  y <- wp_vegas_predict(vegas_state(t = 1:30, down = 4, ytg = 5, yl = 35))
  expect_lt(max(abs(diff(y))), 0.015)
})

test_that("a team ahead that can kneel out the clock wins", {
  # up 1, 1st down, the other team has no timeouts: four kneels run off 2:12
  x <- vegas_state(t = c(30, 90, 119), sd = 1)
  x$defteam_timeouts_remaining <- 0
  expect_true(all(wp_vegas_predict(x) >= 0.999))
  # with 3 timeouts it can't from 1:30, and WP is the model's again
  x$defteam_timeouts_remaining <- 3
  expect_lt(wp_vegas_predict(x)[2], 0.99)
})

test_that("a tie at the end of regulation is worth the start of overtime", {
  x <- vegas_state(t = 3, spread = c(-7, 0, 7))
  ot <- wp_ot_start(x)
  expect_lt(abs(ot[2] - 0.5), 0.03)
  expect_true(ot[1] < ot[2] && ot[2] < ot[3])
})

test_that("down 1-3 on 4th down at the end, WP is the value of the kick", {
  x <- vegas_state(t = c(3, 8), sd = -3, down = 4, ytg = 5, yl = 10)
  expect_equal(wp_vegas_predict(x), wp_fg_value(x, wp_vegas_model))
})

test_that("a team behind deep in its own half with seconds left rarely wins", {
  x <- vegas_state(t = c(3, 10, 30), sd = -1, yl = 80)
  wp <- wp_vegas_predict(x)
  expect_lt(wp[1], 0.02)
  expect_true(wp[1] < wp[2] && wp[2] < wp[3])
})
