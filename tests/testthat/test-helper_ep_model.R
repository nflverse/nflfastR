test_that("ep_model_predict returns the probabilities of the next score", {
  g <- expand.grid(
    down = 1:4,
    ytg = c(1, 5, 10, 20),
    yl = c(95, 50, 21),
    t = c(1800, 600, 30, 0),
    season = c(2003, 2025)
  )
  p <- ep_state(g$down, g$ytg, g$yl, g$t, g$season)
  expect_identical(
    colnames(p),
    c(
      "Touchdown",
      "Opp_Touchdown",
      "Field_Goal",
      "Opp_Field_Goal",
      "Safety",
      "Opp_Safety",
      "No_Score"
    )
  )
  expect_all_true(as.vector(p > 0))
  expect_equal(rowSums(p), rep(1, nrow(g)))
  no_plays <- matrix(
    numeric(),
    ncol = length(ep_model_features),
    dimnames = list(NULL, ep_model_features)
  )
  expect_identical(dim(ep_model_predict(no_plays)), c(0L, 7L))
})

test_that("expected points follow field position, down, distance and clock", {
  expect_gt(ep_state_points(yl = 1, ytg = 1), 5.5)
  expect_lt(ep_state_points(yl = 99), 0)
  expect_gt(ep_state_points(down = 1), ep_state_points(down = 3))
  expect_gt(ep_state_points(down = 4, ytg = 1, yl = 40), 0)
  expect_lt(ep_state_points(down = 4, ytg = 10, yl = 80), 0)
  expect_gt(ep_state(t = 0)[, "No_Score"], 0.9)
  expect_lt(ep_state(t = 900)[, "No_Score"], 0.1)
})

test_that("expected points don't rise with the distance to the goal line or to go", {
  g <- expand.grid(
    yl = 1:99,
    ytg = 1:20,
    down = 1:4,
    t = c(1500, 900, 300, 100),
    season = c(2005, 2012, 2025)
  )
  # the states that occur: 1st & 10 and every distance on the other downs
  g <- g[g$ytg <= g$yl & (g$down > 1 | g$ytg == 10), ]
  g$ep <- ep_state_points(g$down, g$ytg, g$yl, g$t, g$season)
  # consecutive rows are a yard apart on the same down, distance and clock
  same <- diff(g$ytg) == 0 & diff(g$down) == 0 & diff(g$yl) == 1
  expect_lt(max(diff(g$ep)[same]), 0.05)
  g <- g[order(g$season, g$t, g$down, g$yl, g$ytg), ]
  same <- diff(g$yl) == 0 & diff(g$down) == 0 & diff(g$ytg) == 1
  expect_lt(max(diff(g$ep)[same]), 0.05)
})

test_that("states outside of the range of the model get the nearest state", {
  expect_identical(ep_state(yl = 0, ytg = 1), ep_state(yl = 1, ytg = 1))
  expect_identical(ep_state(yl = 100), ep_state(yl = 99))
  expect_identical(ep_state(ytg = 0), ep_state(ytg = 1))
  expect_identical(ep_state(ytg = 60), ep_state(ytg = 40))
  expect_identical(ep_state(t = -6), ep_state(t = 0))
})

test_that("plays with a missing feature are NA and don't change the others", {
  p <- ep_state(down = c(1, NA, 2), yl = c(75, 50, NA), t = c(900, 900, 900))
  expect_identical(p[1, ], ep_state()[1, ])
  expect_all_true(as.vector(is.na(p[2:3, ])))
})

test_that("the blocks of plays don't change the predictions", {
  yl <- rep(1:99, 30)
  # equal up to rounding: with an optimized BLAS the matrix products can round
  # the last digit differently for different block sizes
  expect_equal(ep_state(yl = yl, block = 7L), ep_state(yl = yl))
})

test_that("the model only uses the era of a season", {
  expect_identical(ep_state(season = 2001), ep_state(season = 2005))
  expect_identical(ep_state(season = 2018), ep_state(season = 2021))
  expect_identical(ep_state(season = 2022), ep_state(season = 2030))
  expect_gt(max(abs(ep_state(season = 2021) - ep_state(season = 2022))), 0)
  expect_gt(max(abs(ep_state(season = 2005) - ep_state(season = 2006))), 0)
})
