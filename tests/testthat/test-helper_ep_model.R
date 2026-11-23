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

test_that("states outside of the range of the model get the nearest state", {
  expect_identical(ep_state(yl = 0, ytg = 1), ep_state(yl = 1, ytg = 1))
  expect_identical(ep_state(yl = 100), ep_state(yl = 99))
  expect_identical(ep_state(ytg = 0), ep_state(ytg = 1))
  expect_identical(ep_state(ytg = 60), ep_state(ytg = 40))
  expect_identical(ep_state(t = -6), ep_state(t = 0))
  expect_identical(ep_state(t = -60), ep_state(t = 0))
  # computed term by term
  expect_identical(ep_state(yl = 105, ytg = 2.5), ep_state(yl = 99, ytg = 2.5))
})

test_that("plays with a missing feature are NA and don't change the others", {
  p <- ep_state(down = c(1, NA, 2), yl = c(75, 50, NA), t = c(900, 900, 900))
  expect_identical(p[1, ], ep_state()[1, ])
  expect_all_true(as.vector(is.na(p[2:3, ])))
  for (season in c(2020, 2025)) {
    p <- ep_state(down = c(1, 5, 0), season = season)
    expect_identical(p[1, ], ep_state(season = season)[1, ])
    expect_all_true(as.vector(is.na(p[2:3, ])))
  }
})

test_that("a model with another layout of the terms is an error", {
  model <- ep_model
  model$layout$terms <- rev(model$layout$terms)
  expect_snapshot(ep_state(model = model), error = TRUE)
})

test_that("the tables give the predictions of the terms", {
  g <- expand.grid(
    down = 1:4,
    ytg = c(1, 3, 10, 25),
    yl = c(2, 9, 33, 64, 97),
    t = c(1790, 845, 299, 61, 4),
    season = c(2003, 2010, 2016, 2019, 2025),
    timeouts = 0:3
  )
  state <- make_model_mutations(data.frame(
    season = g$season,
    posteam = "A",
    home_team = rep_len(c("A", "B"), nrow(g)),
    roof = rep_len(c("outdoors", "dome", "closed"), nrow(g)),
    down = g$down,
    ydstogo = g$ytg,
    yardline_100 = g$yl,
    half_seconds_remaining = g$t,
    posteam_timeouts_remaining = g$timeouts,
    defteam_timeouts_remaining = (g$timeouts + g$down) %% 4
  ))
  features <- model_matrix(state, ep_model_features)
  eta <- ep_model_eta_exact(features, ep_model$coefficients)
  exact <- ep_model_softmax(lapply(seq_len(ncol(eta)), function(j) eta[, j]))
  expect_equal(unname(ep_model_predict(features)), exact, tolerance = 1e-12)
  # a yard line that isn't a whole number is computed term by term
  ep <- ep_state_points(yl = c(40, 40.5, 41))
  expect_lt(abs(ep[2] - (ep[1] + ep[3]) / 2), 0.01)
})

test_that("the same state gets the same prediction wherever it is", {
  # identical, not only equal: EPA of a play that doesn't change the state has
  # to be 0 on every platform
  yl <- c(75, rep(1:99, 30), 75)
  p <- ep_state(yl = yl)
  expect_identical(p[1, ], p[length(yl), ])
})

test_that("expected points are smooth in the clock", {
  t <- 0:1800
  for (s in list(c(1, 10, 75), c(1, 10, 35), c(3, 5, 50), c(2, 7, 20))) {
    ep <- ep_state_points(down = s[1], ytg = s[2], yl = s[3], t = t)
    step <- abs(diff(ep))
    expect_lt(max(step[t[-1] > 120]), 0.01)
    expect_lt(max(step), 0.1)
  }
})

test_that("the model only uses the era of a season", {
  expect_identical(ep_state(season = 2001), ep_state(season = 2005))
  expect_identical(ep_state(season = 2018), ep_state(season = 2021))
  expect_identical(ep_state(season = 2022), ep_state(season = 2030))
  expect_gt(max(abs(ep_state(season = 2021) - ep_state(season = 2022))), 0)
  expect_gt(max(abs(ep_state(season = 2005) - ep_state(season = 2006))), 0)
})
