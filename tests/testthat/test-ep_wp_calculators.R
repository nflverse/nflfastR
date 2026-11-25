test_that("calculate_expected_points works", {
  # This test used to run on CRAN but their changes to env vars which cause
  # check NOTES for multi-threading forced us to skip on cran.
  skip_on_cran()

  data <- data.frame(
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
  ep <- calculate_expected_points(data) |> round_double_to_digits()
  exp <- load_expectation("ep")
  expect_equal(ep, exp)
})

test_that("calculate_expected_points doesn't change its input and keeps its type", {
  skip_on_cran()
  expect_input_unchanged(calculate_expected_points)
})

test_that("calculate_expected_points replaces ep and keeps other columns", {
  skip_on_cran()
  data <- data.frame(
    season = 2019,
    home_team = "SEA",
    posteam = "SEA",
    roof = "outdoors",
    half_seconds_remaining = 1800,
    yardline_100 = 75,
    down = 1,
    ydstogo = 10,
    posteam_timeouts_remaining = 3,
    defteam_timeouts_remaining = 3,
    success = 1
  )
  ep <- calculate_expected_points(data)
  ep_again <- calculate_expected_points(ep)
  expect_identical(ep_again, ep)
  expect_identical(names(ep)[8:9], c("ep", "season"))
  expect_identical(ep$success, 1)
})

test_that("expected points are net of the next possession", {
  skip_on_cran()
  data <- test_tibble(
    "season" = 2025,
    "home_team" = "SEA",
    "posteam" = "SEA",
    "roof" = "outdoors",
    "half_seconds_remaining" = c(900, 20),
    "yardline_100" = 1,
    "down" = 1,
    "ydstogo" = 1,
    "posteam_timeouts_remaining" = 3,
    "defteam_timeouts_remaining" = 3
  )
  ep <- calculate_expected_points(data)
  # a touchdown is likely on the goal line, and worth less than 7. It is worth
  # more near the end of the half, when the other team has less time to answer
  expect_lt(ep$ep[1], 6.94 - 0.5)
  expect_gt(ep$ep[2], ep$ep[1])
  expect_lt(ep$ep[2], 6.94)
})

test_that("calculate_expected_points treats neutral sites as away games", {
  skip_on_cran()
  data <- test_tibble(
    "season" = 2025,
    "home_team" = "SEA",
    "posteam" = c("SEA", "SEA", "DEN"),
    "location" = c("Neutral", "Home", "Home"),
    "roof" = "outdoors",
    "half_seconds_remaining" = 900,
    "yardline_100" = 65,
    "down" = 1,
    "ydstogo" = 10,
    "posteam_timeouts_remaining" = 3,
    "defteam_timeouts_remaining" = 3
  )
  ep <- calculate_expected_points(data)
  # the "home" team at a neutral site has the probabilities of a road team,
  # and not those of a home team
  expect_equal(ep$td_prob[1], ep$td_prob[3])
  expect_false(isTRUE(all.equal(ep$td_prob[1], ep$td_prob[2])))
})

test_that("the calculator uses the same free kick value as the play-by-play", {
  skip_on_cran()
  # a state where a safety is likely, so the exact free kick value is used, and
  # where it is far from the shortcut: little time left, in a season where the
  # kickoff start is far from the free kick spot
  data <- test_tibble(
    "season" = 2012,
    "home_team" = "SEA",
    "posteam" = "SEA",
    "roof" = "outdoors",
    "half_seconds_remaining" = 15,
    "yardline_100" = 99,
    "down" = 4,
    "ydstogo" = 10,
    "posteam_timeouts_remaining" = 3,
    "defteam_timeouts_remaining" = 3
  )
  model_data <- make_model_mutations(data)
  probs <- get_preds(model_data)
  expect_true(ep_exact_w(probs$Safety, probs$Opp_Safety))

  ep_with <- function(exact_w) {
    sv <- cbind(
      get_kickoff_values(model_data, exact_w = exact_w),
      ep_continue_factors(model_data)
    ) |>
      score_values(model_data$season)
    ep_from_probs(
      probs$Touchdown,
      probs$Opp_Touchdown,
      probs$Field_Goal,
      probs$Opp_Field_Goal,
      probs$Safety,
      probs$Opp_Safety,
      sv
    )
  }
  expect_equal(calculate_expected_points(data)$ep, ep_with(TRUE))
  # and that is different from the shortcut
  expect_gt(abs(ep_with(TRUE) - ep_with(FALSE)), 0.005)
})

test_that("calculate_win_probability works", {
  # This test used to run on CRAN but their changes to env vars which cause
  # check NOTES for multi-threading forced us to skip on cran.
  skip_on_cran()

  data <- test_tibble(
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
  wp <- calculate_win_probability(data) |> round_double_to_digits()
  exp <- load_expectation("wp")
  expect_equal(wp, exp)
})

test_that("calculate_win_probability doesn't change its input and keeps its type", {
  skip_on_cran()
  expect_input_unchanged(calculate_win_probability, wp_calculator_input())
})

test_that("calculate_win_probability replaces wp and keeps other columns", {
  skip_on_cran()
  data <- wp_calculator_input()
  data$home <- 5
  wp <- calculate_win_probability(data)
  wp_again <- calculate_win_probability(wp)
  expect_identical(wp_again, wp)
  expect_identical(names(wp), c(names(data), "wp", "vegas_wp"))
  expect_identical(wp$home, 5)
})
