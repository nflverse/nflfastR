test_that("extra point probability follows the seasons", {
  expect_equal(
    xp_make_prob(c(2009, 2014, 2015, 2022, 2023, 2026)),
    c(0.99, 0.99, 0.94, 0.94, 0.96, 0.96)
  )
  expect_equal(xp_make_prob(NA_real_), NA_real_)
})

test_that("kickoff touchback spot follows the kickoff rules", {
  seasons <- c(2009, 2015, 2016, 2023, 2024, 2025, 2026)
  expect_equal(
    kickoff_touchback_yardline(seasons),
    c(80, 80, 75, 75, 70, 65, 65)
  )
  expect_equal(kickoff_touchback_yardline(NA_real_), NA_real_)
})

test_that("overtime rules are inferred from season and game id", {
  # regular season weeks 1-17 (18 since 2021), postseason after that
  expect_equal(ep_ot_rule("2011_05_NO_TB", 2011), "SD")
  expect_equal(ep_ot_rule("2012_05_NO_TB", 2012), "MSD")
  expect_equal(ep_ot_rule("2024_05_NO_TB", 2024), "MSD")
  expect_equal(ep_ot_rule("2025_05_NO_TB", 2025), "BOTH")
  expect_equal(ep_ot_rule("2009_19_NO_TB", 2009), "SD")
  expect_equal(ep_ot_rule("2010_18_NO_TB", 2010), "MSD")
  expect_equal(ep_ot_rule("2021_20_NO_TB", 2021), "MSD")
  expect_equal(ep_ot_rule("2022_20_NO_TB", 2022), "BOTH")
})

test_that("a kickoff follows an overtime score only if the game goes on", {
  # sudden death: never
  expect_equal(ep_ot_continues("SD", 1, 0, c(7, 3, 2)), c(0, 0, 0))
  # modified sudden death: only a first possession field goal gives the other
  # team a possession, and a field goal that ties on the second possession
  expect_equal(ep_ot_continues("MSD", 1, 0, c(7, 3, 2)), c(0, 1, 0))
  expect_equal(ep_ot_continues("MSD", 2, -3, c(7, 3)), c(0, 1))
  expect_equal(ep_ot_continues("MSD", 2, 0, c(7, 3)), c(0, 0))
  expect_equal(ep_ot_continues("MSD", 3, 0, 3), 0)
  # both teams get a possession
  expect_equal(ep_ot_continues("BOTH", 1, 0, c(7, 3, 2)), c(1, 1, 0))
  expect_equal(ep_ot_continues("BOTH", 2, -7, c(7, 3)), c(1, 1))
  expect_equal(ep_ot_continues("BOTH", 2, -3, c(7, 3)), c(0, 1))
  # unknown possession or score differential: assume the game goes on
  expect_equal(ep_ot_continues("BOTH", NA, 0, 7), 1)
})

test_that("overtime continue factors only change overtime rows", {
  pbp <- data.frame(
    game_id = "2025_05_NO_TB",
    season = 2025,
    qtr = c(4, 5, 5, 5),
    drive = c(10, 11, 11, 13),
    score_differential = 0
  )
  factors <- ep_continue_factors(pbp)
  # regulation is always 1
  expect_equal(unlist(factors[1, ], use.names = FALSE), rep(1, 6))
  # first overtime possession of a "both teams" game: scores go on, a safety
  # ends it
  expect_equal(factors$c_td_own[2], 1)
  expect_equal(factors$c_saf_own[2], 0)
  # third possession: sudden death
  expect_equal(unlist(factors[4, 1:3], use.names = FALSE), rep(0, 3))
  # without what is needed to tell possessions apart it is 1
  expect_equal(
    unlist(ep_continue_factors(pbp[, "qtr", drop = FALSE]), use.names = FALSE),
    rep(1, 24)
  )
})

test_that("score values are net of the other team's possession", {
  kv <- data.frame(
    v_own = 1.2,
    v_opp = 1.0,
    w_own = 1.5,
    w_opp = 1.4,
    c_td_own = 1,
    c_fg_own = 1,
    c_saf_own = 1,
    c_td_opp = 1,
    c_fg_opp = 1,
    c_saf_opp = 1
  )
  sv <- score_values(kv, 2020)
  expect_equal(sv$td_own, 6 + 0.94 - 1.0)
  expect_equal(sv$fg_own, 3 - 1.0)
  expect_equal(sv$saf_own, 2 + 1.5)
  expect_equal(sv$td_opp, -(6 + 0.94 - 1.2))
  expect_equal(sv$fg_opp, -(3 - 1.2))
  expect_equal(sv$saf_opp, -(2 + 1.4))
  # a score that ends the game is worth its points: no extra point, no kickoff
  ends <- kv
  ends$c_td_own <- 0
  expect_equal(score_values(ends, 2020)$td_own, 6)
  # extra points were more reliable before 2015 and are a bit more since 2023
  expect_equal(score_values(kv, 2010)$td_own, 6 + 0.99 - 1.0)
  expect_equal(score_values(kv, 2024)$td_own, 6 + 0.96 - 1.0)
  # the other team's view is the same values with the roles swapped
  expect_equal(swap_score_values(swap_score_values(sv)), sv)
  expect_equal(swap_score_values(sv)$td_own, -sv$td_opp)
})

test_that("kickoff values come from the clock, season and teams", {
  skip_on_cran()
  data <- tibble::tibble(
    "season" = c(2015, 2018, 2025, 2025, 2025, 2025),
    "home_team" = "SEA",
    "posteam" = c("SEA", "SEA", "SEA", "SEA", "SEA", "DEN"),
    "roof" = "outdoors",
    "half_seconds_remaining" = c(900, 900, 900, 0, 20, 900),
    "yardline_100" = 75,
    "down" = 1,
    "ydstogo" = 10,
    "posteam_timeouts_remaining" = 3,
    "defteam_timeouts_remaining" = 3
  ) |>
    make_model_mutations()
  kv <- get_kickoff_values(data)
  expect_named(kv, c("v_own", "v_opp", "w_own", "w_opp"))

  # no kickoff after the half ends
  expect_equal(unlist(kv[4, ], use.names = FALSE), rep(0, 4))
  # much less value with 20 seconds left than with 15 minutes left
  expect_lt(kv$v_own[5], 0.5 * kv$v_own[3])
  expect_gt(kv$v_own[3], 0)
  # a touchback further from the goal line is worth more: own 20 (2015) <
  # own 25 (2018) < own 35 (2025)
  expect_lt(kv$v_own[1], kv$v_own[2])
  expect_lt(kv$v_own[2], kv$v_own[3])
  # the free kick spot is better than the touchback
  expect_gt(kv$w_own[3], kv$v_own[3])
  # the road team receiving is worth less than the home team receiving
  expect_lt(kv$v_own[6], kv$v_own[3])

  # reusing the predictions for the possession team's kickoff state gives the
  # same values
  ko <- data
  ko$yardline_100 <- kickoff_touchback_yardline(ko$season)
  ko$down1 <- 1
  ko$down2 <- 0
  ko$down3 <- 0
  ko$down4 <- 0
  ko$ydstogo <- 10
  expect_equal(get_kickoff_values(data, kickoff_preds = get_preds(ko)), kv)
})

test_that("expected points are net of the next possession", {
  skip_on_cran()
  data <- tibble::tibble(
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
