test_that("the conversion value follows the seasons", {
  expect_equal(
    conversion_value(c(2009, 2014, 2015, 2022, 2023, 2026)),
    c(0.99, 0.99, 0.94, 0.94, 0.96, 0.96)
  )
  expect_equal(conversion_value(NA_real_), NA_real_)
})

test_that("the kickoff start spot follows the kickoff rules", {
  seasons <- c(2009, 2015, 2016, 2023, 2024, 2025, 2026)
  expect_equal(
    kickoff_start_yardline(seasons),
    c(80, 80, 75, 75, 70, 69, 69)
  )
  expect_equal(kickoff_start_yardline(NA_real_), NA_real_)
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
  expect_equal(ep_ot_continues("BOTH", 2, -7, 7), 1)
  expect_equal(ep_ot_continues("BOTH", 2, -3, c(7, 3)), c(0, 1))
  # a second possession score that still trails ends the game, because both
  # teams have had their possession (a field goal down seven)
  expect_equal(ep_ot_continues("BOTH", 2, -7, 3), 0)
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
  # regulation is always 1 (a conversion follows a touchdown, which is not a
  # forced two point try)
  expect_equal(unlist(factors[1, 1:8], use.names = FALSE), rep(1, 8))
  expect_equal(unlist(factors[1, 9:11], use.names = FALSE), rep(FALSE, 3))
  # first overtime possession of a "both teams" game: scores go on, a safety
  # ends it
  expect_equal(factors$c_td_own[2], 1)
  expect_equal(factors$c_saf_own[2], 0)
  # third possession: sudden death
  expect_equal(unlist(factors[4, 1:3], use.names = FALSE), rep(0, 3))
  # without what is needed to tell possessions apart it is 1
  no_context <- ep_continue_factors(pbp[, "qtr", drop = FALSE])
  expect_equal(unlist(no_context[, 1:8], use.names = FALSE), rep(1, 32))
  expect_false(any(unlist(no_context[, 9:11])))
})

test_that("score values are net of the other team's possession", {
  kv <- data.frame(
    v = 1.1,
    w = 1.5,
    c_td_own = 1,
    c_fg_own = 1,
    c_saf_own = 1,
    c_td_opp = 1,
    c_fg_opp = 1,
    c_saf_opp = 1,
    k_td_own = 1,
    k_td_opp = 1,
    two_td_own = FALSE,
    two_td_opp = FALSE
  )
  sv <- score_values(kv, 2020)
  expect_equal(sv$td_own, 6 + 0.94 - 1.1)
  expect_equal(sv$fg_own, 3 - 1.1)
  expect_equal(sv$saf_own, 2 + 1.5)
  expect_equal(sv$td_opp, -(6 + 0.94 - 1.1))
  expect_equal(sv$fg_opp, -(3 - 1.1))
  expect_equal(sv$saf_opp, -(2 + 1.5))
  # a score that ends the game is worth its points: no extra point, no kickoff
  ends <- kv
  ends$c_td_own <- 0
  ends$k_td_own <- 0
  expect_equal(score_values(ends, 2020)$td_own, 6)
  # a touchdown that is followed by a two point try that is certain to be
  # attempted, and that ends the game either way
  two <- kv
  two$c_td_own <- 0
  two$two_td_own <- TRUE
  expect_equal(score_values(two, 2020)$td_own, 6 + 2 * two_point_make_prob)
  # extra points were more reliable before 2015 and are a bit more since 2023
  expect_equal(score_values(kv, 2010)$td_own, 6 + 0.99 - 1.1)
  expect_equal(score_values(kv, 2024)$td_own, 6 + 0.96 - 1.1)
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
  expect_named(kv, c("v", "w"))

  # no kickoff after the half ends
  expect_equal(unlist(kv[4, ], use.names = FALSE), rep(0, 2))
  # much less value with 20 seconds left than with 15 minutes left
  expect_lt(kv$v[5], 0.5 * kv$v[3])
  expect_gt(kv$v[3], 0)
  # a start further from the goal line is worth more: own 20 (2015) <
  # own 25 (2018) < own 31 (2025)
  expect_lt(kv$v[1], kv$v[2])
  expect_lt(kv$v[2], kv$v[3])
  # the free kick spot is better than the touchback
  expect_gt(kv$w[3], kv$v[3])
  # it is the same value whichever team has the ball (rows 3 and 6 are the
  # same game state from each team's view)
  expect_equal(kv$v[6], kv$v[3])

  # reusing the predictions for the possession team's kickoff state gives the
  # same values
  ko <- data
  ko$yardline_100 <- kickoff_start_yardline(ko$season)
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

test_that("kickoff values solve their own equations", {
  skip_on_cran()
  data <- tibble::tibble(
    "season" = c(2015, 2020, 2025, 2025),
    "home_team" = "SEA",
    "posteam" = c("SEA", "DEN", "SEA", "DEN"),
    "roof" = "outdoors",
    "half_seconds_remaining" = c(1500, 900, 1500, 600),
    "yardline_100" = 75,
    "down" = 1,
    "ydstogo" = 10,
    "posteam_timeouts_remaining" = 3,
    "defteam_timeouts_remaining" = 3
  ) |>
    make_model_mutations()
  kv <- get_kickoff_values(data)

  # the free kick is worth the kickoff plus the extra field position (when
  # more than 5 minutes are left in the half)
  v <- kv$v
  yards <- kickoff_start_yardline(data$season) - safety_free_kick_yardline
  expect_equal(kv$w, v + safety_free_kick_ep_per_yard * yards)

  # V = a + b V + d W on the average of the two teams' kickoff states
  ko <- data
  ko$yardline_100 <- kickoff_start_yardline(ko$season)
  ko$down1 <- 1
  ko$down2 <- 0
  ko$down3 <- 0
  ko$down4 <- 0
  ko_opp <- ko
  ko_opp$home <- 1 - ko$home
  p <- (get_preds(ko) + get_preds(ko_opp)) / 2
  td <- p$Touchdown - p$Opp_Touchdown
  fg <- p$Field_Goal - p$Opp_Field_Goal
  sf <- p$Safety - p$Opp_Safety
  xp <- conversion_value(data$season)
  expect_equal(
    v,
    td * (6 + xp) + 3 * fg + 2 * sf - (td + fg) * v + sf * kv$w
  )
})

test_that("expected points at the kickoff state match the kickoff value", {
  skip_on_cran()
  # at a neutral site both teams are the same, so the kickoff possession is
  # worth exactly what the EP formula gives at the touchback spot
  data <- tibble::tibble(
    "season" = c(2015, 2020, 2025),
    "home_team" = "SEA",
    "posteam" = "SEA",
    "location" = "Neutral",
    "roof" = "outdoors",
    "half_seconds_remaining" = c(1500, 900, 600),
    "yardline_100" = 75,
    "down" = 1,
    "ydstogo" = 10,
    "posteam_timeouts_remaining" = 3,
    "defteam_timeouts_remaining" = 3
  ) |>
    make_model_mutations()
  kv <- get_kickoff_values(data)

  ko <- data
  ko$yardline_100 <- kickoff_start_yardline(ko$season)
  ko$down1 <- 1
  ko$down2 <- 0
  ko$down3 <- 0
  ko$down4 <- 0
  p <- get_preds(ko)
  sv <- score_values(
    cbind(kv, ep_continue_factors(data)),
    data$season
  )
  ep <- ep_from_probs(
    p$Touchdown,
    p$Opp_Touchdown,
    p$Field_Goal,
    p$Opp_Field_Goal,
    p$Safety,
    p$Opp_Safety,
    sv
  )
  expect_equal(ep, kv$v)
})

test_that("calculate_expected_points treats neutral sites as away games", {
  skip_on_cran()
  data <- tibble::tibble(
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

test_that("the exact free kick value solves the equations of both states", {
  skip_on_cran()
  data <- tibble::tibble(
    "season" = c(2025, 2025, 2025, 2019),
    "home_team" = "SEA",
    "posteam" = c("SEA", "DEN", "SEA", "SEA"),
    "roof" = "outdoors",
    "half_seconds_remaining" = c(300, 60, 1, 10),
    "yardline_100" = 75,
    "down" = 1,
    "ydstogo" = 10,
    "posteam_timeouts_remaining" = 3,
    "defteam_timeouts_remaining" = 3
  ) |>
    make_model_mutations()
  kv <- get_kickoff_values(data, exact_w = TRUE)

  state <- function(yardline_100, flip) {
    d <- data
    d$yardline_100 <- yardline_100
    d$down1 <- 1
    d$down2 <- 0
    d$down3 <- 0
    d$down4 <- 0
    if (flip) {
      d$home <- 1 - d$home
    }
    get_preds(d)
  }
  coefs <- function(yardline_100) {
    p <- (state(yardline_100, FALSE) + state(yardline_100, TRUE)) / 2
    td <- p$Touchdown - p$Opp_Touchdown
    fg <- p$Field_Goal - p$Opp_Field_Goal
    sf <- p$Safety - p$Opp_Safety
    list(
      a = td * (6 + conversion_value(data$season)) + 3 * fg + 2 * sf,
      b = -(td + fg),
      d = sf
    )
  }
  k <- coefs(kickoff_start_yardline(data$season))
  f <- coefs(safety_free_kick_yardline)

  # V and W solve the equations of the kickoff state and the free kick state
  expect_equal(kv$v, k$a + k$b * kv$v + k$d * kv$w)
  expect_equal(kv$w, f$a + f$b * kv$v + f$d * kv$w)
  # with a second left the free kick is worth little, not the kickoff value
  # plus the extra field position of the shortcut
  expect_lt(kv$w[3] - kv$v[3], 0.5 * safety_free_kick_ep_per_yard * 4)
})

test_that("a second possession touchdown down 7 in overtime always goes for two", {
  # both teams get a possession: the touchdown leaves the team down 1, so it
  # goes for two, and the game ends after the try either way
  td <- ep_ot_td("BOTH", 2, -7)
  expect_equal(td$cont, 0)
  expect_equal(td$conv, 1)
  expect_true(td$two)
  # a touchdown on the first possession is followed by a try and the game goes on
  td <- ep_ot_td("BOTH", 1, 0)
  expect_equal(c(td$cont, td$conv), c(1, 1))
  expect_false(td$two)
  # a touchdown that wins the game on the second possession has no try
  td <- ep_ot_td("BOTH", 2, -3)
  expect_equal(c(td$cont, td$conv), c(0, 0))
  # sudden death and modified sudden death touchdowns end the game
  expect_equal(ep_ot_td("SD", 1, 0)$conv, 0)
  expect_equal(ep_ot_td("MSD", 1, 0)$conv, 0)

  pbp <- data.frame(
    game_id = "2025_05_NO_TB",
    season = 2025,
    qtr = 5,
    drive = c(11, 12, 12),
    score_differential = c(0, -7, -1)
  )
  factors <- ep_continue_factors(pbp)
  # second possession, down 7: the touchdown is a two point try that ends the game
  expect_equal(
    unlist(
      factors[2, c("c_td_own", "k_td_own", "two_td_own")],
      use.names = FALSE
    ),
    c(0, 1, 1)
  )
  # and the try itself is measured against the two point value
  expect_true(factors$two_try[3])
  expect_false(factors$two_try[1])
})

test_that("the free kick value comes from the model where a safety matters", {
  skip_on_cran()
  data <- tibble::tibble(
    "season" = c(2010, 2010, 2025),
    "home_team" = "SEA",
    "posteam" = "SEA",
    "roof" = "outdoors",
    "half_seconds_remaining" = c(900, 900, 900),
    "yardline_100" = 75,
    "down" = 1,
    "ydstogo" = 10,
    "posteam_timeouts_remaining" = 3,
    "defteam_timeouts_remaining" = 3
  ) |>
    make_model_mutations()
  shortcut <- get_kickoff_values(data)
  exact <- get_kickoff_values(data, exact_w = c(FALSE, TRUE, TRUE))
  # rows that are not flagged use the shortcut
  expect_equal(exact$w[1], shortcut$w[1])
  # the exact value differs from the shortcut in an early season, where the
  # shortcut is further off, and is much closer to it with the recent touchback
  early_gap <- abs(exact$w[2] - shortcut$w[2])
  recent_gap <- abs(exact$w[3] - shortcut$w[3])
  expect_gt(early_gap, 0.05)
  expect_lt(recent_gap, early_gap)
  # V barely moves
  expect_lt(abs(exact$v[2] - shortcut$v[2]), 0.02)
})
