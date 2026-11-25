test_that("the two point make probability depends on the spot", {
  expect_equal(
    two_point_prob(c(1, 2, 3, 5, 6, 15, NA)),
    c(0.58, 0.49, 0.49, 0.49, 0.20, 0.20, 0.49)
  )
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
    qtr = c(4, 5, 5, 5, 5, 5),
    drive = c(10, 11, 11, 13, 12, 12),
    score_differential = c(0, 0, 0, 0, -7, -1)
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
  # second possession, down 7: the touchdown is a two point try that ends the
  # game, and the try itself is measured against the two point value
  expect_equal(
    unlist(
      factors[5, c("c_td_own", "k_td_own", "two_td_own")],
      use.names = FALSE
    ),
    c(0, 1, 1)
  )
  expect_identical(factors$two_try, c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE))
  # without what is needed to tell possessions apart it is 1
  no_context <- ep_continue_factors(pbp[, "qtr", drop = FALSE])
  expect_equal(unlist(no_context[, 1:8], use.names = FALSE), rep(1, 48))
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
  expect_equal(score_values(two, 2020)$td_own, 6 + 2 * two_point_prob_from_2)
  # extra points were more reliable before 2015 and are a bit more since 2023
  expect_equal(score_values(kv, 2010)$td_own, 6 + 0.99 - 1.1)
  expect_equal(score_values(kv, 2024)$td_own, 6 + 0.96 - 1.1)
  # the other team's view is the same values with the roles swapped
  expect_equal(swap_score_values(swap_score_values(sv)), sv)
  expect_equal(swap_score_values(sv)$td_own, -sv$td_opp)
})

test_that("kickoff values come from the clock, season and teams", {
  skip_on_cran()
  data <- test_tibble(
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
  ko$yardline_100 <- wp_kickoff_start(ko$season)
  ko$down1 <- 1
  ko$down2 <- 0
  ko$down3 <- 0
  ko$down4 <- 0
  ko$ydstogo <- 10
  expect_equal(get_kickoff_values(data, kickoff_preds = get_preds(ko)), kv)
})

test_that("expected points at the kickoff state match the kickoff value", {
  skip_on_cran()
  # at a neutral site both teams are the same, so the kickoff possession is
  # worth exactly what the EP formula gives at the touchback spot
  data <- test_tibble(
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
  ko$yardline_100 <- wp_kickoff_start(ko$season)
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

test_that("kickoff values solve the equations of the kickoff and free kick states", {
  skip_on_cran()
  data <- test_tibble(
    "season" = c(2012, 2025, 2025, 2025, 2019),
    "home_team" = "SEA",
    "posteam" = c("SEA", "SEA", "DEN", "SEA", "SEA"),
    "roof" = "outdoors",
    "half_seconds_remaining" = c(900, 900, 60, 1, 10),
    "yardline_100" = 75,
    "down" = 1,
    "ydstogo" = 10,
    "posteam_timeouts_remaining" = 3,
    "defteam_timeouts_remaining" = 3
  ) |>
    make_model_mutations()
  shortcut <- get_kickoff_values(data)
  exact <- get_kickoff_values(data, exact_w = c(TRUE, TRUE, TRUE, TRUE, FALSE))

  # V = a + b V + d W on the average of the two teams' states
  coefs <- function(yardline_100) {
    state <- function(flip) {
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
    p <- (state(FALSE) + state(TRUE)) / 2
    td <- p$Touchdown - p$Opp_Touchdown
    fg <- p$Field_Goal - p$Opp_Field_Goal
    sf <- p$Safety - p$Opp_Safety
    list(
      a = td * (6 + conversion_value(data$season)) + 3 * fg + 2 * sf,
      b = -(td + fg),
      d = sf
    )
  }
  k <- coefs(wp_kickoff_start(data$season))
  f <- coefs(safety_free_kick_yardline)

  # the shortcut solves the equation of the kickoff state, the exact values
  # also that of the free kick state
  expect_equal(shortcut$v, k$a + k$b * shortcut$v + k$d * shortcut$w)
  expect_equal(exact$v, k$a + k$b * exact$v + k$d * exact$w)
  i <- 1:4
  expect_equal(exact$w[i], (f$a + f$b * exact$v + f$d * exact$w)[i])
  # rows that are not flagged use the shortcut
  expect_equal(exact[5, ], shortcut[5, ])
  # the exact W differs from the shortcut in a season where the kickoff start
  # is far from the free kick spot (2011 to 2015), much less with the recent
  # touchback. V barely moves
  early_gap <- abs(exact$w[1] - shortcut$w[1])
  expect_gt(early_gap, 0.05)
  expect_lt(abs(exact$w[2] - shortcut$w[2]), early_gap)
  expect_lt(abs(exact$v[1] - shortcut$v[1]), 0.02)
  # with a second left the free kick is worth little, not the kickoff value
  # plus the extra field position of the shortcut
  expect_lt(exact$w[4] - exact$v[4], 0.5 * (shortcut$w[4] - shortcut$v[4]))
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
})

test_that("the exact free kick value is selected from the probabilities only", {
  expect_equal(
    ep_exact_w(c(0.001, 0.02, 0.006, NA), c(0.002, 0.001, 0.006, 0.5)),
    c(FALSE, TRUE, TRUE, FALSE)
  )
})

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

test_that("non-plays with a stale posteam don't flip home_wp (#606)", {
  # This needs a download and gsisdecoder so it can't run on CRAN
  skip_on_cran()
  skip_if_not_installed("gsisdecoder")
  skip_if_offline("github.com")

  # play 1017 is the note "End of quarter - 1.35 pm". It sits between a BUF
  # extra point and the kickoff to CLE and still carried BUF as posteam while
  # its win probability came from the next play, which belongs to CLE.
  pbp <- build_nflfastR_pbp("2016_15_CLE_BUF")
  row <- function(id) pbp[pbp$play_id == id, ]
  note <- row(1017)
  nxt <- row(1035)

  expect_equal(note$home_wp, nxt$home_wp)
  expect_equal(note$vegas_home_wp, nxt$vegas_home_wp)
  # the extra point before the note used to get a WPA of about -0.5
  expect_lt(abs(row(989)$wpa), 0.05)
  expect_lt(abs(note$wpa), 1e-8)
})
