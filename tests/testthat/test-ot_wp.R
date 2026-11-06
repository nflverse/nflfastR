test_that("overtime rule keys follow the rule changes", {
  key <- function(season, type) sub("\\|[0-9]$", "", ot_rule_key(season, type))
  expect_equal(key(2011, "REG"), "SD|900")
  expect_equal(key(2012, "REG"), "MSD|900")
  expect_equal(key(2017, "REG"), "MSD|600")
  expect_equal(key(2024, "REG"), "MSD|600")
  expect_equal(key(2025, "REG"), "BOTH|600")
  expect_equal(key(2009, "POST"), "SD|900")
  expect_equal(key(2010, "POST"), "MSD|900")
  expect_equal(key(2021, "POST"), "MSD|900")
  expect_equal(key(2022, "POST"), "BOTH|900")
})

test_that("season type is derived from the game id", {
  expect_equal(ot_season_type("2019_17_GB_CHI", 2019), "REG")
  expect_equal(ot_season_type("2019_18_GB_CHI", 2019), "POST")
  expect_equal(ot_season_type("2021_18_GB_CHI", 2021), "REG")
  expect_equal(ot_season_type("2021_19_GB_CHI", 2021), "POST")
})

test_that("overtime win probabilities are valid and sensible", {
  for (s in c(2008, 2014, 2018, 2025)) {
    for (tp in c("REG", "POST")) {
      secs <- if (tp == "REG" && s >= 2017) 600 else 900
      wp <- ot_wp1(season = s, season_type = tp, secs = secs)
      # receiving the opening kickoff is an advantage, but not a huge one
      expect_gt(wp, 0.5)
      expect_lt(wp, 0.65)
    }
  }
  # the favorite is better off
  expect_gt(ot_wp1(spread = 7), ot_wp1(spread = 0))
  expect_gt(ot_wp1(spread = 0), ot_wp1(spread = -7))
})

test_that("overtime: both teams get a ball (2025+ regular season)", {
  # opponent just scored a TD on the first possession: still a chance
  trailing_td <- ot_wp1(season = 2025, d = -7, phase = 2, secs = 450)
  expect_gt(trailing_td, 0.03)
  expect_lt(trailing_td, 0.2)
  # under the old modified sudden death rules the game would have been over, so
  # the model never asks about that state, but a FG deficit must be better than TD
  trailing_fg <- ot_wp1(season = 2025, d = -3, phase = 2, secs = 450)
  expect_gt(trailing_fg, trailing_td)
  # the team scoring the TD on the first possession is a big favorite
  pat <- ot_wp_states(
    ot_state(season = 2025, kind = "pat", d = 6, phase = 1, secs = 450),
    use_spread = TRUE
  )
  expect_gt(pat, 0.85)
  expect_lt(pat, 1)
})

test_that("overtime: ties are worth a half win", {
  # no time left and tied
  expect_equal(
    ot_wp1(
      season = 2025,
      kind = "scrimmage",
      d = 0,
      phase = 3,
      secs = 0,
      y = 60,
      down = 1,
      ytg = 10
    ),
    0.5,
    tolerance = 0.02
  )
  # two point conversion to win or lose in the BOTH era is worth the make prob
  twopt <- ot_wp_states(
    ot_state(season = 2025, kind = "twopt", d = -1, phase = 2, secs = 20),
    use_spread = FALSE
  )
  expect_equal(twopt, 0.4735, tolerance = 1e-3)
})

test_that("overtime: a short clock takes away the advantage if out of range", {
  # 1st & 10 at the offense's own 30. With 10 seconds left it is very unlikely
  # to score, so the game is close to the 0.5 of a tie. With lots of time it
  # is a clear advantage.
  long <- ot_wp1(
    season = 2025,
    kind = "scrimmage",
    phase = 3,
    secs = 500,
    y = 70,
    down = 1
  )
  short <- ot_wp1(
    season = 2025,
    kind = "scrimmage",
    phase = 3,
    secs = 10,
    y = 70,
    down = 1
  )
  expect_lt(abs(short - 0.5), abs(long - 0.5))
  expect_lt(short, 0.6)
})

test_that("overtime: a short clock does not matter if in field goal range", {
  # 1st & 10 at the opponent's 20 with 20 seconds left: a field goal wins the
  # game, so this is far from the 0.5 of a tie
  in_range <- ot_wp1(
    season = 2025,
    kind = "scrimmage",
    phase = 3,
    secs = 20,
    y = 20,
    down = 1
  )
  expect_gt(in_range, 0.75)
})

test_that("overtime: situation matters", {
  goal <- ot_wp1(kind = "scrimmage", y = 2, ytg = 2, secs = 500)
  deep <- ot_wp1(kind = "scrimmage", y = 90, down = 3, ytg = 20, secs = 500)
  expect_gt(goal, 0.8)
  expect_lt(deep, 0.5)
})

test_that("overtime: teams that have to score do not punt", {
  # trailing by 3 on the second possession, 4th & 6 at own 24 is not hopeless
  go <- ot_wp1(
    season = 2025,
    kind = "scrimmage",
    d = -3,
    phase = 2,
    secs = 230,
    y = 76,
    down = 4,
    ytg = 6
  )
  expect_gt(go, 0.08)
  expect_lt(go, 0.3)
})

test_that("overtime: end of clock field goal attempts", {
  st <- ot_state(
    season = 2022,
    kind = "scrimmage",
    d = 0,
    phase = 3,
    secs = 5,
    y = 40,
    down = 4,
    ytg = 7
  )
  st$p_fg <- 0.45
  # a make wins, a miss is a tie
  expect_equal(
    ot_wp_states(st, use_spread = FALSE),
    0.5 + 0.5 * 0.45,
    tolerance = 0.02
  )
})

test_that("overtime: tempo curve never makes finishing a possession harder with more time", {
  secs <- seq(1, 900, by = 1)
  ratio <- ot_tempo(secs) / secs
  expect_all_true(diff(ratio) <= 1e-12)
  # no hurry with lots of time, never faster than about 6.5 seconds per play
  expect_equal(ot_tempo(600), 1)
  expect_gte(min(ot_tempo(secs)), 0.17)
})

test_that("overtime: the team with the ball has a better shot with 60 than 30 seconds", {
  # out of field goal range, tied. This did not hold with an overly aggressive
  # tempo curve (30 seconds looked better than 60)
  wp <- function(secs) {
    ot_wp1(
      season = 2025,
      kind = "scrimmage",
      phase = 3,
      secs = secs,
      y = 70,
      down = 1
    )
  }
  expect_gt(wp(60), wp(30))
  expect_gt(wp(30), wp(10))
})

test_that("overtime: postseason periods don't end the game", {
  # trailing by 3 on the second possession with 20 seconds left in the period.
  # In the regular season the game is nearly over. In the postseason the next
  # period starts and the drive goes on.
  wp <- function(type, secs) {
    ot_wp1(
      season = 2025,
      season_type = type,
      kind = "scrimmage",
      d = -3,
      phase = 2,
      secs = secs,
      y = 70
    )
  }
  expect_lt(wp("REG", 20), 0.15)
  expect_equal(wp("POST", 20), wp("POST", 900))
  expect_gt(wp("POST", 20), 0.3)
})

test_that("overtime: a 4th down field goal that wins is always an option", {
  # tied in sudden death, 4th & 3 at the 20: kicking wins the game 90% of the
  # time, so this can't be worth much less than that no matter how much time
  # is left
  st <- ot_state(
    season = 2025,
    kind = "scrimmage",
    phase = 3,
    secs = 400,
    y = 20,
    down = 4,
    ytg = 3
  )
  st$p_fg <- 0.9
  expect_gt(ot_wp_states(st, use_spread = FALSE), 0.9)
  # a field goal that is very unlikely to be good is not forced
  st$yardline_100 <- 60
  st$p_fg <- 0.02
  expect_lt(ot_wp_states(st, use_spread = FALSE), 0.6)
})
