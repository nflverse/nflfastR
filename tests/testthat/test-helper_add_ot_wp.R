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
  # and so is a team near the goal line
  expect_gt(ot_wp1(kind = "scrimmage", y = 2, ytg = 2, secs = 500), 0.8)
  expect_lt(
    ot_wp1(kind = "scrimmage", y = 90, down = 3, ytg = 20, secs = 500),
    0.5
  )
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
  # from its spot
  twopt <- ot_wp_states(
    ot_state(
      season = 2025,
      kind = "twopt",
      d = -1,
      phase = 2,
      secs = 20,
      y = c(2, 1, 7)
    ),
    use_spread = FALSE
  )
  expect_equal(twopt, two_point_prob(c(2, 1, 7)), tolerance = 1e-3)
})

test_that("overtime: the clock matters out of field goal range only", {
  # 1st & 10, tied in sudden death. At the own 30 with seconds left a score is
  # very unlikely, so the game is close to the 0.5 of a tie, and more time is
  # always better. This did not hold with an overly aggressive tempo curve (30
  # seconds looked better than 60).
  wp <- function(secs, y) {
    ot_wp1(
      season = 2025,
      kind = "scrimmage",
      phase = 3,
      secs = secs,
      y = y,
      down = 1
    )
  }
  expect_gt(wp(500, 70), wp(60, 70))
  expect_gt(wp(60, 70), wp(30, 70))
  expect_gt(wp(30, 70), wp(10, 70))
  expect_lt(wp(10, 70), 0.6)
  # at the opponent's 20 a field goal wins the game, even with 20 seconds left
  expect_gt(wp(20, 20), 0.75)
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

test_that("calculate_win_probability() values overtime rows with the overtime model", {
  data <- test_tibble(
    receive_2h_ko = 0,
    home_team = "SEA",
    posteam = "SEA",
    score_differential = c(0, 0, -3, 0),
    half_seconds_remaining = 450,
    game_seconds_remaining = 450,
    spread_line = c(0, 7, 0, 0),
    down = 1,
    ydstogo = 10,
    yardline_100 = 70,
    posteam_timeouts_remaining = 2,
    defteam_timeouts_remaining = 2,
    qtr = c(5, 5, 5, 4),
    season = 2025,
    season_type = "REG",
    ot_possession = c(1, 1, 2, NA)
  )
  wp <- calculate_win_probability(data)
  ot <- ot_wp_states(
    ot_state(
      season = 2025,
      kind = "scrimmage",
      d = c(0, 0, -3),
      phase = c(1, 1, 2),
      secs = 450,
      spread = c(0, 7, 0),
      y = 70
    ),
    use_spread = TRUE
  )
  expect_equal(wp$vegas_wp[1:3], ot)
  # wp ignores the spread
  expect_equal(wp$wp[1], wp$wp[2])
  # regulation rows are unchanged
  expect_equal(
    wp$vegas_wp[4],
    calculate_win_probability(data[4, names(data) != "qtr"])$vegas_wp
  )
  # without a possession number overtime rows can't be valued
  expect_warning(
    no_poss <- calculate_win_probability(data[names(data) != "ot_possession"]),
    "possession number"
  )
  expect_equal(is.na(no_poss$vegas_wp), c(TRUE, TRUE, TRUE, FALSE))
})

test_that("calculate_win_probability() derives the overtime possession from the drives", {
  data <- test_tibble(
    game_id = "2025_05_KC_SEA",
    season = 2025,
    season_type = "REG",
    qtr = 5,
    drive = c(20, 20, 21),
    receive_2h_ko = 0,
    home_team = "SEA",
    posteam = c("SEA", "SEA", "KC"),
    score_differential = c(0, 0, -3),
    half_seconds_remaining = c(600, 560, 420),
    game_seconds_remaining = c(600, 560, 420),
    spread_line = 2.5,
    down = c(1, 2, 1),
    ydstogo = 10,
    yardline_100 = c(70, 64, 75),
    posteam_timeouts_remaining = 2,
    defteam_timeouts_remaining = 2
  )
  derived <- calculate_win_probability(data)
  data$ot_possession <- c(1, 1, 2)
  given <- calculate_win_probability(data)
  expect_equal(derived$vegas_wp, given$vegas_wp)
  expect_equal(derived$wp, given$wp)
  expect_false(anyNA(derived$vegas_wp))
})

test_that("a timeout doesn't start a new overtime possession", {
  # one drive with timeouts (no team) in the middle, then the other team
  x <- data.frame(
    game_id = "2021_04_TEN_NYJ",
    qtr = 5,
    fixed_drive = c(25, 25, 25, 25, 25, 26, 26),
    posteam = c("NYJ", "NYJ", NA, "NYJ", NA, "TEN", "TEN"),
    kickoff_attempt = c(1, 0, 0, 0, 0, 1, 0)
  )
  expect_equal(ot_possession_number(x)[c(2, 4, 7)], c(1, 1, 2))
})
