test_that("unnest_play_stats returns one row per stat", {
  plays <- data.frame(playId = c(1, 2))
  plays$playStats <- list(
    data.frame(
      statId = 10L,
      yards = "5",
      team.abbreviation = "KC",
      playerName = "P.Mahomes",
      gsisPlayer.id = "00-0033873"
    ),
    NULL
  )
  expect_identical(
    unnest_play_stats(plays),
    data.table::data.table(
      playId = 1,
      statId = 10,
      yards = 5L,
      teamAbbr = "KC",
      player.displayName = "P.Mahomes",
      player.esbId = "00-0033873"
    )
  )
})

test_that("normalize_raw_columns converts types, teams, blanks and names by reference", {
  dt <- data.table::data.table(
    `Play Type` = c(TRUE, NA),
    yards = c(1L, 2L),
    yardline = factor(c("SD 20", "KC 30")),
    desc = c(" Pass\r\nshort  ", "")
  )
  normalize_raw_columns(dt)
  expect_identical(
    dt,
    data.table::data.table(
      play_type = c(1, NA),
      yards = c(1, 2),
      yardline = c("LAC 20", "KC 30"),
      desc = c("Pass short", "")
    )
  )
})

test_that("empty_strings_to_na only changes empty strings by reference", {
  dt <- data.table::data.table(x = c("", "a", NA), y = c(1, 2, 3))
  empty_strings_to_na(dt)
  expect_identical(
    dt,
    data.table::data.table(x = c(NA, "a", NA), y = c(1, 2, 3))
  )
})

test_that("fix_posteams takes posteam from pre_play_by_play", {
  pbp <- data.table::data.table(
    posteam = c("KC", "KC", "KC", "NYJ"),
    pre_play_by_play = c(
      "NYJ  1-10  KC 40",
      NA,
      "KC  1-10  NYJ 40",
      "KC  1-10"
    ),
    play_description = c("pass", "pass", "Timeout #1 by KC", "GAME")
  )
  fix_posteams(pbp)
  expect_identical(pbp$posteam, c("NYJ", "KC", NA, NA))
})

test_that("fix_bad_games assigns teams by possession", {
  pbp <- data.table::data.table(
    posteam = c("KC", "KC", "NYJ"),
    home_team = "KC",
    away_team = "NYJ",
    drive_how_ended_description = c("Touchdown", "Punt", "Fumble"),
    td_team = c("NYJ", "KC", NA),
    return_team = c(NA, "KC", NA),
    fumble_recovery_1_team = c(NA, NA, "NYJ"),
    fumble_lost = c(0, 0, 1),
    timeout_team = c(NA, NA, "KC"),
    penalty_team = c("KC", NA, "KC"),
    safety_team = c(NA, "NYJ", NA),
    play_description = c(
      "pass PENALTY on NYJ-12 holding",
      "punt",
      "Timeout #2 by NYJ at 1:00"
    )
  )
  fix_bad_games(pbp)
  expect_identical(pbp$td_team, c("KC", "NYJ", NA))
  expect_identical(pbp$return_team, c(NA, "NYJ", NA))
  expect_identical(pbp$fumble_recovery_1_team, c(NA, NA, "KC"))
  expect_identical(pbp$timeout_team, c(NA, NA, "NYJ"))
  # the penalized team from the play text (kept without one)
  expect_identical(pbp$penalty_team, c("NYJ", NA, "KC"))
  # the safety goes to the team without the ball
  expect_identical(pbp$safety_team, c(NA, "NYJ", NA))
})

test_that("clean_names gives snake case names like janitor", {
  x <- c(
    "possessionTeam.franchise.currentLogo.url",
    "inside20",
    "half_sack_1",
    "HTTPServer",
    "a2B",
    "_x_",
    "Play Type",
    "Größe",
    "don't",
    "pct%",
    "#plays",
    "2ndDown",
    "",
    "playId",
    "playID",
    "play_id",
    "play_id_2"
  )
  expect_identical(
    clean_names(x),
    c(
      "possession_team_franchise_current_logo_url",
      "inside20",
      "half_sack_1",
      "http_server",
      "a2b",
      "x",
      "play_type",
      "grosse",
      "dont",
      "pct_percent",
      "number_plays",
      "x2nd_down",
      "x_2",
      "play_id",
      "play_id_2",
      "play_id_3",
      "play_id_2_2"
    )
  )
})

test_that("clean_names_cached gives the names of clean_names", {
  x <- c("Play Type", "playId", "playId")
  expect_identical(clean_names_cached(x), clean_names(x))
  expect_identical(clean_names_cached(x), clean_names(x))
})
