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
    tibble::tibble(
      playId = 1,
      statId = 10,
      yards = 5L,
      teamAbbr = "KC",
      player.displayName = "P.Mahomes",
      player.esbId = "00-0033873"
    )
  )
})

test_that("the switch branches of tidy_play_stats match stat_ids", {
  loop <- Find(
    \(e) is.call(e) && identical(e[[1]], as.name("for")),
    as.list(body(tidy_play_stats))
  )
  sw <- Find(
    \(e) is.call(e) && identical(e[[1]], as.name("switch")),
    as.list(loop[[4]])
  )
  branches <- names(as.list(sw))[-(1:2)]
  expect_identical(as.integer(branches), stat_ids$stat_id)
})

test_that("games without play stats give an empty play stats table", {
  plays <- data.frame(playId = c(1, 2))
  plays$playStats <- list(NULL, data.frame())
  stats <- unnest_play_stats(plays)
  expect_identical(nrow(stats), 0L)
  play_stats <- tidy_play_stats(stats)
  expect_identical(nrow(play_stats), 0L)
  expect_type(play_stats$play_id, "integer")
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
    play_description = c("pass", "punt", "Timeout #2 by NYJ at 1:00")
  )
  fix_bad_games(pbp)
  expect_identical(pbp$td_team, c("KC", "NYJ", NA))
  expect_identical(pbp$return_team, c(NA, "NYJ", NA))
  expect_identical(pbp$fumble_recovery_1_team, c(NA, NA, "KC"))
  expect_identical(pbp$timeout_team, c(NA, NA, "NYJ"))
})

test_that("clean_names_cached gives the names of janitor", {
  x <- c("Play Type", "playId", "playId")
  expect_identical(clean_names_cached(x), janitor::make_clean_names(x))
  expect_identical(clean_names_cached(x), janitor::make_clean_names(x))
})
