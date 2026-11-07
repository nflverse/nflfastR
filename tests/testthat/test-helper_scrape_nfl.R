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

test_that("games without play stats give an empty play stats table", {
  plays <- data.frame(playId = c(1, 2))
  plays$playStats <- list(NULL, data.frame())
  stats <- unnest_play_stats(plays)
  expect_identical(nrow(stats), 0L)
  play_stats <- tidy_play_stats(stats)
  expect_identical(nrow(play_stats), 0L)
  expect_type(play_stats$play_id, "integer")
})

test_that("normalize_raw_columns converts types, teams, blanks and names", {
  raw <- tibble::tibble(
    `Play Type` = c(TRUE, NA),
    yards = c(1L, 2L),
    yardline = factor(c("SD 20", "KC 30")),
    desc = c(" Pass\r\nshort  ", "")
  )
  expect_identical(
    normalize_raw_columns(raw),
    tibble::tibble(
      play_type = c(1, NA),
      yards = c(1, 2),
      yardline = c("LAC 20", "KC 30"),
      desc = c("Pass short", "")
    )
  )
})

test_that("empty_strings_to_na only changes empty strings", {
  df <- tibble::tibble(x = c("", "a", NA), y = c(1, 2, 3))
  expect_identical(
    empty_strings_to_na(df),
    tibble::tibble(x = c(NA, "a", NA), y = c(1, 2, 3))
  )
})

test_that("clean_names_cached gives the names of janitor", {
  x <- c("Play Type", "playId", "playId")
  expect_identical(clean_names_cached(x), janitor::make_clean_names(x))
  expect_identical(clean_names_cached(x), janitor::make_clean_names(x))
})
