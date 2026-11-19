test_that("add_xpass doesn't change its input and keeps its type", {
  expect_input_unchanged(add_xpass)
})

test_that("xpass follows down, distance, score and clock", {
  g <- expand.grid(
    down = 1:4,
    ytg = c(1, 5, 10, 20),
    yl = c(90, 50, 5),
    sd = c(-14, 0, 14),
    t = c(3300, 1900, 600, 100)
  )
  p <- xpass_state(g$down, g$ytg, g$yl, g$sd, g$t)
  expect_all_true(p > 0 & p < 1)
  # passing downs
  expect_gt(xpass_state(3, 10), 0.9)
  expect_lt(xpass_state(3, 1), 0.5)
  expect_gt(xpass_state(2, 10), xpass_state(2, 2))
  # teams that are behind late throw, teams that are ahead run
  expect_gt(xpass_state(sd = -10, t = 240), 0.9)
  expect_lt(xpass_state(sd = 10, t = 240), 0.2)
})

test_that("xpass only uses the game situation", {
  pbp <- load_test_pbp()
  other <- pbp
  other$wp <- 0.5
  other$vegas_wp <- 0.5
  other$spread_line <- 0
  other$roof <- "dome"
  other$home_team <- other$away_team
  other$week <- 1
  expect_identical(
    suppressMessages(add_xpass(other))$xpass,
    suppressMessages(add_xpass(pbp))$xpass
  )
})

test_that("xpass covers all seasons since 2001 and only uses their era", {
  expect_all_true(!is.na(xpass_state(season = 2001:2030)))
  expect_all_true(is.na(xpass_state(season = 1999:2000)))
  expect_equal(xpass_state(season = 2018), xpass_state(season = 2030))
  expect_equal(xpass_state(season = 2006), xpass_state(season = 2013))
  expect_equal(xpass_state(season = 2001), xpass_state(season = 2005))
})
