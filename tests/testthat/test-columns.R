context("columns")

source('helpers.R')

test_that("Columns identical across all 3 possibilities", {

  g1 <- fast_scraper(game_ids[1], source = "nfl", pp = F)
  g2 <- fast_scraper(game_ids[2], source = "nfl", pp = F)
  g3 <- fast_scraper(old_game_ids[1], source = "live", pp = F)

  expect_identical(names(g1), names(g2))
  expect_identical(names(g2), names(g3))
})



