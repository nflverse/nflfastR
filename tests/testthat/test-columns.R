context("columns")

source('helpers.R')

test_that("Columns identical across old and new sources", {

  g1 <- fast_scraper(game_ids[1])
  g2 <- fast_scraper(game_ids[2])

  expect_identical(names(g1), names(g2))
})



