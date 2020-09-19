context("scrapers")

source('helpers.R')

test_that("Scraper with source nfl works for an old and new game at once: no pp", {

  x2 <- fast_scraper(game_ids, pp = F) %>%
    extract_desc()

  expect_identical(x2[1], desc_1_nfl_source)
  expect_identical(x2[2], desc_2_nfl_source)
})


test_that("Scraper with source live works for an old and new game at once: no pp", {

  x4 <- fast_scraper(old_game_ids, source = "old", pp = F) %>%
    extract_desc()

  expect_identical(x4[1], desc_1_nfl_source)
  expect_identical(x4[2], desc_2_live_source)
})

