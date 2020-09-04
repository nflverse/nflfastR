context("scrapers")

source('helpers.R')

test_that("Scraper with source nfl works for an old and new game at once: no pp", {

  x2 <- fast_scraper(game_ids, pp = F) %>%
    extract_desc()

  expect_identical(x2[1], desc_1_nfl_source)
  expect_identical(x2[2], desc_2_nfl_source)
})


test_that("Scraper with source live works for an old and new game at once: no pp", {

  x4 <- fast_scraper(old_game_ids, source = "live", pp = F) %>%
    extract_desc()

  expect_identical(x4[1], desc_1_nfl_source)
  expect_identical(x4[2], desc_2_live_source)
})


test_that("Scraper with source nfl works for an old and new game at once: pp", {

  skip_if_not_installed('furrr')
  skip_if_not_installed('future')

  x1 <- fast_scraper(game_ids, pp = T) %>%
    extract_desc()

  expect_identical(x1[1], desc_1_nfl_source)
  expect_identical(x1[2], desc_2_nfl_source)
})


test_that("Scraper with source live works for an old and new game at once: pp", {

  skip_if_not_installed('furrr')
  skip_if_not_installed('future')

  x3 <- fast_scraper(old_game_ids, source = "live", pp = T) %>%
    extract_desc()

  expect_identical(x3[1], desc_1_nfl_source)
  expect_identical(x3[2], desc_2_live_source)
})

