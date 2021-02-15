context("schedule")

source('helpers.R')

test_that("Schedule scraper", {

  skip_on_cran()

  games <- fast_scraper_schedules(1999)
  expect_identical(games$game_id[1], game_ids[1])

  games <- fast_scraper_schedules(2019)
  expect_identical(games$game_id[1], game_ids[2])

})
