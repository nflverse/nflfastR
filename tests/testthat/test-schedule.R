context("schedule")

source('helpers.R')

test_that("Schedule scraper", {

  games <- fast_scraper_schedules(2009)
  expect_identical(games$game_id[1], game_ids[1])
  expect_identical(games$old_game_id[1], old_game_ids[1])

  games <- fast_scraper_schedules(2019)
  expect_identical(games$game_id[1], game_ids[2])
  expect_identical(games$old_game_id[1], old_game_ids[2])

})
