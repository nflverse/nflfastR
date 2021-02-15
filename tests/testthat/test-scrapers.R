context("scrapers")

source('helpers.R')

test_that("Scraper works for an old and new game at once", {

  skip_on_cran()

  x2 <- fast_scraper(game_ids) %>%
    extract_desc()

  expect_identical(x2[1], desc_1_nfl_source)
  expect_identical(x2[2], desc_2_nfl_source)
})

test_that("Wrapper works for an old and new game at once", {

  skip_on_cran()

  wrapper <- build_nflfastR_pbp(game_ids, decode = FALSE)

  descriptions <- wrapper %>%
    extract_desc()

  names <- wrapper %>%
    dplyr::filter(down == 1) %>%
    dplyr::group_by(game_id) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::pull(name)

  ids <- wrapper %>%
    dplyr::filter(down == 1) %>%
    dplyr::group_by(game_id) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::pull(id)

  expect_identical(descriptions[1], desc_1_nfl_source)
  expect_identical(descriptions[2], desc_2_nfl_source)

  expect_identical(names[1], "J.Anderson")
  expect_identical(names[2], "A.Jones")

  expect_identical(ids[1], "00-0000316")
  expect_identical(ids[2], "32013030-2d30-3033-3332-3933ed82c0de")

})

test_that("Decoding player IDs works", {

  skip_on_cran()

  wrapper <- build_nflfastR_pbp(game_ids, decode = TRUE)

  ids <- wrapper %>%
    dplyr::filter(down == 1) %>%
    dplyr::group_by(game_id) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::pull(id)

  expect_identical(ids[1], "00-0000316")
  expect_identical(ids[2], "00-0033293")

})
