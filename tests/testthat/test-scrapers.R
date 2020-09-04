context("scrapers")

# function to take some games and return
# the first play description from each game
# on a 1st down play
extract_desc <- function(pbp) {
  pbp %>%
    add_qb_epa() %>%
    dplyr::filter(down == 1) %>%
    dplyr::group_by(game_id) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::pull(desc) %>%
    return()
}

# for checking play descriptions after scraping
# the first is the same for both sources since it's a 2009 game
desc_1_nfl_source <- "(14:53) B.Roethlisberger pass short left to H.Ward to PIT 47 for 5 yards (C.Hope)."
desc_2_nfl_source <- "(15:00) 33-A.Jones left tackle to GB 25 for no gain (58-R.Smith)."

desc_2_live_source <- "(15:00) A.Jones left tackle to GB 25 for no gain (R.Smith)."

game_ids <- c("2009_01_TEN_PIT", "2019_01_GB_CHI")
old_game_ids <- c("2009091000", "2019090500")

###
# section 1: schedule scraper
##

test_that("Schedule scraper", {

  games <- fast_scraper_schedules(2009)
  expect_identical(games$game_id[1], game_ids[1])
  expect_identical(games$old_game_id[1], old_game_ids[1])

  games <- fast_scraper_schedules(2019)
  expect_identical(games$game_id[1], game_ids[2])
  expect_identical(games$old_game_id[1], old_game_ids[2])

})

###
# section 2: fast scraper
##

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

###
# section 3: make sure columns are identical across scrapers
##

test_that("Columns identical across all 3 possibilities", {

  g1 <- fast_scraper(game_ids[1], source = "nfl", pp = F)
  g2 <- fast_scraper(game_ids[2], source = "nfl", pp = F)
  g3 <- fast_scraper(old_game_ids[1], source = "live", pp = F)

  expect_identical(names(g1), names(g2))
  expect_identical(names(g2), names(g3))
})



