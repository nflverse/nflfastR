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

# grab a couple games
game_ids <- dplyr::bind_rows(
  fast_scraper_schedules(2009) %>%
    dplyr::slice(1),
  fast_scraper_schedules(2019) %>%
    dplyr::slice(1)
)

# for checking play descriptions after scraping
# the first is the same for both sources since it's a 2009 game
desc_1_nfl_source <- "(14:53) B.Roethlisberger pass short left to H.Ward to PIT 47 for 5 yards (C.Hope)."
desc_2_nfl_source <- "(15:00) 33-A.Jones left tackle to GB 25 for no gain (58-R.Smith)."

desc_2_live_source <- "(15:00) A.Jones left tackle to GB 25 for no gain (R.Smith)."


# make sure nfl source works with pp
x1 <- fast_scraper(game_ids %>% dplyr::pull(game_id), pp = T) %>%
  extract_desc()

test_that("Scraper with source nfl works for an old and new game at once: pp", {
  expect_identical(x1[1], desc_1_nfl_source)
  expect_identical(x1[2], desc_2_nfl_source)
})

# make sure nfl source works without pp
x2 <- fast_scraper(game_ids %>% dplyr::pull(game_id), pp = F) %>%
  extract_desc()

test_that("Scraper with source nfl works for an old and new game at once: no pp", {
  expect_identical(x2[1], desc_1_nfl_source)
  expect_identical(x2[2], desc_2_nfl_source)
})

# make sure live source works without pp
x3 <- fast_scraper(game_ids %>% dplyr::pull(old_game_id), source = "live", pp = F) %>%
  extract_desc()

test_that("Scraper with source live works for an old and new game at once: no pp", {
  expect_identical(x3[1], desc_1_nfl_source)
  expect_identical(x3[2], desc_2_live_source)
})

# make sure live source works without pp
x4 <- fast_scraper(game_ids %>% dplyr::pull(old_game_id), source = "live", pp = F) %>%
  extract_desc()

test_that("Scraper with source live works for an old and new game at once: no pp", {
  expect_identical(x4[1], desc_1_nfl_source)
  expect_identical(x4[2], desc_2_live_source)
})
