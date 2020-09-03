context("scrapers")

# grab a couple games
game_ids <- c(
  fast_scraper_schedules(2003) %>%
    dplyr::slice(1) %>%
    dplyr::pull(game_id),
  fast_scraper_schedules(2019) %>%
    dplyr::slice(1) %>%
    dplyr::pull(game_id)
)

# make sure scraper works with pp
x1 <- fast_scraper(game_ids, pp = T) %>%
  clean_pbp() %>%
  add_qb_epa() %>%
  dplyr::filter(down == 1) %>%
  dplyr::group_by(game_id) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::pull(desc)

test_that("Scraper works for an old and new game at once: pp", {
  expect_identical(x1[1], "(14:54) T.Canidate left end to WAS 32 for 3 yards (S.Ellis).")
  expect_identical(x1[2], "(15:00) 33-A.Jones left tackle to GB 25 for no gain (58-R.Smith).")
})

# make sure scraper works without pp
x2 <- fast_scraper(game_ids, pp = F) %>%
  clean_pbp() %>%
  add_qb_epa() %>%
  dplyr::filter(down == 1) %>%
  dplyr::group_by(game_id) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::pull(desc)

test_that("Scraper works for an old and new game at once: no pp", {
  expect_identical(x2[1], "(14:54) T.Canidate left end to WAS 32 for 3 yards (S.Ellis).")
  expect_identical(x2[2], "(15:00) 33-A.Jones left tackle to GB 25 for no gain (58-R.Smith).")
})

