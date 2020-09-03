context("scrapers")

# grab a couple games
game_ids <- dplyr::bind_rows(
  fast_scraper_schedules(2009) %>%
    dplyr::slice(1),
  fast_scraper_schedules(2019) %>%
    dplyr::slice(1)
)

# make sure scraper works with pp
x1 <- fast_scraper(game_ids %>% dplyr::pull(game_id), pp = T) %>%
  clean_pbp() %>%
  add_qb_epa() %>%
  dplyr::filter(down == 1) %>%
  dplyr::group_by(game_id) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::pull(desc)

test_that("Scraper with source nfl works for an old and new game at once: pp", {
  expect_identical(x1[1], "(14:53) B.Roethlisberger pass short left to H.Ward to PIT 47 for 5 yards (C.Hope).")
  expect_identical(x1[2], "(15:00) 33-A.Jones left tackle to GB 25 for no gain (58-R.Smith).")
})

# make sure nfl source works without pp
x2 <- fast_scraper(game_ids %>% dplyr::pull(game_id), pp = F) %>%
  clean_pbp() %>%
  add_qb_epa() %>%
  dplyr::filter(down == 1) %>%
  dplyr::group_by(game_id) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::pull(desc)

test_that("Scraper with source nfl works for an old and new game at once: no pp", {
  expect_identical(x2[1], "(14:53) B.Roethlisberger pass short left to H.Ward to PIT 47 for 5 yards (C.Hope).")
  expect_identical(x2[2], "(15:00) 33-A.Jones left tackle to GB 25 for no gain (58-R.Smith).")
})

# make sure nfl source works without pp
x2 <- fast_scraper(game_ids %>% dplyr::pull(game_id), pp = F) %>%
  clean_pbp() %>%
  add_qb_epa() %>%
  dplyr::filter(down == 1) %>%
  dplyr::group_by(game_id) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::pull(desc)

test_that("Scraper with source nfl works for an old and new game at once: no pp", {
  expect_identical(x2[1], "(14:53) B.Roethlisberger pass short left to H.Ward to PIT 47 for 5 yards (C.Hope).")
  expect_identical(x2[2], "(15:00) 33-A.Jones left tackle to GB 25 for no gain (58-R.Smith).")
})


# make sure live source works without pp
x3 <- fast_scraper(game_ids %>% dplyr::pull(old_game_id), source = "live", pp = F) %>%
  clean_pbp() %>%
  add_qb_epa() %>%
  dplyr::filter(down == 1) %>%
  dplyr::group_by(game_id) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::pull(desc)

test_that("Scraper with source live works for an old and new game at once: no pp", {
  expect_identical(x3[1], "(14:53) B.Roethlisberger pass short left to H.Ward to PIT 47 for 5 yards (C.Hope).")
  expect_identical(x3[2], "(15:00) 33-A.Jones left tackle to GB 25 for no gain (58-R.Smith).")
})

# make sure live source works without pp
x4 <- fast_scraper(game_ids %>% dplyr::pull(old_game_id), source = "live", pp = F) %>%
  clean_pbp() %>%
  add_qb_epa() %>%
  dplyr::filter(down == 1) %>%
  dplyr::group_by(game_id) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::pull(desc)

test_that("Scraper with source live works for an old and new game at once: no pp", {
  expect_identical(x4[1], "(14:53) B.Roethlisberger pass short left to H.Ward to PIT 47 for 5 yards (C.Hope).")
  expect_identical(x4[2], "(15:00) 33-A.Jones left tackle to GB 25 for no gain (58-R.Smith).")
})

