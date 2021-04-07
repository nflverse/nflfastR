testthat::test_that("PBP builder shows messages when called from global env", {
  testthat::skip_on_cran()
  testthat::expect_message({
    local(
      envir = new.env(parent = globalenv()),
      expr = {
        ids <- c("2020_12_MIA_NYJ", "2020_16_MIA_LV")
        pbp <- nflfastR::build_nflfastR_pbp(ids)
      }
    )
  })
})

testthat::test_that("Exported functions show messages when called from global env", {
  testthat::skip_on_cran()
  testthat::expect_message({
    local(
      envir = new.env(parent = globalenv()),
      expr = {
        `%>%` <- magrittr::`%>%`
        ids <- c("2020_12_MIA_NYJ", "2020_16_MIA_LV")
        pbp <- nflfastR::fast_scraper(ids) %>%
          nflfastR::clean_pbp() %>%
          nflfastR::add_qb_epa() %>%
          nflfastR::add_xyac() %>%
          nflfastR::add_xpass() %>%
          nflfastR::decode_player_ids()
      }
    )
  })
})

testthat::test_that("PBP builder suppresses messages when called from non-global env", {
  testthat::skip_on_cran()
  testthat::expect_silent({
    local(
      envir = new.env(parent = baseenv()),
      expr = {
        ids <- c("2020_12_MIA_NYJ", "2020_16_MIA_LV")
        pbp <- nflfastR::build_nflfastR_pbp(ids)
      }
    )
  })
})

testthat::test_that("Exported functions suppress messages when called from non-global env", {
  testthat::skip_on_cran()
  testthat::expect_silent({
    local(
      envir = new.env(parent = baseenv()),
      expr = {
        `%>%` <- magrittr::`%>%`
        ids <- c("2020_12_MIA_NYJ", "2020_16_MIA_LV")
        pbp <- nflfastR::fast_scraper(ids) %>%
          nflfastR::clean_pbp() %>%
          nflfastR::add_qb_epa() %>%
          nflfastR::add_xyac() %>%
          nflfastR::add_xpass() %>%
          nflfastR::decode_player_ids()
      }
    )
  })
})
