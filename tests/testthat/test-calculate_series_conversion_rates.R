test_that("calculate_series_conversion_rates works", {
  # This test used to run on CRAN but their changes to env vars which cause
  # check NOTES for multi-threading forced us to skip on cran.
  skip_on_cran()

  pbp <- load_test_pbp()

  sc <- calculate_series_conversion_rates(pbp = pbp, weekly = FALSE) %>%
    round_double_to_digits()
  sc_weekly <- calculate_series_conversion_rates(pbp = pbp, weekly = TRUE) %>%
    round_double_to_digits()

  exp_sc <- load_expectation("sc")
  exp_sc_weekly <- load_expectation("sc_weekly")

  expect_s3_class(sc, "tbl_df")
  expect_s3_class(sc_weekly, "tbl_df")

  expect_equal(sc, exp_sc)
  expect_equal(sc_weekly, exp_sc_weekly)
})
