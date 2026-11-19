test_that("calculate_series_conversion_rates works", {
  # This test used to run on CRAN but their changes to env vars which cause
  # check NOTES for multi-threading forced us to skip on cran.
  skip_on_cran()

  pbp <- load_test_pbp()

  sc <- calculate_series_conversion_rates(pbp = pbp, weekly = FALSE)
  sc_weekly <- calculate_series_conversion_rates(pbp = pbp, weekly = TRUE)

  expect_s3_class(sc, "tbl_df")
  expect_s3_class(sc_weekly, "tbl_df")

  expect_identical(sc, load_expected_sc("sc"))
  expect_identical(sc_weekly, load_expected_sc("sc_weekly"))
})

test_that("calculate_series_conversion_rates covers all series cases", {
  pbp <- series_test_pbp()

  expect_snapshot_value(
    calculate_series_conversion_rates(pbp, weekly = FALSE),
    style = "deparse"
  )
  expect_snapshot_value(
    calculate_series_conversion_rates(pbp, weekly = TRUE),
    style = "deparse"
  )
})

test_that("calculate_series_conversion_rates checks the columns of pbp", {
  pbp <- series_test_pbp()
  pbp$series <- NULL
  pbp$down <- NULL

  expect_snapshot(calculate_series_conversion_rates(pbp), error = TRUE)
})
