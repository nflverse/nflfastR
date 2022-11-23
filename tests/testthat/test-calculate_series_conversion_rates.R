test_that("calculate_series_conversion_rates works", {
  pbp <- load_test_pbp()

  sc <- calculate_series_conversion_rates(pbp = pbp, weekly = FALSE)
  sc_weekly <- calculate_series_conversion_rates(pbp = pbp, weekly = TRUE)

  expect_snapshot_file(save_test_object(sc), "sc.csv", cran = TRUE)
  expect_snapshot_file(save_test_object(sc_weekly), "sc_weekly.csv", cran = TRUE)

  expect_s3_class(sc, "tbl_df")
  expect_s3_class(sc_weekly, "tbl_df")
})
