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

test_that("calculate_series_conversion_rates doesn't change its input", {
  expect_input_unchanged(
    calculate_series_conversion_rates,
    series_test_pbp(),
    same_class = FALSE
  )
})
