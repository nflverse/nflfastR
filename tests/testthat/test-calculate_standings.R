test_that("calculate_standings() is defunct", {
  expect_snapshot(calculate_standings(), error = TRUE)
})
