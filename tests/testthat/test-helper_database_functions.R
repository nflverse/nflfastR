test_that("update_db() is deprecated", {
  withr::local_options(lifecycle_verbosity = "error")
  expect_error(update_db(), class = "defunctError")
})
