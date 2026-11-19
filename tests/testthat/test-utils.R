test_that("squish collapses the same white space as str_squish", {
  x <- c(" a\u000b\u0085b  c\t\n", " ", "", NA)
  expect_identical(squish(x), c("a b c", "", "", NA))
})

test_that("load_sysdata_booster loads a model once", {
  withr::defer(rm(list = "sysdata_test", envir = nflfastR_cache))
  b <- load_sysdata_booster(
    "test",
    xpass_model$trees_raw,
    "0.0.1",
    "test model"
  )
  expect_s3_class(b, "xgb.Booster")
  # cached: the raw vector isn't used again
  expect_identical(
    load_sysdata_booster("test", stop("not cached"), "0.0.1", "test model"),
    b
  )
})

test_that("load_sysdata_booster names the model and the xgboost versions", {
  expect_snapshot(
    load_sysdata_booster("test_error", stop("boom"), "0.0.1", "test model"),
    error = TRUE,
    transform = \(x) {
      gsub(
        as.character(packageVersion("xgboost")),
        "<installed>",
        x,
        fixed = TRUE
      )
    }
  )
  expect_null(nflfastR_cache$sysdata_test_error)
})
