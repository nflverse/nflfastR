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

test_that("is_valid_game_id checks season, week and teams", {
  expect_identical(
    is_valid_game_id(c(
      "2021_02_LAC_KC",
      "2002_17_OAK_SD",
      "Hello World",
      "2028_01_LAC_JAX",
      "2022_27_LAC_BUF",
      "2021_02_LAC_KAC"
    )),
    c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
  )
})

test_that("cli_message interpolates in the caller and respects verbose", {
  f <- function() {
    n <- 2
    cli_message(
      "{n} season{?s} \\
      and more",
      .cli_fct = cli::cli_ul
    )
    cli_message("done", .cli_fct = cli::cli_alert_success)
    cli_message("careful", .cli_fct = cli::cli_alert_warning)
  }
  mask_time <- \(x) gsub("[0-9]{2}:[0-9]{2}:[0-9]{2}", "<time>", x)
  withr::local_options(nflfastR.verbose = TRUE)
  expect_snapshot(f(), transform = mask_time)
  withr::local_options(nflfastR.verbose = FALSE)
  expect_snapshot(f(), transform = mask_time)
})
