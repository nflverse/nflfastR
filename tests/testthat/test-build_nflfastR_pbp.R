test_that("build_nflfastR_pbp works (local data)", {
  # This test used to run on CRAN but their changes to env vars which cause
  # check NOTES for multi-threading forced us to skip on cran. It uses locally
  # available data so it can't break because of failed downloads
  skip_on_cran()

  pbp <- load_test_pbp(dir = test_dir)
  expect_s3_class(pbp, "nflverse_data")
  pbp <- strip_nflverse_attributes(pbp) |>
    # we gotta round floating point numbers because of different model output
    # across platforms
    round_double_to_digits()
  exp <- load_expectation("pbp")
  expect_equal(pbp, exp)
})

test_that("build_nflfastR_pbp works (outside CRAN)", {
  # this test is almost the same as above. However, it requires data download
  # and will therefore not run on CRAN but everywhere else.
  skip_on_cran()

  skip_if_offline("github.com")
  pbp <- load_test_pbp(dir = NULL)
  pbp <- strip_nflverse_attributes(pbp) |>
    # we gotta round floating point numbers because of different model output
    # across platforms
    round_double_to_digits()
  exp <- load_expectation("pbp")
  expect_equal(pbp, exp)
})

test_that("default_play is synced with build_nflfastR_pbp", {
  # `default_play` is a table of 1 row that is supposed to match the
  # output structure of build_nflfastR_pbp. It is used to initialize the
  # data table in pbp DBs.
  # This test makes sure that it is synced with build_nflfastR_pbp

  exp <- load_expectation("pbp")

  names_and_types_exp <- vapply(exp, class, FUN.VALUE = character(1L))
  names_and_types_def <- vapply(default_play, class, FUN.VALUE = character(1L))

  expect_identical(names_and_types_def, names_and_types_exp)
  expect_snapshot_value(names_and_types_def, style = "json2")
})
