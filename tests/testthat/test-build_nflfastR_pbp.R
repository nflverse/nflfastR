test_that("build_nflfastR_pbp works (local data)", {
  # This test used to run on CRAN but their changes to env vars which cause
  # check NOTES for multi-threading forced us to skip on cran. It uses locally
  # available data so it can't break because of failed downloads
  skip_on_cran()

  pbp <- load_test_pbp(dir = test_dir)
  expect_s3_class(pbp, "nflverse_data")
  pbp <- strip_nflverse_attributes(pbp) %>%
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
  pbp <- strip_nflverse_attributes(pbp) %>%
    # we gotta round floating point numbers because of different model output
    # across platforms
    round_double_to_digits()
  exp <- load_expectation("pbp")
  expect_equal(pbp, exp)
})
