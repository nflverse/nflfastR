test_that("build_nflfastR_pbp works (on CRAN)", {
  # this test will run on everywhere, including CRAN. It uses locally available data
  # so it can't break because of failed downloads
  pbp <- load_test_pbp(dir = test_dir)
  expect_s3_class(pbp, "nflverse_data")
  pbp <- strip_nflverse_attributes(pbp)
  exp <- load_expectation("pbp")
  expect_equal(pbp, exp)
})

test_that("build_nflfastR_pbp works (outside CRAN)", {
  # this test is almost the same as above. However, it requires data download
  # and will therefore not run on CRAN but everywhere else.
  skip_on_cran()
  skip_if_offline("github.com")
  pbp <- load_test_pbp(dir = NULL)
  pbp <- strip_nflverse_attributes(pbp)
  exp <- load_expectation("pbp")
  expect_equal(pbp, exp)
})
