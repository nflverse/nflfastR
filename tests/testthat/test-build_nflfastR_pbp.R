test_that("build_nflfastR_pbp works (on CRAN)", {
  # this test will run on everywhere, including CRAN. It uses locally available data
  # so it can't break because of failed downloads
  pbp <- load_test_pbp(dir = test_dir)

  expect_snapshot_file(save_test_object(pbp), "pbp.csv", cran = TRUE)
  expect_s3_class(pbp, "nflverse_data")
})

test_that("build_nflfastR_pbp works (outside CRAN)", {
  # this test is almost the same as above. However, it requires data download
  # and will therefore not run on CRAN but everywhere else.
  skip_on_cran()
  skip_if_offline("github.com")
  pbp <- load_test_pbp(dir = NULL)
  expect_snapshot_file(save_test_object(pbp), "pbp.csv", cran = TRUE)
  expect_s3_class(pbp, "nflverse_data")
})
