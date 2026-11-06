test_that("non-plays with a stale posteam don't flip home_wp (#606)", {
  # This needs a download and gsisdecoder so it can't run on CRAN
  skip_on_cran()
  skip_if_not_installed("gsisdecoder")
  skip_if_offline("github.com")

  # play 1017 is the note "End of quarter - 1.35 pm". It sits between a BUF
  # extra point and the kickoff to CLE and still carried BUF as posteam while
  # its win probability came from the next play, which belongs to CLE.
  pbp <- build_nflfastR_pbp("2016_15_CLE_BUF")
  row <- function(id) pbp[pbp$play_id == id, ]
  note <- row(1017)
  nxt <- row(1035)

  expect_equal(note$home_wp, nxt$home_wp)
  expect_equal(note$vegas_home_wp, nxt$vegas_home_wp)
  # the extra point before the note used to get a WPA of about -0.5
  expect_lt(abs(row(989)$wpa), 0.05)
  expect_lt(abs(note$wpa), 1e-8)
})
