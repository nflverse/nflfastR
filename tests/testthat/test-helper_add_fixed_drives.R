test_that("a drive or series ends in its last result before the end of the half", {
  # a kneel down to center the ball for a field goal
  expect_identical(
    drive_result(c(NA, "QB kneel", NA, "Field goal", NA, "End of half")),
    "Field goal"
  )
  # a drive that starts with a recovered kickoff fumble
  expect_identical(
    drive_result(c("Turnover", NA, NA, "Field goal", "End of half")),
    "Field goal"
  )
  expect_identical(drive_result(c("Turnover", NA, "Touchdown")), "Touchdown")
  expect_identical(
    drive_result(c("QB kneel", "QB kneel", "End of half")),
    "QB kneel"
  )
  expect_identical(drive_result(c(NA, "End of half")), "End of half")
  expect_identical(drive_result(c(NA, NA)), NA_character_)
})
