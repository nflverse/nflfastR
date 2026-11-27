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

test_that("the kicking team has the ball on recovered and reversed kickoffs", {
  d <- data.frame(
    play_type = c(rep(c("kickoff", "kickoff", "pass"), 3), "kickoff"),
    kickoff_attempt = c(0, 1, 0, NA, 1, 0, 0, 1, 0, 1),
    own_kickoff_recovery = c(0, 0, 0, NA, 1, 0, 0, 0, 0, 0),
    fumble_lost = c(0, 1, 0, NA, 0, 0, 0, 0, 0, 1)
  )
  expect_identical(
    kicking_team_ball(d),
    c(TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE)
  )
})

test_that("a recovered kickoff starts a new drive unless it opens the game", {
  d <- data.table::data.table(
    game_id = "2024_19_GB_PHI",
    game_half = "Half1",
    desc = c("GAME", "kicks", "run", "kicks onside", "run"),
    posteam = c(NA, "GB", "PHI", "GB", "PHI"),
    defteam = c(NA, "PHI", "GB", "PHI", "GB"),
    play_type = c(NA, "kickoff", "run", "kickoff", "run"),
    kickoff_attempt = c(NA, 1, 0, 1, 0),
    own_kickoff_recovery = c(NA, 0, 0, 1, 0),
    fumble_lost = c(NA, 1, 0, 0, 0),
    touchdown = c(NA, 0, 1, 0, 0),
    td_team = c(NA, NA, "PHI", NA, NA),
    safety = c(NA, 0, 0, 0, 0),
    interception = c(NA, 0, 0, 0, 0),
    first_down_rush = 0,
    first_down_pass = 0,
    first_down_penalty = 0,
    field_goal_result = NA_character_,
    punt_attempt = 0,
    qb_kneel = 0,
    down = c(NA, NA, 1, NA, 1),
    yards_gained = c(NA, 0, 75, 0, 5),
    ydstogo = c(NA, 0, 10, 0, 10)
  )
  withr::local_options(nflfastR.verbose = FALSE)
  add_drive_results(d)
  expect_identical(d$fixed_drive, c(1, 1, 1, 2, 2))
})
