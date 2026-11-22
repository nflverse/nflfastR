test_that("all patches fit the raw data", {
  skip_on_cran()
  skip_if_offline("github.com")
  problems <- pbp_patch_problems(dir = NULL)
  expect_identical(paste(problems$message, problems$todo), character())
})

test_that("check_pbp_patches() finds and prunes obsolete and unused patches", {
  file <- local_pbp_patches(
    "game_id: 2011_13_DET_NO; play_id: 771; action: set; column: time; from: 02:43; to: 02:42; reason: obsolete",
    "game_id: 2011_13_DET_NO; play_id: 842; action: drop_play; reason: unused, the parser removes the duplicate",
    "game_id: 2011_13_DET_NO; play_id: 1152; action: drop_play; reason: needed",
    "game_id: 2011_13_DET_NO; play_id: 99999; action: drop_play; reason: not applied"
  )
  g <- readRDS(file.path(test_dir, "games_raw_fixes.rds"))
  expect_snapshot(
    check_pbp_patches(dir = test_dir, prune = TRUE, file = file, games = g),
    error = TRUE
  )
  expect_identical(read_pbp_patches(file)$play_id, c(1152, 99999))
  expect_identical(
    readLines(file),
    c(
      "game_id: 2011_13_DET_NO",
      "play_id: 1152",
      "action: drop_play",
      "reason: needed",
      "",
      "game_id: 2011_13_DET_NO",
      "play_id: 99999",
      "action: drop_play",
      "reason: not applied"
    )
  )
})

test_that("patches that are only unused together aren't all unused", {
  local_pbp_patches(
    "game_id: 2019_01_GB_CHI; play_id: 1; action: drop_play; reason: r",
    "game_id: 2019_01_GB_CHI; play_id: 2; action: drop_play; reason: r"
  )
  # the output is the same as long as one of the patches is there
  local_mocked_bindings(
    get_pbp_nfl = \(...) NULL,
    patched_game_output = \(id, ...) list(pbp = id %in% names(pbp_patches()))
  )
  problems <- pbp_patch_problems(dir = test_dir, unused = TRUE)
  expect_identical(problems$play_id, 1)
  expect_identical(problems$status, "unused")
})

test_that("check_pbp_patches() only prunes the file of the checked patches", {
  local_pbp_patches(
    "game_id: 2011_13_DET_NO; play_id: 1152; action: drop_play; reason: r"
  )
  other <- withr::local_tempfile(
    lines = c(
      "game_id: 2011_13_DET_NO",
      "play_id: 1655",
      "action: drop_play",
      "reason: r"
    )
  )
  expect_snapshot(
    check_pbp_patches(dir = test_dir, prune = TRUE, file = other),
    error = TRUE,
    transform = \(x) gsub(other, "<file>", x, fixed = TRUE)
  )
})
