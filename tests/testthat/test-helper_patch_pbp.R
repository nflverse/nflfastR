test_that("the patch file has the fields each action needs", {
  patches <- read_pbp_patches()
  expect_named(
    patches,
    c(
      "game_id",
      "play_id",
      "action",
      "stat_id",
      "column",
      "from",
      "to",
      "reason"
    )
  )
  expect_all_true(is_valid_game_id(patches$game_id))
  expect_in(patches$action, c("drop_play", "drop_stat", "set"))
  expect_all_true(!is.na(patches$reason))

  is_set <- patches$action == "set"
  expect_all_true(!is.na(patches$play_id[!is_set]))
  expect_all_true(is.na(patches$column[!is_set]))
  expect_all_true(is.na(patches$from[!is_set]) & is.na(patches$to[!is_set]))
  expect_identical(
    is.na(patches$stat_id),
    patches$action != "drop_stat"
  )
  expect_all_true(!is.na(patches$column[is_set]))
})

test_that("set patches of the patch file fit the parser", {
  patches <- read_pbp_patches()
  patches <- patches[patches$action == "set"]
  pbp <- suppressMessages(get_pbp_nfl("2019_01_GB_CHI", dir = test_dir))
  expect_in(patches$column, names(pbp))
  # from and to convert to the class of the column and differ
  same <- character()
  for (i in seq_len(nrow(patches))) {
    column <- pbp[[patches$column[i]]]
    from <- expect_no_error(patch_value(patches$from[i], column, "", ""))
    to <- expect_no_error(patch_value(patches$to[i], column, "", ""))
    if (identical(from, to)) {
      same <- c(same, paste(patches$game_id[i], patches$play_id[i]))
    }
  }
  expect_identical(same, character())
})

test_that("patches of the patch file don't overlap", {
  patches <- read_pbp_patches()
  key <- c("game_id", "play_id", "action", "stat_id", "column")
  expect_identical(anyDuplicated(patches, by = key), 0L)
  # a set patch of all plays doesn't overlap set patches of single plays
  overlap <- patches[
    action == "set",
    list(overlap = anyNA(play_id) && .N > 1),
    by = c("game_id", "column")
  ]
  expect_all_false(overlap$overlap)
  # no patch changes a play that a drop_play patch removes
  dropped <- patches[action == "drop_play", list(game_id, play_id)]
  on_dropped <- patches[action != "drop_play"][
    dropped,
    on = c("game_id", "play_id"),
    nomatch = NULL
  ]
  expect_identical(nrow(on_dropped), 0L)
})

test_that("the patch file is sorted", {
  patches <- read_pbp_patches()
  expect_identical(
    patches,
    patches[order(game_id, play_id, action, stat_id, column, na.last = FALSE)]
  )
})

test_that("drop_play and drop_stat patches remove raw plays and stats", {
  local_pbp_patches(
    "game_id: 2019_01_GB_CHI; play_id: 2; action: drop_play; reason: r",
    "game_id: 2019_01_GB_CHI; play_id: 1; action: drop_stat; stat_id: 10; reason: r"
  )
  plays <- data.table::data.table(playId = c(1, 2, 3))
  expect_identical(
    patch_drop_plays(plays, "2019_01_GB_CHI"),
    data.table::data.table(playId = c(1, 3))
  )
  expect_identical(patch_drop_plays(plays, "2019_02_GB_CHI"), plays)

  stats <- data.table::data.table(
    playId = c(1, 1, 1, 2),
    statId = c(10, 5, NA, 10)
  )
  expect_identical(
    patch_drop_stats(stats, "2019_01_GB_CHI"),
    data.table::data.table(playId = c(1, 1, 2), statId = c(5, NA, 10))
  )
})

test_that("set patches change a column before anything is derived from it", {
  local_pbp_patches(
    "game_id: 2019_01_GB_CHI; play_id: 260; action: set; column: yardline; from: GB 40; to: GB 35; reason: r",
    "game_id: 2019_01_GB_CHI; play_id: 260; action: set; column: posteam; from: CHI; to: GB; reason: r"
  )
  pbp <- suppressMessages(get_pbp_nfl("2019_01_GB_CHI", dir = test_dir))
  expect_identical(
    as.list(pbp[play_id == 260, list(yardline, yardline_number, posteam)]),
    list(yardline = "GB 35", yardline_number = 35, posteam = "GB")
  )
})

test_that("fast_scraper() muffles patches that don't fit the raw data", {
  local_pbp_patches(
    "game_id: 2019_01_GB_CHI; play_id: 99999; action: drop_play; reason: r"
  )
  expect_no_warning(
    suppressMessages(load_game("2019_01_GB_CHI", p = \(...) NULL, test_dir))
  )
})

test_that("patches that don't fit the raw data warn", {
  local_pbp_patches(
    "game_id: 2019_01_GB_CHI; play_id: 9; action: drop_play; reason: r",
    "game_id: 2019_01_GB_CHI; play_id: 1; action: drop_stat; stat_id: 99; reason: r"
  )
  plays <- data.table::data.table(playId = c(1, 2))
  expect_warning(
    expect_identical(patch_drop_plays(plays, "2019_01_GB_CHI"), plays),
    class = "nflfastR_patch_not_applied"
  )
  stats <- data.table::data.table(playId = 1, statId = 10)
  expect_warning(
    expect_identical(patch_drop_stats(stats, "2019_01_GB_CHI"), stats),
    class = "nflfastR_patch_not_applied"
  )
})

test_that("patches that don't fit say whether the raw data was fixed", {
  local_pbp_patches(
    "game_id: 2019_01_GB_CHI; play_id: 1; action: drop_stat; stat_id: 99; reason: r",
    "game_id: 2019_01_GB_CHI; play_id: 9; action: drop_stat; stat_id: 10; reason: r",
    "game_id: 2019_01_GB_CHI; play_id: 1; action: set; column: posteam; from: CHI; to: GB; reason: r",
    "game_id: 2019_01_GB_CHI; play_id: 2; action: set; column: posteam; from: CHI; to: GB; reason: r"
  )
  obsolete <- function(code) {
    out <- logical()
    withCallingHandlers(
      code,
      nflfastR_patch_not_applied = function(w) {
        out <<- c(out, w$obsolete)
        invokeRestart("muffleWarning")
      }
    )
    out
  }
  # a missing stat ID isn't obsolete for sure, even if the play is there
  stats <- data.table::data.table(playId = 1, statId = 10)
  expect_identical(
    obsolete(patch_drop_stats(stats, "2019_01_GB_CHI")),
    c(FALSE, FALSE)
  )
  pbp <- data.table::data.table(play_id = c(1, 2), posteam = c("GB", "KC"))
  expect_identical(
    obsolete(patch_set_values(pbp, "2019_01_GB_CHI", late = TRUE)),
    c(TRUE, FALSE)
  )
})

test_that("set patches change values of the class of the column", {
  local_pbp_patches(
    "game_id: 2019_01_GB_CHI; action: set; column: start_time; from: a; to: b; reason: r",
    "game_id: 2019_01_GB_CHI; play_id: 1; action: set; column: quarter; from: 1; to: 2; reason: r",
    "game_id: 2019_01_GB_CHI; play_id: 2; action: set; column: posteam; from: GB; reason: r",
    "game_id: 2019_01_GB_CHI; play_id: 3; action: set; column: down; to: 4; reason: r",
    "game_id: 2019_01_GB_CHI; play_id: 3; action: set; column: yardline; to: GB 25; reason: r"
  )
  pbp <- data.table::data.table(
    play_id = c(1, 2, 3),
    start_time = "a",
    quarter = 1L,
    posteam = "GB",
    down = c(1, 2, NA),
    yardline = c("GB 40", "GB 30", "")
  )
  # posteam is one of late_patch_columns
  patch_set_values(pbp, "2019_01_GB_CHI")
  expect_identical(pbp$posteam, rep("GB", 3))
  patch_set_values(pbp, "2019_01_GB_CHI", late = TRUE)
  expect_identical(
    pbp,
    data.table::data.table(
      play_id = c(1, 2, 3),
      start_time = "b",
      quarter = c(2L, 1L, 1L),
      posteam = c("GB", NA, "GB"),
      down = c(1, 2, 4),
      yardline = c("GB 40", "GB 30", "GB 25")
    )
  )
})

test_that("set patches only change the value they expect", {
  local_pbp_patches(
    "game_id: 2019_01_GB_CHI; play_id: 1; action: set; column: posteam; from: CHI; to: GB; reason: r",
    "game_id: 2019_01_GB_CHI; play_id: 9; action: set; column: posteam; from: CHI; to: GB; reason: r",
    "game_id: 2019_01_GB_CHI; action: set; column: down; from: 1; to: 2; reason: r"
  )
  pbp <- data.table::data.table(
    play_id = 1:7,
    posteam = "KC",
    down = c(1, 1, 2, 3, 4, 5, NA)
  )
  expect_snapshot({
    patch_set_values(pbp, "2019_01_GB_CHI")
    patch_set_values(pbp, "2019_01_GB_CHI", late = TRUE)
  })
  expect_identical(pbp$posteam, rep("KC", 7))
  expect_identical(pbp$down, c(1, 1, 2, 3, 4, 5, NA))
})

test_that("set patches with a wrong column or value fail", {
  pbp <- data.table::data.table(play_id = 1, down = 1)
  local_pbp_patches(
    "game_id: 2019_01_GB_CHI; play_id: 1; action: set; column: downs; from: 1; to: 2; reason: r"
  )
  expect_snapshot(patch_set_values(pbp, "2019_01_GB_CHI"), error = TRUE)
  local_pbp_patches(
    "game_id: 2019_01_GB_CHI; play_id: 1; action: set; column: down; from: 1; to: two; reason: r"
  )
  expect_snapshot(patch_set_values(pbp, "2019_01_GB_CHI"), error = TRUE)
})

test_that("play and stat IDs of the patch file must be numbers", {
  expect_snapshot(
    local_pbp_patches(
      "game_id: 2019_01_GB_CHI; play_id: 12a4; action: set; column: down; from: 1; to: 2; reason: r"
    ),
    error = TRUE
  )
})

test_that("unknown fields of the patch file fail", {
  expect_snapshot(
    local_pbp_patches("game_id: 2019_01_GB_CHI; play: 1; reason: r"),
    error = TRUE
  )
})
