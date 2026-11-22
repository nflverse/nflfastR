test_that("the patch file is valid", {
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
  pbp <- suppressMessages(get_pbp_nfl("2019_01_GB_CHI", dir = test_dir))
  expect_in(patches$column[is_set], names(pbp))
  # from and to convert to the class of the column and differ
  for (i in which(is_set)) {
    column <- pbp[[patches$column[i]]]
    from <- expect_no_error(patch_value(patches$from[i], column, "", ""))
    to <- expect_no_error(patch_value(patches$to[i], column, "", ""))
    expect_false(identical(from, to))
  }
  # a set patch of all plays doesn't overlap set patches of single plays
  overlap <- patches[
    is_set,
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

  key <- c("game_id", "play_id", "action", "stat_id", "column")
  expect_false(anyDuplicated(patches, by = key) > 0)
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

  stats <- data.table::data.table(playId = c(1, 1, 2), statId = c(10, 5, 10))
  expect_identical(
    patch_drop_stats(stats, "2019_01_GB_CHI"),
    data.table::data.table(playId = c(1, 2), statId = c(5, 10))
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
  stats <- data.table::data.table(playId = 1, statId = 10)
  expect_identical(
    obsolete(patch_drop_stats(stats, "2019_01_GB_CHI")),
    c(TRUE, FALSE)
  )
  pbp <- data.table::data.table(play_id = c(1, 2), posteam = c("GB", "KC"))
  expect_identical(
    obsolete(patch_set_values(pbp, "2019_01_GB_CHI")),
    c(TRUE, FALSE)
  )
})

test_that("set patches change values of the class of the column", {
  local_pbp_patches(
    "game_id: 2019_01_GB_CHI; action: set; column: start_time; from: a; to: b; reason: r",
    "game_id: 2019_01_GB_CHI; play_id: 1; action: set; column: quarter; from: 1; to: 2; reason: r",
    "game_id: 2019_01_GB_CHI; play_id: 2; action: set; column: posteam; from: GB; reason: r",
    "game_id: 2019_01_GB_CHI; play_id: 3; action: set; column: down; to: 4; reason: r"
  )
  pbp <- data.table::data.table(
    play_id = c(1, 2, 3),
    start_time = "a",
    quarter = 1L,
    posteam = "GB",
    down = c(1, 2, NA)
  )
  patch_set_values(pbp, "2019_01_GB_CHI")
  expect_identical(
    pbp,
    data.table::data.table(
      play_id = c(1, 2, 3),
      start_time = "b",
      quarter = c(2L, 1L, 1L),
      posteam = c("GB", NA, "GB"),
      down = c(1, 2, 4)
    )
  )
})

test_that("set patches only change the value they expect", {
  local_pbp_patches(
    "game_id: 2019_01_GB_CHI; play_id: 1; action: set; column: posteam; from: CHI; to: GB; reason: r",
    "game_id: 2019_01_GB_CHI; play_id: 9; action: set; column: posteam; from: CHI; to: GB; reason: r"
  )
  pbp <- data.table::data.table(play_id = c(1, 2), posteam = "KC")
  expect_snapshot(patch_set_values(pbp, "2019_01_GB_CHI"))
  expect_identical(pbp$posteam, c("KC", "KC"))
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

test_that("unknown fields of the patch file fail", {
  expect_snapshot(
    local_pbp_patches("game_id: 2019_01_GB_CHI; play: 1; reason: r"),
    error = TRUE
  )
})
