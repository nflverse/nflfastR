test_that("the play stats leave out the plays and stats that patches drop", {
  local_pbp_patches(
    "game_id: 2011_13_DET_NO; play_id: 1152; action: drop_play; reason: r",
    "game_id: 2011_13_DET_NO; play_id: 1655; action: drop_stat; stat_id: 11; reason: r"
  )
  ps <- playstats_of_game(
    "2011_13_DET_NO",
    p = \(...) NULL,
    stat_ids = 1:1000,
    dir = test_dir,
    skip_local = FALSE
  )
  expect_disjoint(ps$play_id, 1152)
  expect_identical(ps$stat_id[ps$play_id == 1655], 3L)
})
