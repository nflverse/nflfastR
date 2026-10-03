test_that("pbp_patch is formatted correctly", {
  patch_data <- .patch_read_data()
  skip_if(is.null(patch_data))

  # check for duplicates
  expect_identical(patch_data, dplyr::distinct(patch_data))

  # check for correct sorting (new games on top)
  expect_identical(
    patch_data,
    dplyr::arrange(patch_data, dplyr::desc(game_id), play_id, column)
  )

  # verify game IDs
  patch_ids <- unique(patch_data$game_id)
  expect_null(nflfastR:::verify_game_ids(patch_ids))

  # verify column names
  patch_columns <- unique(patch_data$column)
  expect_all_true(patch_columns %in% names(nflfastR:::default_play))
})
