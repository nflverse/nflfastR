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

  # if one of the above fails, try running .patch_clean_data()

  # verify game IDs
  patch_ids <- unique(patch_data$game_id)
  expect_null(verify_game_ids(patch_ids))

  # verify column names
  patch_columns <- unique(patch_data$column)
  expect_in(patch_columns, names(default_play))

  # check column classes
  expect_identical(
    vapply(patch_data, class, FUN.VALUE = character(1L)),
    c(
      "game_id" = "character",
      "play_id" = "integer",
      "column" = "character",
      "value" = "list"
    )
  )

  # avoid NA game_ids play_id, or column
  expect_false(anyNA(patch_data$game_id))
  expect_false(anyNA(patch_data$play_id))
  expect_false(anyNA(patch_data$column))

  # enforce that value is always of length 1
  expect_all_equal(
    vapply(patch_data$value, length, FUN.VALUE = integer(1L)),
    1L
  )
})
