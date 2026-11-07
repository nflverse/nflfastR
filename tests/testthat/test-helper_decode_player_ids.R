test_that("decode_player_ids doesn't change its input and keeps its type", {
  skip_if_not_installed("gsisdecoder")
  skip_if_offline("github.com")
  expect_input_unchanged(decode_player_ids)
})
