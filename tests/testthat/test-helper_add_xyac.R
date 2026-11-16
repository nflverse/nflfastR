test_that("add_xyac doesn't change its input and keeps its type", {
  expect_input_unchanged(add_xyac)
})

test_that("add_xyac results don't depend on the chunk size", {
  pbp <- pbp_input_types()$tibble
  expected <- add_xyac(pbp)
  local_mocked_bindings(xyac_chunk_size = 7L)
  expect_identical(add_xyac(pbp), expected)
})
