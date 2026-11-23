test_that("add_xyac doesn't change its input and keeps its type", {
  expect_input_unchanged(add_xyac)
})

test_that("add_xyac results don't depend on the chunk size", {
  pbp <- pbp_input_types()$tibble
  expected <- add_xyac(pbp)
  local_mocked_bindings(xyac_chunk_size = 7L)
  # equal up to rounding: with an optimized BLAS the matrix products of the EP
  # model can round the last digit differently for different chunks
  expect_equal(add_xyac(pbp), expected)
})
