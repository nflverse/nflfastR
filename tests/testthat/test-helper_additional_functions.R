test_that("clean_pbp doesn't change its input and returns a tibble", {
  expect_input_unchanged(clean_pbp, same_class = FALSE)
  expect_s3_class(clean_pbp(pbp_input_types()$data.table), "tbl_df")
})

test_that("add_qb_epa doesn't change its input and keeps its type", {
  expect_input_unchanged(add_qb_epa)
})
