test_that("squish collapses the same white space as str_squish", {
  x <- c(" a\u000b\u0085b  c\t\n", " ", "", NA)
  expect_identical(squish(x), c("a b c", "", "", NA))
})
