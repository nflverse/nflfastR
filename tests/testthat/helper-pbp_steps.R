# The exported pbp steps work by reference inside build_nflfastR_pbp(). Called
# by users, they must never change their input and must return the type of
# the input.
pbp_input_types <- function() {
  pbp <- load_test_pbp()
  plain <- strip_nflverse_attributes(pbp)
  class(plain) <- "data.frame"
  list(
    nflverse_data = pbp,
    tibble = tibble::as_tibble(plain),
    data.frame = plain,
    data.table = data.table::as.data.table(plain)
  )
}

expect_input_unchanged <- function(fn, same_class = TRUE) {
  for (x in pbp_input_types()) {
    before <- rlang::duplicate(x, shallow = FALSE)
    out <- fn(x)
    expect_identical(x, before)
    if (same_class) {
      expect_identical(class(out), class(x))
    }
  }
}
