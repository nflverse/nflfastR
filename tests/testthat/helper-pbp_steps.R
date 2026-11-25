# The exported pbp steps work by reference inside build_nflfastR_pbp(). Called
# by users, they must never change their input and must return the type of
# the input.
pbp_input_types <- function(pbp = load_test_pbp()) {
  plain <- strip_nflverse_attributes(pbp)
  class(plain) <- "data.frame"
  list(
    # nflverse_data for load_test_pbp(), the type of other inputs
    input = pbp,
    nflreadr_data = as_nflreadr_data(plain),
    tibble = as_test_tibble(plain),
    data.frame = plain,
    data.table = data.table::as.data.table(plain)
  )
}

expect_input_unchanged <- function(
  fn,
  pbp = load_test_pbp(),
  same_class = TRUE
) {
  for (x in pbp_input_types(pbp)) {
    before <- rlang::duplicate(x, shallow = FALSE)
    out <- fn(x)
    expect_identical(x, before)
    if (same_class) {
      expect_identical(class(out), class(x))
    }
  }
}

wp_calculator_input <- function() {
  data.frame(
    receive_2h_ko = 0,
    home_team = "SEA",
    posteam = "SEA",
    score_differential = 0,
    half_seconds_remaining = 1800,
    game_seconds_remaining = 3600,
    spread_line = 3,
    down = 1,
    ydstogo = 10,
    yardline_100 = 75,
    posteam_timeouts_remaining = 3,
    defteam_timeouts_remaining = 3
  )
}
