# xpass of game states (vectorized), other things equal
xpass_state <- function(
  down = 1,
  ytg = 10,
  yl = 75,
  sd = 0,
  t = 3000,
  season = 2025
) {
  out <- data.frame(
    season = season,
    play_type = "pass",
    posteam = "A",
    pass = 1,
    rush = 0,
    down = down,
    ydstogo = ytg,
    yardline_100 = yl,
    score_differential = sd,
    game_seconds_remaining = t,
    half_seconds_remaining = ifelse(t > 1800, t - 1800, t),
    qtr = 4 - pmin(t %/% 900, 3),
    posteam_timeouts_remaining = 3,
    defteam_timeouts_remaining = 3
  )
  suppressMessages(add_xpass(out))$xpass
}
