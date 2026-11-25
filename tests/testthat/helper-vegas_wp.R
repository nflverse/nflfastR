# Game states for the vegas WP model (vectorized), other things equal
vegas_state <- function(
  t = 3000,
  sd = 0,
  spread = 0,
  down = 1,
  ytg = 10,
  yl = 75,
  home = 1,
  season = 2025
) {
  data.frame(
    season = season,
    game_seconds_remaining = t,
    half_seconds_remaining = ifelse(t > 1800, t - 1800, t),
    score_differential = sd,
    posteam_spread = spread,
    home = home,
    receive_2h_ko = 0,
    down = down,
    ydstogo = ytg,
    yardline_100 = yl,
    posteam_timeouts_remaining = 3,
    defteam_timeouts_remaining = 3
  )
}
