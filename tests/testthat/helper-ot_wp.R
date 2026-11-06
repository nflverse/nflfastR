ot_state <- function(
  season = 2025,
  season_type = "REG",
  kind = "kickoff",
  d = 0,
  phase = 1,
  secs = 600,
  spread = 0,
  y = 65,
  down = 1,
  ytg = 10
) {
  data.frame(
    season = season,
    season_type = season_type,
    kind = kind,
    yardline_100 = y,
    down = down,
    ydstogo = ytg,
    secs = secs,
    d = d,
    spread = spread,
    phase = phase,
    p_make = 0.94
  )
}
ot_wp1 <- function(...) ot_wp_states(ot_state(...), use_spread = TRUE)
