# Probabilities of the next score for game states (vectorized), other things
# equal
ep_state <- function(
  down = 1,
  ytg = 10,
  yl = 75,
  t = 900,
  season = 2025,
  timeouts = 3,
  ...
) {
  state <- make_model_mutations(data.frame(
    season = season,
    posteam = "A",
    home_team = "B",
    roof = "outdoors",
    down = down,
    ydstogo = ytg,
    yardline_100 = yl,
    half_seconds_remaining = t,
    posteam_timeouts_remaining = timeouts,
    defteam_timeouts_remaining = timeouts
  ))
  ep_model_predict(model_matrix(state, ep_model_features), ...)
}

# Expected points of game states with 7, 3 and 2 points for the scores
ep_state_points <- function(...) {
  as.vector(ep_state(...) %*% c(7, -7, 3, -3, 2, -2, 0))
}
