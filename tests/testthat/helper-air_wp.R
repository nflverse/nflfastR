# A pass play (vectorized in air_yards) and the WP at its catch point
air_pass <- function(
  air_yards,
  down = 2,
  ydstogo = 8,
  yardline_100 = 60,
  qtr = 2
) {
  g <- 1800 + 600
  test_tibble(
    game_id = "2025_01_KC_LAC",
    season = 2025,
    qtr = qtr,
    posteam = "KC",
    home_team = "LAC",
    away_team = "KC",
    play_type = "pass",
    air_yards = air_yards,
    down = down,
    ydstogo = ydstogo,
    yardline_100 = yardline_100,
    score_differential = 0,
    home = 0,
    receive_2h_ko = 1,
    half_seconds_remaining = 600,
    game_seconds_remaining = g,
    elapsed_share = (3600 - g) / 3600,
    posteam_timeouts_remaining = 3,
    defteam_timeouts_remaining = 3,
    wp = 0.5,
    wpa = 0,
    two_point_attempt = 0,
    penalty = 0,
    yards_after_catch = NA_real_,
    complete_pass = 0,
    fixed_drive = 1,
    kickoff_attempt = 0
  )
}

air_wp_of <- function(x) {
  out <- add_air_yac_wp_variables(data.table::as.data.table(x))
  out$air_wpa + out$wp
}
