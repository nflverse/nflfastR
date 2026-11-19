# Expected output of calculate_series_conversion_rates() on load_test_pbp(),
# without rounding: the rates are means of counts and don't depend on models
load_expected_sc <- function(type = c("sc", "sc_weekly")) {
  type <- match.arg(type)
  strip_nflverse_attributes(
    readRDS(file.path(test_dir, paste0("expected_", type, ".rds")))
  )
}

# Plays that reach every case of calculate_series_conversion_rates(): every
# series result, conversions on every down, QB kneels, plays without down or
# series result, a play without posteam, teams only on offense or only on
# defense in a week, and two seasons and weeks. One row per play.
series_test_pbp <- function() {
  series <- function(season, week, posteam, defteam, series, result, downs) {
    success <- as.numeric(result %in% c("First down", "Touchdown"))
    data.frame(
      season = season,
      week = week,
      posteam = posteam,
      defteam = defteam,
      series = series,
      series_success = success,
      series_result = result,
      down = downs
    )
  }
  rbind(
    series(2023L, 1L, "A", "B", 1, "First down", c(1, 2)),
    series(2023L, 1L, "A", "B", 2, "Turnover on downs", c(1, 2, 3, 4)),
    series(2023L, 1L, "A", "B", 3, "Missed field goal", 1),
    series(2023L, 1L, "A", "B", 4, "QB kneel", c(1, 2)),
    series(2023L, 1L, "A", "B", 5, "First down", NA),
    series(2023L, 1L, "B", "A", 6, "Opp touchdown", 1),
    series(2023L, 1L, "B", "A", 7, "Safety", c(1, 2, 3)),
    series(2023L, 1L, "B", "A", 8, NA, 3),
    series(2023L, 1L, "B", "A", 9, "Touchdown", c(1, 2, 3)),
    series(2023L, 2L, "C", "D", 1, "Punt", c(1, 2, 3, 4)),
    series(2023L, 2L, "C", "D", 2, "End of half", 1),
    series(2023L, 2L, "C", "D", 3, "Field goal", c(1, 2, 3)),
    series(2023L, 2L, "C", "D", 4, "First down", c(1, 2, 3, 4)),
    series(2023L, 2L, "C", "D", 5, "Turnover", 2),
    series(2023L, 2L, NA, "E", 1, "First down", 1),
    series(2022L, 1L, "A", "C", 1, "Touchdown", 1)
  )
}
