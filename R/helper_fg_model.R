# Field goal make probability
#
# A lookup table, `fg_prob_table` in R/sysdata.rda, built in
# data-raw/fg_model.R the same way as nfl4th's field goal model: a smooth curve
# in distance for every kicking era and roof (outdoors or not), corrected for
# selection at long range (only teams who trust their kicker attempt long field
# goals, so the observed make rate out there is too high for the average team),
# falling with distance, fading to zero from 65 to 75 yards and zero beyond.
# Rows are yardline_100 1 to 99 (a kick of yardline_100 + 18 yards), columns
# are era x roof.

# Kicking has improved steadily since 1999. The eras are recency buckets, not
# rule changes. The last two are the ones nfl4th uses.
fg_era <- function(season) {
  findInterval(season, c(2004, 2009, 2014, 2023)) + 1L
}

# probability that a field goal from yardline_100 is good. A missing season is
# the most recent one, a missing roof is outdoors.
fg_make_prob <- function(season, roof, yardline_100) {
  n <- max(length(season), length(roof), length(yardline_100))
  season <- if (is.null(season)) rep(NA_real_, n) else rep_len(season, n)
  roof <- if (is.null(roof)) {
    rep(NA_character_, n)
  } else {
    rep_len(as.character(roof), n)
  }
  yardline_100 <- rep_len(yardline_100, n)
  if (anyNA(season)) {
    season[is.na(season)] <- nflreadr::most_recent_season()
  }
  outdoors <- is.na(roof) | roof == "outdoors"
  col <- paste0(
    "era",
    fg_era(season),
    ifelse(outdoors, "_outdoors", "_indoors")
  )
  row <- pmin(pmax(round(yardline_100), 1), 99)
  p <- rep(NA_real_, n)
  ok <- !is.na(row)
  p[ok] <- fg_prob_table[cbind(
    row[ok],
    match(col[ok], colnames(fg_prob_table))
  )]
  p
}
