################################################################################
# Author: Ben Baldwin
# Stlyeguide: styler::tidyverse_style()
################################################################################

#' Add expected pass columns
#'
#' @inheritParams clean_pbp
#' @description Build columns from the expected dropback model. The model
#' gives the probability of a dropback (a pass, sack, or scramble) from the game
#' situation only: down, distance, field position, score, time remaining,
#' timeouts, and the era of the season (for league-wide trends). It doesn't
#' know anything about the teams. Must be run on a dataframe that has already had
#' [clean_pbp()] run on it. Note that the functions [build_nflfastR_pbp()] and
#' the database function [update_pbp_db()] already include this function.
#' @return The input Data Frame of the parameter `pbp` with the following columns
#' added:
#' \describe{
#' \item{xpass}{Probability of dropback scaled from 0 to 1.}
#' \item{pass_oe}{Dropback percent over expected on a given play scaled from 0 to 100.}
#' }
#' @export
add_xpass <- function(pbp, ...) {
  if (nrow(pbp) == 0) {
    user_message("Nothing to do. Return passed data frame.", "info")
    return(pbp)
  }
  by_ref <- in_pipeline(pbp, ...)
  d <- if (by_ref) pbp else as_pbp_dt(pbp)
  drop <- intersect(c("xpass", "pass_oe"), names(d))
  if (length(drop) > 0) {
    data.table::set(d, j = drop, value = NULL)
  }
  plays <- prepare_xpass_data(d)
  valid <- which(plays$valid_play == 1)

  if (length(valid) > 0) {
    user_message("Computing xpass...", "todo")

    # the model is only needed for valid plays
    xpass <- rep(NA_real_, nrow(d))
    xpass[valid] <- stats::predict(
      xpass_booster(),
      model_matrix(
        subset_columns(plays, valid, xpass_model_features),
        xpass_model_features
      )
    )
    data.table::set(d, j = "xpass", value = xpass)
    # (fifelse() because a missing rush or pass makes pass_oe missing)
    d[,
      pass_oe := data.table::fifelse(
        rush == 0 & pass == 0,
        NA_real_,
        100 * (pass - xpass)
      )
    ]

    message_completed("added xpass and pass_oe", ...)
  } else {
    data.table::set(d, j = c("xpass", "pass_oe"), value = NA_real_)
    user_message(
      "No non-NA values for xpass calculation detected. xpass and pass_oe set to NA",
      "info"
    )
  }
  if (by_ref) d else as_input_type(d, pbp)
}

# The features of the xpass model and valid_play as list of columns
prepare_xpass_data <- function(pbp) {
  # a list of columns, so the era columns don't change pbp
  plays <- as.list(pbp)
  # the eras of make_model_mutations(), except that 2001 belongs to era1: it is
  # the only supported season of era0
  plays$era1 <- as.numeric(plays$season <= 2005)
  plays$era2 <- as.numeric(plays$season > 2005 & plays$season <= 2013)
  plays$era3 <- as.numeric(plays$season > 2013 & plays$season <= 2017)
  plays$era4 <- as.numeric(plays$season > 2017)
  # no xpass in 1999 and 2000: these seasons aren't supported anymore (#568)
  plays$valid_play <- as.numeric(
    plays$season >= 2001 &
      plays$play_type %chin%
        c("no_play", "pass", "run") &
      !is.na(plays$posteam) &
      !is.na(plays$down) &
      !is.na(plays$defteam_timeouts_remaining) &
      !is.na(plays$posteam_timeouts_remaining) &
      !is.na(plays$yardline_100) &
      !is.na(plays$score_differential)
  )
  plays
}

# The expected dropback model is an xgboost model of `pass` on all plays with a
# down that are a dropback or a run (penalties included, kneels and spikes not)
# since 2001. It only knows the game situation and the era of the season, which
# lets it follow league-wide trends. The eras are the ones of the other models
# (see make_model_mutations()), except that 2001 belongs to era1 (2001 to 2005)
# and there is no era0: 1999 and 2000 aren't supported anymore (#568), which
# would leave era0 with a single season. It has no team strength (pregame line)
# and no win probability, so it doesn't change when a win probability model
# does.
#
# Built by data-raw/xpass_model.R and stored in R/sysdata.rda as `xpass_model`
# (list with the xgboost model as raw UBJSON).
xpass_model_features <- c(
  "down",
  "ydstogo",
  "yardline_100",
  "score_differential",
  "half_seconds_remaining",
  "game_seconds_remaining",
  "posteam_timeouts_remaining",
  "defteam_timeouts_remaining",
  "qtr",
  "era1",
  "era2",
  "era3",
  "era4"
)

xpass_booster <- function() {
  load_sysdata_booster(
    "xpass",
    xpass_model$trees_raw,
    xpass_model$xgboost_version,
    "expected pass model"
  )
}
