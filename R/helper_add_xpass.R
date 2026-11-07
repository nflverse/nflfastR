################################################################################
# Author: Ben Baldwin
# Stlyeguide: styler::tidyverse_style()
################################################################################

#' Add expected pass columns
#'
#' @inheritParams clean_pbp
#' @description Build columns from the expected dropback model. Will return
#' `NA` on data prior to 2006 since that was before NFL started marking scrambles.
#' Must be run on a dataframe that has already had [clean_pbp()] run on it.
#' Note that the functions [build_nflfastR_pbp()] and
#' the database function [update_db()] already include this function.
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
      load_model("xpass"),
      model_matrix(
        subset_columns(plays, valid, xpass_model_features),
        xpass_model_features
      )
    )
    pass_oe <- data.table::fifelse(
      !is.na(xpass),
      100 * (d$pass - xpass),
      NA_real_
    )
    pass_oe <- data.table::fifelse(d$rush == 0 & d$pass == 0, NA_real_, pass_oe)
    data.table::set(d, j = c("xpass", "pass_oe"), value = list(xpass, pass_oe))

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
  # a list of columns, so make_model_mutations() doesn't change pbp
  plays <- make_model_mutations(as.list(pbp))
  plays$valid_play <- data.table::fifelse(
    plays$season >= 2006 &
      plays$play_type %chin% c("no_play", "pass", "run") &
      !is.na(plays$posteam) &
      !is.na(plays$down) &
      !is.na(plays$defteam_timeouts_remaining) &
      !is.na(plays$posteam_timeouts_remaining) &
      !is.na(plays$yardline_100) &
      !is.na(plays$score_differential),
    1,
    0
  )
  plays
}

xpass_model_features <- c(
  "down",
  "ydstogo",
  "yardline_100",
  "qtr",
  "wp",
  "vegas_wp",
  "era2",
  "era3",
  "era4",
  "score_differential",
  "home",
  "half_seconds_remaining",
  "posteam_timeouts_remaining",
  "defteam_timeouts_remaining",
  "outdoors",
  "retractable",
  "dome"
)
