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
  pbp <- pbp %>% dplyr::select(-tidyselect::any_of(c("xpass", "pass_oe")))
  plays <- prepare_xpass_data(pbp)

  if (!nrow(plays %>% dplyr::filter(.data$valid_play == 1)) == 0) {
    user_message("Computing xpass...", "todo")

    pred <- stats::predict(
      fastrmodels::xpass_model,
      as.matrix(plays %>% dplyr::select(-"valid_play"))
      ) %>%
      tibble::as_tibble() %>%
      dplyr::rename(xpass = "value") %>%
      dplyr::bind_cols(plays) %>%
      dplyr::select("xpass", "valid_play")

    pbp <- pbp %>%
      dplyr::bind_cols(pred) %>%
      dplyr::mutate(
        xpass = dplyr::if_else(
          .data$valid_play == 1, .data$xpass, NA_real_
        ),
        pass_oe = dplyr::if_else(!is.na(.data$xpass), 100 * (.data$pass - .data$xpass), NA_real_),
        pass_oe = dplyr::if_else(.data$rush == 0 & .data$pass == 0, NA_real_, .data$pass_oe)
      ) %>%
      dplyr::select(-"valid_play")

    message_completed("added xpass and pass_oe", ...)
  } else {
    pbp <- pbp %>%
      dplyr::mutate(
        xpass = NA_real_,
        pass_oe = NA_real_
      )
    user_message("No non-NA values for xpass calculation detected. xpass and pass_oe set to NA", "info")
  }
  return(pbp)
}

prepare_xpass_data <- function(pbp) {

  plays <- pbp %>%
    dplyr::mutate(
      valid_play = dplyr::if_else(
        .data$season >= 2006 &
          .data$play_type %in% c("no_play", "pass", "run") &
          !is.na(.data$posteam) &
          !is.na(.data$down) &
          !is.na(.data$defteam_timeouts_remaining) &
          !is.na(.data$posteam_timeouts_remaining) &
          !is.na(.data$yardline_100) &
          !is.na(.data$score_differential),
        1, 0
      )
    ) %>%
    make_model_mutations() %>%
    dplyr::select(
      "valid_play",
      "down",
      "ydstogo",
      "yardline_100",
      "qtr",
      "wp",
      "vegas_wp",
      "era2", "era3", "era4",
      "score_differential",
      "home",
      "half_seconds_remaining",
      "posteam_timeouts_remaining",
      "defteam_timeouts_remaining",
      "outdoors", "retractable", "dome"
    )

  return(plays)
}
