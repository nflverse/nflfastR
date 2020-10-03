################################################################################
# Author: Ben Baldwin
# Stlyeguide: styler::tidyverse_style()
################################################################################

#' Add expected pass columns
#'
#' @inheritParams clean_pbp
#' @description Build columns from the expected dropback model will return \code{NA} on data prior to 2006
#' since that was before NFL started marking scrambles. Must be run on a dataframe that has already had
#' \code{\link{clean_pbp}} run on it, such as data from the data repository or
#' \code{\link{build_nflfastR_pbp}} or built using the database function \code{\link{update_db}}.
#' @return The input Data Frame of the parameter \code{pbp} with the following columns
#' added:
#' \describe{
#' \item{xpass}{Probability of dropback scaled from 0 to 1.}
#' \item{pass_oe}{Dropback percent over expected on a given play scaled from 0 to 100.}
#' }
#' @export
#' @import dplyr
#' @importFrom rlang .data

add_xpass <- function(pbp, ...) {

  plays <- prepare_xpass_data(pbp)

  if (!nrow(plays %>% dplyr::filter(.data$valid_play == 1)) == 0) {

    usethis::ui_todo("Computing xpass...")

    xpass_model <- NULL
    suppressWarnings(
      # load the model from github because it is too big for the package
      try(
        load(url("https://github.com/guga31bb/nflfastR-data/blob/master/models/xpass_model.Rdata?raw=true")),
        silent = TRUE
      )
    )

    if (!is.null(xpass_model)) {
      pred <- stats::predict(xpass_model, as.matrix(plays %>% dplyr::select(-"valid_play"))) %>%
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

    } else {# means xpass_model isn't available
      usethis::ui_oops("This function needs to download the model data from GitHub. Please check your Internet connection and try again!")
    }
  } else {
    pbp <- pbp %>%
      dplyr::mutate(
        xpass = NA_real_,
        pass_oe = NA_real_
      )
    usethis::ui_info("No non-NA values for xpass calculation detected. xpass and pass_oe set to NA")
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
    select(
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
