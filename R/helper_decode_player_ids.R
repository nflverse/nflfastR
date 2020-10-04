################################################################################
# Author: Sebastian Carl
# Purpose: Function to decode play-by-play player IDs.
# Code Style Guide: styler::tidyverse_style()
################################################################################

#' Decode the player IDs in nflfastR play-by-ply data
#'
#' @inheritParams clean_pbp
#' @details Take all columns ending with \code{player_id} as well as
#' \code{passer_id}, \code{rusher_id}, \code{receiver_id}, \code{id} of an
#' nflfastR play-by-play Data set and decode the player IDs to the commonly
#' known GSIS ID format 00-00xxxxx.
#' The function requires the package \code{furrr} if the data frame
#' \code{pbp} has more than 4500 rows.
#' @return The input Data Frame of the parameter 'pbp' with decoded player IDs.
#' @importFrom rlang .data
#' @importFrom dplyr mutate_at vars mutate pull
#' @importFrom tidyselect any_of ends_with
#' @importFrom tibble tibble
#' @importFrom stringr str_sub str_replace_all str_length
#' @export
#' @examples
#' \donttest{
#' # Decode IDs by Patrick Mahomes and Julio Jones
#' decode_player_ids(data.frame(
#'   name = c("P.Mahomes", "J.Jones"),
#'   id = c(
#'     "32013030-2d30-3033-3338-3733fa30c4fa",
#'     "32013030-2d30-3032-3739-3434d4d3846d"
#'   )
#' ))
#' }
decode_player_ids <- function(pbp, ...) {
  usethis::ui_todo("Start decoding player ids, please wait...")
  ret <- pbp %>%
    dplyr::mutate_at(
      dplyr::vars(
        tidyselect::any_of(c("passer_id", "rusher_id", "receiver_id", "id")),
        tidyselect::ends_with("player_id")
      ),
      decode_ids_cpp
    )

  message_completed("Decoding completed.", ...)

  return(ret)
}

decode_ids <- function(var, parproc = FALSE) {
  if (parproc == TRUE) {
    ret <- furrr::future_map_chr(var, convert_to_gsis_id)
  } else {
    ret <- purrr::map_chr(var, convert_to_gsis_id)
  }
  return(ret)
}

convert_to_gsis_id <- function(new_id) {
  if (is.na(new_id) | stringr::str_length(new_id) != 36) {
    ret <- new_id
  } else {
    to_decode <- new_id %>%
      stringr::str_sub(5, -9) %>%
      stringr::str_replace_all("-", "")
    hex_raw <- sapply(seq(1, nchar(to_decode), by = 2), function(x) substr(to_decode, x, x + 1))
    ret <- rawToChar(as.raw(strtoi(hex_raw, 16L)))
  }
  return(ret)
}
