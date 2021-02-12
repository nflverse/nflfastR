################################################################################
# Author: Sebastian Carl
# Purpose: Function to decode play-by-play player IDs.
# Code Style Guide: styler::tidyverse_style()
################################################################################

#' Decode the player IDs in nflfastR play-by-play data
#'
#' @inheritParams clean_pbp
#' @param fast If \code{TRUE} the IDs will be decoded with the high efficient
#' function \link[gsisdecoder]{decode_ids}. If \code{FALSE} an nflfastR internal
#' function will be used for decoding (it is generally not recommended to do this,
#' unless there is a problem with \link[gsisdecoder]{decode_ids}
#' which can take several days to fix on CRAN.)
#'
#' @description Takes all columns ending with \code{'player_id'} as well as the
#' variables \code{'passer_id'}, \code{'rusher_id'}, \code{'fantasy_id'},
#' \code{'receiver_id'}, and \code{'id'} of an nflfastR play-by-play data set
#' and decodes the player IDs to the commonly known GSIS ID format 00-00xxxxx.
#'
#' The function uses by default the high efficient \link[gsisdecoder]{decode_ids}
#' of the package \href{https://cran.r-project.org/package=gsisdecoder}{\code{gsisdecoder}}.
#' In the unlikely event that there is a problem with this function, an nflfastR
#' internal decoder can be used with the option \code{fast = FALSE}. In that case
#' the function requires the package \code{furrr} if the data frame
#' \code{pbp} has more than 4500 rows.
#'
#' @return The input data frame of the parameter \code{pbp} with decoded player IDs.
#' @importFrom rlang .data
#' @importFrom dplyr mutate_at vars mutate pull
#' @importFrom tidyselect any_of ends_with
#' @importFrom tibble tibble
#' @importFrom stringr str_sub str_replace_all str_length
#' @export
#' @examples
#' \donttest{
#' # Decode data frame consisting of some names and ids
#' decode_player_ids(data.frame(
#'   name = c("P.Mahomes", "B.Baldwin", "P.Mahomes", "S.Carl", "J.Jones"),
#'   id = c(
#'   "32013030-2d30-3033-3338-3733fa30c4fa",
#'   NA_character_,
#'   "00-0033873",
#'   NA_character_,
#'   "32013030-2d30-3032-3739-3434d4d3846d"
#'   )
#' ))
#' }
decode_player_ids <- function(pbp, ..., fast = TRUE) {
  if (!fast) {
    if (!requireNamespace("furrr", quietly = TRUE) & nrow(pbp) > 4500) {
      usethis::ui_stop("Package {usethis::ui_value('furrr')} required to decode big data frames. Please install it with {usethis::ui_code('install.packages(\"furrr\")')}.")
    } else if (requireNamespace("furrr", quietly = TRUE) & nrow(pbp) > 4500) {
      usethis::ui_todo("Start decoding player ids, please wait...")
      future::plan("multiprocess")
      pp <- TRUE
    } else {
      usethis::ui_todo("Start decoding player ids, please wait...")
      pp <- FALSE
    }

    ret <- pbp %>%
      dplyr::mutate_at(
        dplyr::vars(
          tidyselect::any_of(c("passer_id", "rusher_id", "receiver_id", "id", "fantasy_id")),
          tidyselect::ends_with("player_id")
        ),
        decode_ids, pp
      )
  } else if (fast) {
    if (!requireNamespace("gsisdecoder", quietly = TRUE)) {
      usethis::ui_stop("Package {usethis::ui_value('gsisdecoder')} required for fast decoding. Please install it with {usethis::ui_code('install.packages(\"gsisdecoder\")')}.")
    }

    usethis::ui_todo("Start decoding player ids...")

    ret <- pbp %>%
      dplyr::mutate_at(
        dplyr::vars(
          tidyselect::any_of(c("passer_id", "rusher_id", "receiver_id", "id")),
          tidyselect::ends_with("player_id")
        ),
        gsisdecoder::decode_ids
      )
  }

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
