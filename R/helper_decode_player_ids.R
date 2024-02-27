################################################################################
# Author: Sebastian Carl
# Purpose: Function to decode play-by-play player IDs.
# Code Style Guide: styler::tidyverse_style()
################################################################################

#' Decode the player IDs in nflfastR play-by-play data
#'
#' @inheritParams clean_pbp
#' @param fast If `TRUE` the IDs will be decoded with the high efficient
#' function [decode_ids][gsisdecoder::decode_ids]. If `FALSE` an nflfastR internal
#' function will be used for decoding (it is generally not recommended to do this,
#' unless there is a problem with [decode_ids][gsisdecoder::decode_ids]
#' which can take several days to fix on CRAN.)
#'
#' @description Takes all columns ending with \code{'player_id'} as well as the
#' variables \code{'passer_id'}, \code{'rusher_id'}, \code{'fantasy_id'},
#' \code{'receiver_id'}, and \code{'id'} of an nflfastR play-by-play data set
#' and decodes the player IDs to the commonly known GSIS ID format 00-00xxxxx.
#'
#' The function uses by default the high efficient [decode_ids][gsisdecoder::decode_ids]
#' of the package [`gsisdecoder`](https://cran.r-project.org/package=gsisdecoder).
#' In the unlikely event that there is a problem with this function, an nflfastR
#' internal decoder can be used with the option `fast = FALSE`.
#'
#' The 2022 play by play data introduced new player IDs that can't be decoded
#' with gsisdecoder. In that case, IDs are joined through [nflreadr::load_players].
#'
#' @return The input data frame of the parameter `pbp` with decoded player IDs.
#' @export
#' @examples
#' \donttest{
#' # Decode data frame consisting of some names and ids
#' decode_player_ids(data.frame(
#'   name = c("P.Mahomes", "B.Baldwin", "P.Mahomes", "S.Carl", "J.Jones"),
#'   id = c(
#'     "32013030-2d30-3033-3338-3733fa30c4fa",
#'     NA_character_,
#'     "00-0033873",
#'     NA_character_,
#'     "32013030-2d30-3032-3739-3434d4d3846d"
#'   )
#' ))
#' }
decode_player_ids <- function(pbp, ..., fast = TRUE) {
  # need newer version of nflreadr to use load_players
  rlang::check_installed("nflreadr (>= 1.3.0)", "to decode player IDs.")

  if (isFALSE(fast)) {

    if (nrow(pbp) > 1000 && is_sequential()) {
      cli::cli_alert_info(c(
        "It is recommended to use parallel processing when trying to to decode big data frames.",
        "Please consider running {.code future::plan(\"multisession\")}! ",
        "Will go on sequentially..."
      ))
    }
    decode_gsis <- decode_ids

  } else if (isTRUE(fast)) {

    rlang::check_installed("gsisdecoder", "to run fast decoding of player IDs.")
    decode_gsis <- gsisdecoder::decode_ids

  }

  user_message("Decode player ids...", "todo")

  players <- nflreadr::load_players()

  id_vector <- players$gsis_id
  names(id_vector) <- players$esb_id

  ret <- pbp %>%
    dplyr::mutate_at(
      dplyr::vars(
        tidyselect::any_of(c("passer_id", "rusher_id", "receiver_id", "id", "fantasy_id")),
        tidyselect::ends_with("player_id")
      ),
      function(id, id_vec = id_vector){
        chars <- nchar(id)
        dplyr::case_when(
          is.na(chars) ~ NA_character_,
          # this means it's gsis ID. 30 30 2d 30 30 translates to 00-00
          stringr::str_sub(id, 5, 16) == "3030-2d30-30" ~ decode_gsis(id),
          # if it's not gsis, it is likely elias. We drop names to avoid confusion
          nchar(id) == 36 ~ unname(id_vec[extract_elias(id, decoder = decode_gsis)]),
          TRUE ~ id
        )
      }
    )

  message_completed("Decoding of player ids completed", ...)

  ret
}

decode_ids <- function(var) {
  furrr::future_map_chr(var, convert_to_gsis_id)
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

extract_elias <- function(smart_id, decoder){
  name_abbr <- decoder(smart_id) %>% substr(1,3)
  id_no <- stringr::str_remove_all(smart_id, "-") %>%
    stringr::str_sub(11, 16)
  elias_id <- paste0(name_abbr, id_no)
  elias_id
}
