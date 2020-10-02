################################################################################
# Author: Sebastian Carl
# Purpose: Wrapper around multiple nflfastR functions
# Code Style Guide: styler::tidyverse_style()
################################################################################

# The idea for this wrapper as well as some helper functions and the documentation
# style are heavily borrowed from the r-lib package pkgdown (https://github.com/r-lib/pkgdown/blob/master/R/build.r)

#' Build a complete nflfastR data set
#'
#' @description
#' `build_nflfastR_pbp()` is a convenient wrapper around 5 nflfastR functions:
#'
#' * [fast_scraper()]
#' * [clean_pbp()]
#' * [add_qb_epa()]
#' * [add_xyac()]
#' * [decode_player_ids()]
#'
#' Please see the documentation of each function to learn about the output.
#'
#' @inheritParams fast_scraper
#' @param decode If \code{TRUE}, the function \code{\link{decode_player_ids}} will be executed.
#' @param rules If \code{FALSE}, printing of the header and footer in the console output will be suppressed.
#' @return An nflfastR play-by-play data frame with like it can be loaded from \url{https://github.com/guga31bb/nflfastR-data}.
#' @importFrom rlang .data
#' @importFrom dplyr mutate_at vars mutate pull
#' @importFrom tidyselect any_of ends_with
#' @importFrom tibble tibble
#' @importFrom stringr str_sub str_replace_all str_length
#' @export
#' @examples
#' \donttest{
#' # Build nflfastR pbp for the 2018 and 2019 Super Bowls
#' build_nflfastR_pbp(c("2018_21_NE_LA", "2019_21_SF_KC"))
#' }
build_nflfastR_pbp <- function(game_ids, source = "nfl", pp = FALSE, ..., decode = FALSE, rules = TRUE) {
  if (rules) {
    rule("Build nflfastR play-by-play data")
  }

  game_count <- length(game_ids)
  builder <- TRUE

  if (game_count > 1) {
    usethis::ui_todo("Start download of {length(game_ids)} games...")
  } else {
    usethis::ui_todo("Start download of {length(game_ids)} game...")
  }

  ret <- fast_scraper(game_ids = game_ids, source = source, pp = pp, ..., in_builder = builder) %>%
    clean_pbp(in_builder = builder) %>%
    add_qb_epa(in_builder = builder) %>%
    add_xyac(in_builder = builder)

  if (decode) {
    ret <- decode_player_ids(ret, in_builder = builder)
  }

  if (rules) {
    rule("DONE")
  }

  return(ret)
}
