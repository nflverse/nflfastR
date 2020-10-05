################################################################################
# Author: Sebastian Carl
# Purpose: Wrapper around multiple nflfastR functions
# Code Style Guide: styler::tidyverse_style()
################################################################################

# The idea for this wrapper as well as some helper functions and the documentation
# style are heavily borrowed from the r-lib package pkgdown (https://github.com/r-lib/pkgdown/blob/master/R/build.r)

#' Build a Complete nflfastR Data Set
#'
#' @description
#' \code{build_nflfastR_pbp} is a convenient wrapper around 5 nflfastR functions:
#'
#' \itemize{
#'  \item{\code{\link{fast_scraper}}}
#'  \item{\code{\link{clean_pbp}}}
#'  \item{\code{\link{add_qb_epa}}}
#'  \item{\code{\link{add_xyac}}}
#'  \item{\code{\link{decode_player_ids}}}
#' }
#'
#' Please see the documentation of each function to learn about the output.
#'
#' @inheritParams fast_scraper
#' @param decode If \code{TRUE}, the function \code{\link{decode_player_ids}} will be executed.
#' @param rules If \code{FALSE}, printing of the header and footer in the console output will be suppressed.
#' @return An nflfastR play-by-play data frame like it can be loaded from \url{https://github.com/guga31bb/nflfastR-data}.
#' @details To load valid game_ids please use the package function \code{\link{fast_scraper_schedules}}.
#'
#' The \code{source} parameter controls from which source the data is being
#' loaded. The old parameters \code{rs} as well as \code{gc}
#' are not valid anymore. Please use \code{nfl} or \code{old}.
#'
#' The \code{pp} parameter controls if the scraper should use parallel processing.
#' Please note that the initiating process takes a few seconds which means it
#' may be better to set \code{pp = FALSE} if you are scraping just a few games.
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
  if (rules) rule_header("Build nflfastR Play-by-Play Data")

  game_count <- length(game_ids)
  builder <- TRUE

  if (game_count > 1) {
    usethis::ui_todo("Start download of {game_count} games...")
  } else {
    usethis::ui_todo("Start download of {game_count} game...")
  }

  ret <- fast_scraper(game_ids = game_ids, source = source, pp = pp, ..., in_builder = builder) %>%
    clean_pbp(in_builder = builder) %>%
    add_qb_epa(in_builder = builder) %>%
    add_xyac(in_builder = builder) #%>%
    # add_xpass(in_builder = builder)

  if (decode) {
    ret <- decode_player_ids(ret, in_builder = builder)
  }

  if (rules) rule_footer("DONE")

  return(ret)
}
