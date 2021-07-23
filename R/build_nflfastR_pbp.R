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
#' `build_nflfastR_pbp` is a convenient wrapper around 6 nflfastR functions:
#'
#' \itemize{
#'  \item{[fast_scraper()]}
#'  \item{[clean_pbp()]}
#'  \item{[add_qb_epa()]}
#'  \item{[add_xyac()]}
#'  \item{[add_xpass()]}
#'  \item{[decode_player_ids()]}
#' }
#'
#' Please see either the documentation of each function or
#' [the nflfastR Field Descriptions website](https://www.nflfastr.com/articles/field_descriptions.html)
#' to learn about the output.
#'
#' @inheritParams fast_scraper
#' @param decode If `TRUE`, the function [decode_player_ids()] will be executed.
#' @param rules If `FALSE`, printing of the header and footer in the console output will be suppressed.
#' @return An nflfastR play-by-play data frame like it can be loaded from <https://github.com/nflverse/nflfastR-data>.
#' @details To load valid game_ids please use the package function [fast_scraper_schedules()].
#' @seealso For information on parallel processing and progress updates please
#' see [nflfastR].
#' @export
#' @examples
#' \donttest{
#' # Build nflfastR pbp for the 2018 and 2019 Super Bowls
#' build_nflfastR_pbp(c("2018_21_NE_LA", "2019_21_SF_KC"))
#'
#' # It is also possible to directly use the
#' # output of `fast_scraper_schedules` as input
#' library(dplyr, warn.conflicts = FALSE)
#' fast_scraper_schedules(2020) %>%
#'   tail(3) %>%
#'   build_nflfastR_pbp()
#'
#' \dontshow{
#' # Close open connections for R CMD Check
#' future::plan("sequential")
#' }
#' }
build_nflfastR_pbp <- function(game_ids,
                               source = lifecycle::deprecated(),
                               pp = lifecycle::deprecated(),
                               ...,
                               decode = TRUE,
                               rules = TRUE) {

  if (lifecycle::is_present(source)) {
    lifecycle::deprecate_warn(
      when = "4.0.0",
      what = "build_nflfastR_pbp(source = )",
      details = "The source argument isn't used anymore and will be dropped in a future release."
    )
  }

  if (lifecycle::is_present(pp)) {
    lifecycle::deprecate_warn(
      when = "4.0.0",
      what = "build_nflfastR_pbp(pp = )",
      details = cli::cli_text(c(
        "We have dropped the in-package activation of parallel processing as ",
        "this is considered bad practice.\n",
        "Please choose an appropriate plan before calling the function, e.g. ",
        "{.code future::plan(\"multisession\")}"
      ))
    )
  }

  if (!is.vector(game_ids) && is.data.frame(game_ids)) game_ids <- game_ids$game_id

  if (!is.vector(game_ids)) cli::cli_abort("Param {.code game_ids} is not a valid vector!")

  if (isTRUE(decode) && !is_installed("gsisdecoder")) {
    cli::cli_abort("Package {.val gsisdecoder} required for decoding. Please install it with {.code install.packages(\"gsisdecoder\")}.")
  }

  if (isTRUE(rules)) rule_header("Build nflfastR Play-by-Play Data")

  game_count <- ifelse(is.vector(game_ids), length(game_ids), nrow(game_ids))
  builder <- TRUE

  if (game_count > 1) {
    cli::cli_ul("{my_time()} | Start download of {game_count} games...")
  } else {
    cli::cli_ul("{my_time()} | Start download of {game_count} game...")
  }

  ret <- fast_scraper(game_ids = game_ids, source = source, pp = pp, ..., in_builder = builder) %>%
    clean_pbp(in_builder = builder) %>%
    add_qb_epa(in_builder = builder) %>%
    add_xyac(in_builder = builder) %>%
    add_xpass(in_builder = builder)

  if (isTRUE(decode)) {
    ret <- decode_player_ids(ret, in_builder = builder)
  }

  if (isTRUE(rules)) rule_footer("DONE")

  return(ret)
}
