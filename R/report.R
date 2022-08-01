#' Get a Situation Report on System, nflverse Package Versions and Dependencies
#'
#' @description This function gives a quick overview of the versions of R and
#'   the operating system as well as the versions of nflverse packages, options,
#'   and their dependencies. It's primarily designed to help you get a quick
#'   idea of what's going on when you're helping someone else debug a problem.
#' @details See [`nflreadr::nflverse_sitrep`] for details.
#' @name report
#' @examples
#' \donttest{
#' report(recursive = FALSE)
#' report(pkg = "nflreadr", recursive = TRUE)
#' }
NULL

#' @export
report <- nflreadr::nflverse_sitrep
