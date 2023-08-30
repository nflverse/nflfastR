#' Get a Situation Report on System, nflverse Package Versions and Dependencies
#'
#' @description This function gives a quick overview of the versions of R and
#'   the operating system as well as the versions of nflverse packages, options,
#'   and their dependencies. It's primarily designed to help you get a quick
#'   idea of what's going on when you're helping someone else debug a problem.
#' @details See [`nflreadr::nflverse_sitrep`] for details.
#' @inheritDotParams nflreadr::nflverse_sitrep
#' @inherit nflreadr::nflverse_sitrep
#' @examples
#' \donttest{
#' \dontshow{
#' # set CRAN mirror to avoid failing checks in weird scenarios
#' old_ops <- options(repos = c("CRAN" = "https://cran.rstudio.com/"))
#' }
#'
#' report(recursive = FALSE)
#' nflverse_sitrep(pkg = "nflreadr", recursive = TRUE)
#'
#' \dontshow{
#' # restore old options
#' options(old_ops)
#' }
#' }
#' @export
report <- function(...) nflreadr::nflverse_sitrep(...)

#' @export
#' @name nflverse_sitrep
#' @rdname report
#' @importFrom nflreadr nflverse_sitrep
NULL
