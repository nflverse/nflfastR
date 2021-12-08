#' Get a Situation Report on System, nflverse Package Versions and Dependencies
#'
#' @inherit nflreadr::nflverse_sitrep
#' @examples
#' \donttest{
#' report(recursive = FALSE)
#' report(pkg = "nflreadr", recursive = TRUE)
#' }
#' @export
report <- nflreadr::nflverse_sitrep
