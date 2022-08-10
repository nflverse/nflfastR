#' @inherit nflreadr::load_pbp
#' @inheritDotParams nflreadr::load_pbp
#' @param ... Arguments passed on to nflreadr::load_pbp
#' @examples
#' \donttest{
#' pbp <- load_pbp(2019:2020)
#' dplyr::glimpse(pbp)
#' }
#' @export
load_pbp <- function(...) nflreadr::load_pbp(...)
