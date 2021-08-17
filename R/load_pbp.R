#' @inherit nflreadr::load_pbp
#' @inheritDotParams nflreadr::load_pbp
#' @param qs `r lifecycle::badge("deprecated")` has no effect and will be
#' removed in a future release.
#' @param ... Arguments passed on to nflreadr::load_pbp
#' @examples
#' \donttest{
#' pbp <- load_pbp(2019:2020)
#' dplyr::glimpse(pbp)
#' }
#' @export
load_pbp <- function(..., qs = lifecycle::deprecated()){
  if (lifecycle::is_present(qs)) {
    lifecycle::deprecate_warn(
      when = "4.3.0",
      what = "load_pbp(qs = )",
      details = cli::cli_text("The {.val qs} argument is deprecated and replaced by {.val file_type} - see {.code ?nflreadr::load_pbp} for details.")
    )
  }
  nflreadr::load_pbp(...)
}
