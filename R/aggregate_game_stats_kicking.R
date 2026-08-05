#' Summarize Kicking Stats
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function was deprecated because we have a new, much better and
#' harmonized approach in [`calculate_stats()`].
#'
#' @param ... Leave this empty. The function is deprecated
#' @return Nothing. Function is deprecated.
#' @export
#' @keywords internal
#' @examples
#' \donttest{
#' try({# to avoid CRAN test problems
#'  # calculate_player_stats_kicking()
#' })
#' }
calculate_player_stats_kicking <- function(...) {
  lifecycle::deprecate_stop(
    "5.0 (2024)",
    "calculate_player_stats_kicking()",
    "calculate_stats()"
  )
}
