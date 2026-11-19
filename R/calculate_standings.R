#' Compute Division Standings and Conference Seeds from Play by Play
#'
#' @description
#' `r lifecycle::badge("defunct")`
#'
#' This function is defunct. Please use [nflseedR::nfl_standings()] instead.
#'
#' @param nflverse_object,tiebreaker_depth,playoff_seeds Defunct.
#'
#' @keywords internal
#' @export
calculate_standings <- function(
  nflverse_object,
  tiebreaker_depth = 3,
  playoff_seeds = NULL
) {
  lifecycle::deprecate_stop(
    "5.1.0",
    "calculate_standings()",
    "nflseedR::nfl_standings()"
  )
}
