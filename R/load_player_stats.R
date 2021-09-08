#' @inherit nflreadr::load_player_stats
#' @inheritDotParams nflreadr::load_player_stats
#' @param qs `r lifecycle::badge("deprecated")` has no effect and will be
#' removed in a future release.
#' @param ... Arguments passed on to nflreadr::load_player_stats
#'
#' @seealso The function [calculate_player_stats()] and the corresponding examples
#' on [the nflfastR website](https://www.nflfastr.com/articles/nflfastR.html#example-11-replicating-official-stats)
#' @examples
#' \donttest{
#' stats <- load_player_stats()
#' dplyr::glimpse(stats)
#' }
#' @export
load_player_stats <- function(..., qs = lifecycle::deprecated()){
  if (lifecycle::is_present(qs)) {
    lifecycle::deprecate_warn(
      when = "4.3.0",
      what = "load_pbp(qs = )",
      details = cli::cli_text("The {.val qs} argument is deprecated and replaced by {.val file_type} - see {.code ?nflreadr::load_player_stats} for details.")
    )
  }

  # if the dots are empty, we now have the same behavior like nflreadr which
  # differs from the previous versions where it was "load all seasons"
  if (rlang::is_empty(list(...))){
    cli::cli_warn(
      c("We have changed the behavior of {.var load_player_stats()} as of nflfastR 4.3.0.",
        "Calling it without an argument will return the current season only instead of all available seasons.",
        "Please try {.var load_player_stats(seasons = TRUE)} to get all seasons."
      ),
      .frequency = "regularly", .frequency_id = "player_stats_warning"
    )
  }

  # if dots are not empty, use them in nflreadr
  nflreadr::load_player_stats(...)
}
