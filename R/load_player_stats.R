#' Load Official Game Stats
#'
#' @description Loads weekly stats for all passers, rushers and receivers in the
#' nflfastR play-by-play data from the 1999 season to the most recent season
#'
#' @param qs Whether to use the function [qs::qdeserialize()] for more efficient loading.
#' @return Weekly stats for all passers, rushers and receivers in the nflfastR
#' play-by-play data from the 1999 season to the most recent season
#'
#' @seealso The function [calculate_player_stats()] and the corresponding examples
#' on [the nflfastR website](https://www.nflfastr.com/articles/nflfastR.html#example-11-replicating-official-stats)
#' @examples
#' \donttest{
#' stats <- load_player_stats()
#' dplyr::glimpse(stats)
#' }
#' @export
load_player_stats <- function(qs = FALSE) {

  if (isTRUE(qs) && !is_installed("qs")) {
    cli::cli_abort("Package {.val qs} required for argument {.val qs = TRUE}. Please install it.")
  }

  if (isTRUE(qs)) {
    .url <- "https://github.com/nflverse/nflfastR-data/blob/master/data/player_stats.qs?raw=true"
    out <- qs_from_url(.url)
  } else {
    .url <- "https://github.com/nflverse/nflfastR-data/blob/master/data/player_stats.rds?raw=true"
    con <- url(.url)
    out <- readRDS(con)
    close(con)
  }

  return(out)
}
