#' Load complete nflfastR Play by Play Datasets
#'
#' @description Loads multiple seasons from the nflfastR datarepo either into memory
#' or writes them into a database using some forwarded arguments in the dots.
#'
#' @param seasons A vector of 4-digit years associated with given NFL seasons.
#' @param ... Additional arguments passed to an underlying function that writes
#' the season data into a database (used by [update_db()]).
#' @param qs Wheter to use the function [qs::qdeserialize()] for more efficient loading.
#' @return The complete nflfastR dataset as returned by [build_nflfastR_pbp()] for
#' all given `seasons`
#'
#' @seealso For information on parallel processing and progress updates please
#' see [nflfastR].
#' @examples
#' \donttest{
#' pbp <- load_pbp(2019:2020, qs = TRUE)
#' dplyr::glimpse(pbp)
#' }
#' @export
load_pbp <- function(seasons, ..., qs = FALSE) {
  dots <- rlang::dots_list(...)

  if (all(c("dbConnection", "tablename") %in% names(dots))) in_db <- TRUE else in_db <- FALSE

  if (isTRUE(qs) && !is_installed("qs")) {
    cli::cli_abort("Package {.val qs} required for argument {.val qs = TRUE}. Please install it.")
  }

  most_recent <- most_recent_season()

  if (!all(seasons %in% 1999:most_recent)) {
    cli::cli_abort("Please pass valid seasons between 1999 and {most_recent}")
  }

  if (length(seasons) > 1 && is_sequential() && isFALSE(in_db)) {
    cli::cli_alert_info(c(
      "It is recommended to use parallel processing when trying to load multiple seasons.\n",
      "Please consider running {.code future::plan(\"multisession\")}! ",
      "Will go on sequentially..."
    ))
  }

  p <- progressr::progressor(along = seasons)

  if (isFALSE(in_db)) {
    out <- furrr::future_map_dfr(seasons, single_season, p = p, qs = qs)
  }

  if (isTRUE(in_db)) {
    purrr::walk(seasons, single_season, p, ..., qs = qs)
    out <- NULL
  }

  return(out)
}

# Helper function that is called by load_pbp above
single_season <- function(season, p, dbConnection = NULL, tablename = NULL, qs = FALSE) {
  if (isTRUE(qs)) {
    .url <- glue::glue("https://github.com/nflverse/nflfastR-data/blob/master/data/play_by_play_{season}.qs?raw=true")
    pbp <- qs_from_url(.url)
  }
  if (isFALSE(qs)) {
    .url <- glue::glue("https://github.com/nflverse/nflfastR-data/blob/master/data/play_by_play_{season}.rds?raw=true")
    con <- url(.url)
    pbp <- readRDS(con)
    close(con)
  }
  if (!is.null(dbConnection) && !is.null(tablename)) {
    DBI::dbWriteTable(dbConnection, tablename, pbp, append = TRUE)
    out <- NULL
  } else {
    out <- pbp
  }
  p(sprintf("season=%g", season))
  return(out)
}
