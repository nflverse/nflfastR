#' Update or create a nflfastR play by play database
#'
#' \code{update_db} updates or creates a SQLite database with \code{nflfastR}
#' play by play data of all completed games since 1999.
#'
#' @details This function creates and updates a data table with the name \code{tblname}
#' within a SQLite database located in \code{dbdir} and named \code{dbname}.
#' The data table combines all play by play data for every available game back
#' to the 1999 season and adds the most recent completed games as soon as they
#' are available for \code{nflfastR}.
#'
#' The argument \code{force_rebuild} is of hybrid type. It can rebuild the play
#' by play data table either for the whole nflfastR era (with \code{force_rebuild = TRUE})
#' or just for specified seasons (e.g. \code{force_rebuild = c(2018, 2020)}).
#' The latter is intended to be used for running seasons because the NFL fixes
#' bugs in the play by play data during the week and it is recommended to rebuild
#' the current season every Wednesday during the season.
#'
#' The parameter \code{db_connection} is intended for advanced users who want
#' to use other DBI drivers, such as MariaDB, Postgres or odbc. Please note that
#' the arguments \code{dbdir} and \code{dbname} are dropped in case a \code{db_connection}
#' is provided but the argument \code{tblname} will still be used to write the
#' data table into the database.
#'
#' @param dbdir Directory in which the database is or shall be located
#' @param dbname File name of an existing or desired SQLite database within \code{dbdir}
#' @param tblname The name of the play by play data table within the database
#' @param force_rebuild Hybrid parameter (logical or numeric) to rebuild parts
#' of or the complete play by play data table within the database (please see details for further information)
#' @param db_connection A \code{DBIConnection} object, as returned by
#' \code{\link[DBI]{dbConnect}} (please see details for further information)
#' @importFrom rlang .data
#' @export
update_db <- function(dbdir = ".",
                      dbname = "pbp_db",
                      tblname = "nflfastR_pbp",
                      force_rebuild = FALSE,
                      db_connection = NULL) {

  if (!requireNamespace("DBI", quietly = TRUE) |
    (!requireNamespace("RSQLite", quietly = TRUE) & is.null(db_connection))) {
    stop("Packages \"DBI\" and \"RSQLite\" needed for database communication. Please install them.")
  }

  if (!dir.exists(dbdir) & is.null(db_connection)) {
    message(glue::glue("Directory {dbdir} doesn't exist yet. Try creating..."))
    dir.create(dbdir)
  }

  db <- glue::glue("{dbdir}/{dbname}")

  if (is.null(db_connection)) {
    connection <- DBI::dbConnect(RSQLite::SQLite(), db)
  } else {
    connection <- db_connection
  }

  # create db if it doesn't exist or user forces rebuild
  if (!DBI::dbExistsTable(connection, tblname)) {
    build_db(tblname, connection, rebuild = "NEW")
  } else if (DBI::dbExistsTable(connection, tblname) & all(force_rebuild != FALSE)) {
    build_db(tblname, connection, rebuild = force_rebuild)
  }

  # get completed games using Lee's file (thanks Lee!)
  message("Checking for missing completed games...")
  completed_games <- readRDS(url("https://github.com/leesharpe/nfldata/blob/master/data/games.rds?raw=true")) %>%
    # completed games since 1999, excluding the broken games
    dplyr::filter(.data$season >= 1999, !is.na(.data$result), !.data$game_id %in% c("1999_01_BAL_STL", "2000_06_BUF_MIA", "2000_03_SD_KC")) %>%
    dplyr::arrange(.data$gameday) %>%
    dplyr::pull(.data$game_id)

  # function below
  missing <- get_missing_games(completed_games, connection, tblname)

  # rebuild db if number of missing games is too large
  if(length(missing) >= 5) {
    # message("The number of missing games is so large that rebuilding the database is more efficient.")
    build_db(tblname, connection, rebuild = unique(stringr::str_sub(missing, 1, 4)))
    missing <- get_missing_games(completed_games, connection, tblname)
  }

  # if there's missing games, scrape and write to db
  if (length(missing) > 0) {
    if (!requireNamespace("furrr", quietly = TRUE)) {
      is_installed_furrr <- FALSE
      message("Package \"furrr\" not installed. Can't use parallel processing. Please consider installing it.")
      message("Will go on sequentially...")
    } else {
      is_installed_furrr <- TRUE
    }

    # prevent the fast_scraper() warning for pp = TRUE with less than 5 games
    if (is_installed_furrr == TRUE & length(missing) < 5) {
      is_installed_furrr <- FALSE
    }

    message(glue::glue("Starting download of {length(missing)} games ..."))
    new_pbp <- fast_scraper(missing, pp = is_installed_furrr) %>%
      clean_pbp() %>%
      add_qb_epa() %>%
      add_xyac()

    if (nrow(new_pbp) == 0) {
      message("Raw data of new games are not yet ready. Please try again in about 10 minutes.")
    } else {
      message("Appending new data to database...")
      DBI::dbWriteTable(connection, tblname, new_pbp, append = TRUE)
    }
  }

  DBI::dbDisconnect(connection)
  message("Database update completed.")
}

# this is a helper function to build nflfastR database from Scratch
build_db <- function(tblname = "nflfastR_pbp", db_conn, rebuild = FALSE) {

  valid_seasons <- readRDS(url("https://github.com/leesharpe/nfldata/blob/master/data/games.rds?raw=true")) %>%
    dplyr::filter(.data$season >= 1999 & !is.na(.data$result)) %>%
    dplyr::group_by(.data$season) %>%
    dplyr::summarise() %>%
    dplyr::ungroup()

  if (all(rebuild == TRUE)) {
    message(glue::glue("Purging all rows from {tblname} in your connected database..."))
    DBI::dbExecute(db_conn, glue::glue("DELETE FROM {tblname}"))
    seasons <- valid_seasons %>% dplyr::pull("season")
    message(glue::glue("Starting download of {length(seasons)} seasons between {min(seasons)} and {max(seasons)}..."))
  } else if (is.numeric(rebuild) & any(rebuild %in% valid_seasons$season)) {
    string <- paste0(rebuild, collapse = ", ")
    message(glue::glue("Purging {string} season(s) from {tblname} in your connected database..."))
    DBI::dbExecute(db_conn, glue::glue("DELETE FROM {tblname} WHERE season IN ({string})"))
    seasons <- valid_seasons %>% dplyr::filter(.data$season %in% rebuild) %>% dplyr::pull("season")
    message(glue::glue("Starting download of {string} season(s)..."))
  } else if (all(rebuild == "NEW")) {
    message(glue::glue("Can't find the data table '{tblname}' in your database. Will load the play by play data from scratch."))
    # DBI::dbExecute(db_conn, glue::glue("DELETE FROM {tblname}"))
    seasons <- valid_seasons %>% dplyr::pull("season")
    message(glue::glue("Starting download of {length(seasons)} seasons between {min(seasons)} and {max(seasons)}..."))
  } else {
    seasons <- NULL
    message("No valid value passed to argument 'force_rebuild'.")
  }

  if (!is.null(seasons)) {
    progressr::with_progress({
      p <- progressr::progressor(along = seasons)
      purrr::walk(seasons, load_cleaned_pbp, db_conn, p, tblname)
    })
  }
}

# this is a helper function to add one season of data
# from the data repo to a database connection
load_cleaned_pbp <- function(season, dbConnection, p, tablename) {
  pbp_cleaned <- readRDS(
    url(glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{season}.rds"))
  )
  DBI::dbWriteTable(dbConnection, tablename, pbp_cleaned, append = TRUE)
  p(sprintf("season=%g", season))
}

# this is a helper function to check a list of completed games
# against the games that exist in a database connection
get_missing_games <- function(completed_games, dbConnection, tablename) {
  db_ids <- dplyr::tbl(dbConnection, tablename) %>%
    dplyr::select("game_id") %>%
    dplyr::distinct() %>%
    dplyr::collect() %>%
    dplyr::pull("game_id")

  need_scrape <- completed_games[!completed_games %in% db_ids]

  message(glue::glue("You have {length(db_ids)} games and are missing {length(need_scrape)}."))
  return(need_scrape)
}
