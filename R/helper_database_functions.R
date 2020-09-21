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
#' The \code{force_rebuild} parameter controls if the data table within the
#' database (not the database itself because there might be other data in it)
#' should be removed and rebuilt from scratch (mostly because of bugfixes in
#' nflfastR or its underlying data)
#'
#' @param dbdir Directory in which the database is or shall be located
#' @param dbname File name of an existing or desired SQLite database within \code{dbdir}
#' @param tblname The name of the play by play data table within the database
#' @param force_rebuild Logical parameter to rebuild the play by play data table
#' within the database from scratch in case the cleaned data were updated
#' @importFrom rlang .data
#' @export
update_db <- function(dbdir = ".",
                      dbname = "pbp_db",
                      tblname = "nflfastR_pbp",
                      force_rebuild = FALSE) {

  if (!requireNamespace("DBI", quietly = TRUE) |
    !requireNamespace("RSQLite", quietly = TRUE)) {
    stop("Packages \"DBI\" and \"RSQLite\" needed for database communication. Please install them.")
  }

  # create path to db
  db <- glue::glue("{dbdir}/{dbname}")

  # create db if it doesn't exist or user forces rebuild
  if (!file.exists(db)) {
    message(glue::glue("Can't find database {db}. Will try to create it and load the play by play data into the data table \"{tblname}\"."))
    build_db(dbdir, dbname, tblname)
  } else if (file.exists(db) & force_rebuild) {
    message(glue::glue("Start rebuilding the data table \"{tblname}\" in your database {db}."))
    build_db(dbdir, dbname, tblname)
  }

  # get completed games using Lee's file (thanks Lee!)
  message("Checking for missing completed games...")
  completed_games <- readRDS(url("https://github.com/leesharpe/nfldata/blob/master/data/games.rds?raw=true")) %>%
    # completed games since 1999, excluding the broken games
    dplyr::filter(.data$season >= 1999, !is.na(.data$result), !.data$game_id %in% c("1999_01_BAL_STL", "2000_06_BUF_MIA", "2000_03_SD_KC")) %>%
    dplyr::arrange(.data$gameday) %>%
    dplyr::pull(.data$game_id)

  #message("Connecting to database...")
  connection <- DBI::dbConnect(RSQLite::SQLite(), db)

  # function below
  missing <- get_missing_games(completed_games, connection, tblname)

  # rebuild db if number of missing games is too large
  if(length(missing) >= 50) {
    DBI::dbDisconnect(connection)
    message("The number of missing games is so large that rebuilding the database is more efficient.")
    build_db(dbdir, dbname, tblname)
    connection <- DBI::dbConnect(RSQLite::SQLite(), db)
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

    message(glue::glue("Starting download of {length(missing)} games ..."))
    new_pbp <- fast_scraper(missing, pp = is_installed_furrr) %>%
      clean_pbp() %>%
      add_qb_epa() %>%
      add_xyac()

    if (nrow(new_pbp) == 0) {
      message("Raw data of new games are not yet ready. Please try again in about 10 minutes.")
    } else {
      message("Appending new data to database...")
      RSQLite::dbWriteTable(connection, tblname, new_pbp, append = TRUE)
    }
  }

  DBI::dbDisconnect(connection)
  message("Procedure completed.")
}

# this is a helper function to build nflfastR database from Scratch
build_db <- function(dbdir = ".", dbname = "pbp_db", tblname = "nflfastR_pbp") {
  if (!dir.exists(dbdir)) {
    message(glue::glue("Directory {dbdir} doesn't exist yet. Try creating..."))
    dir.create(dbdir)
  }

  db <- glue::glue("{dbdir}/{dbname}")

  #message("Connecting to database...")
  connection <- DBI::dbConnect(RSQLite::SQLite(), db)

  if (DBI::dbExistsTable(connection, tblname)) {
    message(glue::glue("Purging old {tblname} table from database..."))
    DBI::dbRemoveTable(connection, tblname)
  }

  #message("Checking for completed games...")
  games <- readRDS(url("https://github.com/leesharpe/nfldata/blob/master/data/games.rds?raw=true"))
  complete_games <- games %>%
    dplyr::filter(.data$season >= 1999 & !is.na(.data$result)) %>%
    dplyr::arrange(.data$gameday)

  seasons <- 1999:dplyr::last(complete_games$season)
  message(glue::glue("Starting download of {length(seasons)} seasons between {min(seasons)} and {max(seasons)}..."))
  progressr::with_progress({
    p <- progressr::progressor(along = seasons)
    purrr::walk(seasons, load_cleaned_pbp, connection, p, tblname)
  })
  DBI::dbDisconnect(connection)
  #message("Process completed.")
}

# this is a helper function to add one season of data
# from the data repo to a database connection
load_cleaned_pbp <- function(season, dbConnection, p, tablename) {
  pbp_cleaned <- readRDS(
    url(glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{season}.rds"))
  )
  RSQLite::dbWriteTable(dbConnection, tablename, pbp_cleaned, append = TRUE)
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
