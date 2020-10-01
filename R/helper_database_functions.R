#' Update or create a nflfastR play by play database
#'
#' \code{update_db} updates or creates a database with \code{nflfastR}
#' play by play data of all completed games since 1999.
#'
#' @details This function creates and updates a data table with the name \code{tblname}
#' within a SQLite database (other drivers via \code{db_connection}) located in
#' \code{dbdir} and named \code{dbname}.
#' The data table combines all play by play data for every available game back
#' to the 1999 season and adds the most recent completed games as soon as they
#' are available for \code{nflfastR}.
#'
#' The argument \code{force_rebuild} is of hybrid type. It can rebuild the play
#' by play data table either for the whole nflfastR era (with \code{force_rebuild = TRUE})
#' or just for specified seasons (e.g. \code{force_rebuild = c(2019, 2020)}).
#' Please note the following behavior:
#' \itemize{
#'  \item{\code{force_rebuild = TRUE}}{: The data table with the name \code{tblname}
#'   will be removed completely and rebuilt from scratch. This is helpful when
#'   new columns are added during the Off-Season.}
#'  \item{\code{force_rebuild = c(2019, 2020)}}{: The data table with the name \code{tblname}
#'  will be preserved and only rows from the 2019 and 2020 seasons will be
#'  deleted and re-added. This is intended to be used for ongoing seasons because
#'  the NFL fixes bugs in the underlying data during the week and we recommend
#'  rebuilding the current season every Thursday during the season.}
#' }
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
    usethis::ui_stop("Packages {usethis::ui_value('DBI')} and {usethis::ui_value('RSQLite')} needed for database communication. Please install them.")
  }

  if (any(force_rebuild == "NEW")) {
    usethis::ui_stop("The argument {usethis::ui_value('force_rebuild = NEW')} is only for internal usage!")
  }

  if (!(is.logical(force_rebuild) | is.numeric(force_rebuild))) {
    usethis::ui_stop("The argument {usethis::ui_value('force_rebuild')} has to be either logical or numeric!")
  }

  if (!dir.exists(dbdir) & is.null(db_connection)) {
    usethis::ui_oops("Directory {usethis::ui_path(dbdir)} doesn't exist yet. Try creating...")
    dir.create(dbdir)
  }

  if (is.null(db_connection)) {
    connection <- DBI::dbConnect(RSQLite::SQLite(), glue::glue("{dbdir}/{dbname}"))
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
  usethis::ui_todo("Checking for missing completed games...")
  completed_games <- readRDS(url("https://github.com/leesharpe/nfldata/blob/master/data/games.rds?raw=true")) %>%
    # completed games since 1999, excluding the broken games
    dplyr::filter(.data$season >= 1999, !is.na(.data$result), !.data$game_id %in% c("1999_01_BAL_STL", "2000_06_BUF_MIA", "2000_03_SD_KC")) %>%
    dplyr::arrange(.data$gameday) %>%
    dplyr::pull(.data$game_id)

  # function below
  missing <- get_missing_games(completed_games, connection, tblname)

  # rebuild db if number of missing games is too large
  if(length(missing) > 5) {
    # message("The number of missing games is so large that rebuilding the database is more efficient.")
    build_db(tblname, connection, show_message = FALSE, rebuild = as.numeric(unique(stringr::str_sub(missing, 1, 4))))
    missing <- get_missing_games(completed_games, connection, tblname)
  }

  # if there's missing games, scrape and write to db
  if (length(missing) > 0) {
    if (!requireNamespace("furrr", quietly = TRUE)) {
      is_installed_furrr <- FALSE
      usethis::ui_info("Package {usethis::ui_value('furrr')} not installed. Can't use parallel processing. Please consider installing it.")
      usethis::ui_info("Will go on sequentially...")
    } else {
      is_installed_furrr <- TRUE
    }

    # prevent the fast_scraper() warning for pp = TRUE with less than 5 games
    if (is_installed_furrr == TRUE & length(missing) < 5) {
      is_installed_furrr <- FALSE
    }

    usethis::ui_todo("Starting download of {length(missing)} game(s) ...")
    new_pbp <- fast_scraper(missing, pp = is_installed_furrr) %>%
      clean_pbp() %>%
      add_qb_epa() %>%
      add_xyac()

    if (nrow(new_pbp) == 0) {
      usethis::ui_oops("Raw data of new games are not yet ready. Please try again in about 10 minutes.")
    } else {
      usethis::ui_todo("Appending new data to database...")
      DBI::dbWriteTable(connection, tblname, new_pbp, append = TRUE)
    }
  }

  usethis::ui_info("Path to your db: {usethis::ui_path(DBI::dbGetInfo(connection)$dbname)}")
  DBI::dbDisconnect(connection)
  usethis::ui_done("{usethis::ui_field('Database update completed.')}")
}

# this is a helper function to build nflfastR database from Scratch
build_db <- function(tblname = "nflfastR_pbp", db_conn, rebuild = FALSE, show_message = TRUE) {

  valid_seasons <- readRDS(url("https://github.com/leesharpe/nfldata/blob/master/data/games.rds?raw=true")) %>%
    dplyr::filter(.data$season >= 1999 & !is.na(.data$result)) %>%
    dplyr::group_by(.data$season) %>%
    dplyr::summarise() %>%
    dplyr::ungroup()

  if (all(rebuild == TRUE)) {
    usethis::ui_todo("Purging the complete data table {usethis::ui_value(tblname)} in your connected database...")
    DBI::dbRemoveTable(db_conn, tblname)
    seasons <- valid_seasons %>% dplyr::pull("season")
    usethis::ui_todo("Starting download of {length(seasons)} seasons between {min(seasons)} and {max(seasons)}...")
  } else if (is.numeric(rebuild) & all(rebuild %in% valid_seasons$season)) {
    string <- paste0(rebuild, collapse = ", ")
    if (show_message){usethis::ui_todo("Purging {string} season(s) from the data table {usethis::ui_value(tblname)} in your connected database...")}
    DBI::dbExecute(db_conn, glue::glue_sql("DELETE FROM {`tblname`} WHERE season IN ({vals*})", vals = rebuild, .con = db_conn))
    seasons <- valid_seasons %>% dplyr::filter(.data$season %in% rebuild) %>% dplyr::pull("season")
    usethis::ui_todo("Starting download of the {string} season(s)...")
  } else if (all(rebuild == "NEW")) {
    usethis::ui_info("Can't find the data table {usethis::ui_value(tblname)} in your database. Will load the play by play data from scratch.")
    seasons <- valid_seasons %>% dplyr::pull("season")
    usethis::ui_todo("Starting download of {length(seasons)} seasons between {min(seasons)} and {max(seasons)}...")
  } else {
    seasons <- NULL
    usethis::ui_oops("At least one invalid value passed to argument {usethis::ui_code('force_rebuild')}. Please try again with valid input.")
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

  usethis::ui_info("You have {length(db_ids)} games and are missing {length(need_scrape)}.")
  return(need_scrape)
}
