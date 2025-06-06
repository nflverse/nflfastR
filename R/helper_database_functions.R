################################################################################
# Author: Sebastian Carl
# Purpose: Create and update database with nflfastR pbp data
# Code Style Guide: styler::tidyverse_style()
################################################################################

#' Update or Create a nflfastR Play-by-Play Database
#'
#' `update_db` updates or creates a database with `nflfastR`
#' play by play data of all completed games since 1999.
#'
#' @details This function creates and updates a data table with the name `tblname`
#' within a SQLite database (other drivers via `db_connection`) located in
#' `dbdir` and named `dbname`.
#' The data table combines all play by play data for every available game back
#' to the 1999 season and adds the most recent completed games as soon as they
#' are available for `nflfastR`.
#'
#' The argument `force_rebuild` is of hybrid type. It can rebuild the play
#' by play data table either for the whole nflfastR era (with `force_rebuild = TRUE`)
#' or just for specified seasons (e.g. `force_rebuild = c(2019, 2020)`).
#' Please note the following behavior:
#' * `force_rebuild = TRUE`: The data table with the name `tblname`
#'   will be removed completely and rebuilt from scratch. This is helpful when
#'   new columns are added during the Off-Season.
#' * `force_rebuild = c(2019, 2020)`: The data table with the name `tblname`
#'   will be preserved and only rows from the 2019 and 2020 seasons will be
#'   deleted and re-added. This is intended to be used for ongoing seasons because
#'   the NFL fixes bugs in the underlying data during the week and we recommend
#'   rebuilding the current season every Thursday during the season.
#'
#' The parameter `db_connection` is intended for advanced users who want
#' to use other DBI drivers, such as MariaDB, Postgres or odbc. Please note that
#' the arguments `dbdir` and `dbname` are dropped in case a `db_connection`
#' is provided but the argument `tblname` will still be used to write the
#' data table into the database.
#'
#' @param dbdir Directory in which the database is or shall be located. Can also
#'   be set globally with `options(nflfastR.dbdirectory)`
#' @param dbname File name of an existing or desired SQLite database within `dbdir`
#' @param tblname The name of the play by play data table within the database
#' @param force_rebuild Hybrid parameter (logical or numeric) to rebuild parts
#' of or the complete play by play data table within the database (please see details for further information)
#' @param db_connection A `DBIConnection` object, as returned by
#' [DBI::dbConnect()] (please see details for further information)
#' @export
update_db <- function(dbdir = getOption("nflfastR.dbdirectory", default = "."),
                      dbname = "pbp_db",
                      tblname = "nflfastR_pbp",
                      force_rebuild = FALSE,
                      db_connection = NULL) {

  rule_header("Update nflfastR Play-by-Play Database")

  if (!is_installed("DBI") | !is_installed("purrr") |
      (!is_installed("RSQLite") & is.null(db_connection))) {
    cli::cli_abort("{my_time()} | Packages {.pkg DBI}, {.pkg RSQLite} and {.pkg purrr} required for database communication. Please install them.")
  }

  if (any(force_rebuild == "NEW")) {
    cli::cli_abort("{my_time()} | The argument {.arg force_rebuild = NEW} is only for internal usage!")
  }

  if (!(is.logical(force_rebuild) | is.numeric(force_rebuild))) {
    cli::cli_abort("{my_time()} | The argument {.arg force_rebuild} has to be either logical or numeric!")
  }

  if (!dir.exists(dbdir) & is.null(db_connection)) {
    cli::cli_alert_danger("{my_time()} | Directory {.file {dbdir}} doesn't exist yet. Try creating...")
    dir.create(dbdir)
  }

  if (is.null(db_connection)) {
    connection <- DBI::dbConnect(RSQLite::SQLite(), file.path(dbdir, dbname))
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
  user_message("Checking for missing completed games...", "todo")
  completed_games <- nflreadr::load_schedules() |>
    # completed games since 1999, excluding the broken games
    dplyr::filter(.data$season >= 1999, !is.na(.data$result), !.data$game_id %in% c("1999_01_BAL_STL", "2000_06_BUF_MIA", "2000_03_SD_KC")) |>
    dplyr::arrange(.data$gameday) |>
    dplyr::pull(.data$game_id)

  # function below
  missing <- get_missing_games(completed_games, connection, tblname)

  # rebuild db if number of missing games is too large
  if(length(missing) > 16) {# limit set to >16 to make sure this doesn't get triggered on gameday (e.g. week 17)
    build_db(tblname, connection, show_message = FALSE, rebuild = as.numeric(unique(stringr::str_sub(missing, 1, 4))))
    missing <- get_missing_games(completed_games, connection, tblname)
  }

  # if there's missing games, scrape and write to db
  if (length(missing) > 0) {
    new_pbp <- build_nflfastR_pbp(missing, rules = FALSE)

    if (nrow(new_pbp) == 0) {
      user_message("Raw data of new games are not yet ready. Please try again in about 10 minutes.", "oops")
    } else {
      user_message("Appending new data to database...", "todo")
      DBI::dbWriteTable(connection, tblname, new_pbp, append = TRUE)
    }
  }

  # Remove default play which is just a helper to define columns correctly
  DBI::dbExecute(
    connection,
    glue::glue_sql(
      "DELETE FROM {`tblname`} WHERE game_id IN ({vals*})",
      vals = "9999_99_DEF_TYP",
      .con = connection
      )
    )

  message_completed("Database update completed", in_builder = TRUE)
  cli::cli_alert_info("{my_time()} | Path to your db: {.file {DBI::dbGetInfo(connection)$dbname}}")
  if (is.null(db_connection)) DBI::dbDisconnect(connection)
  rule_footer("DONE")
}

# this is a helper function to build nflfastR database from Scratch
build_db <- function(tblname = "nflfastR_pbp", db_conn, rebuild = FALSE, show_message = TRUE) {

  valid_seasons <- nflreadr::load_schedules() |>
    dplyr::filter(.data$season >= 1999 & !is.na(.data$result)) |>
    dplyr::group_by(.data$season) |>
    dplyr::summarise() |>
    dplyr::ungroup()

  if (all(rebuild == TRUE)) {
    cli::cli_ul("{my_time()} | Purging the complete data table {.val {tblname}}
                in your connected database...")
    DBI::dbRemoveTable(db_conn, tblname)
    seasons <- valid_seasons |> dplyr::pull("season")
    cli::cli_ul("{my_time()} | Starting download of {length(seasons)} seasons
                between {min(seasons)} and {max(seasons)}...")
  } else if (is.numeric(rebuild) & all(rebuild %in% valid_seasons$season)) {
    # s <- glue::glue_collapse(rebuild, sep = ", ", last = ", and ")
    # string <- stringr::str_c(stringr::str_sub(s, 1, 11), "...", stringr::str_sub(s, -16, -1))
    if (show_message){cli::cli_ul("{my_time()} | Purging
                                  {cli::qty(length(rebuild))}season{?s} {rebuild}
                                  from the data table {.val {tblname}} in your
                                  connected database...")}
    DBI::dbExecute(db_conn, glue::glue_sql("DELETE FROM {`tblname`} WHERE season IN ({vals*})", vals = rebuild, .con = db_conn))
    seasons <- valid_seasons |> dplyr::filter(.data$season %in% rebuild) |> dplyr::pull("season")
    cli::cli_ul("{my_time()} | Starting download of the {length(rebuild)}
                season{?s} {rebuild}")
  } else if (all(rebuild == "NEW")) {
    cli::cli_alert_info("{my_time()} | Can't find the data table {.val {tblname}}
                        in your database. Will load the play by play data from
                        scratch.")
    seasons <- valid_seasons |> dplyr::pull("season")
    cli::cli_ul("{my_time()} | Starting download of {length(seasons)} seasons
                between {min(seasons)} and {max(seasons)}...")
  } else {
    seasons <- NULL
    cli::cli_alert_danger("{my_time()} | At least one invalid value passed to argument {.arg force_rebuild}. Please try again with valid input.")
  }

  if (!is.null(seasons)) {
    # this function lives in R/utils.R
    write_pbp(seasons, dbConnection = db_conn, tablename = tblname)
  }
}

# this is a helper function to check a list of completed games
# against the games that exist in a database connection
get_missing_games <- function(completed_games, dbConnection, tablename) {
  db_ids <- dplyr::tbl(dbConnection, tablename) |>
    dplyr::select("game_id") |>
    dplyr::filter(.data$game_id != "9999_99_DEF_TYP") |>
    dplyr::distinct() |>
    dplyr::collect() |>
    dplyr::pull("game_id")

  need_scrape <- completed_games[!completed_games %in% c(db_ids, "9999_99_DEF_TYP")]

  cli::cli_alert_info("{my_time()} | You have {length(db_ids)} game{?s} and are missing {length(need_scrape)}.")
  return(need_scrape)
}
