#' Update or Create a nflfastR Play-by-Play Database
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated. Please use [update_pbp_db()] instead.
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
#' @keywords internal
#' @export
update_db <- function(
  dbdir = getOption("nflfastR.dbdirectory", default = "."),
  dbname = "pbp_db",
  tblname = "nflfastR_pbp",
  force_rebuild = FALSE,
  db_connection = NULL
) {
  lifecycle::deprecate_warn("6.1.0", "update_db()", "update_pbp_db()")

  rule_header("Update nflfastR Play-by-Play Database")

  if (
    !is_installed("DBI") |
      (!is_installed("RSQLite") & is.null(db_connection))
  ) {
    cli::cli_abort(
      "{my_time()} | Packages {.pkg DBI} and {.pkg RSQLite} required for database communication. Please install them."
    )
  }

  if (any(force_rebuild == "NEW")) {
    cli::cli_abort(
      "{my_time()} | The argument {.arg force_rebuild = NEW} is only for internal usage!"
    )
  }

  if (!(is.logical(force_rebuild) | is.numeric(force_rebuild))) {
    cli::cli_abort(
      "{my_time()} | The argument {.arg force_rebuild} has to be either logical or numeric!"
    )
  }

  if (!dir.exists(dbdir) & is.null(db_connection)) {
    cli_message(
      "Directory {.file {dbdir}} doesn't exist yet. Try creating...",
      .cli_fct = cli::cli_alert_danger
    )
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
  } else if (
    DBI::dbExistsTable(connection, tblname) & all(force_rebuild != FALSE)
  ) {
    build_db(tblname, connection, rebuild = force_rebuild)
  }

  # get completed games using Lee's file (thanks Lee!)
  cli_message("Checking for missing completed games...", .cli_fct = cli::cli_ul)
  # completed games since 1999, excluding the broken games
  completed_games <- data.table::setDT(load_schedules())[
    season >= 1999 & !is.na(result) & !game_id %chin% missing_raw_games
  ][order(gameday), game_id]

  # function below
  missing <- get_missing_games(completed_games, connection, tblname)

  # rebuild db if number of missing games is too large
  if (length(missing) > 16) {
    # limit set to >16 to make sure this doesn't get triggered on gameday (e.g. week 17)
    build_db(
      tblname,
      connection,
      show_message = FALSE,
      rebuild = as.numeric(unique(stringi::stri_sub(missing, 1, 4)))
    )
    missing <- get_missing_games(completed_games, connection, tblname)
  }

  # if there's missing games, scrape and write to db
  if (length(missing) > 0) {
    new_pbp <- build_nflfastR_pbp(missing, rules = FALSE)

    if (nrow(new_pbp) == 0) {
      cli_message(
        "Raw data of new games are not yet ready. Please try again in about 10 minutes.",
        .cli_fct = cli::cli_alert_danger
      )
    } else {
      cli_message("Appending new data to database...", .cli_fct = cli::cli_ul)
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

  cli_message("Database update completed", .cli_fct = cli::cli_alert_success)
  cli_message("Path to your db: {.file {DBI::dbGetInfo(connection)$dbname}}")
  if (is.null(db_connection)) {
    DBI::dbDisconnect(connection)
  }
  rule_footer("DONE")
}

# this is a helper function to build nflfastR database from Scratch
build_db <- function(
  tblname = "nflfastR_pbp",
  db_conn,
  rebuild = FALSE,
  show_message = TRUE
) {
  valid_seasons <- data.table::setDT(load_schedules())[
    season >= 1999 & !is.na(result),
    sort(unique(season))
  ]

  if (all(rebuild == TRUE)) {
    cli_message(
      "Purging the complete data table {.val {tblname}}
                in your connected database...",
      .cli_fct = cli::cli_ul
    )
    DBI::dbRemoveTable(db_conn, tblname)
    seasons <- valid_seasons
    cli_message(
      "Starting download of {length(seasons)} seasons
                between {min(seasons)} and {max(seasons)}...",
      .cli_fct = cli::cli_ul
    )
  } else if (is.numeric(rebuild) & all(rebuild %in% valid_seasons)) {
    if (show_message) {
      cli_message(
        "Purging
                                  {cli::qty(length(rebuild))}season{?s} {rebuild}
                                  from the data table {.val {tblname}} in your
                                  connected database...",
        .cli_fct = cli::cli_ul
      )
    }
    DBI::dbExecute(
      db_conn,
      glue::glue_sql(
        "DELETE FROM {`tblname`} WHERE season IN ({vals*})",
        vals = rebuild,
        .con = db_conn
      )
    )
    seasons <- valid_seasons[valid_seasons %in% rebuild]
    cli_message(
      "Starting download of the {length(rebuild)}
                season{?s} {rebuild}",
      .cli_fct = cli::cli_ul
    )
  } else if (all(rebuild == "NEW")) {
    cli_message(
      "Can't find the data table {.val {tblname}}
                        in your database. Will load the play by play data from
                        scratch."
    )
    seasons <- valid_seasons
    cli_message(
      "Starting download of {length(seasons)} seasons
                between {min(seasons)} and {max(seasons)}...",
      .cli_fct = cli::cli_ul
    )
  } else {
    seasons <- NULL
    cli_message(
      "At least one invalid value passed to argument {.arg force_rebuild}. Please try again with valid input.",
      .cli_fct = cli::cli_alert_danger
    )
  }

  if (!is.null(seasons)) {
    # this function lives in R/utils.R
    write_pbp(seasons, dbConnection = db_conn, tablename = tblname)
  }
}

# this is a helper function to check a list of completed games
# against the games that exist in a database connection
get_missing_games <- function(completed_games, dbConnection, tablename) {
  db_ids <- DBI::dbGetQuery(
    dbConnection,
    glue::glue_sql(
      "SELECT DISTINCT game_id FROM {`tablename`} WHERE game_id != {dummy}",
      dummy = "9999_99_DEF_TYP",
      .con = dbConnection
    )
  )[["game_id"]]

  need_scrape <- completed_games[
    !completed_games %chin% c(db_ids, "9999_99_DEF_TYP")
  ]

  cli_message(
    "You have {length(db_ids)} game{?s} and are missing {length(need_scrape)}."
  )
  return(need_scrape)
}
