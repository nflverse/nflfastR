#' Update or Create a nflverse Play-by-Play Data Table in a Connected Database
#'
#' @description
#' The nflfastR play-by-play era dates back to 1999. To analyze all the data
#' efficiently, there is practically no alternative to working with a database.
#'
#' This function helps to create and maintain a table containing all
#' play-by-play data of the nflfastR era in a connected database.
#' Primarily, the preprocessed data from [load_pbp] is written to the database
#' and, if necessary, supplemented with the latest games using
#' [build_nflfastR_pbp].
#'
#' @param conn A `DBIConnection` object, as returned by [DBI::dbConnect()]
#' @inheritParams rlang::args_dots_empty
#' @inheritParams DBI::dbExistsTable
#' @param seasons Hybrid argument (logical or numeric) to update parts
#' of or the complete play by play table within the database.
#'
#' It can update the play by play data table either for the whole nflfastR era
#' (with `seasons = TRUE`) or just for specified seasons
#' (e.g. `seasons = 2024:2025`).
#'
#' Defaults to [most_recent_season]. Please see details for further information.
#'
#' @details
#' ## The `season` argument
#'
#' The season argument controls how the table in the connected database is
#' handled.
#'
#' With `seasons = TRUE`, the table in argument `name` will be removed completely
#' (by calling [DBI::dbRemoveTable]) and all seasons of the nflfastR era will be
#' added to a fresh table. This is helpful when new columns are added during the
#' offseason.
#'
#' With a numerical vector, e.g. `seasons = 2024:2025`, the table in argument
#' `name` will be preserved and only rows from the given seasons will be deleted
#' and re-added (by calling [DBI::dbAppendTable]). This is intended to be used
#' for ongoing seasons because the NFL fixes bugs in the underlying data during
#' the week and we recommend rebuilding the current season every Thursday during
#' the season.
#'
#' The default behavior is `seasons = most_recent_season()`, which means that
#' only the most recent season is updated or added.
#'
#' To keep the table, and thus also the schema, but update all play-by-play
#' data of the nflfastR era, set
#' ```
#' seasons = seq(1999, most_recent_season())
#' ```
#'
#' If seasons contains multiple seasons, it is possible to control whether the
#' seasons are loaded individually and written to the database, or whether
#' multiple seasons should be processed in chunks. The latter is more efficient
#' because fewer write operations are required, but at the same time, the data
#' must first be stored in memory. The option `“nflfastR.db_chunk_size”` can
#' be used to control how many seasons are loaded together in a chunk and
#' written to the database. With the following option, for example, 5 seasons
#' are always loaded together and written to the database.
#' ```
#' options("nflfastR.db_chunk_size" = 5L)
#' ```
#'
#' @returns Always returns the database connection invisibly.
#' @export
#'
#' @examples
#' \donttest{
#' con <- DBI::dbConnect(duckdb::duckdb())
#' try({# to avoid CRAN test problems
#' update_pbp_db(con, seasons = 2024)
#' })
#' }
update_pbp_db <- function(
  conn,
  ...,
  name = "nflverse_pbp",
  seasons = most_recent_season()
) {
  rlang::check_installed("DBI", "to communicate with databases")
  rlang::check_dots_empty()

  # Validate connection and table name --------------------------------------

  if (!DBI::dbIsValid(conn)) {
    cli::cli_abort(
      "The connection in argument {.arg conn} is invalid. \\
      Do you need to run {.fun DBI::dbConnect}?"
    )
  }

  rule_header("Update nflverse Play-by-Play Data in Connected Database")

  initiated <- FALSE
  if (!DBI::dbExistsTable(conn = conn, name = name)) {
    do_it <- confirm(
      "The data table {.val {name}} does not yet exist in your connected database.
      Do you wish to create it? (Y/n)"
    )
    if (do_it) {
      initiated <- db_initiate_pbp(conn = conn, name = name)
    } else {
      rule_footer("ABORTED")
      return(invisible(conn))
    }
  }

  # Validate seasons --------------------------------------------------------

  if (is.numeric(seasons)) {
    invalid <- setdiff(seasons, valid_seasons())
    if (length(invalid) > 0) {
      cli::cli_abort(
        "The following {cli::qty(length(invalid))} season{?s} {?is/are} \\
        invalid: {.val {invalid}}"
      )
    }
    ret <- db_drop_seasons(conn = conn, name = name, seasons = seasons)
  } else if (isTRUE(seasons)) {
    # We need this block inside if (isTRUE(seasons)) to make sure we run
    # the else block in the right conditions
    if (isFALSE(initiated)) {
      do_it <- confirm(
        "Purge table {.val {name}} in your connected database? (Y/n)"
      )
      if (do_it) {
        ret <- DBI::dbRemoveTable(conn = conn, name = name)
        cli_message("Removed {.val {name}}")
        initiated <- db_initiate_pbp(conn = conn, name = name)
      } else {
        rule_footer("ABORTED")
        return(invisible(conn))
      }
    }
  } else {
    cli::cli_abort(
      "Argument {.arg seasons} must be either a vector of valid \\
      seasons or scalar TRUE"
    )
  }

  seasons <- if (isTRUE(seasons)) valid_seasons() else seasons

  # Append seasons ----------------------------------------------------------
  ret <- db_write_pbp_seasons(conn = conn, name = name, seasons = seasons)

  # Process missing games ---------------------------------------------------
  db_games <- db_query_game_ids(conn = conn, name = name, seasons = seasons)
  completed_games <- completed_game_ids(seasons = seasons)
  missing_games <- setdiff(completed_games, db_games)

  # This block is only relevant on game days
  if (length(missing_games) > 0) {
    # we enter this block if some completed games are missing in load_pbp.
    # This can happen on game days
    vec <- cli::cli_vec(missing_games, list("vec-trunc" = 5L))
    cli_message(
      "The following {cli::no(length(missing_games))} game{?s} {?is/are} not \\
      yet available via {.fun load_pbp} and {?is/are} therefore parsed directly \\
      with {.fun build_nflfastR_pbp} and appended to table {.val {name}}: \\
      {.val {vec}}"
    )
    # build pbp of missing games. If raw pbp isn't ready, the function will
    # return an empty dataframe for that game
    new_pbp <- build_nflfastR_pbp(missing_games, rules = FALSE)
    ret <- DBI::dbAppendTable(
      conn = conn,
      name = name,
      value = new_pbp
    )
    # Check how many new games have been added
    new_ids <- unique(new_pbp[["game_id"]])
    cli_message(
      "Appended {cli::no(length(new_ids))} game{?s} to table {.val {name}}",
      .cli_fct = cli::cli_alert_success
    )
    # Let user know that some games are still missing
    still_missing <- setdiff(missing_games, new_ids)
    vec <- cli::cli_vec(still_missing, list("vec-trunc" = 5L))
    cli_message(
      "Raw pbp data for the following {cli::no(length(still_missing))} game{?s} \\
      still missing: {.val {vec}}. Please try again in about 10 minutes.",
      .cli_fct = cli::cli_alert_warning
    )
  }

  # Remove Dummy ------------------------------------------------------------
  ret <- db_remove_dummy(conn = conn, name = name)

  # Finish ------------------------------------------------------------------
  cli_message(
    "Database update completed",
    .cli_fct = cli::cli_alert_success
  )
  rule_footer("DONE")

  invisible(conn)
}

db_query_game_ids <- function(conn, name, seasons) {
  res <- DBI::dbGetQuery(
    conn = conn,
    statement = glue::glue_sql(
      "SELECT DISTINCT game_id FROM {`name`} WHERE season IN ({seasons*});",
      .con = conn
    )
  )
  res[["game_id"]]
}

db_remove_dummy <- function(conn, name) {
  n_drops <- DBI::dbExecute(
    conn = conn,
    statement = glue::glue_sql(
      "DELETE FROM {`name`} WHERE game_id IN ({vals*})",
      vals = "9999_99_DEF_TYP",
      .con = conn
    )
  )
  invisible(TRUE)
}

db_write_pbp_seasons <- function(conn, name, seasons) {
  vec <- cli::cli_vec(seasons, list("vec-trunc" = 5L))
  cli_message(
    "Append {.val {vec}} {cli::qty(length(seasons))}\\
    season{?s} to table {.val {name}}"
  )
  chunks <- compute_chunks(
    seasons,
    chunk_size = getOption("nflfastR.db_chunk_size", 1L)
  )
  p <- progressr::progressor(along = chunks)
  for (chunk in chunks) {
    ret <- DBI::dbAppendTable(
      conn = conn,
      name = name,
      value = load_pbp(seasons = chunk)
    )
    p("Appending...")
  }
  invisible(TRUE)
}

db_drop_seasons <- function(conn, name, seasons) {
  vec <- cli::cli_vec(seasons, list("vec-trunc" = 5L))
  cli_message(
    "Drop {.val {vec}} {cli::qty(length(seasons))}\\
    season{?s} from table {.val {name}}"
  )
  n_drops <- DBI::dbExecute(
    conn = conn,
    statement = glue::glue_sql(
      "DELETE FROM {`name`} WHERE season IN ({vals*})",
      vals = seasons,
      .con = conn
    )
  )
  invisible(TRUE)
}

db_initiate_pbp <- function(conn, name) {
  cli_message(
    "Initiate table {.val {name}} with nflverse pbp schema"
  )
  ret <- DBI::dbCreateTable(
    conn = conn,
    name = name,
    fields = default_play
  )
  invisible(TRUE)
}

completed_game_ids <- function(seasons) {
  scheds <- nflreadr::load_schedules(seasons = seasons)
  scheds <- data.table::setDT(scheds)
  scheds[
    !is.na(result) & !game_id %in% missing_raw_games,
    game_id
  ]
}
utils::globalVariables(c("result", "game_id"), add = TRUE)

missing_raw_games <- c("1999_01_BAL_STL", "2000_06_BUF_MIA", "2000_03_SD_KC")

valid_seasons <- function() {
  seq(1999, nflreadr::most_recent_season())
}

# https://stackoverflow.com/a/3321659
compute_chunks <- function(x, chunk_size = 4) {
  split(x, ceiling(seq_along(x) / chunk_size))
}

confirm <- function(msg, ..., .envir = parent.frame()) {
  cli::cli_alert_info(
    text = msg,
    wrap = FALSE,
    .envir = .envir
  )
  ans <- readline()
  tolower(ans) %in% c("", "y", "yes", "yeah", "yep")
}
