# Thanks Lee!
add_game_data <- function(pbp, games = NULL, ...) {
  # the tests pass a locally stored games file
  if (is.null(games)) {
    games <- nflreadr::load_schedules()
  } else if (
    !inherits(games, "nflverse_data") ||
      !identical(attr(games, "nflverse_type"), "games and schedules")
  ) {
    cli::cli_abort(
      "{.arg games} must be the schedules of {.fn nflreadr::load_schedules}."
    )
  }

  # nflreadr returns an empty table (with a warning) if the download fails.
  # Without the game data, EP and WP would use missing roofs and lines.
  missing <- setdiff(pbp$game_id, games$game_id)
  if (length(missing) > 0) {
    cli::cli_abort(c(
      "Can't add the game data of {length(missing)} game{?s}: {.val {missing}}.",
      "i" = if (nrow(games) == 0) {
        "The schedules couldn't be loaded. Please try again later."
      } else {
        "The schedules don't have {cli::qty(length(missing))}{?this game/these games}."
      }
    ))
  }

  # add the game columns by reference. A join with [.data.table would
  # allocate about twice the size of pbp
  cols <- c(
    "old_game_id",
    "away_score",
    "home_score",
    "location",
    "result",
    "total",
    "spread_line",
    "total_line",
    "div_game",
    "roof",
    "surface",
    "temp",
    "wind",
    "home_coach",
    "away_coach",
    "stadium",
    "stadium_id",
    "gameday"
  )
  game_row <- match(pbp$game_id, games$game_id)
  data.table::set(
    pbp,
    j = sub("^stadium$", "game_stadium", cols),
    value = lapply(cols, function(col) games[[col]][game_row])
  )
  data.table::set(pbp, j = "game_date", value = pbp$gameday)

  cli_message("added game variables", .cli_fct = cli::cli_alert_success)
  pbp
}
