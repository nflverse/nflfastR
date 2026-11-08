################################################################################
# Author: Ben Baldwin
# Purpose: Function to add Lee Sharpe's game data
# Code Style Guide: styler::tidyverse_style()
################################################################################

# Thanks Lee!
add_game_data <- function(pbp, games = NULL, ...) {
  out <- pbp
  warn <- 0
  tryCatch(
    expr = {
      # we use dir to specify the directory of a locally stored games file
      # for unit tests
      if (is.null(games)) {
        games <- nflreadr::load_schedules()
      } else {
        stopifnot(
          inherits(games, "nflverse_data"),
          isTRUE(attr(games, "nflverse_type") == "games and schedules")
        )
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
      game_row <- match(out$game_id, games$game_id)
      data.table::set(
        out,
        j = sub("^stadium$", "game_stadium", cols),
        value = lapply(cols, function(col) games[[col]][game_row])
      )
      data.table::set(out, j = "game_date", value = out$gameday)

      user_message("added game variables", "done")
    },
    error = function(e) {
      message("The following error has occured:")
      message(e)
    },
    warning = function(w) {
      if (warn == 1) {
        message(
          "Warning: The data hosting servers are down, so we can't add game data in the moment!"
        )
      } else {
        message("The following warning has occured:")
        message(w)
      }
    },
    finally = {}
  )
  return(out)
}
