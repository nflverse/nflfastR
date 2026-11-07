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

      # update join by reference
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
      game_data <- data.table::setDT(lapply(
        stats::setNames(nm = c("game_id", cols)),
        function(col) games[[col]]
      ))
      data.table::setnames(game_data, "stadium", "game_stadium")
      new_cols <- setdiff(names(game_data), "game_id")
      out[game_data, on = "game_id", (new_cols) := mget(paste0("i.", new_cols))]
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
