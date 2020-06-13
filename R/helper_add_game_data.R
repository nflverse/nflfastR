################################################################################
# Author: Ben Baldwin
# Purpose: Function to add Lee Sharpe's game data
# Code Style Guide: styler::tidyverse_style()
################################################################################

# Thanks Lee!

add_game_data <- function(pbp) {
  out <- pbp
  tryCatch(
    expr = {
      url <- "https://github.com/leesharpe/nfldata/blob/master/data/games.rds?raw=true"

      request <- httr::HEAD(url)

      if (request$status_code %in% c(404, 500)) {
        warning(warn <- 1)
      }

      out <- out %>%
        dplyr::left_join(
          readRDS(url(url)) %>%
            dplyr::select(
              game_id, away_score, home_score, location, result, total,
              spread_line, total_line, div_game, roof, surface, temp, wind,
              home_coach, away_coach, stadium, stadium_id, gameday
            ) %>%
            dplyr::rename(game_stadium = stadium),
          by = c("game_id")
        ) %>%
        dplyr::mutate(
          game_date = gameday
        )

      message("added game variables")
    },
    error = function(e) {
      message("The following error has occured:")
      message(e)
    },
    warning = function(w) {
      if (warn == 1) {
        message(glue::glue("Warning: The data hosting servers are down, so we can't add game data in the moment!"))
      } else {
        message("The following warning has occured:")
        message(w)
      }
    },
    finally = {
    }
  )
  return(out)
}
