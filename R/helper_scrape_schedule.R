################################################################################
# Author: Sebastian Carl
# Purpose: Function for scraping season schedule from the NFL RS Feed
# Code Style Guide: styler::tidyverse_style()
################################################################################

get_season_schedule <- function(season) {
  season_schedule <- data.frame()
  tryCatch(
    expr = {
      request <-
        httr::GET(url = glue::glue("http://www.nfl.com/feeds-rs/schedules/{season}"))

      if (request$status_code == 404) {
        warning(warn <- 1)
      }

      raw_data <- request %>%
        httr::content(as = "text", encoding = "UTF-8") %>%
        jsonlite::fromJSON(flatten = TRUE)

      if (is.null(raw_data %>% purrr::pluck("gameSchedules"))) {
        warning(warn <- 2)
      }

      season_schedule <- raw_data %>%
        purrr::pluck("gameSchedules") %>%
        as.data.frame() %>%
        janitor::clean_names() %>%
        dplyr::mutate(
          # add Lee Sharpe's very useful alt_game_id
          alt_game_id = dplyr::if_else(
            season_type %in% c("REG", "POST"),
            glue::glue("{season}_{formatC(week, width=2, flag=\"0\")}_{visitor_team_abbr}_{home_team_abbr}"),
            NA_character_
          )
        ) %>%
        dplyr::select(
          season:game_id,
          alt_game_id,
          game_date:iso_time,
          home_team = home_team_abbr,
          away_team = visitor_team_abbr,
          home_team_name = home_display_name,
          away_team_name = visitor_display_name,
          home_nickname,
          away_nickname = visitor_nickname,
          home_team_id,
          away_team_id = visitor_team_id,
          game_type,
          week_name,
          site_city = site_site_city,
          site_fullname = site_site_fullname,
          site_state = site_site_state,
          site_roof_type
          #network_channel
        ) %>%
        dplyr::arrange(game_id)
    },
    error = function(e) {
      message("The following error has occured:")
      message(e)
    },
    warning = function(w) {
      if (warn == 1) {
        message(glue::glue("Warning: The requested Season {season} is invalid!"))
      } else if (warn == 2) {
        message(glue::glue("Warning: Either the requested season {season} is invalid or no data available at this time!"))
      } else {
        message("The following warning has occured:")
        message(w)
      }
    },
    finally = {
    }
  )
  return(season_schedule)
}
