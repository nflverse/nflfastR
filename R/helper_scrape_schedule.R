################################################################################
# Author: Sebastian Carl and Ben Baldwin
# Purpose: Function for scraping games that have been put in github repo
# Code Style Guide: styler::tidyverse_style()
################################################################################

get_season_schedule <- function(season) {
  season_schedule <- data.frame()
  tryCatch(
    expr = {
      url <- glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/schedules/sched_{season}.rds")

      request <- httr::HEAD(url = url)

      if (request$status_code == 404) {
        warning(warn <- 2)
      } else if (request$status_code == 500) {
        warning(warn <- 1)
      }

      # I know it's bad to call the server twice but couldn't figure out how
      # to parse the content in the request variable

      season_schedule <-
        readRDS(
          url(
            glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/schedules/sched_{season}.rds")
          )
        )
    },
    error = function(e) {
      message("The following error has occured:")
      message(e)
    },
    warning = function(w) {
      if (warn == 1) {
        message(glue::glue("Warning: The data hosting servers are down, please try again later!"))
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
