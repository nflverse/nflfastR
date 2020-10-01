################################################################################
# Author: Sebastian Carl
# Purpose: Function for loading schedules and rosters from nflfastR repos
# Code Style Guide: styler::tidyverse_style()
################################################################################

#' @importFrom glue glue
#' @importFrom httr HEAD
#' @importFrom tibble tibble
get_scheds_and_rosters <- function(season, type) {
  out <- tibble::tibble()
  tryCatch(
    expr = {
      if (type == "schedule") {
        path <- glue::glue("https://github.com/guga31bb/nflfastR-data/blob/master/schedules/sched_{season}.rds?raw=true")
      } else if (type == "roster") {
        path <- glue::glue("https://github.com/mrcaseb/nflfastR-roster/blob/master/data/seasons/roster_{season}.rds?raw=true")
      }

      request <- httr::HEAD(url = path)

      if (request$status_code == 404) {
        warning(warn <- 2)
      } else if (request$status_code == 500) {
        warning(warn <- 1)
      }

      out <- readRDS(url(path))
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
  return(out)
}
