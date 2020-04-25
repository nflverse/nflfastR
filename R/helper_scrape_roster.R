################################################################################
# Author: Sebastian Carl
# Purpose: Function for scraping team rosters and player IDs from NFL.com
# Code Style Guide: styler::tidyverse_style()
################################################################################

# Grab roster for a specific ID and season
#
# This is a sub-function for the get_rosters function.
#
# @param teamId (character string) Specifies the unique teamId
# for which the roster is scraped
# @param season 4-digit year associated with a given NFL season

grab_roster <- function(teamId, season) {

  # If a combination of Team ID and season is passed for which there
  # is no data because the corresponding team did not exist in that season,
  # then the error must be caught so that the function does not abort.
  # Therefore the whole expression is packed into a tryCatch environment.
  # Since we want to return the dataframe 'roster_raw' we declare it as an
  # emtpty dataframe first to ensure we can return it
  roster_raw <- data.frame()

  tryCatch(
    expr = {
      roster_raw <-
        # Send a GET request for provided teamID and seaon
        httr::GET(url = glue::glue("http://www.nfl.com/feeds-rs/roster/{teamId}/{season}")) %>%
        # retrieve content as character vector
        httr::content(as = "text", encoding = "UTF-8") %>%
        # parse the character vetor
        jsonlite::fromJSON(flatten = TRUE) %>%
        # put it in a dataframe
        data.frame()

      # The message actually is shown after the work because if an error occurs
      # we want to show another message with the error function
      # message(glue::glue("Scraped Roster of TeamID {teamId} for the '{season}' Season"))
    },
    error = function(e) {
      message(glue::glue("The Team with the ID '{teamId}' doesn't exist for the '{season}' season!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(roster_raw)
}
