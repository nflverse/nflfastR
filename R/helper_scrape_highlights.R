################################################################################
# Author: Sebastian Carl
# Purpose: Scrape urls of game highlightclips
# Code Style Guide: styler::tidyverse_style()
################################################################################

get_pbp_highlights <- function(gameId) {
  highlights <- data.frame()
  tryCatch(
    expr = {
      request <- httr::GET(
        url = glue::glue("http://www.nfl.com/feeds-rs/playbyplay/{gameId}")
      )

      if (request$status_code == 404) {
        warning(warn <- 1)
      }

      raw_data <- request %>%
        httr::content(as = "text", encoding = "UTF-8") %>%
        jsonlite::fromJSON(flatten = TRUE)

      if (is.null(raw_data %>% purrr::pluck("drives"))) {
        warning(warn <- 2)
      }

      # message(glue::glue("Scraped Highlights for GameID {gameId}"))

      highlights <- purrr::map_df(raw_data$drives$sequence, function(x) {
        plays <- raw_data$drives$plays[[x]]
        plays$game_id <- gameId
        return(plays)
      }) %>%
        janitor::clean_names()

      if (!"highlight_video_content_type" %in% colnames(highlights)) {
        highlights <- data.frame()
        warning(warn <- 3)
      } else {
        highlights <- highlights %>%
          dplyr::filter(!is.na(highlight_video_content_type)) %>%
          dplyr::mutate(
            highlight_video_url = dplyr::if_else(
              is.na(highlight_video_video_file_url),
              as.character(highlight_video_video_detail_page_url),
              as.character(highlight_video_video_file_url)
            )
          ) %>%
          dplyr::select(game_id, play_id, highlight_video_url)
      }
    },
    error = function(e) {
      message("The following error has occured:")
      message(e)
    },
    warning = function(w) {
      if (warn == 1) {
        message(glue::glue("Warning: The requested GameID {gameId} is invalid!"))
      } else if (warn == 2) {
        message(glue::glue("Warning: Drive data for GameID {gameId} are missing completely!"))
      } else if (warn == 3) {
        message(glue::glue("Warning: No Clips available for GameID {gameId}!"))
      } else {
        message("The following warning has occured:")
        message(w)
      }
    },
    finally = {
    }
  )
  return(highlights)
}
