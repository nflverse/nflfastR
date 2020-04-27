################################################################################
# Author: Sebastian Carl, Ben Baldwin
# Purpose: Function for scraping pbp data from the NFL RS Feed
# Code Style Guide: styler::tidyverse_style()
################################################################################

get_pbp_rs <- function(gameId) {
  combined <- data.frame()
  tryCatch(
    expr = {
      request <-
        httr::GET(
          url = glue::glue("http://www.nfl.com/feeds-rs/playbyplay/{gameId}")
        )

      if (request$status_code == 404) {
        warning(warn <- 1)
      }

      raw_data <- request %>%
        httr::content(as = "text", encoding = "UTF-8") %>%
        jsonlite::fromJSON(flatten = TRUE)

      if (is.null(raw_data %>% purrr::pluck("drives")) |
          !is.list(raw_data %>% purrr::pluck("drives", "plays"))) {
        warning(warn <- 2)
      }

      # message(glue::glue("Scraped play by play data for GameID {gameId}..."))

      game_info <- raw_data$gameSchedule %>%
        purrr::discard(is.list) %>%
        purrr::compact() %>%
        as.data.frame() %>%
        dplyr::bind_cols(
          raw_data$gameSchedule$site %>%
            purrr::compact() %>%
            as.data.frame()
        ) %>%
        janitor::clean_names() %>%
        dplyr::mutate(
          game_id = as.character(game_id),
          game_date = as.Date(game_date, format = "%m/%d/%Y"),
          game_year = format(game_date, "%Y") %>% as.numeric(),
          game_month = format(game_date, "%m") %>% as.numeric()
        ) %>%
        dplyr::mutate_if(is.factor, as.character) %>%
        dplyr::rename(
          home_team = home_team_abbr,
          away_team_id = visitor_team_id,
          away_team = visitor_team_abbr,
          away_display_name = visitor_display_name,
          away_nickname = visitor_nickname
        )

      drives <- raw_data$drives %>%
        dplyr::rename_all(function(x) paste0("drive_", x)) %>%
        dplyr::mutate(
          game_Id = as.character(gameId),
          ydsnet = drive_yards + drive_yardsPenalized
        ) %>%
        dplyr::rename(drive_number = drive_sequence) %>%
        janitor::clean_names()

      plays <-
        purrr::map_df(raw_data$drives$sequence, function(x) {
          plays <- raw_data$drives$plays[[x]] %>% dplyr::select(-sequence)
          plays$drive_number <- x
          return(plays)
        }) %>%
        dplyr::mutate(timeOfDay = as.character(timeOfDay)) %>%
        dplyr::select(playId:playDescription, drive_number) %>%
        dplyr::mutate_if(is.logical, as.numeric) %>%
        janitor::clean_names() %>%
        dplyr::rename(
          posteam = team_id,
          posteam_id = team_eid,
          scoring_team_abbr = scoring_team_id,
          scoring_team_id = scoring_team_eid,
          quarter_end = end_quarter
        ) %>%
        dplyr::mutate(
          # Fill in the rows with missing posteam with the lag:
          posteam = dplyr::if_else(
            (quarter_end == 1 | play_type == "TIMEOUT"),
            dplyr::lag(posteam),
            posteam),
          posteam_id = dplyr::if_else(
            (quarter_end == 1 | play_type == "TIMEOUT"),
            dplyr::lag(posteam_id),
            posteam_id),
          yardline = dplyr::if_else(
            ((quarter_end == 1 | play_type == "TIMEOUT") & is.na(yardline)),
            dplyr::lag(yardline),
            yardline),
          yardline_side = dplyr::if_else(
            ((quarter_end == 1 | play_type == "TIMEOUT") & is.na(yardline_side)),
            dplyr::lag(yardline_side),
            yardline_side),
          yardline_number = dplyr::if_else(
            ((quarter_end == 1 | play_type == "TIMEOUT") & is.na(yardline_number)),
            dplyr::lag(yardline_number),
            yardline_number),
          yardline_side = dplyr::if_else(
            yardline_number == 50,
            "MID",
            yardline_side
          )
        )


      # fix for missing quarter in these games
      if (gameId == 2002090803 | game_info$season[1] == 2000) {
        plays <- plays %>% dplyr::mutate(
          quarter = 1 + cumsum(quarter_end) - quarter_end
        )
      }

      stats <-
        purrr::map_df(plays$play_stats, function(x) {
          stats <- x
        }) %>%
        dplyr::select(playId:player.gsisId) %>%
        dplyr::mutate(
          yards = as.integer(yards),
          playStatSeq = as.numeric(playStatSeq),
          statId = as.numeric(statId),
          player.esbId = player.gsisId,
          player.displayName = dplyr::if_else(
            !is.na(player.lastName),
            glue::glue("{substr(player.firstName, 1, 1)}.{player.lastName}"),
            player.lastName
          )
        ) %>%
        dplyr::arrange(playStatSeq)

      # if I don't put this here it breaks
      suppressWarnings(
        pbp_stats <-
          purrr::map_df(unique(stats$playId), function(x) {
            sum_play_stats(x, stats = stats)
          }) %>%
          dplyr::mutate(play_id = as.integer(play_id)) %>%
          dplyr::select(-penalty)
      )

      combined <- game_info %>%
        dplyr::left_join(drives, by = "game_id") %>%
        dplyr::left_join(plays, by = "drive_number") %>%
        dplyr::left_join(pbp_stats, by = "play_id") %>%
        dplyr::mutate_if(is.logical, as.numeric) %>%
        dplyr::mutate_if(is.integer, as.numeric) %>%
        dplyr::select(-drive_plays, -play_stats) %>%
        dplyr::rename(play_type_nfl = play_type, drive = drive_number, sp = scoring)
    },
    error = function(e) {
      message("The following error has occured:")
      message(e)
    },
    warning = function(w) {
      if (warn == 1) {
        message(glue::glue("Warning: The requested GameID {gameId} is invalid!"))
      } else if (warn == 2) {
        message(glue::glue("Warning: Drive or play data for GameID {gameId} missing completely!"))
      } else {
        message("The following warning has occured:")
        message(w)
      }
    },
    finally = {
    }
  )
  return(combined)
}
