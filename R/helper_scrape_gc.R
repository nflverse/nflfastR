################################################################################
# Author: Ben Baldwin
# Stlyeguide: styler::tidyverse_style()
################################################################################

# Build a tidy version of scraped gamecenter data
# Data exist since 2009
#
# @param gameId Specifies the game



get_pbp_gc <- function(gameId) {
  combined <- data.frame()
  tryCatch(
    expr = {

      if (gameId == 2013092206) {
        warning(warn <- 1)
      }

      if (gameId %in% c(2013112401, 2013120101))
        message(
          glue::glue(
            "Note: most yardage columns for game ID {as.character(gameId)} are missing. Use the RS scraper instead with source = 'rs'"
          )
        )

      url = paste0("http://www.nfl.com/liveupdate/game-center/", gameId, "/",
                   gameId, "_gtd.json")
      request <- httr::GET(url)


      if (request$status_code == 404) {
        warning(warn <- 3)
      }

      date_parse <- stringr::str_extract(paste(gameId), pattern = "[0-9]{8}")
      date_year <- stringr::str_sub(date_parse, 1, 4)
      date_month <- stringr::str_sub(date_parse, 5, 6)
      date_day <- stringr::str_sub(date_parse, nchar(date_parse) - 1,
                                   nchar(date_parse))

      #fix for season maker
      if (date_month == '01' | date_month == '02') {
        date_season <- as.numeric(date_year) - 1
      } else {
        date_season <- as.numeric(date_year)
      }

      if (date_year < 2009) {
        warning(warn <- 2)
      }

      game_json <- request %>%
        httr::content(as = "text", encoding = "UTF-8") %>%
        jsonlite::fromJSON(flatten = TRUE) %>%
        purrr::pluck(1)

      # message(glue::glue("Scraping gamecenter play by play data for GameID {gameId}..."))


      #excluding last element since it's "crntdrv" and not an actual
      drives <- game_json$drives[-length(game_json$drives)]

      #list of plays
      #each play has "players" column which is a list of player stats from the play
      plays <- suppressWarnings(purrr::map_dfr(1:length(drives),
                                               function(x) {
                                                 cbind("drive" = x,
                                                       data.frame(do.call(
                                                         rbind,
                                                         drives[[x]]$plays
                                                       ))[, c(1:11)]) %>% dplyr::mutate(play_id = names(drives[[x]]$plays), play_id = as.integer(play_id))
                                               }))
      plays$quarter_end <- dplyr::if_else(
        stringr::str_detect(plays$desc, "(END QUARTER)|(END GAME)|(End of quarter)"), 1, 0)
      plays$home_team <- game_json$home$abbr
      plays$away_team <- game_json$away$abbr

      #get df with 1 line per statId
      stats <- purrr::map_dfr(1:nrow(plays),
                              function(x) {
                                dplyr::bind_rows(plays[x,]$players[[1]], .id = "player_id") %>%
                                  dplyr::mutate(play_id = plays[x,]$play_id)
                              }
      ) %>%
        dplyr::mutate(
          sequence = as.numeric(sequence),
          statId = as.numeric(statId),
          play_id = as.character(play_id),
          yards = as.integer(yards)
        ) %>%
        dplyr::arrange(play_id, sequence) %>%
        dplyr::rename(
          playId = play_id, teamAbbr = clubcode,
          player.esbId = player_id,
          player.displayName = playerName,
          playStatSeq = sequence
        )


      pbp_stats <- purrr::map_df(unique(stats$playId), function(x) {
        sum_play_stats(x, stats = stats)
      })



      combined <- plays %>%
        dplyr::left_join(pbp_stats, by = "play_id") %>%
        dplyr::mutate_if(is.logical, as.numeric) %>%
        dplyr::mutate_if(is.integer, as.numeric) %>%
        dplyr::select(-players) %>%
        dplyr::rename(yardline = yrdln, quarter = qtr, play_description = desc, yards_to_go = ydstogo) %>%
        tidyr::unnest(cols = c(sp, quarter, down, time, yardline, yards_to_go, ydsnet, posteam,
                        play_description, note)) %>%
        dplyr::mutate(
          posteam_id = posteam,
          game_id = gameId,
          game_year = date_year,
          game_month = date_month,
          game_date = as.Date(paste(date_month,
                                    date_day,
                                    date_year, sep = "/"),
                              format = "%m/%d/%Y"),
          season = date_season,

          #fix up yardline before doing stuff. from nflscrapr
          yardline = dplyr::if_else(yardline == "50", "MID 50", yardline),
          yardline = dplyr::if_else(nchar(yardline) == 0 | is.null(yardline) |
                                      yardline == "NULL",
                                 dplyr::lag(yardline), yardline),

          # have to do all this nonsense to make goal_to_go and yardline_side for compatibility with later functions
          yardline_side = purrr::map_chr(stringr::str_split(yardline, " "),
                                         function(x) x[1]),
          yardline_number = as.numeric(purrr::map_chr(stringr::str_split(yardline, " "),
                                                      function(x) x[2])),
          goal_to_go = dplyr::if_else(yardline_side != posteam &
                                        ((yards_to_go == yardline_number) |
                                           (yards_to_go <= 1 & yardline_number == 1)),
                                      1, 0),
          down = as.double(down),
          quarter = as.double(quarter)

        )

    },
    error = function(e) {
      message("The following error has occured:")
      message(e)
    },
    warning = function(w) {
      if (warn == 1) {
        message(glue::glue("You asked for {gameId}, which is broken. Use the RS scraper instead"))
      } else if (warn == 2) {
        message(glue::glue("You asked a game from {date_year}, which only goes back to 2009. Use the RS scraper instead with source = 'rs'"))
      } else if (warn == 3) {
        message(glue::glue("Warning: The requested GameID {gameId} is invalid!"))
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

