################################################################################
# Author: Ben Baldwin
# Stlyeguide: styler::tidyverse_style()
################################################################################

# Build a tidy version of scraped gamecenter data
# Data exist since 1999
#
# @param gameId Specifies the game

get_pbp_gc <- function(gameId) {
  combined <- data.frame()
  tryCatch(
    expr = {

      #testing only
      #gameId = '2013120812'
      #gameId = '2019_01_GB_CHI'
      #gameId = '2009_18_NYJ_CIN'
      #gameId = '2007_01_ARI_SF'
      #gameId = '1999_01_BAL_STL'

      if (gameId %in% c("2000_03_SD_KC", "2000_06_BUF_MIA", "1999_01_BAL_STL")) {
        warning(warn <- 1)
      }

      season <- as.integer(substr(gameId, 1, 4))

      #postseason games are in here too
      url <- glue::glue('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/raw/{season}/{gameId}.rds')

      request <- httr::HEAD(url = url)

      if (request$status_code == 404) {
        warning(warn <- 3)
      } else if (request$status_code == 500) {
        warning(warn <- 4)
      }

      raw <- readRDS(url(url))

      game_json <- raw %>% purrr::pluck(1)

      date_parse <- names(raw)[1] %>% stringr::str_extract(pattern = "[0-9]{8}")
      date_year <- stringr::str_sub(date_parse, 1, 4)
      date_month <- stringr::str_sub(date_parse, 5, 6)
      date_day <- stringr::str_sub(date_parse, nchar(date_parse) - 1,
                                   nchar(date_parse))

      week <- as.integer(substr(gameId, 6, 7))
      if (week <= 17) {
        season_type <- 'REG'
      } else {
        season_type <- 'POST'
      }

      if (date_year < 1999) {
        warning(warn <- 2)
      }

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

      #drive info
      d <- tibble::tibble(drives) %>%
        tidyr::unnest_wider(drives) %>%
        #dplyr::select(-plays) %>%
        tidyr::unnest_wider(start, names_sep="_") %>%
        tidyr::unnest_wider(end, names_sep="_") %>%
        dplyr::mutate(drive = 1 : dplyr::n()) %>%
        dplyr::rename(
          drive_play_count = numplays,
          drive_time_of_possession = postime,
          drive_first_downs = fds,
          drive_inside20 = redzone,
          drive_quarter_start = start_qtr,
          drive_quarter_end = end_qtr,
          drive_end_transition = result,
          drive_game_clock_start = start_time,
          drive_game_clock_end = end_time,
          drive_start_yard_line = start_yrdln,
          drive_end_yard_line = end_yrdln
        ) %>%
        dplyr::mutate(
          drive_inside20 = dplyr::if_else(drive_inside20, 1, 0),
          drive_how_ended_description = drive_end_transition,
          drive_ended_with_score = dplyr::if_else(drive_how_ended_description == "Touchdown" | drive_how_ended_description == 'Field Goal', 1, 0),
          drive_start_transition  = dplyr::lag(drive_how_ended_description, 1),
          drive_how_started_description  = drive_start_transition
        ) %>%
        dplyr::select(
          drive, drive_play_count, drive_time_of_possession,
          drive_first_downs, drive_inside20, drive_ended_with_score,
          drive_quarter_start, drive_quarter_end,
          drive_end_transition, drive_how_ended_description,
          drive_game_clock_start, drive_game_clock_end,
          drive_start_yard_line, drive_end_yard_line,
          drive_start_transition, drive_how_started_description
        )

      combined <- plays %>%
        dplyr::left_join(pbp_stats, by = "play_id") %>%
        dplyr::mutate_if(is.logical, as.numeric) %>%
        dplyr::mutate_if(is.integer, as.numeric) %>%
        dplyr::select(-players, -note) %>%
        #Weirdly formatted and missing anyway
        dplyr::mutate(note = NA_character_) %>%
        dplyr::rename(yardline = yrdln, quarter = qtr, play_description = desc, yards_to_go = ydstogo)  %>%
        tidyr::unnest(cols = c(sp, quarter, down, time, yardline, yards_to_go, ydsnet, posteam, play_description, note)) %>%
        dplyr::left_join(d, by = "drive") %>%
        dplyr::mutate(
          posteam_id = posteam,
          game_id = gameId,
          game_year = as.integer(date_year),
          game_month = as.integer(date_month),
          game_date = as.Date(paste(date_month,
                                    date_day,
                                    date_year, sep = "/"),
                              format = "%m/%d/%Y"),
          season = season,

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
          quarter = as.double(quarter),
          week = week,
          season_type = season_type,
          #missing from older gc data
          drive_real_start_time = NA_character_,
          start_time = NA_character_,
          stadium = NA_character_,
          weather = NA_character_,
          nfl_api_id = NA_character_,
          play_clock = NA_character_,
          play_deleted = NA_real_,
          play_type_nfl = NA_character_,
          drive_yards_penalized = NA_real_,
          end_clock_time = NA_character_,
          end_yard_line = NA_character_
        ) %>%
        dplyr::group_by(drive) %>%
        dplyr::mutate(
          drive_play_id_started = min(play_id, na.rm = T),
          drive_play_seq_started = min(play_id, na.rm = T),
          drive_play_id_ended = max(play_id, na.rm = T),
          drive_play_seq_ended = max(play_id, na.rm = T)

        ) %>%
        dplyr::ungroup()


    },
    error = function(e) {
      message("The following error has occured:")
      message(e)
    },
    warning = function(w) {
      if (warn == 1) {
        message(glue::glue("You asked for {gameId}, which is broken. Skipping."))
      } else if (warn == 2) {
        message(glue::glue("You asked a game from {date_year}, but data only goes back to 1999."))
      } else if (warn == 3) {
        message(glue::glue("Warning: The requested GameID {gameId} is invalid!"))
      } else if (warn == 4) {
        message(glue::glue("Warning: The data hosting servers are down, please try again later!"))
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




