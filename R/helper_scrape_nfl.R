################################################################################
# Author: Sebastian Carl, Ben Baldwin
# Purpose: Function for scraping pbp data from the new NFL web site
# Code Style Guide: styler::tidyverse_style()
################################################################################

# Build a tidy version of scraped NFL data
#
# @param id Specifies the game

get_pbp_nfl <- function(id) {
  combined <- data.frame()
  tryCatch(
    expr = {

      #testing
      #id = '2019_01_GB_CHI'
      #id = '2015_01_CAR_JAX'
      #id = '2011_01_NO_GB'

      season <- substr(id, 1, 4)
      week <- as.integer(substr(id, 6, 7))

      path <- "https://github.com/guga31bb/nflfastR-data/raw/master/raw"

      request <- httr::HEAD(glue::glue("{path}/{season}/{id}.rds"))

      if (request$status_code == 404) {
        warning(warn <- 1)
      } else if (request$status_code == 500) {
        warning(warn <- 2)
      }

      raw_data <- readRDS(url(glue::glue("{path}/{season}/{id}.rds")))

      season_type <- dplyr::if_else(week <= 17, "REG", "POST")

      # game_info <- raw_data$data$viewer$gameDetail

      game_id <- raw_data$data$viewer$gameDetail$id
      home_team <- raw_data$data$viewer$gameDetail$homeTeam$abbreviation
      away_team <- raw_data$data$viewer$gameDetail$visitorTeam$abbreviation
      weather <- dplyr::if_else(
        is.null(raw_data$data$viewer$gameDetail$weather$shortDescription),
        NA_character_,
        raw_data$data$viewer$gameDetail$weather$shortDescription
      )
      stadium <- dplyr::if_else(
        is.null(raw_data$data$viewer$gameDetail$stadium),
        NA_character_,
        raw_data$data$viewer$gameDetail$stadium
      )
      start_time <- raw_data$data$viewer$gameDetail$startTime

      game_info <- data.frame(
        game_id,
        home_team,
        away_team,
        weather,
        stadium,
        start_time
      ) %>%
        tibble::as_tibble() %>%
        dplyr::mutate(game_id = as.character(game_id))

      plays <- raw_data$data$viewer$gameDetail$plays %>% dplyr::mutate(game_id = as.character(game_id))

      #fill missing posteam info for this time
      if ((home_team == 'JAC' | away_team == 'JAC') & season <= 2015) {
        plays <- plays %>%
          dplyr::mutate(
            possessionTeam.abbreviation = stringr::str_extract(plays$prePlayByPlay, '[A-Z]{2,3}(?=\\s)'),
            possessionTeam.abbreviation = dplyr::if_else(
              possessionTeam.abbreviation %in% c('OUT', 'END', 'NA'),
              NA_character_, possessionTeam.abbreviation
            ),
            possessionTeam.abbreviation = dplyr::if_else(
              possessionTeam.abbreviation == 'JAX', 'JAC', possessionTeam.abbreviation
            )
          )
      }

      drives <- raw_data$data$viewer$gameDetail$drives %>%
        dplyr::mutate(ydsnet = yards + yardsPenalized) %>%
        # these are already in plays
        dplyr::select(
          -possessionTeam.abbreviation,
          -possessionTeam.nickName,
          -possessionTeam.franchise.currentLogo.url
        ) %>%
        janitor::clean_names()
      colnames(drives) <- paste0("drive_", colnames(drives))

      stats <- tidyr::unnest(plays %>% dplyr::select(-yards), cols = c(playStats)) %>%
        dplyr::mutate(
          yards = as.integer(yards),
          statId = as.numeric(statId),
          team.abbreviation = as.character(team.abbreviation)
        ) %>%
        dplyr::rename(
          player.esbId = gsisPlayer.id,
          player.displayName = playerName,
          teamAbbr = team.abbreviation
        ) %>%
        dplyr::select(
          playId,
          statId,
          yards,
          teamAbbr,
          player.displayName,
          player.esbId
        )

      # if I don't put this here it breaks
      suppressWarnings(
        pbp_stats <-
          purrr::map_df(unique(stats$playId), function(x) {
            sum_play_stats(x, stats = stats)
          }) %>%
          dplyr::mutate(play_id = as.integer(play_id))
      )

      combined <- game_info %>%
        dplyr::left_join(plays %>% dplyr::select(-playStats), by = c("game_id")) %>%
        dplyr::left_join(drives, by = c("driveSequenceNumber" = "drive_order_sequence")) %>%
        dplyr::left_join(pbp_stats, by = c("playId" = "play_id")) %>%
        dplyr::mutate_if(is.logical, as.numeric) %>%
        dplyr::mutate_if(is.integer, as.numeric) %>%
        dplyr::mutate_if(is.factor, as.character) %>%
        janitor::clean_names() %>%
        dplyr::select(-drive_play_count, -drive_time_of_possession, -next_play_type) %>%
        dplyr::rename(
          time = clock_time,
          play_type_nfl = play_type,
          posteam = possession_team_abbreviation,
          yardline = yard_line,
          sp = scoring_play,
          drive = drive_sequence_number,
          nfl_api_id = game_id,
          drive_play_count = drive_play_count_2,
          drive_time_of_possession = drive_time_of_possession_2,
          ydsnet = drive_ydsnet
        ) %>%
        dplyr::mutate(
          posteam_id = posteam,
          # have to do all this nonsense to make goal_to_go and yardline_side for compatibility with later functions
          yardline_side = purrr::map_chr(
            stringr::str_split(yardline, " "),
            function(x) x[1]
          ),
          yardline_number = as.numeric(purrr::map_chr(
            stringr::str_split(yardline, " "),
            function(x) x[2]
          )),
          quarter_end = dplyr::if_else(stringr::str_detect(play_description, "END QUARTER"), 1, 0),
          game_year = as.integer(season),
          season = as.integer(season),
          # this is only needed for epa and dropped later
          game_month = as.integer(11),
          game_id = as.character(glue::glue('{season}_{formatC(week, width=2, flag=\"0\")}_{away_team}_{home_team}')),
          play_description = play_description_with_jersey_numbers,
          week = week,
          season_type = season_type,
          play_clock = as.character(play_clock),
          st_play_type = as.character(st_play_type),
          td_team = dplyr::if_else(
            season >= 2011 & season <= 2015 & posteam == 'JAC' &
              drive_how_ended_description == 'Touchdown' & !is.na(td_team),
            'JAC', td_team
          ),
          yardline_side = dplyr::if_else(
            season >= 2011 & season <= 2015 & yardline_side == 'JAX',
            'JAC', yardline_side
          )
        ) %>%
        dplyr::mutate_all(dplyr::na_if, "")

      # combined <-
      #   combined %>%
      #   dplyr::select(
      #     tidyselect::one_of(
      #       c(pbp_stat_columns, api_cols, save_cols)
      #     )
      #   )
    },
    error = function(e) {
      message("The following error has occured:")
      message(e)
    },
    warning = function(w) {
      if (warn == 1) {
        message(glue::glue("Warning: The requested GameID {id} is invalid!"))
      } else if (warn == 2) {
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


# otherwise scraping a lot of seasons breaks
save_cols <- c(
  "game_id", "nfl_api_id", "home_team", "away_team",
  "season", "game_month",
  "game_year", "time", "down", "drive_net_yards",
  "drive", "first_down", "goal_to_go", "order_sequence",
  "play_description", "play_review_status",
  "play_type_nfl", "quarter", "sp",
  "scoring_play_type", "special_teams_play",
  "time_of_day",
  "yardline", "yards",
  "yards_to_go", "latest_play",
  "posteam",
  "scoring_team_id",
  "scoring_team_abbreviation", "scoring_team_nick_name",
  "ydsnet", "drive_yards_penalized",
  "posteam_id", "yardline_side",
  "yardline_number", "quarter_end"
)
