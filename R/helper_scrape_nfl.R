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

      if (request$status_code == 404 & id %in% valid_games) {
        warning(warn <- 3)
      } else if (request$status_code == 500) {
        warning(warn <- 2)
      } else if (request$status_code == 404) {
        warning(warn <- 1)
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
          purrr::map(unique(stats$playId), function(x) {
            sum_play_stats(x, stats = stats)
          }) # %>%
          # dplyr::mutate(play_id = as.integer(play_id))
      )

      pbp_stats <- dplyr::bind_rows(pbp_stats)

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
          #if JAC has the ball and scored, make them the scoring team
          td_team = dplyr::if_else(
            season >= 2011 & season <= 2015 & posteam == 'JAC' &
              drive_how_ended_description == 'Touchdown' & !is.na(td_team),
            'JAC', td_team
          ),
          #if JAC involved in a game and defensive team score, fill in the right team
          td_team = dplyr::if_else(
            #game involving the jags
            season >= 2011 & season <= 2015 & (home_team == 'JAC' | away_team == 'JAC') &
              #defensive TD
              drive_how_ended_description != 'Touchdown' & !is.na(td_team),
            #if home team has ball, then away team scored, otherwise home team scored
            dplyr::if_else(posteam == home_team, away_team, home_team),
            td_team
          ),
          yardline_side = dplyr::if_else(
            season >= 2011 & season <= 2015 & yardline_side == 'JAX',
            'JAC', yardline_side
          ),
          #if there's some random missing drive, fill in with previous drive
          #this fixes a bug with plays appearing out of order after defensive TDs
          drive = dplyr::if_else(
            !is.na(dplyr::lag(drive)) & !is.na(dplyr::lead(drive)),
            dplyr::lag(drive), drive
          ),
          #fix for drives being messed up in this game
          drive = dplyr::case_when(
            id == '2012_04_NO_GB' & play_id == 1085 ~ 4,
            id == '2012_16_BUF_MIA' & play_id == 2571 ~ 15,
            id == '2015_16_CHI_TB' & play_id == 2182 ~ 14,
            TRUE ~ drive
          ),
          time = dplyr::case_when(
            id == '2012_04_NO_GB' & play_id == 1085 ~ '3:34',
            id == '2012_16_BUF_MIA' & play_id == 2571 ~ '8:31',
            TRUE ~ time
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
      } else if (warn == 3) {
        message(glue::glue("Warning: The requested GameID {id} is not loaded yet, please try again later!"))
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

# hard coded 2020 regular season game ids to make sure the output of the
# schedule scraper is not named 'invalid' if the source file not yet exists
valid_games <- c(
  "2020_01_HOU_KC",
  "2020_01_SEA_ATL",
  "2020_01_CLE_BAL",
  "2020_01_NYJ_BUF",
  "2020_01_LV_CAR",
  "2020_01_CHI_DET",
  "2020_01_IND_JAX",
  "2020_01_GB_MIN",
  "2020_01_MIA_NE",
  "2020_01_PHI_WAS",
  "2020_01_LAC_CIN",
  "2020_01_TB_NO",
  "2020_01_ARI_SF",
  "2020_01_DAL_LA",
  "2020_01_PIT_NYG",
  "2020_01_TEN_DEN",
  "2020_02_CIN_CLE",
  "2020_02_NYG_CHI",
  "2020_02_ATL_DAL",
  "2020_02_DET_GB",
  "2020_02_MIN_IND",
  "2020_02_BUF_MIA",
  "2020_02_SF_NYJ",
  "2020_02_LA_PHI",
  "2020_02_DEN_PIT",
  "2020_02_CAR_TB",
  "2020_02_JAX_TEN",
  "2020_02_WAS_ARI",
  "2020_02_BAL_HOU",
  "2020_02_KC_LAC",
  "2020_02_NE_SEA",
  "2020_02_NO_LV",
  "2020_03_MIA_JAX",
  "2020_03_CHI_ATL",
  "2020_03_LA_BUF",
  "2020_03_WAS_CLE",
  "2020_03_TEN_MIN",
  "2020_03_LV_NE",
  "2020_03_SF_NYG",
  "2020_03_CIN_PHI",
  "2020_03_HOU_PIT",
  "2020_03_NYJ_IND",
  "2020_03_CAR_LAC",
  "2020_03_DET_ARI",
  "2020_03_TB_DEN",
  "2020_03_DAL_SEA",
  "2020_03_GB_NO",
  "2020_03_KC_BAL",
  "2020_04_DEN_NYJ",
  "2020_04_ARI_CAR",
  "2020_04_IND_CHI",
  "2020_04_JAX_CIN",
  "2020_04_CLE_DAL",
  "2020_04_NO_DET",
  "2020_04_MIN_HOU",
  "2020_04_SEA_MIA",
  "2020_04_LAC_TB",
  "2020_04_PIT_TEN",
  "2020_04_BAL_WAS",
  "2020_04_NYG_LA",
  "2020_04_NE_KC",
  "2020_04_BUF_LV",
  "2020_04_PHI_SF",
  "2020_04_ATL_GB",
  "2020_05_TB_CHI",
  "2020_05_CAR_ATL",
  "2020_05_CIN_BAL",
  "2020_05_JAX_HOU",
  "2020_05_LV_KC",
  "2020_05_DEN_NE",
  "2020_05_ARI_NYJ",
  "2020_05_PHI_PIT",
  "2020_05_BUF_TEN",
  "2020_05_LA_WAS",
  "2020_05_MIA_SF",
  "2020_05_IND_CLE",
  "2020_05_NYG_DAL",
  "2020_05_MIN_SEA",
  "2020_05_LAC_NO",
  "2020_06_KC_BUF",
  "2020_06_CHI_CAR",
  "2020_06_CIN_IND",
  "2020_06_DET_JAX",
  "2020_06_ATL_MIN",
  "2020_06_WAS_NYG",
  "2020_06_BAL_PHI",
  "2020_06_CLE_PIT",
  "2020_06_HOU_TEN",
  "2020_06_MIA_DEN",
  "2020_06_NYJ_LAC",
  "2020_06_GB_TB",
  "2020_06_LA_SF",
  "2020_06_ARI_DAL",
  "2020_07_NYG_PHI",
  "2020_07_DET_ATL",
  "2020_07_PIT_BAL",
  "2020_07_CLE_CIN",
  "2020_07_GB_HOU",
  "2020_07_LAC_MIA",
  "2020_07_CAR_NO",
  "2020_07_BUF_NYJ",
  "2020_07_DAL_WAS",
  "2020_07_SEA_ARI",
  "2020_07_KC_DEN",
  "2020_07_SF_NE",
  "2020_07_TB_LV",
  "2020_07_CHI_LA",
  "2020_08_ATL_CAR",
  "2020_08_NE_BUF",
  "2020_08_TEN_CIN",
  "2020_08_LV_CLE",
  "2020_08_IND_DET",
  "2020_08_MIN_GB",
  "2020_08_NYJ_KC",
  "2020_08_LA_MIA",
  "2020_08_JAX_LAC",
  "2020_08_NO_CHI",
  "2020_08_SF_SEA",
  "2020_08_DAL_PHI",
  "2020_08_TB_NYG",
  "2020_09_GB_SF",
  "2020_09_DEN_ATL",
  "2020_09_SEA_BUF",
  "2020_09_BAL_IND",
  "2020_09_HOU_JAX",
  "2020_09_CAR_KC",
  "2020_09_DET_MIN",
  "2020_09_CHI_TEN",
  "2020_09_NYG_WAS",
  "2020_09_LV_LAC",
  "2020_09_MIA_ARI",
  "2020_09_PIT_DAL",
  "2020_09_NO_TB",
  "2020_09_NE_NYJ",
  "2020_10_IND_TEN",
  "2020_10_TB_CAR",
  "2020_10_HOU_CLE",
  "2020_10_WAS_DET",
  "2020_10_JAX_GB",
  "2020_10_PHI_NYG",
  "2020_10_CIN_PIT",
  "2020_10_BUF_ARI",
  "2020_10_DEN_LV",
  "2020_10_NYJ_MIA",
  "2020_10_SEA_LA",
  "2020_10_SF_NO",
  "2020_10_BAL_NE",
  "2020_10_MIN_CHI",
  "2020_11_ARI_SEA",
  "2020_11_TEN_BAL",
  "2020_11_DET_CAR",
  "2020_11_PHI_CLE",
  "2020_11_NE_HOU",
  "2020_11_GB_IND",
  "2020_11_PIT_JAX",
  "2020_11_ATL_NO",
  "2020_11_CIN_WAS",
  "2020_11_LAC_DEN",
  "2020_11_DAL_MIN",
  "2020_11_KC_LV",
  "2020_11_LA_TB",
  "2020_12_HOU_DET",
  "2020_12_WAS_DAL",
  "2020_12_BAL_PIT",
  "2020_12_LV_ATL",
  "2020_12_LAC_BUF",
  "2020_12_NYG_CIN",
  "2020_12_TEN_IND",
  "2020_12_CLE_JAX",
  "2020_12_CAR_MIN",
  "2020_12_ARI_NE",
  "2020_12_MIA_NYJ",
  "2020_12_NO_DEN",
  "2020_12_SF_LA",
  "2020_12_KC_TB",
  "2020_12_CHI_GB",
  "2020_12_SEA_PHI",
  "2020_13_DAL_BAL",
  "2020_13_NO_ATL",
  "2020_13_DET_CHI",
  "2020_13_IND_HOU",
  "2020_13_CIN_MIA",
  "2020_13_JAX_MIN",
  "2020_13_LV_NYJ",
  "2020_13_WAS_PIT",
  "2020_13_CLE_TEN",
  "2020_13_LA_ARI",
  "2020_13_NYG_SEA",
  "2020_13_PHI_GB",
  "2020_13_NE_LAC",
  "2020_13_DEN_KC",
  "2020_13_BUF_SF",
  "2020_14_NE_LA",
  "2020_14_DEN_CAR",
  "2020_14_HOU_CHI",
  "2020_14_DAL_CIN",
  "2020_14_GB_DET",
  "2020_14_TEN_JAX",
  "2020_14_KC_MIA",
  "2020_14_ARI_NYG",
  "2020_14_MIN_TB",
  "2020_14_IND_LV",
  "2020_14_NYJ_SEA",
  "2020_14_ATL_LAC",
  "2020_14_NO_PHI",
  "2020_14_WAS_SF",
  "2020_14_PIT_BUF",
  "2020_14_BAL_CLE",
  "2020_15_LAC_LV",
  "2020_15_TB_ATL",
  "2020_15_JAX_BAL",
  "2020_15_BUF_DEN",
  "2020_15_CAR_GB",
  "2020_15_HOU_IND",
  "2020_15_NYJ_LA",
  "2020_15_NE_MIA",
  "2020_15_CHI_MIN",
  "2020_15_CLE_NYG",
  "2020_15_DET_TEN",
  "2020_15_SEA_WAS",
  "2020_15_PHI_ARI",
  "2020_15_KC_NO",
  "2020_15_SF_DAL",
  "2020_15_PIT_CIN",
  "2020_16_MIN_NO",
  "2020_16_SF_ARI",
  "2020_16_NYG_BAL",
  "2020_16_TB_DET",
  "2020_16_CIN_HOU",
  "2020_16_CHI_JAX",
  "2020_16_ATL_KC",
  "2020_16_DEN_LAC",
  "2020_16_MIA_LV",
  "2020_16_CLE_NYJ",
  "2020_16_IND_PIT",
  "2020_16_CAR_WAS",
  "2020_16_LA_SEA",
  "2020_16_PHI_DAL",
  "2020_16_TEN_GB",
  "2020_16_BUF_NE",
  "2020_17_MIA_BUF",
  "2020_17_NO_CAR",
  "2020_17_GB_CHI",
  "2020_17_BAL_CIN",
  "2020_17_PIT_CLE",
  "2020_17_MIN_DET",
  "2020_17_TEN_HOU",
  "2020_17_JAX_IND",
  "2020_17_LAC_KC",
  "2020_17_NYJ_NE",
  "2020_17_DAL_NYG",
  "2020_17_WAS_PHI",
  "2020_17_ATL_TB",
  "2020_17_LV_DEN",
  "2020_17_ARI_LA",
  "2020_17_SEA_SF"
)
