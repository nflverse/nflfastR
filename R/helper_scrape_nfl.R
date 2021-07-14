################################################################################
# Author: Sebastian Carl, Ben Baldwin
# Purpose: Function for scraping pbp data from the new NFL web site
# Code Style Guide: styler::tidyverse_style()
################################################################################

# Build a tidy version of scraped NFL data
#
# @param id Specifies the game
get_pbp_nfl <- function(id, dir = NULL, qs = FALSE) {

  if (isTRUE(qs) && !is_installed("qs")) {
    cli::cli_abort("Package {.val qs} required for argument {.val qs = TRUE}. Please install it.")
  }

  combined <- data.frame()
  tryCatch(
    expr = {

      #testing
      #id = '2019_01_GB_CHI'
      # id = '2015_01_CAR_JAX'
      #id = '2011_01_NO_GB'

      season <- substr(id, 1, 4)
      week <- as.integer(substr(id, 6, 7))

      if (is.null(dir)) {
        path <- "https://raw.githubusercontent.com/guga31bb/nflfastR-raw/master/raw"

        if(isFALSE(qs)) fetched <- curl::curl_fetch_memory(glue::glue("{path}/{season}/{id}.rds"))

        if(isTRUE(qs)) fetched <- curl::curl_fetch_memory(glue::glue("{path}/{season}/{id}.qs"))

        if (fetched$status_code == 404 & maybe_valid(id)) {
          warning(warn <- 3)
        } else if (fetched$status_code == 500) {
          warning(warn <- 2)
        } else if (fetched$status_code == 404) {
          warning(warn <- 1)
        }

        if(isFALSE(qs)) raw_data <- read_raw_rds(fetched$content)

        if(isTRUE(qs)) raw_data <- qs::qdeserialize(fetched$content)

      } else {
        # build path to locally stored game files. This functionality is primarily
        # for the data repo maintainer
        if(isFALSE(qs)) p <- glue::glue("{dir}/{season}/{id}.rds")
        if(isTRUE(qs)) p <- glue::glue("{dir}/{season}/{id}.qs")

        if (!file.exists(p)) {
          warning(warn <- 4)
        }

        if(isFALSE(qs)) raw_data <- readRDS(p)
        if(isTRUE(qs)) raw_data <- qs::qread(p)
      }

      season_type <- dplyr::case_when(
        season <= 2020 & week <= 17 ~ "REG",
        season >= 2021 & week <= 18 ~ "REG",
        TRUE ~ "POST"
      )

      # game_info <- raw_data$data$viewer$gameDetail

      game_id <- raw_data$data$viewer$gameDetail$id
      home_team <- raw_data$data$viewer$gameDetail$homeTeam$abbreviation
      away_team <- raw_data$data$viewer$gameDetail$visitorTeam$abbreviation

      # if home team and away team are the same, the game is messed up and needs fixing
      if (home_team == away_team) {

        # get correct home and away from the game ID
        id_parts <- stringr::str_split(id, "_")
        away_team <- id_parts[[1]][3]
        home_team <- id_parts[[1]][4]
        bad_game <- 1

      } else {
        bad_game <- 0
      }

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

      game_info <- tibble::tibble(
        game_id = as.character(game_id),
        home_team,
        away_team,
        weather,
        stadium,
        start_time
      )

      plays <- raw_data$data$viewer$gameDetail$plays %>% dplyr::mutate(game_id = as.character(game_id))

      #fill missing posteam info for this
      if (
        ((home_team %in% c("JAC", "JAX") | away_team %in% c("JAC", "JAX")) & season <= 2015) |
          bad_game == 1
        ) {
        plays <- plays %>%
          dplyr::mutate(
            possessionTeam.abbreviation = stringr::str_extract(plays$prePlayByPlay, '[A-Z]{2,3}(?=\\s)'),
            possessionTeam.abbreviation = dplyr::if_else(
              .data$possessionTeam.abbreviation %in% c('OUT', 'END', 'NA'),
              NA_character_, .data$possessionTeam.abbreviation
            ),
            possessionTeam.abbreviation = dplyr::if_else(
              .data$possessionTeam.abbreviation == 'JAX', 'JAC', .data$possessionTeam.abbreviation
            )
          )

        # for these old games, we're making everything JAC instead of JAX
        home_team <- dplyr::if_else(home_team == "JAX", "JAC", home_team)
        away_team <- dplyr::if_else(away_team == "JAX", "JAC", away_team)
      }

      drives <- raw_data$data$viewer$gameDetail$drives %>%
        dplyr::mutate(ydsnet = .data$yards + .data$yardsPenalized) %>%
        # these are already in plays
        dplyr::select(
          -"possessionTeam.abbreviation",
          -"possessionTeam.nickName",
          -"possessionTeam.franchise.currentLogo.url"
        ) %>%
        janitor::clean_names()
      colnames(drives) <- paste0("drive_", colnames(drives))

      stats <- tidyr::unnest(plays %>% dplyr::select(-"yards"), cols = c("playStats")) %>%
        dplyr::mutate(
          yards = as.integer(.data$yards),
          statId = as.numeric(.data$statId),
          team.abbreviation = as.character(.data$team.abbreviation)
        ) %>%
        dplyr::rename(
          player.esbId = "gsisPlayer.id",
          player.displayName = "playerName",
          teamAbbr = "team.abbreviation"
        ) %>%
        dplyr::select(
          "playId",
          "statId",
          "yards",
          "teamAbbr",
          "player.displayName",
          "player.esbId"
        )

      # there was a penalty on this play so these stat IDs shouldn't exist
      if (id == "2020_10_DEN_LV") {
        stats <- stats %>%
          dplyr::filter(!(.data$playId == 979 & .data$statId %in% c(8, 10, 79)))
      }

      # if I don't put this here it breaks
      suppressWarnings(
        pbp_stats <-
          furrr::future_map(unique(stats$playId), function(x, s) {
            sum_play_stats(x, s)
          }, stats)
      )

      pbp_stats <- dplyr::bind_rows(pbp_stats)

      combined <- game_info %>%
        dplyr::left_join(plays %>% dplyr::select(-"playStats"), by = c("game_id")) %>%
        dplyr::left_join(drives, by = c("driveSequenceNumber" = "drive_order_sequence")) %>%
        dplyr::left_join(pbp_stats, by = c("playId" = "play_id")) %>%
        dplyr::mutate_if(is.logical, as.numeric) %>%
        dplyr::mutate_if(is.integer, as.numeric) %>%
        dplyr::mutate_if(is.factor, as.character) %>%
        janitor::clean_names() %>%
        dplyr::select(-"drive_play_count", -"drive_time_of_possession", -"next_play_type") %>%
        dplyr::rename(
          time = "clock_time",
          play_type_nfl = "play_type",
          posteam = "possession_team_abbreviation",
          yardline = "yard_line",
          sp = "scoring_play",
          drive = "drive_sequence_number",
          nfl_api_id = "game_id",
          drive_play_count = "drive_play_count_2",
          drive_time_of_possession = "drive_time_of_possession_2",
          ydsnet = "drive_ydsnet"
        ) %>%
        dplyr::mutate(
          posteam_id = .data$posteam,
          # have to do all this nonsense to make goal_to_go and yardline_side for compatibility with later functions
          yardline_side = furrr::future_map_chr(
            stringr::str_split(.data$yardline, " "),
            function(x) x[1]
          ),
          yardline_number = as.numeric(furrr::future_map_chr(
            stringr::str_split(.data$yardline, " "),
            function(x) x[2]
          )),
          quarter_end = dplyr::if_else(stringr::str_detect(.data$play_description, "END QUARTER"), 1, 0),
          game_year = as.integer(season),
          season = as.integer(season),
          # this is only needed for epa and dropped later
          game_month = as.integer(11),
          game_id = id,
          play_description = .data$play_description_with_jersey_numbers,
          week = week,
          season_type = season_type,
          play_clock = as.character(.data$play_clock),
          st_play_type = as.character(.data$st_play_type),
          #if JAC has the ball and scored, make them the scoring team
          td_team = dplyr::if_else(
            .data$season <= 2015 & .data$posteam %in% c("JAC", "JAX") &
              .data$drive_how_ended_description == 'Touchdown' & !is.na(.data$td_team),
            'JAC', .data$td_team
          ),
          #if JAC involved in a game and defensive team score, fill in the right team
          td_team = dplyr::if_else(
            #game involving the jags
            .data$season <= 2015 & (.data$home_team %in% c("JAC", "JAX") | .data$away_team %in% c("JAC", "JAX")) &
              #defensive TD
              .data$drive_how_ended_description != 'Touchdown' & !is.na(.data$td_team),
            #if home team has ball, then away team scored, otherwise home team scored
            dplyr::if_else(.data$posteam == .data$home_team, .data$away_team, .data$home_team),
            .data$td_team
          ),
          # fix muffed punt td in JAC game
          td_team = dplyr::if_else(id == "2011_14_TB_JAX" & .data$play_id == 1343, 'JAC', .data$td_team),

          # kickoff return TDs in old JAC games
          td_team = dplyr::if_else(id == "2006_14_IND_JAX" & .data$play_id == 2078, 'JAC', .data$td_team),
          td_team = dplyr::if_else(id == "2007_17_JAX_HOU" & .data$play_id %in% c(1907, 2042), 'HOU', .data$td_team),
          td_team = dplyr::if_else(id == "2008_09_JAX_CIN" & .data$play_id == 3145, 'JAC', .data$td_team),
          td_team = dplyr::if_else(id == "2009_15_IND_JAX" & .data$play_id == 1088, 'IND', .data$td_team),
          td_team = dplyr::if_else(id == "2010_15_JAX_IND" & .data$play_id == 3848, 'IND', .data$td_team),

          # fill in return team for the JAX games
          return_team = dplyr::if_else(
            !is.na(.data$return_team) & .data$season <= 2015 & (.data$home_team %in% c("JAC", "JAX") | .data$away_team %in% c("JAC", "JAX")),
            dplyr::if_else(
              # if the home team has the ball, return team is away team (this is before we flip posteam for kickoffs)
              .data$posteam == .data$home_team, .data$away_team, .data$home_team
            ),
            .data$return_team
          ),
          fumble_recovery_1_team = dplyr::if_else(
            !is.na(.data$fumble_recovery_1_team) & .data$season <= 2015 & (.data$home_team %in% c("JAC", "JAX") | .data$away_team %in% c("JAC", "JAX")),
            # assign possession based on fumble_lost
            dplyr::case_when(
              .data$fumble_lost == 1 & .data$posteam == .data$home_team ~ .data$away_team,
              .data$fumble_lost == 1 & .data$posteam == .data$away_team ~ .data$home_team,
              .data$fumble_lost == 0 & .data$posteam == .data$home_team ~ .data$home_team,
              .data$fumble_lost == 0 & .data$posteam == .data$away_team ~ .data$away_team
            ),
            .data$fumble_recovery_1_team
          ),
          timeout_team = dplyr::if_else(
            # if there's a timeout in the affected seasons
            !is.na(.data$timeout_team) & .data$season <= 2015 & (.data$home_team %in% c("JAC", "JAX") | .data$away_team %in% c("JAC", "JAX")),
            # extract from play description
            # make it JAC instead of JAX to be consistent with everything else
            dplyr::if_else(
              stringr::str_extract(.data$play_description, "(?<=Timeout #[1-3] by )[:upper:]+") == "JAX", "JAC", stringr::str_extract(.data$play_description, "(?<=Timeout #[1-3] by )[:upper:]+")
            ),
            .data$timeout_team
          ),
          # Also fix penalty team for JAC games
          penalty_team = dplyr::if_else(
            # if there's a penalty_team in the affected seasons
            !is.na(.data$penalty_team) & .data$season <= 2015 & (.data$home_team %in% c("JAC", "JAX") | .data$away_team %in% c("JAC", "JAX")),
            # extract from play description
            # make it JAC instead of JAX to be consistent with everything else
            dplyr::if_else(
              stringr::str_extract(.data$play_description, "(?<=PENALTY on )[:upper:]{2,3}") == "JAX",
              "JAC",
              stringr::str_extract(.data$play_description, "(?<=PENALTY on )[:upper:]{2,3}")
            ),
            .data$penalty_team
          ),
          yardline_side = dplyr::if_else(
            .data$season <= 2015 & .data$yardline_side == 'JAX',
            'JAC', .data$yardline_side
          ),
          time = dplyr::case_when(
            id == '2012_04_NO_GB' & .data$play_id == 1085 ~ '3:34',
            id == '2012_16_BUF_MIA' & .data$play_id == 2571 ~ '8:31',
            TRUE ~ .data$time
          ),
          drive_real_start_time = as.character(.data$drive_real_start_time),
          # get the safety team to ensure the correct team gets the points
          # usage of base ifelse is important here for non-scoring games (i.e. early live games)
          safety_team = ifelse(.data$safety == 1, .data$scoring_team_abbreviation, NA_character_),

          # scoring_team_abbreviation messed up on old Jags games so just assume it's defense team
          safety_team = ifelse(
            .data$safety == 1 & .data$season <= 2015 & (.data$home_team %in% c("JAC", "JAX") | .data$away_team %in% c("JAC", "JAX")),
            ifelse(.data$posteam == .data$home_team, .data$away_team, .data$home_team), .data$safety_team
          )

        ) %>%
        dplyr::mutate_all(dplyr::na_if, "")

      # fix for games where home_team == away_team and fields are messed up
      if (bad_game == 1) {
        combined <- combined %>%
          fix_bad_games()
      }

      # nfl didn't fill in first downs on this game
      if (id == '2018_01_ATL_PHI') {
        combined <- combined %>%
          dplyr::mutate(
            first_down_pass = dplyr::if_else(.data$pass_attempt == 1 & .data$first_down == 1, 1, .data$first_down_pass),
            first_down_rush = dplyr::if_else(.data$rush_attempt == 1 & .data$first_down == 1, 1, .data$first_down_rush),

            third_down_converted = dplyr::if_else(.data$first_down == 1 & .data$down == 3, 1, .data$third_down_converted),
            fourth_down_converted = dplyr::if_else(.data$first_down == 1 & .data$down == 4, 1, .data$fourth_down_converted),

            third_down_failed = dplyr::if_else(.data$first_down == 0 & .data$down == 3, 1, .data$third_down_failed),
            fourth_down_failed = dplyr::if_else(.data$first_down == 0 & .data$down == 4 &
                                                  .data$play_type_nfl != "FIELD_GOAL" & .data$play_type_nfl != "PUNT" & .data$play_type_nfl != "PENALTY",
                                                1, .data$fourth_down_failed)
          )
      }

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
      } else if (warn == 4) {
        message(glue::glue("Warning: Either the requested GameID {id} is missing or you've passed an invalid path!"))
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

# helper function to manually fill in fields for problematic games
fix_bad_games <- function(pbp) {

  fixed <- pbp %>%
    dplyr::mutate(
      #if team has the ball and scored, make them the scoring team
      td_team = dplyr::if_else(
          .data$drive_how_ended_description == 'Touchdown' & !is.na(.data$td_team),
        .data$posteam, .data$td_team
      ),
      #if team defensive team score, fill in the right team
      td_team = dplyr::if_else(
        #game involving the jags
          #defensive TD
          .data$drive_how_ended_description != 'Touchdown' & !is.na(.data$td_team),
        #if home team has ball, then away team scored, otherwise home team scored
        dplyr::if_else(.data$posteam == .data$home_team, .data$away_team, .data$home_team),
        .data$td_team
      ),
      # fill in return team
      return_team = dplyr::if_else(
        !is.na(.data$return_team),
        dplyr::if_else(
          # if the home team has the ball, return team is away team (this is before we flip posteam for kickoffs)
          .data$posteam == .data$home_team, .data$away_team, .data$home_team
        ),
        .data$return_team
      ),
      fumble_recovery_1_team = dplyr::if_else(
        !is.na(.data$fumble_recovery_1_team),
        # assign possession based on fumble_lost
        dplyr::case_when(
          .data$fumble_lost == 1 & .data$posteam == .data$home_team ~ .data$away_team,
          .data$fumble_lost == 1 & .data$posteam == .data$away_team ~ .data$home_team,
          .data$fumble_lost == 0 & .data$posteam == .data$home_team ~ .data$home_team,
          .data$fumble_lost == 0 & .data$posteam == .data$away_team ~ .data$away_team
        ),
        .data$fumble_recovery_1_team
      ),
      timeout_team = dplyr::if_else(
        # if there's a timeout in the affected seasons
        !is.na(.data$timeout_team),
        # extract from play description
        stringr::str_extract(.data$play_description, "(?<=Timeout #[1-3] by )[:upper:]+"),
        .data$timeout_team
      )
    )

  return(fixed)

}



