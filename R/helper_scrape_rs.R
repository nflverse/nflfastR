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
      if (game_info$season[1] <= 2002) {
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
        dplyr::arrange(playStatSeq) %>%
        #for for td team not using abbreviations
        dplyr::mutate(teamAbbr = as.character(teamAbbr)) %>%
        fix_team_abbr()

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


fix_team_abbr <- function(pbp) {

  r <- pbp %>%
    dplyr::mutate(
    teamAbbr =
      dplyr::case_when(
        teamAbbr == '1086' ~ 'ARI',
        teamAbbr == '1087' ~ 'ATL',
        teamAbbr == '1088' ~ 'BAL',
        teamAbbr == '1089' ~ 'BUF',
        teamAbbr == '1090' ~ 'CAR',
        teamAbbr == '1091' ~ 'CHI',
        teamAbbr == '1092' ~ 'CIN',
        teamAbbr == '1093' ~ 'CLE',
        teamAbbr == '1094' ~ 'DAL',
        teamAbbr == '1095' ~ 'DEN',
        teamAbbr == '1096' ~ 'DET',
        teamAbbr == '1097' ~ 'GB',
        teamAbbr == '1098' ~ 'IND',
        teamAbbr == '1099' ~ 'JAC',
        teamAbbr == '1100' ~ 'KC',
        teamAbbr == '1101' ~ 'MIA',
        teamAbbr == '1102' ~ 'MIN',
        teamAbbr == '1103' ~ 'NE',
        teamAbbr == '1105' ~ 'NO',
        teamAbbr == '1106' ~ 'NYG',
        teamAbbr == '1107' ~ 'NYJ',
        teamAbbr == '1108' ~ 'OAK',
        teamAbbr == '1109' ~ 'PHI',
        teamAbbr == '1110' ~ 'PIT',
        teamAbbr == '1111' ~ 'SD',
        teamAbbr == '1112' ~ 'SEA',
        teamAbbr == '1113' ~ 'SF',
        teamAbbr == '1114' ~ 'STL',
        teamAbbr == '1115' ~ 'TB',
        teamAbbr == '1116' ~ 'TEN',
        teamAbbr == '1117' ~ 'WAS',
        teamAbbr == '1118' ~ 'HOU',
        teamAbbr == '127' ~ 'ARI',
        teamAbbr == '128' ~ 'ATL',
        teamAbbr == '129' ~ 'BAL',
        teamAbbr == '130' ~ 'BUF',
        teamAbbr == '131' ~ 'CAR',
        teamAbbr == '132' ~ 'CHI',
        teamAbbr == '133' ~ 'CIN',
        teamAbbr == '134' ~ 'CLE',
        teamAbbr == '135' ~ 'DAL',
        teamAbbr == '136' ~ 'DEN',
        teamAbbr == '137' ~ 'DET',
        teamAbbr == '138' ~ 'GB',
        teamAbbr == '139' ~ 'IND',
        teamAbbr == '140' ~ 'JAC',
        teamAbbr == '141' ~ 'KC',
        teamAbbr == '142' ~ 'MIA',
        teamAbbr == '143' ~ 'MIN',
        teamAbbr == '144' ~ 'NE',
        teamAbbr == '145' ~ 'NO',
        teamAbbr == '146' ~ 'NYG',
        teamAbbr == '147' ~ 'NYJ',
        teamAbbr == '148' ~ 'OAK',
        teamAbbr == '149' ~ 'PHI',
        teamAbbr == '150' ~ 'PIT',
        teamAbbr == '151' ~ 'SD',
        teamAbbr == '152' ~ 'SEA',
        teamAbbr == '153' ~ 'SF',
        teamAbbr == '154' ~ 'STL',
        teamAbbr == '155' ~ 'TB',
        teamAbbr == '156' ~ 'TEN',
        teamAbbr == '157' ~ 'WAS',
        teamAbbr == '162' ~ 'ARI',
        teamAbbr == '163' ~ 'ATL',
        teamAbbr == '164' ~ 'BAL',
        teamAbbr == '165' ~ 'BUF',
        teamAbbr == '166' ~ 'CAR',
        teamAbbr == '167' ~ 'CHI',
        teamAbbr == '168' ~ 'CIN',
        teamAbbr == '169' ~ 'CLE',
        teamAbbr == '170' ~ 'DAL',
        teamAbbr == '171' ~ 'DEN',
        teamAbbr == '172' ~ 'DET',
        teamAbbr == '173' ~ 'GB',
        teamAbbr == '174' ~ 'IND',
        teamAbbr == '175' ~ 'JAC',
        teamAbbr == '176' ~ 'KC',
        teamAbbr == '177' ~ 'MIA',
        teamAbbr == '178' ~ 'MIN',
        teamAbbr == '179' ~ 'NE',
        teamAbbr == '181' ~ 'MIN',
        teamAbbr == '182' ~ 'NYG',
        teamAbbr == '183' ~ 'NYJ',
        teamAbbr == '184' ~ 'OAK',
        teamAbbr == '185' ~ 'PHI',
        teamAbbr == '187' ~ 'SD',
        teamAbbr == '188' ~ 'SEA',
        teamAbbr == '189' ~ 'SF',
        teamAbbr == '190' ~ 'STL',
        teamAbbr == '191' ~ 'TB',
        teamAbbr == '192' ~ 'TEN',
        teamAbbr == '193' ~ 'WAS',
        teamAbbr == '2441' ~ 'ARI',
        teamAbbr == '2442' ~ 'ATL',
        teamAbbr == '2444' ~ 'BUF',
        teamAbbr == '2445' ~ 'CAR',
        teamAbbr == '2446' ~ 'CHI',
        teamAbbr == '2447' ~ 'CIN',
        teamAbbr == '2448' ~ 'SEA',
        teamAbbr == '2449' ~ 'DAL',
        teamAbbr == '2451' ~ 'DET',
        teamAbbr == '2452' ~ 'GB',
        teamAbbr == '2453' ~ 'HOU',
        teamAbbr == '2454' ~ 'IND',
        teamAbbr == '2456' ~ 'KC',
        teamAbbr == '2457' ~ 'MIA',
        teamAbbr == '2458' ~ 'MIN',
        teamAbbr == '2461' ~ 'NO',
        teamAbbr == '2462' ~ 'NYG',
        teamAbbr == '2463' ~ 'NYJ',
        teamAbbr == '2464' ~ 'OAK',
        teamAbbr == '2465' ~ 'PHI',
        teamAbbr == '2466' ~ 'PIT',
        teamAbbr == '2468' ~ 'SEA',
        teamAbbr == '2469' ~ 'SF',
        teamAbbr == '2470' ~ 'STL',
        teamAbbr == '2471' ~ 'TB',
        teamAbbr == '2472' ~ 'TEN',
        teamAbbr == '2473' ~ 'WAS',
      TRUE ~ teamAbbr
    )
  )

  return(r)

}


