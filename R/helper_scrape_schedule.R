################################################################################
# Author: Sebastian Carl and Ben Baldwin
# Purpose: Function for scraping season schedule from the NFL RS Feed
# Code Style Guide: styler::tidyverse_style()
################################################################################

get_season_schedule <- function(season) {
  season_schedule <- data.frame()
  tryCatch(
    expr = {
      request <-
        httr::GET(url = glue::glue("http://www.nfl.com/feeds-rs/schedules/{season}"))

      if (request$status_code == 404) {
        warning(warn <- 1)
      }

      #detect xml or json
      #first do the xml stuff if detected
      if (stringr::str_detect(request$headers$`content-type`, 'xml')) {
        #message('Detected XML, parsing schedule!')

        raw_data <- request %>%
          xml2::read_xml() %>%
          xml2::xml_find_all('.//gameSchedule')

        if (length(raw_data) == 0) {
          warning(warn <- 2)
        }

        #get all the main stuff
        attrs <- xml2::xml_attrs(xml2::xml_find_all(raw_data, "//gameSchedule"))
        names <- attrs[[1]] %>% names()
        sched <- dplyr::bind_cols(attrs) %>%
          t() %>%
          as.data.frame() %>%
          tibble::remove_rownames()
        names(sched) <- names
        sched <- sched %>% janitor::clean_names()

        #now dig out the site stuff
        path = xml2::xml_find_all(raw_data, "//site")

        site_id <- xml2::xml_attr(x = path, attr="siteId") %>% as.data.frame()
        site_city <- xml2::xml_attr(x = path, attr="siteCity")  %>% as.data.frame()
        site_state <- xml2::xml_attr(x = path, attr="siteState")  %>% as.data.frame()
        site_fullname <- xml2::xml_attr(x = path, attr="siteFullname")  %>% as.data.frame()
        site_roof_type <- xml2::xml_attr(x = path, attr="roofType") %>% as.data.frame()

        sites <- dplyr::bind_cols(
          site_id,
          site_city,
          site_fullname,
          site_state,
          site_roof_type
        )
        names(sites) <- names(xml2::xml_attrs(path[[1]]))
        sites <- sites %>% janitor::clean_names()

        season_schedule <- dplyr::bind_cols(sched, sites) %>%
          tibble::as_tibble() %>%
          tibble::remove_rownames()

        season_schedule[] <- lapply(season_schedule, as.character)
        season_schedule <- season_schedule %>%
          dplyr::mutate(
            week = as.integer(week),
            game_id = as.integer(game_id),
            season = as.integer(season),
            # add Lee Sharpe's very useful alt_game_id
            alt_game_id = dplyr::if_else(
              season_type %in% c("REG", "POST"),
              glue::glue("{season}_{formatC(week, width=2, flag=\"0\")}_{visitor_team_abbr}_{home_team_abbr}"),
              NA_character_
            )
          ) %>%
          dplyr::select(
            season,
            season_type,
            week,
            game_id,
            alt_game_id,
            game_date,
            game_time_eastern,
            game_time_local,
            home_team = home_team_abbr,
            away_team = visitor_team_abbr,
            home_team_name = home_display_name,
            away_team_name = visitor_display_name,
            home_nickname,
            away_nickname = visitor_nickname,
            home_team_id,
            away_team_id = visitor_team_id,
            game_type,
            week_name,
            site_city,
            site_fullname,
            site_state,
            site_roof_type = roof_type
          )
      #or do the json stuff if no xml detected
      } else {
        #message('Detected JSON, parsing schedule!')
        raw_data <- request %>%
          httr::content(as = "text", encoding = "UTF-8") %>%
          jsonlite::fromJSON(flatten = TRUE)

        if (is.null(raw_data %>% purrr::pluck("gameSchedules"))) {
          warning(warn <- 2)
        }

        season_schedule <- raw_data %>%
          purrr::pluck("gameSchedules") %>%
          as.data.frame() %>%
          janitor::clean_names() %>%
          dplyr::mutate(
            # add Lee Sharpe's very useful alt_game_id
            alt_game_id = dplyr::if_else(
              season_type %in% c("REG", "POST"),
              glue::glue("{season}_{formatC(week, width=2, flag=\"0\")}_{visitor_team_abbr}_{home_team_abbr}"),
              NA_character_
            )
          ) %>%
          dplyr::select(
            season,
            season_type,
            week,
            game_id,
            alt_game_id,
            game_date,
            game_time_eastern,
            game_time_local,
            home_team = home_team_abbr,
            away_team = visitor_team_abbr,
            home_team_name = home_display_name,
            away_team_name = visitor_display_name,
            home_nickname,
            away_nickname = visitor_nickname,
            home_team_id,
            away_team_id = visitor_team_id,
            game_type,
            week_name,
            site_city = site_site_city,
            site_fullname = site_site_fullname,
            site_state = site_site_state,
            site_roof_type
            #network_channel
          ) %>%
          dplyr::arrange(game_id)
      }
    },
    error = function(e) {
      message("The following error has occured:")
      message(e)
    },
    warning = function(w) {
      if (warn == 1) {
        message(glue::glue("Warning: The requested Season {season} is invalid!"))
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
  return(season_schedule)
}
