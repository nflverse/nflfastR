################################################################################
# Author: Ben Baldwin, Sebastian Carl
# Purpose: Function to add cp and cpoe variables
# Code Style Guide: styler::tidyverse_style()
################################################################################

add_cp <- function(pbp) {
  passes <- pbp %>%
    dplyr::filter(complete_pass == 1 | incomplete_pass == 1 | interception == 1) %>%
    #this step is necessary because NFL doesn't record receiver on incomplete passes prior to 2009
    dplyr::mutate(receiver_player_name =
                    stringr::str_extract(desc, "(?<=((to)|(for))\\s[:digit:]{0,2}\\-{0,1})[A-Z][A-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?")) %>%
    dplyr::filter(
      !is.na(air_yards) & air_yards >= -15 & air_yards < 70 & !is.na(receiver_player_name) &
        !is.na(pass_location) & season >= 2006
    ) %>%
    dplyr::mutate(air_is_zero = dplyr::if_else(air_yards == 0, 1, 0)) %>%
    dplyr::select(
      game_id, play_id, complete_pass, air_yards, pass_location,
      air_is_zero, yardline_100, ydstogo, down, season
    ) %>%
    dplyr::mutate(
      #since cpoe model uses season as a factor and there's no 2020 data to estimate the model
      #pretend that seasons > 2019 are 2019
      #if anyone has better ideas here, please get in touch!
      season = dplyr::if_else(season > 2019, 2019, season)
    )

  if (!nrow(passes) == 0) {
    passes$cp <- mgcv::predict.gam(cp_models, passes, type = "response")
    passes_with_cp <- passes %>%
      dplyr::select(game_id, play_id, cp)

    pbp <- pbp %>%
      dplyr::left_join(passes_with_cp, by = c("game_id", "play_id")) %>%
      dplyr::mutate(
        cp = dplyr::if_else(!is.na(cp), cp, NA_real_),
        cpoe = dplyr::if_else(!is.na(cp), 100 * (complete_pass - cp), NA_real_)
      )
    message("added cp and cpoe")
  } else {
    pbp <- pbp %>%
      dplyr::mutate(
        cp = NA_real_,
        cpoe = NA_real_
      )
    message("No non-NA values for cp calculation detected. cp and cpoe set to NA")
  }

  return(pbp)
}


