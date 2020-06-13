################################################################################
# Author: Ben Baldwin, Sebastian Carl
# Purpose: Function to add cp and cpoe variables.
# CP model created by Zach Feldman: https://github.com/z-feldman/Expected_Completion_NFL
# Code Style Guide: styler::tidyverse_style()
################################################################################

add_cp <- function(pbp) {

  #testing only
  #pbp <- g

  passes <- prepare_cp_data(pbp)

  if (!nrow(passes %>% dplyr::filter(valid_pass == 1)) == 0) {

    pred <- stats::predict(cp_model, as.matrix(passes %>% dplyr::select(-complete_pass, -valid_pass))) %>%
      tibble::as_tibble() %>%
      dplyr::rename(cp = value) %>%
      dplyr::bind_cols(passes) %>%
      dplyr::select(cp, valid_pass)

    pbp <- pbp %>%
      dplyr::bind_cols(pred) %>%
      dplyr::mutate(
        cp = dplyr::if_else(
          valid_pass == 1, cp, NA_real_
        ),
        cpoe = dplyr::if_else(!is.na(cp), 100 * (complete_pass - cp), NA_real_)
      ) %>%
      dplyr::select(-valid_pass)

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


### helper function for getting the data ready
prepare_cp_data <- function(pbp) {

  # valid pass play: at least -15 air yards, less than 70 air yards, has intended receiver, has pass location
  passes <- pbp %>%
    dplyr::mutate(receiver_player_name =
                    stringr::str_extract(desc, "(?<=((to)|(for))\\s[:digit:]{0,2}\\-{0,1})[A-Z][A-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?"),
                  pass_middle = dplyr::if_else(pass_location == 'middle', 1, 0),
                  air_is_zero= dplyr::if_else(air_yards == 0,1,0),
                  distance_to_sticks = air_yards - ydstogo,
                  valid_pass = dplyr::if_else(
                    (complete_pass == 1 | incomplete_pass == 1 | interception == 1) &
                      !is.na(air_yards) & air_yards >= -15 & air_yards <70 & !is.na(receiver_player_name) & !is.na(pass_location),
                    1, 0
                  )
    ) %>%
    dplyr::select(
      complete_pass, air_yards, yardline_100, ydstogo,
                  down1, down2, down3, down4, air_is_zero, pass_middle,
                  era2, era3, era4, qb_hit, home, model_week,
                  outdoors, retractable, dome, distance_to_sticks, valid_pass
      )

}





