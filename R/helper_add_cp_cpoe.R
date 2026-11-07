################################################################################
# Author: Ben Baldwin, Sebastian Carl
# Purpose: Function to add cp and cpoe variables.
# CP model created by Zach Feldman: https://github.com/z-feldman/Expected_Completion_NFL
# Code Style Guide: styler::tidyverse_style()
################################################################################

add_cp <- function(pbp) {
  # testing only
  # pbp <- g

  passes <- prepare_cp_data(pbp)
  valid <- which(passes$valid_pass == 1)

  if (length(valid) > 0) {
    # the model is only needed for valid passes
    cp <- rep(NA_real_, nrow(pbp))
    cp[valid] <- stats::predict(
      load_model("cp"),
      as.matrix(
        passes[valid, ] |> dplyr::select(-"complete_pass", -"valid_pass")
      )
    )
    pbp$cp <- cp
    pbp$cpoe <- data.table::fifelse(
      !is.na(cp),
      100 * (pbp$complete_pass - cp),
      NA_real_
    )

    user_message("added cp and cpoe", "done")
  } else {
    pbp <- pbp |>
      dplyr::mutate(
        cp = NA_real_,
        cpoe = NA_real_
      )
    user_message(
      "No non-NA values for cp calculation detected. cp and cpoe set to NA",
      "info"
    )
  }

  return(pbp)
}


### helper function for getting the data ready
prepare_cp_data <- function(pbp) {
  # valid pass play: at least -15 air yards, less than 70 air yards, has intended receiver, has pass location
  passes <- pbp |>
    dplyr::mutate(
      # all conditions of a valid pass except the receiver (helper)
      tmp_valid = (.data$complete_pass == 1 |
        .data$incomplete_pass == 1 |
        .data$interception == 1) &
        !is.na(.data$air_yards) &
        .data$air_yards >= -15 &
        .data$air_yards < 70 &
        !is.na(.data$pass_location),
      # the receiver name only matters if the other conditions aren't FALSE
      # and there is no receiver id
      receiver_player_name = apply_where(
        !(.data$tmp_valid %in% FALSE) & is.na(.data$receiver_player_id),
        .data$desc,
        function(x) {
          stringr::str_extract(
            x,
            "(?<=((to)|(for))\\s[:digit:]{0,2}\\-{0,1})[A-Z][A-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?"
          )
        }
      ),
      pass_middle = dplyr::if_else(.data$pass_location == "middle", 1, 0),
      air_is_zero = dplyr::if_else(.data$air_yards == 0, 1, 0),
      distance_to_sticks = .data$air_yards - .data$ydstogo,
      valid_pass = dplyr::if_else(
        .data$tmp_valid &
          (!is.na(.data$receiver_player_name) |
            !is.na(.data$receiver_player_id)),
        1,
        0
      ),
      tmp_valid = NULL
    ) |>
    dplyr::select(
      "complete_pass",
      "air_yards",
      "yardline_100",
      "ydstogo",
      "down1",
      "down2",
      "down3",
      "down4",
      "air_is_zero",
      "pass_middle",
      "era2",
      "era3",
      "era4",
      "qb_hit",
      "home",
      "outdoors",
      "retractable",
      "dome",
      "distance_to_sticks",
      "valid_pass"
    )
}
