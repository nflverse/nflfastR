################################################################################
# Author: Ben Baldwin, Sebastian Carl
# Purpose: Function to add cp and cpoe variables.
# CP model created by Zach Feldman: https://github.com/z-feldman/Expected_Completion_NFL
# Code Style Guide: styler::tidyverse_style()
################################################################################

add_cp <- function(pbp) {
  # pbp is the data.table of build_nflfastR_pbp(), changed by reference
  passes <- prepare_cp_data(pbp)
  valid <- which(passes$valid_pass)

  if (length(valid) > 0) {
    # the model is only needed for valid passes
    cp <- rep(NA_real_, nrow(pbp))
    cp[valid] <- stats::predict(
      load_model("cp"),
      model_matrix(
        subset_columns(passes, valid, cp_model_features),
        cp_model_features
      )
    )
    data.table::set(
      pbp,
      j = c("cp", "cpoe"),
      value = list(
        cp,
        data.table::fifelse(
          !is.na(cp),
          100 * (pbp$complete_pass - cp),
          NA_real_
        )
      )
    )
    user_message("added cp and cpoe", "done")
  } else {
    data.table::set(pbp, j = c("cp", "cpoe"), value = NA_real_)
    user_message(
      "No non-NA values for cp calculation detected. cp and cpoe set to NA",
      "info"
    )
  }

  pbp
}

### helper function for getting the data ready
# The features of the cp model and valid_pass as list of columns
prepare_cp_data <- function(pbp) {
  valid <- is_valid_pass(pbp)
  # the receiver name only matters if the other conditions aren't FALSE and
  # there is no receiver id
  receiver_player_name <- apply_where(
    !(valid %in% FALSE) & is.na(pbp$receiver_player_id),
    pbp$desc,
    function(x) {
      stringi::stri_extract_first_regex(
        x,
        "(?<=((to)|(for))\\s[:digit:]{0,2}\\-{0,1})[A-Z][A-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?"
      )
    }
  )
  passes <- lapply(stats::setNames(nm = cp_model_features), function(f) {
    pbp[[f]]
  })
  passes$pass_middle <- data.table::fifelse(pbp$pass_location == "middle", 1, 0)
  passes$air_is_zero <- data.table::fifelse(pbp$air_yards == 0, 1, 0)
  passes$distance_to_sticks <- pbp$air_yards - pbp$ydstogo
  # a valid pass also has an intended receiver
  passes$valid_pass <- valid &
    (!is.na(receiver_player_name) | !is.na(pbp$receiver_player_id))
  passes
}

# Valid pass play for the cp and xyac models: at least -15 air yards, less
# than 70 air yards, has pass location. The intended receiver is checked by
# the callers.
is_valid_pass <- function(pbp) {
  (pbp$complete_pass == 1 | pbp$incomplete_pass == 1 | pbp$interception == 1) &
    !is.na(pbp$air_yards) &
    pbp$air_yards >= -15 &
    pbp$air_yards < 70 &
    !is.na(pbp$pass_location)
}

cp_model_features <- c(
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
  "distance_to_sticks"
)
