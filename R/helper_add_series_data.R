################################################################################
# Author: Sebastian Carl, Ben Baldwin
# Purpose: Function to add series variables analogue Lee Sharpe's Version
# Code Style Guide: styler::tidyverse_style()
################################################################################

## series =
##  starts at 1, each new first down increments, numbers shared across both teams
##  NA: kickoffs, extra point/two point conversion attempts, non-plays, no posteam
## series_success =
##  1: scored touchdown, gained enough yards for first down
##  0: everything else
add_series_data <- function(pbp) {
  out <-
    pbp %>%
    dplyr::mutate(
      old_posteam = .data$posteam,
      posteam = dplyr::case_when(
        # on kickoffs the kicking team is the defteam but this should be swapped
        # in terms of this function if the kickoff is recovered
        .data$kickoff_attempt == 1 & (.data$own_kickoff_recovery == 1 | .data$fumble_lost == 1) ~ .data$defteam,
        # if a kickoff has to be replayed due to a penalty and is then recovered,
        # the prior (reversed) kickoff shouldn't be a new drive/series
        stringr::str_detect(.data$desc, kickoff_finder) &
          .data$own_kickoff_recovery == 0 &
          dplyr::lead(.data$own_kickoff_recovery == 1) ~ .data$defteam,
        TRUE ~ .data$posteam
      )
    ) %>%
    dplyr::group_by(.data$game_id, .data$game_half) %>%
    dplyr::mutate(
      row = 1:dplyr::n(),
      new_series = dplyr::if_else(
        # a new drive
        .data$fixed_drive != dplyr::lag(.data$fixed_drive) |
          # or a first down on the prior play except touchdown plays
          ((dplyr::lag(.data$first_down_rush) == 1 |
              dplyr::lag(.data$first_down_pass) == 1 |
              dplyr::lag(.data$first_down_penalty) == 1) &
             dplyr::lag(.data$touchdown) == 0) |
          # or the first play
          .data$row == 1,
        1, 0
      ),
      new_series = dplyr::if_else(is.na(.data$new_series), 0, .data$new_series)
    ) %>%
    # now compute series number with cumsum (for the calculation NA are being relaced with 0)
    dplyr::group_by(.data$game_id) %>%
    dplyr::mutate(
      series = cumsum(.data$new_series),
      tmp_result = dplyr::case_when(
        (.data$first_down_penalty == 1 | .data$first_down_rush == 1 | .data$first_down_pass == 1) & touchdown == 0 ~ "First down",
        .data$touchdown == 1 & .data$posteam == .data$td_team ~ "Touchdown",
        .data$touchdown == 1 & .data$posteam != .data$td_team ~ "Opp touchdown",
        .data$field_goal_result == "made" ~ "Field goal",
        .data$field_goal_result %in% c("blocked", "missed") ~ "Missed field goal",
        .data$safety == 1 ~ "Safety",
        .data$play_type == "punt" | .data$punt_attempt == 1 ~ "Punt",
        .data$interception == 1 | .data$fumble_lost == 1 ~ "Turnover",
        .data$down == 4 & .data$yards_gained < .data$ydstogo & .data$play_type != "no_play" ~ "Turnover on downs",
        .data$qb_kneel == 1 ~ "QB kneel",
        stringr::str_detect(.data$desc, "(END QUARTER 2)|(END QUARTER 4)|(END GAME)") ~ "End of half"
      )
    ) %>%
    dplyr::group_by(.data$game_id, .data$series) %>%
    dplyr::mutate(
      series_result =
        dplyr::if_else(
          # if it's end of half, take the first thing we see
          dplyr::last(stats::na.omit(.data$tmp_result)) == "End of half",
          dplyr::first(stats::na.omit(.data$tmp_result)),
          # otherwise take the last
          dplyr::last(stats::na.omit(.data$tmp_result))
        ),
      series_success = dplyr::if_else(
        .data$series_result %in% c("Touchdown", "First down"), 1, 0
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(posteam = .data$old_posteam) %>%
    dplyr::select(-"row", -"tmp_result", -"new_series", -"old_posteam")

  user_message("added series variables", "done")
  return(out)
}
