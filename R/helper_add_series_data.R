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
##  0: punt, interception, fumble lost, turnover on downs, 4th down FG attempt
##  NA: series is NA, series contains QB spike/kneel, half ended with none of above

#' @import dplyr
#' @importFrom tidyr replace_na
#' @importFrom rlang .data
add_series_data <- function(pbp) {
  out <-
    pbp %>%
    dplyr::group_by(.data$game_id) %>%
    dplyr::mutate(
      # make down numeric
      down = as.numeric(.data$down),
      # create a first down indicator which marks first down for the offense
      # AND first down after change of possesion (-> drivenumber increases)
      # we don't want a first down being indicated for XP, 2P, KO
      first_down = dplyr::if_else(
        #earn first down
        (.data$first_down_rush == 1 | .data$first_down_pass == 1 | .data$first_down_penalty == 1 |
           #defensive TD
           (.data$touchdown == 1 & .data$td_team != .data$posteam) |
           # posteam changes
           (
             #change in posteam
             .data$posteam != dplyr::lead(.data$posteam) |
               #change in posteam in t+2 and na posteam in t+1
               (.data$posteam != dplyr::lead(.data$posteam, 2) & is.na(dplyr::lead(.data$posteam))) |
               #change in posteam in t+3 and na posteam in t+1 and t+2
               (.data$posteam != dplyr::lead(.data$posteam, 3) & is.na(dplyr::lead(.data$posteam, 2)) & is.na(dplyr::lead(.data$posteam)))
           )
        ) &
          (.data$extra_point_attempt == 0 & .data$two_point_attempt == 0 & .data$kickoff_attempt == 0),
        1, 0
      ),
      # after setting the first down indicator we modify it for the end of a half
      first_down = dplyr::if_else(.data$game_half != dplyr::lead(.data$game_half), 1, .data$first_down),
      # the 'trigger' is being used for calculatung cumsum because we don't want the
      # series number to increase in the play the first down occured but in the next play
      trigger = dplyr::lag(.data$first_down, 1, 0)
    ) %>%
    # now compute series number with cumsum (for the calculation NA are being relaced with 0)
    dplyr::mutate(series = cumsum(tidyr::replace_na(.data$trigger, 0)) + 1) %>%
    dplyr::mutate(
      # now modificated series number for special cases
      series = dplyr::if_else(
        .data$kickoff_attempt == 1 | .data$extra_point_attempt == 1 |
          .data$two_point_attempt == 1 | is.na(.data$down) |
          is.na(.data$posteam),
        NA_real_,
        .data$series
      ),
      series_success = dplyr::case_when(
        is.na(.data$series) | .data$qb_kneel == 1 | .data$qb_spike == 1 ~ NA_real_,
        (.data$touchdown == 1 & .data$td_team == posteam) | .data$first_down_rush == 1 | .data$first_down_pass == 1 |
          .data$first_down_penalty == 1 ~ 1,
        .data$punt_attempt == 1 | .data$interception == 1 | .data$fumble_lost == 1 |
          .data$fourth_down_failed == 1 | .data$field_goal_attempt == 1 ~ 0,
        TRUE ~ 0
      )
    ) %>%
    dplyr::group_by(.data$game_id, .data$series) %>%
    # set series_success value for the whole series
    dplyr::mutate(series_success = dplyr::last(.data$series_success)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-"first_down", -"trigger")

  message("added series variables")
  return(out)
}
