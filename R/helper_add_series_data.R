################################################################################
# Author: Sebastian Carl
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

add_series_data <- function(pbp) {
  out <-
    pbp %>%
    dplyr::group_by(game_id) %>%
    dplyr::mutate(
      # make down numeric
      down = as.numeric(down),
      # create a first down indicator which marks first down for the offense
      # AND first down after change of possesion (-> drivenumber increases)
      # we don't want a first down being indicated for XP, 2P, KO
      first_down = dplyr::if_else(
        #earn first down
        (first_down_rush == 1 | first_down_pass == 1 | first_down_penalty == 1 |
        #defensive TD
          (touchdown == 1 & td_team != posteam) |
        #drive changes
           (drive < dplyr::lead(drive) | (drive < dplyr::lead(drive, 2) & is.na(dplyr::lead(drive))))
         ) &
          (extra_point_attempt == 0 & two_point_attempt == 0 & kickoff_attempt == 0),
        1, 0
      ),
      # after setting the first down indicator we modify it for the end of a half
      first_down = dplyr::if_else(game_half != dplyr::lead(game_half), 1, first_down),
      # the 'trigger' is being used for calculatung cumsum because we don't want the
      # series number to increase in the play the first down occured but in the next play
      trigger = dplyr::lag(first_down, 1, 0)
    ) %>%
    # now compute series number with cumsum (for the calculation NA are being relaced with 0)
    dplyr::mutate(series = cumsum(tidyr::replace_na(trigger, 0)) + 1) %>%
    dplyr::mutate(
      # now modificated series number for special cases
      series = dplyr::if_else(
        kickoff_attempt == 1 | extra_point_attempt == 1 |
          two_point_attempt == 1 | is.na(down) |
          is.na(posteam),
        NA_real_,
        series
      ),
      series_success = dplyr::case_when(
        is.na(series) | qb_kneel == 1 | qb_spike == 1 ~ NA_real_,
        (touchdown == 1 & td_team == posteam) | first_down_rush == 1 | first_down_pass == 1 |
          first_down_penalty == 1 ~ 1,
        punt_attempt == 1 | interception == 1 | fumble_lost == 1 |
          fourth_down_failed == 1 | field_goal_attempt == 1 ~ 0,
        TRUE ~ 0
      )
    ) %>%
    dplyr::group_by(game_id, series) %>%
    # set series_success value for the whole series
    dplyr::mutate(series_success = dplyr::last(series_success)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-first_down, -trigger)

  message("added series variables")
  return(out)
}
