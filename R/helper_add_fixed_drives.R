################################################################################
# Author: Sebastian Carl, Ben Baldwin
# Purpose: Function to add drive variables
# Code Style Guide: styler::tidyverse_style()
################################################################################

## fixed_drive =
##  starts at 1, each new drive, numbers shared across both teams
## fixed_drive_result =
##  result of  given drive
add_drive_results <- function(d) {
  drive_df <- d %>%
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
      new_drive = dplyr::if_else(
        # change in posteam
        .data$posteam != dplyr::lag(.data$posteam) |
          # change in posteam in t-2 and na posteam in t-1
          (.data$posteam != dplyr::lag(.data$posteam, 2) & is.na(dplyr::lag(.data$posteam))) |
          # change in posteam in t-3 and na posteam in t-1 and t-2
          (.data$posteam != dplyr::lag(.data$posteam, 3) & is.na(dplyr::lag(.data$posteam, 2)) & is.na(dplyr::lag(.data$posteam))),
        1, 0
      ),
      # PAT after defensive TD is not a new drive
      new_drive = dplyr::if_else(
        dplyr::lag(.data$touchdown == 1) &
          (dplyr::lag(.data$posteam) != dplyr::lag(.data$td_team))
          # this last part is needed because otherwise it was overwriting
          # the existing value of new_drive with NA on plays following timeouts
          & !is.na(dplyr::lag(.data$posteam)),
        0,
        .data$new_drive),
      # PAT after defensive TD is not a new drive even if a Timeout follows the TD
      new_drive = dplyr::if_else(
        dplyr::lag(stringr::str_detect(.data$desc, "(Timeout)|(Two-Minute Warning)")) &
          dplyr::lag(.data$touchdown == 1, 2L) &
          (dplyr::lag(.data$posteam, 2L) != dplyr::lag(.data$td_team, 2L)),
        0,
        .data$new_drive,
        missing = .data$new_drive),
      # PAT after defensive TD is not a new drive even if 2 Timeouts follow the TD
      new_drive = dplyr::if_else(
        dplyr::lag(stringr::str_detect(.data$desc, "(Timeout)|(Two-Minute Warning)")) &
          dplyr::lag(stringr::str_detect(.data$desc, "(Timeout)|(Two-Minute Warning)"), 2L) &
          dplyr::lag(.data$touchdown == 1, 3L) &
          (dplyr::lag(.data$posteam, 3L) != dplyr::lag(.data$td_team, 3L)),
        0,
        .data$new_drive,
        missing = .data$new_drive),
      # if same team has the ball as prior play, but prior play was a punt with lost fumble, it's a new drive
      # or if the prior play was a lost fumble or interception
      new_drive = dplyr::if_else(
        # this line is to prevent it from overwriting already-defined new drives with NA
        # when there's a timeout on prior line bc if_else is obnoxious like that
        (.data$new_drive != 1 | is.na(.data$new_drive)) &
          (
            # same team has ball after lost fumble on punt, pass or rush
            (.data$posteam == dplyr::lag(.data$posteam) & dplyr::lag(.data$fumble_lost) == 1 & dplyr::lag(.data$play_type) %in% c("punt", "pass", "run") &
            # but not if the play resulted in a touchdown because otherwise the
            # following extra point or 2pt conversion will be new drives
            dplyr::lag(.data$touchdown) == 0) |

            # same team has ball after lost fumble on punt, pass or rush 2 plays earlier with prior play missing posteam
            (is.na(dplyr::lag(.data$posteam)) &
            # posteam is same as posteam 2 plays ago
            .data$posteam == dplyr::lag(.data$posteam, 2) &
            # lost fumble 2 plays ago
            dplyr::lag(.data$fumble_lost, 2) == 1 & dplyr::lag(.data$play_type, 2) %in% c("punt", "pass", "run") &
            # but not if the lost fumble 2 plays ago resulted in a touchdown because otherwise the
            # following extra point or 2pt conversion will be new drives
            dplyr::lag(.data$touchdown, 2) == 0)
          ),
        1, .data$new_drive
      ),
      # first observation of a half is also a new drive
      new_drive = dplyr::if_else(.data$row == 1, 1, .data$new_drive),

      # if you recovered an onside kick or muffed return, it's a new drive
      new_drive = dplyr::case_when(
        .data$play_type == "kickoff" & (.data$own_kickoff_recovery == 1 | .data$fumble_lost == 1) ~ 1,
        TRUE ~ .data$new_drive
        ),

      # if it's a kickoff and the prior play was a safety, it's a new drive
      new_drive = dplyr::case_when(
        # safety prior play
        .data$kickoff_attempt == 1 & dplyr::lag(.data$safety) == 1 ~ 1,
        # safety 2 plays ago and timeout on previous play
        .data$kickoff_attempt == 1 & dplyr::lag(.data$safety, 2) == 1 & (is.na(dplyr::lag(.data$play_type)) | dplyr::lag(.data$play_type) == "no_play") ~ 1,
        TRUE ~ .data$new_drive
      ),

      # if there's a missing, make it not a new drive (0)
      new_drive = dplyr::if_else(is.na(.data$new_drive), 0, .data$new_drive)
    ) %>%
    dplyr::group_by(.data$game_id) %>%
    dplyr::mutate(
      fixed_drive = cumsum(.data$new_drive),
      tmp_result = dplyr::case_when(
        .data$touchdown == 1 & .data$posteam == .data$td_team ~ "Touchdown",
        .data$touchdown == 1 & .data$posteam != .data$td_team ~ "Opp touchdown",
        .data$field_goal_result == "made" ~ "Field goal",
        .data$field_goal_result %in% c("blocked", "missed") ~ "Missed field goal",
        .data$safety == 1 ~ "Safety",
        .data$play_type == "punt" | .data$punt_attempt == 1 ~ "Punt",
        .data$interception == 1 | .data$fumble_lost == 1 ~ "Turnover",
        .data$down == 4 & .data$yards_gained < .data$ydstogo & .data$play_type != "no_play" ~ "Turnover on downs",
        stringr::str_detect(.data$desc, "(END QUARTER 2)|(END QUARTER 4)|(END GAME)") ~ "End of half"
      )
    ) %>%
    dplyr::group_by(.data$game_id, .data$fixed_drive) %>%
    dplyr::mutate(
      fixed_drive_result =
        dplyr::if_else(
          # if it's end of half, take the first thing we see
          dplyr::last(stats::na.omit(.data$tmp_result)) == "End of half",
          dplyr::first(stats::na.omit(.data$tmp_result)),
          # otherwise take the last
          dplyr::last(stats::na.omit(.data$tmp_result))
        )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(posteam = .data$old_posteam) %>%
    dplyr::select(-"row", -"new_drive", -"tmp_result", -"old_posteam")

  user_message("added fixed drive variables", "done")
  return(drive_df)
}
