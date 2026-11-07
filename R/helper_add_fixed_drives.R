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
  posteam <- drive_posteam(d)
  prev <- group_prev_index(group_key(d$game_id, d$game_half))
  lag <- function(x, k = 1L) lag_in_group(x, prev, k)
  first_row <- is.na(prev)
  td <- d$touchdown == 1
  is_timeout <- stringr::str_detect(d$desc, "(Timeout)|(Two-Minute Warning)")
  play_types <- c("punt", "pass", "run", "field_goal")

  new_drive <- data.table::fifelse(
    # change in posteam
    posteam != lag(posteam) |
      # change in posteam in t-2 and na posteam in t-1
      (posteam != lag(posteam, 2) & is.na(lag(posteam))) |
      # change in posteam in t-3 and na posteam in t-1 and t-2
      (posteam != lag(posteam, 3) &
        is.na(lag(posteam, 2)) &
        is.na(lag(posteam))),
    1,
    0
  )
  # PAT after defensive TD is not a new drive
  new_drive <- data.table::fifelse(
    lag(td) &
      (lag(posteam) != lag(d$td_team)) &
      # this last part is needed because otherwise it was overwriting
      # the existing value of new_drive with NA on plays following timeouts
      !is.na(lag(posteam)),
    0,
    new_drive
  )
  # PAT after defensive TD is not a new drive even if a Timeout follows the TD
  new_drive <- if_else_missing(
    lag(is_timeout) &
      lag(td, 2) &
      (lag(posteam, 2) != lag(d$td_team, 2)),
    0,
    new_drive,
    new_drive
  )
  # PAT after defensive TD is not a new drive even if 2 Timeouts follow the TD
  new_drive <- if_else_missing(
    lag(is_timeout) &
      lag(is_timeout, 2) &
      lag(td, 3) &
      (lag(posteam, 3) != lag(d$td_team, 3)),
    0,
    new_drive,
    new_drive
  )
  # if same team has the ball as prior play, but prior play was a punt with
  # lost fumble, it's a new drive or if the prior play was a lost fumble or
  # interception
  new_drive <- data.table::fifelse(
    # this line is to prevent it from overwriting already-defined new drives
    # with NA when there's a timeout on prior line
    (new_drive != 1 | is.na(new_drive)) &
      (
        # same team has ball after lost fumble on punt, fg, pass or rush
        (posteam == lag(posteam) &
          lag(d$fumble_lost) == 1 &
          lag(d$play_type) %in% play_types &
          # but not if the play resulted in a touchdown because otherwise the
          # following extra point or 2pt conversion will be new drives
          lag(d$touchdown) == 0) |
          # same team has ball after lost fumble on punt, fg, pass or rush 2
          # plays earlier with prior play missing posteam
          (is.na(lag(posteam)) &
            # posteam is same as posteam 2 plays ago
            posteam == lag(posteam, 2) &
            # lost fumble 2 plays ago
            lag(d$fumble_lost, 2) == 1 &
            lag(d$play_type, 2) %in% play_types &
            # but not if the lost fumble 2 plays ago resulted in a touchdown
            # because otherwise the following extra point or 2pt conversion
            # will be new drives
            lag(d$touchdown, 2) == 0)
      ),
    1,
    new_drive
  )
  # first observation of a half is also a new drive
  new_drive[first_row] <- 1
  # if you recovered an onside kick or muffed return, it's a new drive
  new_drive[is_true(
    d$play_type == "kickoff" &
      (d$own_kickoff_recovery == 1 | d$fumble_lost == 1)
  )] <- 1
  # if it's a kickoff and the prior play was a safety, it's a new drive
  new_drive[
    # safety prior play
    is_true(d$kickoff_attempt == 1 & lag(d$safety) == 1) |
      # safety 2 plays ago and timeout on previous play
      is_true(
        d$kickoff_attempt == 1 &
          lag(d$safety, 2) == 1 &
          (is.na(lag(d$play_type)) | lag(d$play_type) == "no_play")
      )
  ] <- 1
  # if there's a missing, make it not a new drive (0)
  new_drive[is.na(new_drive)] <- 0

  fixed_drive <- cumsum_by(new_drive, group_key(d$game_id))
  tmp_result <- first_match(
    list(
      d$touchdown == 1 & posteam == d$td_team,
      d$touchdown == 1 & posteam != d$td_team,
      d$field_goal_result == "made",
      d$field_goal_result %in% c("blocked", "missed"),
      d$safety == 1,
      d$play_type == "punt" | d$punt_attempt == 1,
      d$interception == 1 | d$fumble_lost == 1,
      d$down == 4 & d$yards_gained < d$ydstogo & d$play_type != "no_play",
      stringr::str_detect(d$desc, "(END QUARTER 2)|(END QUARTER 4)|(END GAME)")
    ),
    c(
      "Touchdown",
      "Opp touchdown",
      "Field goal",
      "Missed field goal",
      "Safety",
      "Punt",
      "Turnover",
      "Turnover on downs",
      "End of half"
    )
  )

  d$fixed_drive <- fixed_drive
  d$fixed_drive_result <- group_result(
    tmp_result,
    group_key(d$game_id, fixed_drive)
  )

  user_message("added fixed drive variables", "done")
  return(d)
}

# The team that has the ball in terms of drives and series
drive_posteam <- function(d) {
  posteam <- d$posteam
  swap <-
    # on kickoffs the kicking team is the defteam but this should be swapped
    # in terms of drives and series if the kickoff is recovered
    is_true(
      d$kickoff_attempt == 1 &
        (d$own_kickoff_recovery == 1 | d$fumble_lost == 1)
    ) |
    # if a kickoff has to be replayed due to a penalty and is then
    # recovered, the prior (reversed) kickoff shouldn't be a new drive/series
    is_true(
      stringr::str_detect(d$desc, kickoff_finder) &
        d$own_kickoff_recovery == 0 &
        dplyr::lead(d$own_kickoff_recovery == 1)
    )
  posteam[swap] <- d$defteam[swap]
  posteam
}

# dplyr::case_when() for character results: the label of the first condition
# that is TRUE, NA if none is
first_match <- function(conditions, labels) {
  out <- rep(NA_character_, length(conditions[[1]]))
  for (i in seq_along(conditions)) {
    hit <- is.na(out) & is_true(conditions[[i]])
    out[hit] <- labels[i]
  }
  out
}
