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
  dt <- as_pbp_dt(d)
  # helper columns, dropped below
  data.table::set(dt, j = "drive_posteam", value = drive_posteam(dt))
  data.table::set(
    dt,
    j = "is_timeout",
    value = stringi::stri_detect_regex(
      dt$desc,
      "(Timeout)|(Two-Minute Warning)"
    )
  )

  # new drives, with lags within the game half
  dt[,
    new_drive := {
      pos <- drive_posteam
      lag <- function(x, k = 1L) data.table::shift(x, k)
      td <- touchdown == 1
      play_types <- c("punt", "pass", "run", "field_goal")

      new_drive <- data.table::fifelse(
        # change in posteam
        pos != lag(pos) |
          # change in posteam in t-2 and na posteam in t-1
          (pos != lag(pos, 2) & is.na(lag(pos))) |
          # change in posteam in t-3 and na posteam in t-1 and t-2
          (pos != lag(pos, 3) & is.na(lag(pos, 2)) & is.na(lag(pos))),
        1,
        0
      )
      # PAT after defensive TD is not a new drive
      new_drive <- data.table::fifelse(
        lag(td) &
          (lag(pos) != lag(td_team)) &
          # this last part is needed because otherwise it was overwriting
          # the existing value of new_drive with NA on plays following timeouts
          !is.na(lag(pos)),
        0,
        new_drive
      )
      # PAT after defensive TD is not a new drive even if a Timeout follows
      # the TD
      new_drive <- data.table::fifelse(
        lag(is_timeout) & lag(td, 2) & (lag(pos, 2) != lag(td_team, 2)),
        0,
        new_drive,
        na = new_drive
      )
      # PAT after defensive TD is not a new drive even if 2 Timeouts follow
      # the TD
      new_drive <- data.table::fifelse(
        lag(is_timeout) &
          lag(is_timeout, 2) &
          lag(td, 3) &
          (lag(pos, 3) != lag(td_team, 3)),
        0,
        new_drive,
        na = new_drive
      )
      # if same team has the ball as prior play, but prior play was a punt
      # with lost fumble, it's a new drive or if the prior play was a lost
      # fumble or interception
      new_drive <- data.table::fifelse(
        # this line is to prevent it from overwriting already-defined new
        # drives with NA when there's a timeout on prior line
        (new_drive != 1 | is.na(new_drive)) &
          (
            # same team has ball after lost fumble on punt, fg, pass or rush
            (pos == lag(pos) &
              lag(fumble_lost) == 1 &
              lag(play_type) %in% play_types &
              # but not if the play resulted in a touchdown because otherwise
              # the following extra point or 2pt conversion will be new drives
              lag(touchdown) == 0) |
              # same team has ball after lost fumble on punt, fg, pass or rush
              # 2 plays earlier with prior play missing posteam
              (is.na(lag(pos)) &
                # posteam is same as posteam 2 plays ago
                pos == lag(pos, 2) &
                # lost fumble 2 plays ago
                lag(fumble_lost, 2) == 1 &
                lag(play_type, 2) %in% play_types &
                # but not if the lost fumble 2 plays ago resulted in a
                # touchdown because otherwise the following extra point or 2pt
                # conversion will be new drives
                lag(touchdown, 2) == 0)
          ),
        1,
        new_drive
      )
      # first observation of a half is also a new drive
      new_drive[1L] <- 1
      # if you recovered an onside kick or muffed return, it's a new drive
      new_drive[
        (play_type == "kickoff" &
          (own_kickoff_recovery == 1 | fumble_lost == 1)) %in%
          TRUE
      ] <- 1
      # if it's a kickoff and the prior play was a safety, it's a new drive
      new_drive[
        # safety prior play
        (kickoff_attempt == 1 & lag(safety) == 1) %in%
          TRUE |
          # safety 2 plays ago and timeout on previous play
          (kickoff_attempt == 1 &
            lag(safety, 2) == 1 &
            (is.na(lag(play_type)) | lag(play_type) == "no_play")) %in%
            TRUE
      ] <- 1
      # if there's a missing, make it not a new drive (0)
      new_drive[is.na(new_drive)] <- 0
      new_drive
    },
    by = c("game_id", "game_half")
  ]

  dt[, fixed_drive := cumsum(new_drive), by = "game_id"]
  data.table::set(
    dt,
    j = "tmp_result",
    value = data.table::fcase(
      dt$touchdown == 1 & dt$drive_posteam == dt$td_team                                ,
      "Touchdown"                                                                       ,
      dt$touchdown == 1 & dt$drive_posteam != dt$td_team                                ,
      "Opp touchdown"                                                                   ,
      dt$field_goal_result == "made"                                                    ,
      "Field goal"                                                                      ,
      dt$field_goal_result %in% c("blocked", "missed")                                  ,
      "Missed field goal"                                                               ,
      dt$safety == 1                                                                    ,
      "Safety"                                                                          ,
      dt$play_type == "punt" | dt$punt_attempt == 1                                     ,
      "Punt"                                                                            ,
      dt$interception == 1 | dt$fumble_lost == 1                                        ,
      "Turnover"                                                                        ,
      dt$down == 4 & dt$yards_gained < dt$ydstogo & dt$play_type != "no_play"           ,
      "Turnover on downs"                                                               ,
      stringi::stri_detect_regex(dt$desc, "(END QUARTER 2)|(END QUARTER 4)|(END GAME)") ,
      "End of half"
    )
  )
  dt[,
    fixed_drive_result := drive_result(tmp_result),
    by = c("game_id", "fixed_drive")
  ]
  data.table::set(
    dt,
    j = c("drive_posteam", "is_timeout", "new_drive", "tmp_result"),
    value = NULL
  )

  user_message("added fixed drive variables", "done")
  tibble::as_tibble(data.table::setDF(dt))
}

# The team that has the ball in terms of drives and series
drive_posteam <- function(d) {
  posteam <- d$posteam
  swap <-
    # on kickoffs the kicking team is the defteam but this should be swapped
    # in terms of drives and series if the kickoff is recovered
    (d$kickoff_attempt == 1 &
      (d$own_kickoff_recovery == 1 | d$fumble_lost == 1)) %in%
    TRUE |
    # if a kickoff has to be replayed due to a penalty and is then
    # recovered, the prior (reversed) kickoff shouldn't be a new drive/series
    (stringi::stri_detect_regex(d$desc, kickoff_finder) &
      d$own_kickoff_recovery == 0 &
      data.table::shift(d$own_kickoff_recovery == 1, type = "lead")) %in%
      TRUE
  posteam[swap] <- d$defteam[swap]
  posteam
}

# The result of a drive or series from the results of its plays: the last
# non-missing result, or the first one if the last is the end of a half
drive_result <- function(result) {
  result <- result[!is.na(result)]
  if (length(result) == 0) {
    NA_character_
  } else if (result[length(result)] == "End of half") {
    result[1L]
  } else {
    result[length(result)]
  }
}
