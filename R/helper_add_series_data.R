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
  dt <- as_pbp_dt(pbp)
  # helper column, dropped below
  data.table::set(dt, j = "drive_posteam", value = drive_posteam(dt))

  # new series, with lags within the game half
  dt[,
    new_series := {
      lag <- data.table::shift
      new_series <- data.table::fifelse(
        # a new drive
        fixed_drive != lag(fixed_drive) |
          # or a first down on the prior play except touchdown plays
          ((lag(first_down_rush) == 1 |
            lag(first_down_pass) == 1 |
            lag(first_down_penalty) == 1) &
            lag(touchdown) == 0) |
          # or the first play
          seq_len(.N) == 1L,
        1,
        0
      )
      new_series[is.na(new_series)] <- 0
      new_series
    },
    by = c("game_id", "game_half")
  ]

  # now compute series number with cumsum
  dt[, series := cumsum(new_series), by = "game_id"]
  data.table::set(
    dt,
    j = "tmp_result",
    value = data.table::fcase(
      (dt$first_down_penalty == 1 | dt$first_down_rush == 1 | dt$first_down_pass == 1) & dt$touchdown == 0 ,
      "First down"                                                                                         ,
      dt$touchdown == 1 & dt$drive_posteam == dt$td_team                                                   ,
      "Touchdown"                                                                                          ,
      dt$touchdown == 1 & dt$drive_posteam != dt$td_team                                                   ,
      "Opp touchdown"                                                                                      ,
      dt$field_goal_result == "made"                                                                       ,
      "Field goal"                                                                                         ,
      dt$field_goal_result %in% c("blocked", "missed")                                                     ,
      "Missed field goal"                                                                                  ,
      dt$safety == 1                                                                                       ,
      "Safety"                                                                                             ,
      dt$play_type == "punt" | dt$punt_attempt == 1                                                        ,
      "Punt"                                                                                               ,
      dt$interception == 1 | dt$fumble_lost == 1                                                           ,
      "Turnover"                                                                                           ,
      dt$down == 4 & dt$yards_gained < dt$ydstogo & dt$play_type != "no_play"                              ,
      "Turnover on downs"                                                                                  ,
      dt$qb_kneel == 1                                                                                     ,
      "QB kneel"                                                                                           ,
      stringi::stri_detect_regex(dt$desc, "(END QUARTER 2)|(END QUARTER 4)|(END GAME)")                    ,
      "End of half"
    )
  )
  dt[, series_result := drive_result(tmp_result), by = c("game_id", "series")]
  data.table::set(
    dt,
    j = "series_success",
    value = data.table::fifelse(
      dt$series_result %in% c("Touchdown", "First down"),
      1,
      0
    )
  )
  data.table::set(
    dt,
    j = c("drive_posteam", "new_series", "tmp_result"),
    value = NULL
  )

  user_message("added series variables", "done")
  tibble::as_tibble(data.table::setDF(dt))
}
