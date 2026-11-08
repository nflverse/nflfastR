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
add_series_data <- function(d) {
  # d is the data.table of build_nflfastR_pbp(), changed by reference. It
  # needs the helper columns of add_drive_results() and drops them.

  # new series, with lags within the game half
  d[,
    new_series := {
      lag <- function(x) data.table::shift(x)
      data.table::fifelse(
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
        0,
        na = 0
      )
    },
    by = c("game_id", "game_half")
  ]

  # now compute series number with cumsum
  d[, series := cumsum(new_series), by = "game_id"]
  data.table::set(d, j = "tmp_result", value = play_result(d, series = TRUE))
  d[, series_result := drive_result(tmp_result), by = c("game_id", "series")]
  data.table::set(
    d,
    j = "series_success",
    value = as.numeric(d$series_result %chin% c("Touchdown", "First down"))
  )
  data.table::set(
    d,
    j = c("drive_posteam", "end_of_half", "new_series", "tmp_result"),
    value = NULL
  )

  user_message("added series variables", "done")
  d
}
