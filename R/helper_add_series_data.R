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
  posteam <- drive_posteam(pbp)
  prev <- group_prev_index(group_key(pbp$game_id, pbp$game_half))
  lag <- function(x, k = 1L) lag_in_group(x, prev, k)

  new_series <- data.table::fifelse(
    # a new drive
    pbp$fixed_drive != lag(pbp$fixed_drive) |
      # or a first down on the prior play except touchdown plays
      ((lag(pbp$first_down_rush) == 1 |
        lag(pbp$first_down_pass) == 1 |
        lag(pbp$first_down_penalty) == 1) &
        lag(pbp$touchdown) == 0) |
      # or the first play
      is.na(prev),
    1,
    0
  )
  new_series[is.na(new_series)] <- 0

  # now compute series number with cumsum
  series <- cumsum_by(new_series, group_key(pbp$game_id))
  tmp_result <- first_match(
    list(
      (pbp$first_down_penalty == 1 |
        pbp$first_down_rush == 1 |
        pbp$first_down_pass == 1) &
        pbp$touchdown == 0,
      pbp$touchdown == 1 & posteam == pbp$td_team,
      pbp$touchdown == 1 & posteam != pbp$td_team,
      pbp$field_goal_result == "made",
      pbp$field_goal_result %in% c("blocked", "missed"),
      pbp$safety == 1,
      pbp$play_type == "punt" | pbp$punt_attempt == 1,
      pbp$interception == 1 | pbp$fumble_lost == 1,
      pbp$down == 4 &
        pbp$yards_gained < pbp$ydstogo &
        pbp$play_type != "no_play",
      pbp$qb_kneel == 1,
      stringi::stri_detect_regex(
        pbp$desc,
        "(END QUARTER 2)|(END QUARTER 4)|(END GAME)"
      )
    ),
    c(
      "First down",
      "Touchdown",
      "Opp touchdown",
      "Field goal",
      "Missed field goal",
      "Safety",
      "Punt",
      "Turnover",
      "Turnover on downs",
      "QB kneel",
      "End of half"
    )
  )

  pbp$series <- series
  pbp$series_result <- group_result(
    tmp_result,
    group_key(pbp$game_id, series)
  )
  pbp$series_success <- data.table::fifelse(
    pbp$series_result %in% c("Touchdown", "First down"),
    1,
    0
  )

  user_message("added series variables", "done")
  return(pbp)
}
