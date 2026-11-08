################################################################################
# Author: Sebastian Carl
# Purpose: Functions to add ep(a) and wp(a) variables
# Code Style Guide: styler::tidyverse_style()
################################################################################

add_ep <- function(pbp) {
  out <- pbp |> add_ep_variables()
  user_message("added ep variables", "done")
  return(out)
}

add_air_yac_ep <- function(pbp) {
  if (!any(!is.na(pbp$air_yards))) {
    cols <- c(
      "air_epa",
      "yac_epa",
      "comp_air_epa",
      "comp_yac_epa",
      "home_team_comp_air_epa",
      "away_team_comp_air_epa",
      "home_team_comp_yac_epa",
      "away_team_comp_yac_epa",
      "total_home_comp_air_epa",
      "total_away_comp_air_epa",
      "total_home_comp_yac_epa",
      "total_away_comp_yac_epa",
      "home_team_raw_air_epa",
      "away_team_raw_air_epa",
      "home_team_raw_yac_epa",
      "away_team_raw_yac_epa",
      "total_home_raw_air_epa",
      "total_away_raw_air_epa",
      "total_home_raw_yac_epa",
      "total_away_raw_yac_epa"
    )
    data.table::set(pbp, j = cols, value = NA_real_)
    user_message(
      "No non-NA air_yards detected. air_yac_ep variables set to NA",
      "info"
    )
  } else {
    pbp <- add_air_yac_ep_variables(pbp)
    user_message("added air_yac_ep variables", "done")
  }
  pbp
}

add_wp <- function(pbp) {
  out <- pbp |> add_wp_variables()
  user_message("added wp variables", "done")
  return(out)
}

add_air_yac_wp <- function(pbp) {
  if (!any(!is.na(pbp$air_yards))) {
    cols <- c(
      "air_wpa",
      "yac_wpa",
      "comp_air_wpa",
      "comp_yac_wpa",
      "home_team_comp_air_wpa",
      "away_team_comp_air_wpa",
      "home_team_comp_yac_wpa",
      "away_team_comp_yac_wpa",
      "total_home_comp_air_wpa",
      "total_away_comp_air_wpa",
      "total_home_comp_yac_wpa",
      "total_away_comp_yac_wpa",
      "home_team_raw_air_wpa",
      "away_team_raw_air_wpa",
      "home_team_raw_yac_wpa",
      "away_team_raw_yac_wpa",
      "total_home_raw_air_wpa",
      "total_away_raw_air_wpa",
      "total_home_raw_yac_wpa",
      "total_away_raw_yac_wpa"
    )
    data.table::set(pbp, j = cols, value = NA_real_)
    user_message(
      "No non-NA air_yards detected. air_yac_wp variables set to NA",
      "info"
    )
  } else {
    pbp <- add_air_yac_wp_variables(pbp)
    user_message("added air_yac_wp variables", "done")
  }
  pbp
}

# Run the prediction function fn() only on the rows i of data (a data.table or
# a list of columns). The result has one element for every row of data and is
# NA outside of i. Model predictions of a row don't depend on other rows, so
# the values in i are the same as if fn() ran on all rows.
predict_rows <- function(fn, data, i) {
  out <- rep(NA_real_, length(data[[1L]]))
  if (length(i) > 0) {
    out[i] <- fn(lapply(data, `[`, i))
  }
  out
}

# The columns cols of pbp in the rows i, as list of columns. Steps that work on
# a part of the plays use it to copy only the columns they need.
subset_columns <- function(pbp, i, cols) {
  cols <- intersect(cols, names(pbp))
  lapply(stats::setNames(nm = cols), function(col) pbp[[col]][i])
}

# Expand a data frame of predictions for the rows i to n rows with NA elsewhere
expand_rows <- function(preds, n, i) {
  out <- preds[rep(NA_integer_, n), , drop = FALSE]
  rownames(out) <- NULL
  if (length(i) > 0) {
    out[i, ] <- preds
  }
  out
}

# The features of the EP model, in the order the model expects
ep_model_features <- c(
  "half_seconds_remaining",
  "yardline_100",
  "home",
  "retractable",
  "dome",
  "outdoors",
  "ydstogo",
  "era0",
  "era1",
  "era2",
  "era3",
  "era4",
  "down1",
  "down2",
  "down3",
  "down4",
  "posteam_timeouts_remaining",
  "defteam_timeouts_remaining"
)

# EP predictions for several data frames with one call of the model. Returns a
# list with a data frame of probabilities for each data frame.
get_preds_stacked <- function(data_list) {
  matrices <- lapply(data_list, function(pbp) {
    m <- model_matrix(pbp, ep_model_features)
    if ("location" %in% names(pbp)) {
      m[, "home"] <- data.table::fifelse(pbp$location == "Neutral", 0, pbp$home)
    }
    m
  })
  preds <- stats::predict(load_model("ep"), do.call(rbind, matrices))

  # xgboost v3 returns a matrix of predictions instead of a vector as returned
  # by xgboost v1.
  if (is.vector(preds)) {
    preds <- matrix(preds, ncol = 7, byrow = TRUE)
  }
  preds <- as.data.frame(preds)
  colnames(preds) <- c(
    "Touchdown",
    "Opp_Touchdown",
    "Field_Goal",
    "Opp_Field_Goal",
    "Safety",
    "Opp_Safety",
    "No_Score"
  )
  part <- rep(seq_along(matrices), vapply(matrices, nrow, integer(1)))
  lapply(seq_along(matrices), function(k) {
    out <- preds[part == k, , drop = FALSE]
    rownames(out) <- NULL
    out
  })
}

#get predictions for a set of pbp data
#for predict stage of EP
get_preds <- function(pbp) {
  get_preds_stacked(list(pbp))[[1]]
}

#get predictions for a set of pbp data
#for predict stage
get_preds_wp <- function(pbp) {
  stats::predict(load_model("wp"), model_matrix(pbp, wp_model_features))
}

#get predictions for a set of pbp data
#for predict stage
get_preds_wp_spread <- function(pbp) {
  stats::predict(
    load_model("wp_spread"),
    model_matrix(pbp, wp_spread_model_features)
  )
}


#get the columns needed for wp predictions
#making sure they're in the right order
wp_model_features <- c(
  "receive_2h_ko",
  "home",
  "half_seconds_remaining",
  "game_seconds_remaining",
  "Diff_Time_Ratio",
  "score_differential",
  "down",
  "ydstogo",
  "yardline_100",
  "posteam_timeouts_remaining",
  "defteam_timeouts_remaining"
)

#get the columns needed for wp predictions
#making sure they're in the right order
wp_spread_model_features <- c(
  "receive_2h_ko",
  "spread_time",
  "home",
  "half_seconds_remaining",
  "game_seconds_remaining",
  "Diff_Time_Ratio",
  "score_differential",
  "down",
  "ydstogo",
  "yardline_100",
  "posteam_timeouts_remaining",
  "defteam_timeouts_remaining"
)
prepare_wp_data <- function(pbp) {
  if (anyNA(pbp$spread_line)) {
    broken_games <- sort(unique(pbp$game_id[is.na(pbp$spread_line)]))
    cli::cli_alert_danger(
      "The following game{?s} {?is/are} missing valid spread lines: {.val {broken_games}}."
    )
    cli::cli_alert_warning(
      "nflfastR will manually set the spread for the home team to {.val 1.5} points!"
    )
    cli::cli_alert_warning(
      "If you see this, please reach out to the package maintainers {.url https://github.com/nflverse/nflfastR/issues}"
    )
    pbp[is.na(spread_line), spread_line := 1.5]
  }

  pbp[,
    receive_2h_ko := as.numeric(
      qtr <= 2 & posteam == defteam[!is.na(defteam)][1L]
    ),
    by = "game_id"
  ]
  data.table::set(
    pbp,
    j = "posteam_spread",
    value = data.table::fifelse(
      pbp$home == 1,
      pbp$spread_line,
      -1 * pbp$spread_line
    )
  )
  data.table::set(
    pbp,
    j = "elapsed_share",
    value = (3600 - pbp$game_seconds_remaining) / 3600
  )
  data.table::set(
    pbp,
    j = "spread_time",
    value = pbp$posteam_spread * exp(-4 * pbp$elapsed_share)
  )
  data.table::set(
    pbp,
    j = "Diff_Time_Ratio",
    value = pbp$score_differential /
      (exp(-4 * pbp$elapsed_share))
  )

  pbp
}


#add ep variables
#All of these are heavily borrowed from nflscrapR (Maksim Horowitz, Ronald Yurko, and Samuel Ventura)
add_ep_variables <- function(pbp_data) {
  #testing
  #pbp_data <- g

  # ----------------------------------------------------------------------------
  # ---- special case: deal with FG attempts
  # Now make another dataset that to get the EP probabilities from a missed FG:
  # the scenario data are lists of columns (modifying them doesn't touch
  # pbp_data)
  missed_fg_data <- as.list(pbp_data)
  # Subtract 5.065401 from TimeSecs:
  missed_fg_data$half_seconds_remaining <- missed_fg_data$half_seconds_remaining -
    5.065401

  # Correct the yrdline100:
  missed_fg_data$yardline_100 <- 100 - (missed_fg_data$yardline_100 + 8)
  # Now first down:
  missed_fg_data$down1 <- rep(1, nrow(pbp_data))
  missed_fg_data$down2 <- rep(0, nrow(pbp_data))
  missed_fg_data$down3 <- rep(0, nrow(pbp_data))
  missed_fg_data$down4 <- rep(0, nrow(pbp_data))
  # 10 ydstogo:
  missed_fg_data$ydstogo <- rep(10, nrow(pbp_data))

  # Find the FG attempts and extra points. The predictions of the special
  # cases below are only needed for those plays.
  fg_attempt_i <- which(pbp_data$play_type == "field_goal")
  extrapoint_i <- which(
    (pbp_data$play_type == "extra_point" |
      pbp_data$play_type_nfl == "XP_KICK") &
      (is.na(pbp_data$play_type_nfl) | pbp_data$play_type_nfl != "PAT2")
  )

  # ---- special case: deal with kickoffs (probabilities are updated below)
  # Calculate the EP for receiving a touchback (from the point of view for
  # recieving team)
  kickoff_data <- as.list(pbp_data)

  # Change the yard line to be 80 for 2009-2015 and 75 otherwise
  # (accounting for the fact that Jan 2016 is in the 2015 season:
  kickoff_data$yardline_100 <- data.table::fifelse(
    kickoff_data$season < 2016,
    80,
    75
  )
  # Now first down:
  kickoff_data$down1 <- rep(1, nrow(pbp_data))
  kickoff_data$down2 <- rep(0, nrow(pbp_data))
  kickoff_data$down3 <- rep(0, nrow(pbp_data))
  kickoff_data$down4 <- rep(0, nrow(pbp_data))
  # 10 ydstogo:
  kickoff_data$ydstogo <- rep(10, nrow(pbp_data))

  # Find the kickoffs:
  kickoff_i <- which(
    pbp_data$play_type == "kickoff" | pbp_data$kickoff_attempt == 1
  )

  # Get the predicted probabilites of all plays and of the special cases with
  # one call of the EP model
  preds <- get_preds_stacked(list(
    pbp_data,
    lapply(missed_fg_data, `[`, fg_attempt_i),
    lapply(kickoff_data, `[`, kickoff_i)
  ))
  base_ep_preds <- preds[[1]]
  missed_fg_ep_preds <- expand_rows(preds[[2]], nrow(pbp_data), fg_attempt_i)
  kickoff_preds <- expand_rows(preds[[3]], nrow(pbp_data), kickoff_i)

  # Find the rows where TimeSecs_Remaining became 0 or negative and make all the probs equal to 0:
  end_game_i <- which(missed_fg_data$half_seconds_remaining <= 0)
  missed_fg_ep_preds[end_game_i, ] <- rep(0, ncol(missed_fg_ep_preds))

  # if the half ends, no one scored
  missed_fg_ep_preds[end_game_i, "No_Score"] <- 1

  # Get the probability of making the field goal:
  make_fg_prob <- predict_rows(
    function(data) {
      as.numeric(mgcv::predict.bam(
        fastrmodels::fg_model,
        newdata = as.data.frame(data),
        type = "response"
      ))
    },
    pbp_data,
    sort(unique(c(fg_attempt_i, extrapoint_i)))
  )

  # Multiply each value of the missed_fg_ep_preds by the 1 - make_fg_prob
  missed_fg_ep_preds <- missed_fg_ep_preds * (1 - make_fg_prob)
  # Now update the probabilities for the FG attempts (also includes Opp_Field_Goal probability from missed_fg_ep_preds)
  base_ep_preds[fg_attempt_i, "Field_Goal"] <- make_fg_prob[fg_attempt_i] +
    missed_fg_ep_preds[fg_attempt_i, "Opp_Field_Goal"]
  # Update the other columns based on the opposite possession:
  base_ep_preds[
    fg_attempt_i,
    c(
      "Touchdown",
      "Opp_Field_Goal",
      "Opp_Touchdown",
      "Safety",
      "Opp_Safety",
      "No_Score"
    )
  ] <- missed_fg_ep_preds[
    fg_attempt_i,
    c(
      "Opp_Touchdown",
      "Field_Goal",
      "Touchdown",
      "Opp_Safety",
      "Safety",
      "No_Score"
    )
  ]

  # ----------------------------------------------------------------------------------
  # ---- special case: deal with kickoffs
  # update the columns for Kickoff plays with the touchback probabilities

  # Now update the probabilities:
  base_ep_preds[kickoff_i, ] <- kickoff_preds[kickoff_i, ]

  # ----------------------------------------------------------------------------------
  # Insert probabilities of 0 for everything but No_Score for QB Kneels that
  # occur on the possession team's side of the field:
  # Find these QB Kneels:
  qb_kneels_i <- which(
    pbp_data$play_type == "qb_kneel" & pbp_data$yardline_100 > 50
  )

  # Now update the probabilities:
  base_ep_preds[qb_kneels_i, ] <- 0
  base_ep_preds[qb_kneels_i, "No_Score"] <- 1

  # ----------------------------------------------------------------------------------
  # Create two new columns, ExPoint_Prob and TwoPoint_Prob, for the PAT events:
  base_ep_preds$ExPoint_Prob <- 0
  base_ep_preds$TwoPoint_Prob <- 0

  # Find the indices for these types of plays:
  twopoint_i <- which(pbp_data$two_point_attempt == 1)

  #new: special case for PAT or kickoff with penalty
  #for inserting NAs
  lag <- function(x) data.table::shift(x)
  lead <- function(x) data.table::shift(x, type = "lead")
  st_penalty_i_1 <- which(
    # pat: prior play was TD or PAT or Timeout and next play is PAT and this play isn't a td and it's not a regular down
    (pbp_data$touchdown == 0 &
      is.na(pbp_data$down) &
      (lag(pbp_data$touchdown) == 1 |
        lag(pbp_data$play_type_nfl) == "XP_KICK" |
        lag(pbp_data$timeout) == 1) &
      (lead(pbp_data$two_point_attempt) == 1 |
        lead(pbp_data$extra_point_attempt) == 1 |
        lead(pbp_data$play_type_nfl) == "XP_KICK")) |
      #kickoff: prior play was PAT and next play is kickoff
      ((lag(pbp_data$two_point_attempt) == 1 |
        lag(pbp_data$extra_point_attempt) == 1) &
        lead(pbp_data$kickoff_attempt == 1))
  )

  # down is NA and play type no play
  no_play_without_down <- is.na(pbp_data$down) & pbp_data$play_type == "no_play"
  # has a key term in desc
  st_penalty_term <- no_play_without_down &
    stringi::stri_detect_regex(
      pbp_data$desc,
      "Kick formation|Pass formation|kicks onside|Offside on Free Kick"
    )
  # two point conversion and next play isn't a kickoff
  st_penalty_two_point <- no_play_without_down &
    stringi::stri_detect_fixed(pbp_data$desc, "TWO-POINT CONVERSION") &
    lead(pbp_data$kickoff_attempt) == 0
  st_penalty_i_2 <- which(
    is.na(lead(pbp_data$down)) & (st_penalty_term | st_penalty_two_point)
  )

  # Assign the make_fg_probs of the extra-point PATs:
  base_ep_preds$ExPoint_Prob[extrapoint_i] <- make_fg_prob[extrapoint_i]

  # Assign the TwoPoint_Prob with the historical success rate:
  base_ep_preds$TwoPoint_Prob[twopoint_i] <- 0.4735

  # ----------------------------------------------------------------------------------
  # Insert NAs for timeouts and end of play rows:
  missing_i <- which(
    (pbp_data$timeout == 1 &
      pbp_data$play_type == "no_play" &
      !stringi::stri_detect_regex(
        pbp_data$desc,
        " (pass|sacked|scramble|punts|up the middle|left end|left guard|left tackle|right end|right guard|right tackle) "
      )) |
      is.na(pbp_data$play_type)
  )

  # Now update the probabilities for missing and PATs:
  ep_outcomes <- names(kickoff_preds)
  base_ep_preds[
    c(missing_i, extrapoint_i, twopoint_i, st_penalty_i_1, st_penalty_i_2),
    ep_outcomes
  ] <- 0

  # Rename the events to all have _Prob at the end of them:
  names(base_ep_preds)[match(ep_outcomes, names(base_ep_preds))] <- paste0(
    ep_outcomes,
    "_Prob"
  )

  # Join them together:
  data.table::set(
    pbp_data,
    j = names(base_ep_preds),
    value = as.list(base_ep_preds)
  )

  # Calculate the ExpPts:
  data.table::set(
    pbp_data,
    j = "ExpPts",
    value = (0 * pbp_data$No_Score_Prob) +
      (-3 * pbp_data$Opp_Field_Goal_Prob) +
      (-2 * pbp_data$Opp_Safety_Prob) +
      (-7 * pbp_data$Opp_Touchdown_Prob) +
      (3 * pbp_data$Field_Goal_Prob) +
      (2 * pbp_data$Safety_Prob) +
      (7 * pbp_data$Touchdown_Prob) +
      (1 * pbp_data$ExPoint_Prob) +
      (2 * pbp_data$TwoPoint_Prob)
  )

  #just going to set these to NA bc we have no way of calculating EPA for them
  data.table::set(
    pbp_data,
    i = c(st_penalty_i_1, st_penalty_i_2, missing_i),
    j = "ExpPts",
    value = NA_real_
  )

  #################################################################
  # Calculate EPA:

  # fill missing values with the next value of the same game
  pbp_data[,
    c("ep", "tmp_posteam") := list(
      data.table::nafill(ExpPts, type = "nocb"),
      # nafill() only works for numbers, so we fill the row index instead
      posteam[data.table::nafill(
        data.table::fifelse(is.na(posteam), NA_integer_, seq_len(.N)),
        type = "nocb"
      )]
    ),
    by = "game_id"
  ]

  # get epa for non-scoring plays
  data.table::set(
    pbp_data,
    j = "home_ep",
    value = data.table::fifelse(
      pbp_data$tmp_posteam == pbp_data$home_team,
      pbp_data$ep,
      -pbp_data$ep
    )
  )
  pbp_data[,
    home_epa := data.table::shift(home_ep, type = "lead") - home_ep,
    by = "game_id"
  ]
  data.table::set(
    pbp_data,
    j = "epa",
    value = data.table::fifelse(
      pbp_data$tmp_posteam == pbp_data$home_team,
      pbp_data$home_epa,
      -pbp_data$home_epa
    )
  )
  # scoring plays (later updates win)
  # td
  pbp_data[
    !is.na(td_team),
    epa := data.table::fifelse(td_team == posteam, 7 - ep, -7 - ep)
  ]
  # Offense field goal:
  pbp_data[is.na(td_team) & field_goal_made == 1, epa := 3 - ep]
  # Offense extra-point:
  pbp_data[
    is.na(td_team) & field_goal_made == 0 & extra_point_good == 1,
    epa := 1 - ep
  ]
  # Offense two-point conversion:
  pbp_data[
    is.na(td_team) &
      field_goal_made == 0 &
      extra_point_good == 0 &
      (two_point_rush_good == 1 |
        two_point_pass_good == 1 |
        two_point_pass_reception_good == 1),
    epa := 2 - ep
  ]
  # Failed PAT (both 1 and 2):
  pbp_data[
    is.na(td_team) &
      field_goal_made == 0 &
      extra_point_good == 0 &
      ((extra_point_failed == 1 |
        extra_point_blocked == 1 |
        extra_point_aborted == 1) |
        (two_point_rush_failed == 1 |
          two_point_pass_failed == 1 |
          two_point_pass_reception_failed == 1)),
    epa := 0 - ep
  ]
  # Opponent scores defensive 2 point:
  pbp_data[defensive_two_point_conv == 1, epa := -2 - ep]
  # Safety:
  pbp_data[safety_team == defteam, epa := -2 - ep]
  pbp_data[safety_team == posteam, epa := 2 - ep]

  # Now rename each of the expected points columns to match the style of
  # the updated code:
  data.table::setnames(
    pbp_data,
    c(
      "No_Score_Prob",
      "Opp_Field_Goal_Prob",
      "Opp_Safety_Prob",
      "Opp_Touchdown_Prob",
      "Field_Goal_Prob",
      "Safety_Prob",
      "Touchdown_Prob",
      "ExPoint_Prob",
      "TwoPoint_Prob"
    ),
    c(
      "no_score_prob",
      "opp_fg_prob",
      "opp_safety_prob",
      "opp_td_prob",
      "fg_prob",
      "safety_prob",
      "td_prob",
      "extra_point_prob",
      "two_point_conversion_prob"
    )
  )

  # helper for end of game
  data.table::set(
    pbp_data,
    j = "end_game",
    value = as.numeric(stringi::stri_detect_regex(
      tolower(pbp_data$desc),
      "(end of game)|(end game)"
    ))
  )

  # Change epa for plays occurring at end of half with no scoring plays to be
  # just the difference between 0 and starting ep (the next play of the same
  # game decides that):
  pbp_data[,
    c("tmp_next_qtr", "tmp_next_desc", "tmp_next_end_game") := list(
      data.table::shift(qtr, type = "lead"),
      data.table::shift(desc, type = "lead"),
      data.table::shift(end_game, type = "lead")
    ),
    by = "game_id"
  ]
  # (fifelse() because a missing condition makes epa missing)
  pbp_data[,
    epa := data.table::fifelse(
      ((qtr == 2 & (tmp_next_qtr == 3 | tmp_next_desc == "END QUARTER 2")) |
        (qtr == 4 &
          (tmp_next_qtr == 5 |
            tmp_next_desc == "END QUARTER 4" |
            tmp_next_end_game == 1))) &
        sp == 0 &
        !is.na(play_type),
      0 - ep,
      epa
    )
  ]
  # last play of OT
  pbp_data[,
    epa := data.table::fifelse(
      qtr > 4 & tmp_next_end_game == 1 & sp == 0,
      0 - ep,
      epa
    )
  ]
  pbp_data[
    desc == "END QUARTER 2" | end_game == 1,
    c("epa", "ep") := NA_real_
  ]
  data.table::set(
    pbp_data,
    j = c("tmp_next_qtr", "tmp_next_desc", "tmp_next_end_game"),
    value = NULL
  )

  # Create columns with cumulative epa totals for both teams:
  add_team_totals(pbp_data, pbp_data$epa, "epa")
}

# The value x from the point of view of the home and away team (0 if missing),
# also only for run and pass plays, and their cumulative totals within each
# game. type is "epa" or "wpa".
add_team_totals <- function(pbp, x, type) {
  team_value <- function(team) {
    data.table::nafill(
      data.table::fifelse(pbp$posteam == team, x, -x),
      fill = 0
    )
  }
  play_value <- function(is_type, value) {
    data.table::nafill(data.table::fifelse(is_type, value, 0), fill = 0)
  }
  home <- team_value(pbp$home_team)
  away <- team_value(pbp$away_team)
  is_run <- pbp$play_type == "run"
  is_pass <- pbp$play_type == "pass"
  team_cols <- list(
    home_team = home,
    away_team = away,
    home_team_rush = play_value(is_run, home),
    away_team_rush = play_value(is_run, away),
    home_team_pass = play_value(is_pass, home),
    away_team_pass = play_value(is_pass, away)
  )
  names(team_cols) <- paste0(names(team_cols), "_", type)
  data.table::set(pbp, j = names(team_cols), value = team_cols)
  total_cols <- sub("_team_", "_", paste0("total_", names(team_cols)))
  pbp[,
    (total_cols) := lapply(.SD, cumsum),
    by = "game_id",
    .SDcols = names(team_cols)
  ]
  pbp
}


#################################################################
# Calculate WP and WPA:
add_wp_variables <- function(pbp_data) {
  #testing only
  # pbp_data <- g

  # Initialize the df to store predicted win probability
  OffWinProb <- rep(NA_real_, nrow(pbp_data))
  OffWinProb_spread <- rep(NA_real_, nrow(pbp_data))

  pbp_data <- pbp_data |>
    prepare_wp_data()

  #regulation plays
  regular_i <- which(pbp_data$qtr <= 4)

  # the features of just the regulation plays (all WP features are features of
  # the spread model):
  regular_df <- subset_columns(pbp_data, regular_i, wp_spread_model_features)

  # do predictions for the regular df
  OffWinProb[regular_i] <- get_preds_wp(regular_df)
  OffWinProb_spread[regular_i] <- get_preds_wp_spread(regular_df)

  ## set to NA WP for plays down is missing
  # for kickoffs and PATs, these will get overwritten by the fixes after this

  down_na <- which(is.na(pbp_data$down))
  OffWinProb[down_na] <- NA_real_
  OffWinProb_spread[down_na] <- NA_real_

  ## start PAT fix

  # PAT attempts in a play without down that isn't an onside kick
  pat_formation <- pbp_data$kickoff_attempt == 0 &
    !stringi::stri_detect_fixed(pbp_data$desc, "Onside Kick") &
    is.na(pbp_data$down)
  # plays with 1 point PAT attempts
  pat_i <- which(
    (pat_formation &
      stringi::stri_detect_fixed(pbp_data$desc, "Kick formation")) |
      # or has PAT indicators
      stringi::stri_detect_fixed(pbp_data$desc, "extra point") |
      !is.na(pbp_data$extra_point_result)
  )

  # plays with 2 point PAT attempts
  two_pt_i <- which(
    (pat_formation &
      stringi::stri_detect_fixed(pbp_data$desc, "Pass formation")) |
      # or has PAT indicators
      stringi::stri_detect_fixed(
        pbp_data$desc,
        "TWO-POINT CONVERSION ATTEMPT"
      ) |
      !is.na(pbp_data$two_point_conv_result)
  )

  # some rare 2 point PAT attempts have duplicated matches in 1 point PAT attempts
  # so we remove them in the next line
  pat_i <- pat_i[!pat_i %in% two_pt_i]

  # PAT make probability, only needed for PATs (also in overtime)
  make_pat_prob <- predict_rows(
    function(data) {
      data <- as.data.frame(data)
      data$yardline_100 <- data.table::fifelse(data$season >= 2015, 15, 3)
      as.numeric(
        mgcv::predict.bam(
          fastrmodels::fg_model,
          newdata = data,
          type = "response"
        )
      )
    },
    pbp_data,
    pat_i
  )

  # overtime has its own win probability model (see add_ot_wp_vectors()), the
  # regulation formulas below only apply to regulation
  pat_i_ot <- pat_i[pbp_data$qtr[pat_i] > 4]
  two_pt_i_ot <- two_pt_i[pbp_data$qtr[two_pt_i] > 4]
  pat_i <- pat_i[!pat_i %in% pat_i_ot]
  two_pt_i <- two_pt_i[!two_pt_i %in% two_pt_i_ot]

  # make df of post-PAT plays
  # the scenario data are lists of columns (modifying them doesn't touch
  # pbp_data)
  pat_data <- as.list(pbp_data)
  # swap timeouts
  pat_data$posteam_timeouts_remaining <- pbp_data$defteam_timeouts_remaining
  pat_data$defteam_timeouts_remaining <- pbp_data$posteam_timeouts_remaining
  # swap score
  pat_data$score_differential <- -pbp_data$score_differential
  # 1st and 10
  pat_data$down <- rep(1, nrow(pbp_data))
  pat_data$ydstogo <- rep(10, nrow(pbp_data))
  # flip receive_2h_ko var
  pat_data$receive_2h_ko <- data.table::fcase(
    pbp_data$qtr <= 2 & pbp_data$receive_2h_ko == 0,
    1,
    pbp_data$qtr <= 2 & pbp_data$receive_2h_ko == 1,
    0,
    default = pbp_data$receive_2h_ko
  )
  # switch posteam
  pat_data$posteam <- data.table::fifelse(
    pbp_data$home_team == pbp_data$posteam,
    pbp_data$away_team,
    pbp_data$home_team
  )
  pat_data$yardline_100 <- rep(75, nrow(pbp_data))
  pat_data$home <- data.table::fcase(
    pbp_data$home == 0,
    1,
    pbp_data$home == 1,
    0
  )
  pat_data$posteam_spread <- data.table::fifelse(
    pat_data$home == 1,
    pat_data$spread_line,
    -1 * pat_data$spread_line
  )
  pat_data$elapsed_share <- (3600 - pat_data$game_seconds_remaining) / 3600
  pat_data$spread_time <- pat_data$posteam_spread *
    exp(-4 * pat_data$elapsed_share)

  # the PAT data if the team scores 0, 1 or 2 points
  pat_scored <- function(points) {
    x <- pat_data
    if (points != 0) {
      x$score_differential <- x$score_differential - points
    }
    add_esdtr(x)
  }

  # the predictions below are only needed for PATs in regulation
  pat_rows <- sort(c(pat_i, two_pt_i))

  ## start with spread version
  # get pat if 0, 1, or 2
  pat_0 <- predict_rows(get_preds_wp_spread, pat_scored(0), pat_rows)
  pat_1 <- predict_rows(get_preds_wp_spread, pat_scored(1), pat_rows)
  pat_2 <- predict_rows(get_preds_wp_spread, pat_scored(2), pat_rows)

  # Using nflscrapR version of 2pt make prob on 2nd line here
  pat_go_for_1 <- 1 - (make_pat_prob * pat_1 + (1 - make_pat_prob) * pat_0)
  pat_go_for_2 <- 1 - (0.4735 * pat_2 + (1 - 0.4735) * pat_0)

  OffWinProb_spread[two_pt_i] <- pat_go_for_2[two_pt_i]
  OffWinProb_spread[pat_i] <- pat_go_for_1[pat_i]

  ## repeat for non-spread version
  # get pat if 0, 1, or 2
  pat_0 <- predict_rows(get_preds_wp, pat_scored(0), pat_rows)
  pat_1 <- predict_rows(get_preds_wp, pat_scored(1), pat_rows)
  pat_2 <- predict_rows(get_preds_wp, pat_scored(2), pat_rows)

  # Using nflscrapR version of 2pt make prob on 2nd line here
  pat_go_for_1 <- 1 - (make_pat_prob * pat_1 + (1 - make_pat_prob) * pat_0)
  pat_go_for_2 <- 1 - (0.4735 * pat_2 + (1 - 0.4735) * pat_0)

  OffWinProb[two_pt_i] <- pat_go_for_2[two_pt_i]
  OffWinProb[pat_i] <- pat_go_for_1[pat_i]

  ## end PAT fix

  ## now we need to fix WP on kickoffs, which will be WP associated with touchback
  kickoff_data <- as.list(pbp_data)

  # Change the yard line to be 80 for 2009-2015 and 75 otherwise
  kickoff_data$yardline_100 <- data.table::fifelse(
    kickoff_data$season < 2016,
    80,
    75
  )
  # Now first down:
  kickoff_data$down <- rep(1, nrow(pbp_data))
  kickoff_data$down1 <- rep(1, nrow(pbp_data))
  kickoff_data$down2 <- rep(0, nrow(pbp_data))
  kickoff_data$down3 <- rep(0, nrow(pbp_data))
  kickoff_data$down4 <- rep(0, nrow(pbp_data))
  # 10 ydstogo:
  kickoff_data$ydstogo <- rep(10, nrow(pbp_data))

  # Find the kickoffs in regulation:
  kickoff_i <- which(
    (pbp_data$play_type == "kickoff" | pbp_data$kickoff_attempt == 1) &
      pbp_data$qtr <= 4
  )

  # Get the new predicted probabilites:
  kickoff_preds <- predict_rows(get_preds_wp, kickoff_data, kickoff_i)
  kickoff_preds_spread <- predict_rows(
    get_preds_wp_spread,
    kickoff_data,
    kickoff_i
  )

  # Now update the probabilities:
  OffWinProb[kickoff_i] <- kickoff_preds[kickoff_i]
  OffWinProb_spread[kickoff_i] <- kickoff_preds_spread[kickoff_i]

  ## end fix for kickoffs

  ## overtime: separate rule aware model
  kickoff_i_ot <- which(
    (pbp_data$play_type == "kickoff" | pbp_data$kickoff_attempt == 1) &
      pbp_data$qtr > 4
  )
  ot_i <- which(pbp_data$qtr > 4)
  if (length(ot_i) > 0) {
    # probability that a field goal from the current spot is good
    fg_prob_ot <- rep(NA_real_, nrow(pbp_data))
    ot_data <- as.data.frame(pbp_data[ot_i])
    ot_data$yardline_100[is.na(ot_data$yardline_100)] <- 50
    fg_prob_ot[ot_i] <- as.numeric(
      mgcv::predict.bam(
        fastrmodels::fg_model,
        newdata = ot_data,
        type = "response"
      )
    )
    fg_prob_ot[is.na(pbp_data$yardline_100)] <- NA_real_
    ot_wp <- add_ot_wp_vectors(
      pbp_data,
      kickoff_i = kickoff_i_ot,
      pat_i = pat_i_ot,
      two_pt_i = two_pt_i_ot,
      make_pat_prob = make_pat_prob,
      fg_prob = fg_prob_ot
    )
    OffWinProb[ot_i] <- ot_wp$wp[ot_i]
    OffWinProb_spread[ot_i] <- ot_wp$vegas_wp[ot_i]
  }

  # Now create the win probability columns and return:
  data.table::set(pbp_data, j = "wp", value = OffWinProb)
  data.table::set(pbp_data, j = "vegas_wp", value = OffWinProb_spread)
  # for figuring out posteam on NA wp lines. The team has to come from the
  # same row that wp is filled from below. Non-plays (end of quarter notes,
  # captains announcements, ...) can carry a stale posteam, which flipped
  # home_wp for them when the next play belongs to the other team.
  data.table::set(
    pbp_data,
    j = "tmp_posteam",
    value = data.table::fifelse(
      is.na(OffWinProb) | is.na(pbp_data$posteam),
      NA_character_,
      pbp_data$posteam
    )
  )
  # fill within a game so the last rows of a game can never pick up values
  # from the first play of the next game when several games are processed
  pbp_data[,
    c("wp", "vegas_wp", "tmp_posteam") := list(
      data.table::nafill(wp, type = "nocb"),
      data.table::nafill(vegas_wp, type = "nocb"),
      # nafill() only works for numbers, so we fill the row index instead
      tmp_posteam[data.table::nafill(
        data.table::fifelse(is.na(tmp_posteam), NA_integer_, seq_len(.N)),
        type = "nocb"
      )]
    ),
    by = "game_id"
  ]

  home_has_ball <- pbp_data$tmp_posteam == pbp_data$home_team
  desc_lower <- tolower(pbp_data$desc)

  #add columns for home WP
  data.table::set(
    pbp_data,
    j = "home_wp",
    value = data.table::fifelse(
      home_has_ball,
      pbp_data$wp,
      1 - pbp_data$wp
    )
  )
  data.table::set(
    pbp_data,
    j = "vegas_home_wp",
    value = data.table::fifelse(
      home_has_ball,
      pbp_data$vegas_wp,
      1 - pbp_data$vegas_wp
    )
  )
  # convenience to mark end of game
  data.table::set(
    pbp_data,
    j = "end_game",
    value = as.numeric(
      stringi::stri_detect_regex(desc_lower, "(end of game)|(end game)")
    )
  )
  # convenience for marking home win prob on last line
  data.table::set(
    pbp_data,
    j = "final_value",
    value = data.table::fcase(
      pbp_data$home_score > pbp_data$away_score,
      1,
      pbp_data$away_score > pbp_data$home_score,
      0,
      pbp_data$home_score == pbp_data$away_score,
      0.5
    )
  )
  #make 1 or 0 the final win prob
  pbp_data[
    end_game == 1,
    c("vegas_home_wp", "home_wp") := list(final_value, final_value)
  ]
  data.table::set(pbp_data, j = "away_wp", value = 1 - pbp_data$home_wp)
  # make wp of posteam on last line NA because there's no posteam
  pbp_data[end_game == 1, c("vegas_wp", "wp") := NA_real_]
  data.table::set(pbp_data, j = "def_wp", value = 1 - pbp_data$wp)

  # make wpa
  kneel_or_end <- stringi::stri_detect_regex(
    desc_lower,
    "( kneels )|(end of game)|(end game)"
  )
  pbp_data[,
    vegas_home_wpa := data.table::shift(vegas_home_wp, type = "lead") -
      vegas_home_wp,
    by = "game_id"
  ]
  data.table::set(
    pbp_data,
    j = "vegas_wpa",
    value = data.table::fifelse(
      home_has_ball,
      pbp_data$vegas_home_wpa,
      -pbp_data$vegas_home_wpa
    )
  )
  pbp_data[kneel_or_end, vegas_wpa := NA_real_]
  # home wpa isn't saved but needed for next line
  pbp_data[,
    home_wpa := data.table::shift(home_wp, type = "lead") - home_wp,
    by = "game_id"
  ]
  data.table::set(
    pbp_data,
    j = "wpa",
    value = data.table::fifelse(
      home_has_ball,
      pbp_data$home_wpa,
      -pbp_data$home_wpa
    )
  )
  pbp_data[kneel_or_end, wpa := NA_real_]

  # Home and Away post:

  # same team that wpa is relative to
  post_team <- data.table::fifelse(
    is.na(pbp_data$posteam),
    NA_character_,
    pbp_data$tmp_posteam
  )
  data.table::set(
    pbp_data,
    j = "home_wp_post",
    value = data.table::fifelse(
      post_team == pbp_data$home_team,
      pbp_data$home_wp + pbp_data$wpa,
      pbp_data$home_wp - pbp_data$wpa
    )
  )
  data.table::set(
    pbp_data,
    j = "away_wp_post",
    value = data.table::fifelse(
      post_team == pbp_data$away_team,
      pbp_data$away_wp + pbp_data$wpa,
      pbp_data$away_wp - pbp_data$wpa
    )
  )

  # If next thing is end of game, and post score differential is tied because
  # it's overtime then the game ended in a tie, which counts as 0.5 for both
  # teams:
  ot_tie <- pbp_data$qtr >= 5 &
    stringi::stri_detect_regex(
      data.table::shift(desc_lower, type = "lead"),
      "(end of game)|(end game)"
    ) &
    pbp_data$score_differential_post == 0
  data.table::set(
    pbp_data,
    j = "home_wp_post",
    value = data.table::fifelse(
      ot_tie,
      0.5,
      pbp_data$home_wp_post
    )
  )
  data.table::set(
    pbp_data,
    j = "away_wp_post",
    value = data.table::fifelse(
      ot_tie,
      0.5,
      pbp_data$away_wp_post
    )
  )

  # For plays with playtype of End of Game, use the previous play's WP_post
  # columns as the pre and post, since those are already set to be 1 and 0:
  end_of_game <- pbp_data$end_game == 1
  two_no_plays <- data.table::shift(pbp_data$play_type) == "no_play" &
    pbp_data$play_type == "no_play"
  # (fifelse() because a missing two_no_plays makes the values missing)
  use_prior <- end_of_game | two_no_plays
  pbp_data[,
    c("home_wp_post", "away_wp_post") := list(
      data.table::fifelse(
        use_prior,
        data.table::shift(home_wp_post),
        home_wp_post
      ),
      data.table::fifelse(
        use_prior,
        data.table::shift(away_wp_post),
        away_wp_post
      )
    )
  ]

  # Generate columns to keep track of cumulative rushing and passing WPA
  # values within each game:
  add_team_totals(pbp_data, pbp_data$wpa, "wpa")
}


# helper function to get expected score diff to time ratio
# needed after flipping teams in WP for getting PAT WP
add_esdtr <- function(data) {
  data$Diff_Time_Ratio <- data$score_differential /
    (exp(-4 * data$elapsed_share))
  data
}


#################################################################
# air and YAC EP:
# as with the rest, heavily borrowed from nflscrapR:
# https://github.com/maksimhorowitz/nflscrapR/blob/master/R/add_ep_wp_variables.R
add_air_yac_ep_variables <- function(pbp_data) {
  pass_plays_i <- which(
    !is.na(pbp_data$air_yards) & pbp_data$play_type == "pass"
  )
  # the pass plays as list of columns, the state after the air yards replaces
  # the state before the play
  pass_pbp_data <- subset_columns(
    pbp_data,
    pass_plays_i,
    c(
      ep_model_features,
      "location",
      "down",
      "air_yards",
      "ep",
      "epa",
      "two_point_attempt",
      "penalty",
      "yards_after_catch",
      "complete_pass"
    )
  )
  old_yrdline100 <- pass_pbp_data$yardline_100
  old_ydstogo <- pass_pbp_data$ydstogo
  old_down <- pass_pbp_data$down
  air_yards <- pass_pbp_data$air_yards
  posteam_timeouts_pre <- pass_pbp_data$posteam_timeouts_remaining
  defeam_timeouts_pre <- pass_pbp_data$defteam_timeouts_remaining

  Turnover_Ind <- data.table::fifelse(
    old_down == 4 & air_yards < old_ydstogo,
    1,
    0
  )
  first_down <- air_yards >= old_ydstogo | Turnover_Ind == 1
  pass_pbp_data$yardline_100 <- data.table::fifelse(
    Turnover_Ind == 0,
    old_yrdline100 - air_yards,
    100 - (old_yrdline100 - air_yards)
  )
  pass_pbp_data$ydstogo <- data.table::fifelse(
    first_down,
    10,
    old_ydstogo - air_yards
  )
  down <- data.table::fifelse(first_down, 1, as.numeric(old_down) + 1)
  pass_pbp_data$half_seconds_remaining <- pass_pbp_data$half_seconds_remaining -
    5.704673
  pass_pbp_data$down1 <- data.table::fifelse(down == 1, 1, 0)
  pass_pbp_data$down2 <- data.table::fifelse(down == 2, 1, 0)
  pass_pbp_data$down3 <- data.table::fifelse(down == 3, 1, 0)
  pass_pbp_data$down4 <- data.table::fifelse(down == 4, 1, 0)
  pass_pbp_data$posteam_timeouts_remaining <- data.table::fifelse(
    Turnover_Ind == 1,
    defeam_timeouts_pre,
    posteam_timeouts_pre
  )
  pass_pbp_data$defteam_timeouts_remaining <- data.table::fifelse(
    Turnover_Ind == 1,
    posteam_timeouts_pre,
    defeam_timeouts_pre
  )

  preds <- get_preds(pass_pbp_data)
  airEP <- (preds$Opp_Safety * -2) +
    (preds$Opp_Field_Goal * -3) +
    (preds$Opp_Touchdown * -7) +
    (preds$Safety * 2) +
    (preds$Field_Goal * 3) +
    (preds$Touchdown * 7)
  airEP[which(pass_pbp_data$half_seconds_remaining <= 0)] <- 0

  ep <- pass_pbp_data$ep
  airEPA <- data.table::fifelse(
    old_yrdline100 - air_yards <= 0,
    7 - ep,
    data.table::fifelse(
      old_yrdline100 - air_yards > 99,
      -2 - ep,
      data.table::fifelse(Turnover_Ind == 1, (-1 * airEP) - ep, airEP - ep)
    )
  )
  airEPA <- data.table::fifelse(
    pass_pbp_data$two_point_attempt == 1,
    NA,
    airEPA
  )
  yacEPA <- pass_pbp_data$epa - airEPA
  no_yac <- pass_pbp_data$penalty == 0 &
    pass_pbp_data$yards_after_catch == 0 &
    pass_pbp_data$complete_pass == 1
  yacEPA <- data.table::fifelse(no_yac, 0, yacEPA)
  airEPA <- data.table::fifelse(no_yac, pass_pbp_data$epa, airEPA)

  air_epa <- rep(NA_real_, nrow(pbp_data))
  yac_epa <- rep(NA_real_, nrow(pbp_data))
  air_epa[pass_plays_i] <- airEPA
  yac_epa[pass_plays_i] <- yacEPA
  data.table::set(
    pbp_data,
    j = c("air_epa", "yac_epa"),
    value = list(air_epa, yac_epa)
  )

  add_air_yac_totals(pbp_data, "epa")
}

# Completion and raw air and yac values per team and their cumulative totals
# within each game. type is "epa" or "wpa".
add_air_yac_totals <- function(pbp, type) {
  air <- pbp[[paste0("air_", type)]]
  yac <- pbp[[paste0("yac_", type)]]
  comp_air <- data.table::fifelse(pbp$complete_pass == 1, air, 0)
  comp_yac <- data.table::fifelse(pbp$complete_pass == 1, yac, 0)
  home <- pbp$posteam == pbp$home_team
  away <- pbp$posteam == pbp$away_team
  # the value from the point of view of a team, 0 if missing
  team_value <- function(is_team, x) {
    data.table::nafill(data.table::fifelse(is_team, x, -x), fill = 0)
  }
  team_cols <- list(
    home_team_comp_air = team_value(home, comp_air),
    away_team_comp_air = team_value(away, comp_air),
    home_team_comp_yac = team_value(home, comp_yac),
    away_team_comp_yac = team_value(away, comp_yac),
    home_team_raw_air = team_value(home, air),
    away_team_raw_air = team_value(away, air),
    home_team_raw_yac = team_value(home, yac),
    away_team_raw_yac = team_value(away, yac)
  )
  names(team_cols) <- paste0(names(team_cols), "_", type)
  data.table::set(
    pbp,
    j = paste0(c("comp_air_", "comp_yac_"), type),
    value = list(comp_air, comp_yac)
  )
  data.table::set(pbp, j = names(team_cols), value = team_cols)
  total_cols <- sub("_team_", "_", paste0("total_", names(team_cols)))
  pbp[,
    (total_cols) := lapply(.SD, cumsum),
    by = "game_id",
    .SDcols = names(team_cols)
  ]
  pbp
}


#################################################################
# air and YAC WP:
# as with the rest, heavily borrowed from nflscrapR:
# https://github.com/maksimhorowitz/nflscrapR/blob/master/R/add_ep_wp_variables.R
add_air_yac_wp_variables <- function(pbp_data) {
  pass_plays_i <- which(
    !is.na(pbp_data$air_yards) & pbp_data$play_type == "pass"
  )
  # the pass plays as list of columns, the state after the air yards replaces
  # the state before the play
  pass_pbp_data <- subset_columns(
    pbp_data,
    pass_plays_i,
    c(
      wp_model_features,
      "elapsed_share",
      "air_yards",
      "wp",
      "wpa",
      "two_point_attempt",
      "qtr",
      "penalty",
      "yards_after_catch",
      "complete_pass",
      # for add_ot_air_wp()
      "game_id",
      "season"
    )
  )
  posteam_timeouts_pre <- pass_pbp_data$posteam_timeouts_remaining
  defeam_timeouts_pre <- pass_pbp_data$defteam_timeouts_remaining
  pass_pbp_data$half_seconds_remaining <- pass_pbp_data$half_seconds_remaining -
    5.704673
  pass_pbp_data$game_seconds_remaining <- pass_pbp_data$game_seconds_remaining -
    5.704673
  Diff_Time_Ratio <- pass_pbp_data$score_differential /
    (exp(-4 * pass_pbp_data$elapsed_share))
  Turnover_Ind <- data.table::fifelse(
    pass_pbp_data$down == 4 & pass_pbp_data$air_yards < pass_pbp_data$ydstogo,
    1,
    0
  )
  pass_pbp_data$Diff_Time_Ratio <- data.table::fifelse(
    Turnover_Ind == 1,
    -1 * Diff_Time_Ratio,
    Diff_Time_Ratio
  )
  pass_pbp_data$posteam_timeouts_remaining <- data.table::fifelse(
    Turnover_Ind == 1,
    defeam_timeouts_pre,
    posteam_timeouts_pre
  )
  pass_pbp_data$defteam_timeouts_remaining <- data.table::fifelse(
    Turnover_Ind == 1,
    posteam_timeouts_pre,
    defeam_timeouts_pre
  )

  airWP <- get_preds_wp(pass_pbp_data)
  airWP <- data.table::fifelse(Turnover_Ind == 1, 1 - airWP, airWP)
  airWP[which(pass_pbp_data$half_seconds_remaining <= 0)] <- 0
  airWP[which(pass_pbp_data$game_seconds_remaining <= 0)] <- 0

  airWPA <- airWP - pass_pbp_data$wp
  yacWPA <- pass_pbp_data$wpa - airWPA
  airWPA <- data.table::fifelse(
    pass_pbp_data$two_point_attempt == 1,
    NA,
    airWPA
  )
  yacWPA <- data.table::fifelse(
    pass_pbp_data$two_point_attempt == 1,
    NA,
    yacWPA
  )

  ot_pass_i <- which(pass_pbp_data$qtr > 4)
  if (length(ot_pass_i) > 0) {
    ot_air <- add_ot_air_wp(
      data.table::setDT(pass_pbp_data),
      ot_possession_number(pbp_data)[pass_plays_i]
    )
    airWPA[ot_pass_i] <- ot_air[ot_pass_i] - pass_pbp_data$wp[ot_pass_i]
    yacWPA[ot_pass_i] <- pass_pbp_data$wpa[ot_pass_i] - airWPA[ot_pass_i]
    two_pt <- ot_pass_i[which(pass_pbp_data$two_point_attempt[ot_pass_i] == 1)]
    airWPA[two_pt] <- NA
    yacWPA[two_pt] <- NA
  }

  no_yac <- pass_pbp_data$penalty == 0 &
    pass_pbp_data$yards_after_catch == 0 &
    pass_pbp_data$complete_pass == 1
  yacWPA <- data.table::fifelse(no_yac, 0, yacWPA)
  airWPA <- data.table::fifelse(no_yac, pass_pbp_data$wpa, airWPA)

  air_wpa <- rep(NA_real_, nrow(pbp_data))
  yac_wpa <- rep(NA_real_, nrow(pbp_data))
  air_wpa[pass_plays_i] <- airWPA
  yac_wpa[pass_plays_i] <- yacWPA
  data.table::set(
    pbp_data,
    j = c("air_wpa", "yac_wpa"),
    value = list(air_wpa, yac_wpa)
  )

  add_air_yac_totals(pbp_data, "wpa")
}
