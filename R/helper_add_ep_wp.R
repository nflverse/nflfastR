################################################################################
# Author: Sebastian Carl
# Purpose: Functions to add ep(a) and wp(a) variables
# Code Style Guide: styler::tidyverse_style()
################################################################################

add_ep <- function(pbp) {
  out <- pbp %>% add_ep_variables()
  user_message("added ep variables", "done")
  return(out)
}

add_air_yac_ep <- function(pbp) {
  if (nrow(pbp %>% dplyr::filter(!is.na(.data$air_yards))) == 0) {
    out <- pbp %>%
      dplyr::mutate(
        air_epa = NA_real_,
        yac_epa = NA_real_,
        comp_air_epa = NA_real_,
        comp_yac_epa = NA_real_,
        home_team_comp_air_epa = NA_real_,
        away_team_comp_air_epa = NA_real_,
        home_team_comp_yac_epa = NA_real_,
        away_team_comp_yac_epa = NA_real_,
        total_home_comp_air_epa = NA_real_,
        total_away_comp_air_epa = NA_real_,
        total_home_comp_yac_epa = NA_real_,
        total_away_comp_yac_epa = NA_real_,
        home_team_raw_air_epa = NA_real_,
        away_team_raw_air_epa = NA_real_,
        home_team_raw_yac_epa = NA_real_,
        away_team_raw_yac_epa = NA_real_,
        total_home_raw_air_epa = NA_real_,
        total_away_raw_air_epa = NA_real_,
        total_home_raw_yac_epa = NA_real_,
        total_away_raw_yac_epa = NA_real_
      )
    user_message("No non-NA air_yards detected. air_yac_ep variables set to NA", "info")
  } else {
    out <- pbp %>% add_air_yac_ep_variables()
    user_message("added air_yac_ep variables", "done")
  }
  return(out)
}

add_wp <- function(pbp) {
  out <- pbp %>% add_wp_variables()
  user_message("added wp variables", "done")
  return(out)
}

add_air_yac_wp <- function(pbp) {
  if (nrow(pbp %>% dplyr::filter(!is.na(.data$air_yards))) == 0) {
    out <- pbp %>%
      dplyr::mutate(
        air_wpa = NA_real_,
        yac_wpa = NA_real_,
        comp_air_wpa = NA_real_,
        comp_yac_wpa = NA_real_,
        home_team_comp_air_wpa = NA_real_,
        away_team_comp_air_wpa = NA_real_,
        home_team_comp_yac_wpa = NA_real_,
        away_team_comp_yac_wpa = NA_real_,
        total_home_comp_air_wpa = NA_real_,
        total_away_comp_air_wpa = NA_real_,
        total_home_comp_yac_wpa = NA_real_,
        total_away_comp_yac_wpa = NA_real_,
        home_team_raw_air_wpa = NA_real_,
        away_team_raw_air_wpa = NA_real_,
        home_team_raw_yac_wpa = NA_real_,
        away_team_raw_yac_wpa = NA_real_,
        total_home_raw_air_wpa = NA_real_,
        total_away_raw_air_wpa = NA_real_,
        total_home_raw_yac_wpa = NA_real_,
        total_away_raw_yac_wpa = NA_real_
      )
    user_message("No non-NA air_yards detected. air_yac_wp variables set to NA", "info")
  } else {
    out <- pbp %>% add_air_yac_wp_variables()
    user_message("added air_yac_wp variables", "done")
  }
  return(out)
}

#get predictions for a set of pbp data
#for predict stage of EP
get_preds <- function(pbp) {

  if ("location" %in% names(pbp)) {

    pbp <- pbp %>%
      dplyr::mutate(
        home = dplyr::if_else(.data$location == "Neutral", 0, .data$home)
      )

  }

  preds <- as.data.frame(
    matrix(stats::predict(fastrmodels::ep_model, as.matrix(pbp %>% ep_model_select())), ncol=7, byrow=TRUE)
  )

  colnames(preds) <- c("Touchdown","Opp_Touchdown","Field_Goal","Opp_Field_Goal",
                       "Safety","Opp_Safety","No_Score")

  return(preds)
}

#get predictions for a set of pbp data
#for predict stage
get_preds_wp <- function(pbp) {

  preds <- stats::predict(fastrmodels::wp_model, as.matrix(pbp %>% wp_model_select()))

  return(preds)
}

#get predictions for a set of pbp data
#for predict stage
get_preds_wp_spread <- function(pbp) {

  preds <- stats::predict(fastrmodels::wp_model_spread, as.matrix(pbp %>% wp_spread_model_select()))

  return(preds)
}



#get the columns needed for ep predictions
#making sure they're in the right order
ep_model_select <- function(pbp) {

  pbp <- pbp %>%
    dplyr::select(
      "half_seconds_remaining",
      "yardline_100",
      "home",
      "retractable",
      "dome",
      "outdoors",
      "ydstogo",
      "era0", "era1", "era2", "era3", "era4",
      "down1", "down2", "down3", "down4",
      "posteam_timeouts_remaining",
      "defteam_timeouts_remaining",
    )

  return(pbp)

}

#get the columns needed for wp predictions
#making sure they're in the right order
wp_model_select <- function(pbp) {

  pbp <- pbp %>%
    dplyr::select(
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

  return(pbp)

}

#get the columns needed for wp predictions
#making sure they're in the right order
wp_spread_model_select <- function(pbp) {

  pbp <- pbp %>%
    dplyr::select(
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

  return(pbp)

}
prepare_wp_data <- function(pbp) {

  pbp <- pbp %>%
    dplyr::group_by(.data$game_id) %>%
    dplyr::mutate(
      receive_2h_ko = dplyr::if_else(.data$qtr <= 2 & .data$posteam == dplyr::first(stats::na.omit(.data$defteam)), 1, 0)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      posteam_spread = dplyr::if_else(.data$home == 1, .data$spread_line, -1 * .data$spread_line),
      elapsed_share = (3600 - .data$game_seconds_remaining) / 3600,
      spread_time = .data$posteam_spread * exp(-4 * .data$elapsed_share),
      Diff_Time_Ratio = .data$score_differential / (exp(-4 * .data$elapsed_share))
    )

  return(pbp)

}


#add ep variables
#All of these are heavily borrowed from nflscrapR (Maksim Horowitz, Ronald Yurko, and Samuel Ventura)
add_ep_variables <- function(pbp_data) {

  #testing
  #pbp_data <- g

  #this function is below
  base_ep_preds <- get_preds(pbp_data)

  # ----------------------------------------------------------------------------
  # ---- special case: deal with FG attempts
  # Now make another dataset that to get the EP probabilities from a missed FG:
  missed_fg_data <- pbp_data
  # Subtract 5.065401 from TimeSecs:
  missed_fg_data$half_seconds_remaining <- missed_fg_data$half_seconds_remaining - 5.065401

  # Correct the yrdline100:
  missed_fg_data$yardline_100 <- 100 - (missed_fg_data$yardline_100 + 8)
  # Now first down:
  missed_fg_data$down1 <- rep(1,nrow(pbp_data))
  missed_fg_data$down2 <- rep(0,nrow(pbp_data))
  missed_fg_data$down3 <- rep(0,nrow(pbp_data))
  missed_fg_data$down4 <- rep(0,nrow(pbp_data))
  # 10 ydstogo:
  missed_fg_data$ydstogo <- rep(10,nrow(pbp_data))

  # Get the new predicted probabilites:
  if (nrow(missed_fg_data) > 1) {
    missed_fg_ep_preds <- get_preds(missed_fg_data)
  } else{
    missed_fg_ep_preds <- get_preds(missed_fg_data)
  }

  # Find the rows where TimeSecs_Remaining became 0 or negative and make all the probs equal to 0:
  end_game_i <- which(missed_fg_data$half_seconds_remaining <= 0)
  missed_fg_ep_preds[end_game_i,] <- rep(0,ncol(missed_fg_ep_preds))

  # if the half ends, no one scored
  missed_fg_ep_preds[end_game_i, "No_Score"] <- 1

  # Get the probability of making the field goal:
  make_fg_prob <- as.numeric(mgcv::predict.bam(fastrmodels::fg_model, newdata = pbp_data, type="response"))

  # Multiply each value of the missed_fg_ep_preds by the 1 - make_fg_prob
  missed_fg_ep_preds <- missed_fg_ep_preds * (1 - make_fg_prob)
  # Find the FG attempts:
  fg_attempt_i <- which(pbp_data$play_type == "field_goal")

  # Now update the probabilities for the FG attempts (also includes Opp_Field_Goal probability from missed_fg_ep_preds)
  base_ep_preds[fg_attempt_i, "Field_Goal"] <- make_fg_prob[fg_attempt_i] + missed_fg_ep_preds[fg_attempt_i,"Opp_Field_Goal"]
  # Update the other columns based on the opposite possession:
  base_ep_preds[fg_attempt_i, "Touchdown"] <- missed_fg_ep_preds[fg_attempt_i,"Opp_Touchdown"]
  base_ep_preds[fg_attempt_i, "Opp_Field_Goal"] <- missed_fg_ep_preds[fg_attempt_i,"Field_Goal"]
  base_ep_preds[fg_attempt_i, "Opp_Touchdown"] <- missed_fg_ep_preds[fg_attempt_i,"Touchdown"]
  base_ep_preds[fg_attempt_i, "Safety"] <- missed_fg_ep_preds[fg_attempt_i,"Opp_Safety"]
  base_ep_preds[fg_attempt_i, "Opp_Safety"] <- missed_fg_ep_preds[fg_attempt_i,"Safety"]
  base_ep_preds[fg_attempt_i, "No_Score"] <- missed_fg_ep_preds[fg_attempt_i,"No_Score"]

  # ----------------------------------------------------------------------------------
  # ---- special case: deal with kickoffs
  # Calculate the EP for receiving a touchback (from the point of view for recieving team)
  # and update the columns for Kickoff plays:
  kickoff_data <- pbp_data

  # Change the yard line to be 80 for 2009-2015 and 75 otherwise
  # (accounting for the fact that Jan 2016 is in the 2015 season:
  kickoff_data$yardline_100 <- with(kickoff_data,
                                    ifelse(season < 2016,
                                           80, 75))
  # Now first down:
  kickoff_data$down1 <- rep(1,nrow(pbp_data))
  kickoff_data$down2 <- rep(0,nrow(pbp_data))
  kickoff_data$down3 <- rep(0,nrow(pbp_data))
  kickoff_data$down4 <- rep(0,nrow(pbp_data))
  # 10 ydstogo:
  kickoff_data$ydstogo <- rep(10,nrow(pbp_data))

  # Get the new predicted probabilites:
  kickoff_preds <- get_preds(kickoff_data)

  # Find the kickoffs:
  kickoff_i <- which(pbp_data$play_type == "kickoff" | pbp_data$kickoff_attempt == 1)

  # Now update the probabilities:
  base_ep_preds[kickoff_i, "Field_Goal"] <- kickoff_preds[kickoff_i, "Field_Goal"]
  base_ep_preds[kickoff_i, "Touchdown"] <- kickoff_preds[kickoff_i, "Touchdown"]
  base_ep_preds[kickoff_i, "Opp_Field_Goal"] <- kickoff_preds[kickoff_i, "Opp_Field_Goal"]
  base_ep_preds[kickoff_i, "Opp_Touchdown"] <- kickoff_preds[kickoff_i, "Opp_Touchdown"]
  base_ep_preds[kickoff_i, "Safety"] <- kickoff_preds[kickoff_i, "Safety"]
  base_ep_preds[kickoff_i, "Opp_Safety"] <- kickoff_preds[kickoff_i, "Opp_Safety"]
  base_ep_preds[kickoff_i, "No_Score"] <- kickoff_preds[kickoff_i, "No_Score"]

  # ----------------------------------------------------------------------------------
  # Insert probabilities of 0 for everything but No_Score for QB Kneels that
  # occur on the possession team's side of the field:
  # Find these QB Kneels:
  qb_kneels_i <- which(pbp_data$play_type == "qb_kneel" & pbp_data$yardline_100 > 50)

  # Now update the probabilities:
  base_ep_preds[qb_kneels_i, "Field_Goal"] <- 0
  base_ep_preds[qb_kneels_i, "Touchdown"] <- 0
  base_ep_preds[qb_kneels_i, "Opp_Field_Goal"] <- 0
  base_ep_preds[qb_kneels_i, "Opp_Touchdown"] <- 0
  base_ep_preds[qb_kneels_i, "Safety"] <- 0
  base_ep_preds[qb_kneels_i, "Opp_Safety"] <- 0
  base_ep_preds[qb_kneels_i, "No_Score"] <- 1


  # ----------------------------------------------------------------------------------
  # Create two new columns, ExPoint_Prob and TwoPoint_Prob, for the PAT events:
  base_ep_preds$ExPoint_Prob <- 0
  base_ep_preds$TwoPoint_Prob <- 0

  # Find the indices for these types of plays:
  extrapoint_i <- which((pbp_data$play_type == "extra_point" | pbp_data$play_type_nfl == "XP_KICK") &
                          (is.na(pbp_data$play_type_nfl) | pbp_data$play_type_nfl != "PAT2"))
  twopoint_i <- which(pbp_data$two_point_attempt == 1)

  #new: special case for PAT or kickoff with penalty
  #for inserting NAs
  st_penalty_i_1 <- which(
    # pat: prior play was TD or PAT and next play is PAT and this play isn't a td
    (pbp_data$touchdown == 0 & (dplyr::lag(pbp_data$touchdown == 1) | dplyr::lag(pbp_data$play_type_nfl == "XP_KICK")) &
          (dplyr::lead(pbp_data$two_point_attempt)==1 | dplyr::lead(pbp_data$extra_point_attempt)==1 | dplyr::lead(pbp_data$play_type_nfl) == "XP_KICK")) |
      #kickoff: prior play was PAT and next play is kickoff
      ((dplyr::lag(pbp_data$two_point_attempt)==1 | dplyr::lag(pbp_data$extra_point_attempt)==1) & dplyr::lead(pbp_data$kickoff_attempt == 1))
    )

  st_penalty_i_2 <- which(
      is.na(dplyr::lead(pbp_data$down)) &
         # has a key term in desc
         (((stringr::str_detect(pbp_data$desc, 'Kick formation') & is.na(pbp_data$down) & pbp_data$play_type == 'no_play') |
             (stringr::str_detect(pbp_data$desc, 'Pass formation') & is.na(pbp_data$down) & pbp_data$play_type == 'no_play') |
             (stringr::str_detect(pbp_data$desc, 'kicks onside') & is.na(pbp_data$down) & pbp_data$play_type == 'no_play') |
             (stringr::str_detect(pbp_data$desc, 'Offside on Free Kick') & is.na(pbp_data$down) & pbp_data$play_type == 'no_play') |
             (stringr::str_detect(pbp_data$desc, 'TWO-POINT CONVERSION')) &
             # down is NA and play type no play and next play isn't a kickoff
             is.na(pbp_data$down) & pbp_data$play_type == 'no_play' & dplyr::lead(pbp_data$kickoff_attempt) == 0))
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
                        !stringr::str_detect(pbp_data$desc, ' pass ') &
                        !stringr::str_detect(pbp_data$desc, ' sacked ') &
                        !stringr::str_detect(pbp_data$desc, ' scramble ') &
                        !stringr::str_detect(pbp_data$desc, ' punts ') &
                        !stringr::str_detect(pbp_data$desc, ' up the middle ') &
                        !stringr::str_detect(pbp_data$desc, ' left end ') &
                        !stringr::str_detect(pbp_data$desc, ' left guard ') &
                        !stringr::str_detect(pbp_data$desc, ' left tackle ') &
                        !stringr::str_detect(pbp_data$desc, ' right end ') &
                        !stringr::str_detect(pbp_data$desc, ' right guard ') &
                        !stringr::str_detect(pbp_data$desc, ' right tackle ')
                      ) |
      is.na(pbp_data$play_type))

  # Now update the probabilities for missing and PATs:
  base_ep_preds$Field_Goal[c(missing_i, extrapoint_i, twopoint_i, st_penalty_i_1, st_penalty_i_2)] <- 0
  base_ep_preds$Touchdown[c(missing_i, extrapoint_i, twopoint_i, st_penalty_i_1, st_penalty_i_2)] <- 0
  base_ep_preds$Opp_Field_Goal[c(missing_i, extrapoint_i, twopoint_i, st_penalty_i_1, st_penalty_i_2)] <- 0
  base_ep_preds$Opp_Touchdown[c(missing_i, extrapoint_i, twopoint_i, st_penalty_i_1, st_penalty_i_2)] <- 0
  base_ep_preds$Safety[c(missing_i, extrapoint_i, twopoint_i, st_penalty_i_1, st_penalty_i_2)] <- 0
  base_ep_preds$Opp_Safety[c(missing_i, extrapoint_i, twopoint_i, st_penalty_i_1, st_penalty_i_2)] <- 0
  base_ep_preds$No_Score[c(missing_i, extrapoint_i, twopoint_i, st_penalty_i_1, st_penalty_i_2)] <- 0

  # Rename the events to all have _Prob at the end of them:
  base_ep_preds <- dplyr::rename(base_ep_preds,
                                 Field_Goal_Prob = "Field_Goal",
                                 Touchdown_Prob = "Touchdown",
                                 Opp_Field_Goal_Prob = "Opp_Field_Goal",
                                 Opp_Touchdown_Prob = "Opp_Touchdown",
                                 Safety_Prob = "Safety",
                                 Opp_Safety_Prob = "Opp_Safety",
                                 No_Score_Prob = "No_Score")


  # Join them together:
  pbp_data <- cbind(pbp_data, base_ep_preds)

  # Calculate the ExpPts:
  pbp_data_ep <- dplyr::mutate(pbp_data,
                               ExpPts = (0*.data$No_Score_Prob) + (-3 * .data$Opp_Field_Goal_Prob) +
                                 (-2 * .data$Opp_Safety_Prob) +
                                 (-7 * .data$Opp_Touchdown_Prob) + (3 * .data$Field_Goal_Prob) +
                                 (2 * .data$Safety_Prob) + (7 * .data$Touchdown_Prob) +
                                 (1 * .data$ExPoint_Prob) + (2 * .data$TwoPoint_Prob))

  #just going to set these to NA bc we have no way of calculating EPA for them
  if (length(st_penalty_i_1) > 0) {
    pbp_data_ep$ExpPts[st_penalty_i_1] <- NA_real_
  }

  if (length(st_penalty_i_2) > 0) {
    pbp_data_ep$ExpPts[st_penalty_i_2] <- NA_real_
  }

  pbp_data_ep$ExpPts[missing_i] <- NA_real_

  #################################################################
  # Calculate EPA:

  ### Adding Expected Points Added (EPA) column

  # Create multiple types of EPA columns
  # for each of the possible cases,
  # grouping by GameID (will then just use
  # an ifelse statement to decide which one
  # to use as the final EPA):
  pbp_data_ep %>%
    dplyr::group_by(.data$game_id) %>%
    dplyr::mutate(# Now conditionally assign the EPA, first for possession team
      # touchdowns:
      ep = .data$ExpPts,
      tmp_posteam = .data$posteam
    ) %>%
    tidyr::fill(
      .data$ep, .direction = "up"
    ) %>%
    tidyr::fill(
      .data$tmp_posteam, .direction = "up"
    ) %>%
    dplyr::mutate(
      # get epa for non-scoring plays
      home_ep = dplyr::if_else(.data$tmp_posteam == .data$home_team, .data$ep, - .data$ep),
      home_epa = dplyr::lead(.data$home_ep) - .data$home_ep,
      epa = dplyr::if_else(.data$tmp_posteam == .data$home_team, .data$home_epa, -.data$home_epa),

      # td
      epa = dplyr::if_else(!is.na(.data$td_team),
                           dplyr::if_else(.data$td_team == .data$posteam,
                                          7 - .data$ep, -7 - .data$ep),
                           .data$epa),
      # Offense field goal:
      epa = dplyr::if_else(is.na(.data$td_team) & .data$field_goal_made == 1,
                           3 - .data$ep, .data$epa, missing = .data$epa),
      # Offense extra-point:
      epa = dplyr::if_else(is.na(.data$td_team) & .data$field_goal_made == 0 &
                             .data$extra_point_good == 1,
                           1 - .data$ep, .data$epa, missing = .data$epa),
      # Offense two-point conversion:
      epa = dplyr::if_else(is.na(.data$td_team) & .data$field_goal_made == 0 &
                             .data$extra_point_good == 0 &
                             (.data$two_point_rush_good == 1 |
                                .data$two_point_pass_good == 1 |
                                .data$two_point_pass_reception_good == 1),
                           2 - .data$ep, .data$epa, missing = .data$epa),
      # Failed PAT (both 1 and 2):
      epa = dplyr::if_else(is.na(.data$td_team) & .data$field_goal_made == 0 &
                             .data$extra_point_good == 0 &
                             ((.data$extra_point_failed == 1 |
                                 .data$extra_point_blocked == 1 |
                                 .data$extra_point_aborted == 1) |
                                (.data$two_point_rush_failed == 1 |
                                   .data$two_point_pass_failed == 1 |
                                   .data$two_point_pass_reception_failed == 1)),
                           0 - .data$ep, .data$epa, missing = .data$epa),
      # Opponent scores defensive 2 point:
      epa = dplyr::if_else(
        .data$defensive_two_point_conv == 1, -2 - .data$ep, .data$epa, missing = .data$epa
      ),
      # Safety:
      epa = dplyr::case_when(
        !is.na(.data$safety_team) & .data$safety_team == .data$posteam ~  2 - .data$ep,
        !is.na(.data$safety_team) & .data$safety_team == .data$defteam ~ -2 - .data$ep,
        TRUE ~ .data$epa
      )
      ) %>%
    # Now rename each of the expected points columns to match the style of
    # the updated code:
    dplyr::rename(
                  no_score_prob = "No_Score_Prob",
                  opp_fg_prob = "Opp_Field_Goal_Prob",
                  opp_safety_prob = "Opp_Safety_Prob",
                  opp_td_prob = "Opp_Touchdown_Prob",
                  fg_prob = "Field_Goal_Prob",
                  safety_prob = "Safety_Prob",
                  td_prob = "Touchdown_Prob",
                  extra_point_prob = "ExPoint_Prob",
                  two_point_conversion_prob = "TwoPoint_Prob"
                  ) %>%
    # Create columns with cumulative epa totals for both teams:
    dplyr::mutate(
                  # helper for end of game
                  end_game = ifelse(
                    stringr::str_detect(tolower(.data$desc), "(end of game)|(end game)"),
                    1, 0
                  ),

                  # Change epa for plays occurring at end of half with no scoring
                  # plays to be just the difference between 0 and starting ep:
                  epa = dplyr::if_else(((.data$qtr == 2 &
                                           (dplyr::lead(.data$qtr) == 3 |
                                              dplyr::lead(.data$desc) == "END QUARTER 2")) |
                                          (.data$qtr == 4 &
                                             (dplyr::lead(.data$qtr) == 5 |
                                                dplyr::lead(.data$desc) == "END QUARTER 4" |
                                                dplyr::lead(.data$end_game) == 1))) &
                                         .data$sp == 0 &
                                         !is.na(.data$play_type),
                                       0 - .data$ep, .data$epa),
                  # last play of OT
                  epa = dplyr::if_else(.data$qtr > 4 & dplyr::lead(.data$end_game) == 1 & .data$sp == 0,
                                       0 - .data$ep,
                                       .data$epa),
                  epa = dplyr::if_else(.data$desc == "END QUARTER 2", NA_real_, .data$epa),
                  epa = dplyr::if_else(.data$end_game == 1, NA_real_, .data$epa),
                  ep = dplyr::if_else(.data$desc == "END QUARTER 2", NA_real_, .data$ep),
                  ep = dplyr::if_else(.data$end_game == 1, NA_real_, .data$ep),
                  home_team_epa = dplyr::if_else(.data$posteam == .data$home_team,
                                                 .data$epa, -.data$epa),
                  away_team_epa = dplyr::if_else(.data$posteam == .data$away_team,
                                                 .data$epa, -.data$epa),
                  home_team_epa = dplyr::if_else(is.na(.data$home_team_epa),
                                                 0, .data$home_team_epa),
                  away_team_epa = dplyr::if_else(is.na(.data$away_team_epa),
                                                 0, .data$away_team_epa),
                  total_home_epa = cumsum(.data$home_team_epa),
                  total_away_epa = cumsum(.data$away_team_epa),
                  # Same thing but separating passing and rushing:
                  home_team_rush_epa = dplyr::if_else(.data$play_type == "run",
                                                      .data$home_team_epa, 0),
                  away_team_rush_epa = dplyr::if_else(.data$play_type == "run",
                                                      .data$away_team_epa, 0),
                  home_team_rush_epa = dplyr::if_else(is.na(.data$home_team_rush_epa),
                                                      0, .data$home_team_rush_epa),
                  away_team_rush_epa = dplyr::if_else(is.na(.data$away_team_rush_epa),
                                                      0, .data$away_team_rush_epa),
                  total_home_rush_epa = cumsum(.data$home_team_rush_epa),
                  total_away_rush_epa = cumsum(.data$away_team_rush_epa),
                  home_team_pass_epa = dplyr::if_else(.data$play_type == "pass",
                                                      .data$home_team_epa, 0),
                  away_team_pass_epa = dplyr::if_else(.data$play_type == "pass",
                                                      .data$away_team_epa, 0),
                  home_team_pass_epa = dplyr::if_else(is.na(.data$home_team_pass_epa),
                                                      0, .data$home_team_pass_epa),
                  away_team_pass_epa = dplyr::if_else(is.na(.data$away_team_pass_epa),
                                                      0, .data$away_team_pass_epa),
                  total_home_pass_epa = cumsum(.data$home_team_pass_epa),
                  total_away_pass_epa = cumsum(.data$away_team_pass_epa)) %>%
    dplyr::ungroup() %>%
    return()
}


#################################################################
# Calculate WP and WPA:
add_wp_variables <- function(pbp_data) {

  #testing only
  # pbp_data <- g

  # Initialize the df to store predicted win probability
  OffWinProb <- rep(NA_real_, nrow(pbp_data))
  OffWinProb_spread <- rep(NA_real_, nrow(pbp_data))

  pbp_data <- pbp_data %>%
    prepare_wp_data()

  # First check if there's any overtime plays:
  if (any(pbp_data$qtr > 4)){
    # Find the rows that are overtime:
    overtime_i <- which(pbp_data$qtr > 4)

    # Separate the dataset into regular_df and overtime_df:
    overtime_df <- pbp_data[overtime_i,]

    # Separate routine for overtime:

    # Create a column that is just the first drive of overtime repeated:
    overtime_df$First_Drive <- rep(min(overtime_df$drive,
                                       na.rm = TRUE),
                                   nrow(overtime_df))

    # Calculate the difference in drive number
    overtime_df <- dplyr::mutate(overtime_df,
                                 Drive_Diff = .data$drive - .data$First_Drive)

    # Create an indicator column that means the posteam is losing by 3 and
    # its the second drive of overtime:
    overtime_df$One_FG_Game <- ifelse(overtime_df$score_differential == -3 &
                                        overtime_df$Drive_Diff == 1, 1, 0)

    # Now create a copy of the dataset to then make the EP predictions for when
    # a field goal is scored and its not sudden death:
    overtime_df_ko <- overtime_df

    overtime_df_ko$yrdline100 <- with(overtime_df_ko,
                                      ifelse(game_year < 2016 |
                                               (game_year == 2016 & game_month < 4),
                                             80, 75))

    # Now first down:
    overtime_df_ko$down1 <- rep(1,nrow(overtime_df_ko))
    overtime_df_ko$down2 <- rep(0,nrow(overtime_df_ko))
    overtime_df_ko$down3 <- rep(0,nrow(overtime_df_ko))
    overtime_df_ko$down4 <- rep(0,nrow(overtime_df_ko))
    # 10 ydstogo:
    overtime_df_ko$ydstogo <- rep(10,nrow(overtime_df_ko))

    # Get the predictions from the EP model and calculate the necessary probability:
    overtime_df_ko_preds <- get_preds(overtime_df_ko)

    overtime_df_ko_preds <- dplyr::mutate(overtime_df_ko_preds,
                                          Win_Back = .data$No_Score + .data$Opp_Field_Goal + .data$Opp_Safety + .data$Opp_Touchdown)

    # Calculate the two possible win probability types, Sudden Death and one Field Goal:
    overtime_df$Sudden_Death_WP <- overtime_df$fg_prob + overtime_df$td_prob + overtime_df$safety_prob
    overtime_df$One_FG_WP <- overtime_df$td_prob + (overtime_df$fg_prob * overtime_df_ko_preds$Win_Back)


    # Decide which win probability to use:
    OffWinProb[overtime_i] <- ifelse(overtime_df$game_year >= 2012  & (overtime_df$Drive_Diff == 0 | (overtime_df$Drive_Diff == 1 & overtime_df$One_FG_Game == 1)),
                                     overtime_df$One_FG_WP, overtime_df$Sudden_Death_WP)
    OffWinProb_spread[overtime_i] <-  OffWinProb[overtime_i]

  }

  #regulation plays
  regular_i <- which(pbp_data$qtr <= 4)

  # df of just the regulation plays:
  regular_df <- pbp_data[regular_i,]

  # do predictions for the regular df
  OffWinProb[regular_i] <- get_preds_wp(regular_df)
  OffWinProb_spread[regular_i] <- get_preds_wp_spread(regular_df)

  ## set to NA WP for plays down is missing
  # for kickoffs and PATs, these will get overwritten by the fixes after this

  down_na <- which(is.na(pbp_data$down))
  OffWinProb[down_na] <- NA_real_
  OffWinProb_spread[down_na] <- NA_real_

  ## start PAT fix

  make_pat_prob <- as.numeric(
    mgcv::predict.bam(
      fastrmodels::fg_model,
      newdata = pbp_data %>%
        mutate(
          yardline_100 = ifelse(.data$season >= 2015, 15, 3)
          ), type="response")
    )

  # plays with 1 point PAT attempts
  pat_i <- which(
    (pbp_data$kickoff_attempt == 0 &
       !(stringr::str_detect(pbp_data$desc, 'Onside Kick')) &
       (stringr::str_detect(pbp_data$desc, 'Kick formation')) &
       is.na(pbp_data$down)) |
      # or has PAT indicators
      stringr::str_detect(pbp_data$desc, 'extra point') |
      !is.na(pbp_data$extra_point_result)
    )

  # plays with 2 point PAT attempts
  two_pt_i <- which(
    (pbp_data$kickoff_attempt == 0 &
       !(stringr::str_detect(pbp_data$desc, 'Onside Kick')) &
       (stringr::str_detect(pbp_data$desc, 'Pass formation')) &
       is.na(pbp_data$down)) |
      # or has PAT indicators
      stringr::str_detect(pbp_data$desc, 'TWO-POINT CONVERSION ATTEMPT')  |
      !is.na(pbp_data$two_point_conv_result)
  )

  # some rare 2 point PAT attempts have duplicated matches in 1 point PAT attempts
  # so we remove them in the next line
  pat_i <- pat_i[!pat_i %in% two_pt_i]

  # make df of post-PAT plays
  pat_data <- pbp_data %>%
    dplyr::mutate(
      # swap timeouts
      to_pos = .data$posteam_timeouts_remaining,
      to_def = .data$defteam_timeouts_remaining,
      posteam_timeouts_remaining = .data$to_def,
      defteam_timeouts_remaining = .data$to_pos,
      # swap score
      score_differential = -.data$score_differential,
      # 1st and 10
      down = 1,
      ydstogo = 10,
      # flip receive_2h_ko var
      receive_2h_ko = case_when(
        .data$qtr <= 2 & .data$receive_2h_ko == 0 ~ 1,
        .data$qtr <= 2 & .data$receive_2h_ko == 1 ~ 0,
        TRUE ~ .data$receive_2h_ko
      ),
      # switch posteam
      posteam = if_else(.data$home_team == .data$posteam, .data$away_team, .data$home_team),
      yardline_100 = 75
    ) %>%
    dplyr::mutate(
      home = case_when(
        .data$home == 0 ~ 1,
        .data$home == 1 ~ 0
      ),
      posteam_spread = dplyr::if_else(.data$home == 1, .data$spread_line, -1 * .data$spread_line),
      elapsed_share = (3600 - .data$game_seconds_remaining) / 3600,
      spread_time = .data$posteam_spread * exp(-4 * .data$elapsed_share)
    )

  ## start with spread version
  # get pat if 0, 1, or 2
  pat_0 <- get_preds_wp_spread(pat_data %>% add_esdtr())
  pat_1 <- get_preds_wp_spread(pat_data %>% dplyr::mutate(score_differential = .data$score_differential - 1) %>% add_esdtr())
  pat_2 <- get_preds_wp_spread(pat_data %>% dplyr::mutate(score_differential = .data$score_differential - 2) %>% add_esdtr())

  # Using nflscrapR version of 2pt make prob on 2nd line here
  pat_go_for_1 <- 1 - (make_pat_prob * pat_1 + (1 - make_pat_prob) * pat_0)
  pat_go_for_2 <- 1 - (0.4735 * pat_2 + (1 - 0.4735) * pat_0)

  OffWinProb_spread[two_pt_i] <- pat_go_for_2[two_pt_i]
  OffWinProb_spread[pat_i] <- pat_go_for_1[pat_i]

  ## repeat for non-spread version
  # get pat if 0, 1, or 2
  pat_0 <- get_preds_wp(pat_data %>% add_esdtr())
  pat_1 <- get_preds_wp(pat_data %>% dplyr::mutate(score_differential = .data$score_differential - 1) %>% add_esdtr())
  pat_2 <- get_preds_wp(pat_data %>% dplyr::mutate(score_differential = .data$score_differential - 2) %>% add_esdtr())

  # Using nflscrapR version of 2pt make prob on 2nd line here
  pat_go_for_1 <- 1 - (make_pat_prob * pat_1 + (1 - make_pat_prob) * pat_0)
  pat_go_for_2 <- 1 - (0.4735 * pat_2 + (1 - 0.4735) * pat_0)

  OffWinProb[two_pt_i] <- pat_go_for_2[two_pt_i]
  OffWinProb[pat_i] <- pat_go_for_1[pat_i]

  ## end PAT fix

  ## now we need to fix WP on kickoffs, which will be WP associated with touchback
  kickoff_data <- pbp_data

  # Change the yard line to be 80 for 2009-2015 and 75 otherwise
  kickoff_data$yardline_100 <- with(kickoff_data,
                                    ifelse(season < 2016,
                                           80, 75))
  # Now first down:
  kickoff_data$down <- rep(1,nrow(pbp_data))
  kickoff_data$down1 <- rep(1,nrow(pbp_data))
  kickoff_data$down2 <- rep(0,nrow(pbp_data))
  kickoff_data$down3 <- rep(0,nrow(pbp_data))
  kickoff_data$down4 <- rep(0,nrow(pbp_data))
  # 10 ydstogo:
  kickoff_data$ydstogo <- rep(10,nrow(pbp_data))

  # Get the new predicted probabilites:
  kickoff_preds <- get_preds_wp(kickoff_data)
  kickoff_preds_spread <- get_preds_wp_spread(kickoff_data)

  # Find the kickoffs in regulation:
  kickoff_i <- which((pbp_data$play_type == "kickoff" | pbp_data$kickoff_attempt == 1) & pbp_data$qtr <= 4)

  # Now update the probabilities:
  OffWinProb[kickoff_i] <- kickoff_preds[kickoff_i]
  OffWinProb_spread[kickoff_i] <- kickoff_preds_spread[kickoff_i]

  ## end fix for kickoffs


  # Now create the win probability columns and return:
  pbp_data <- pbp_data %>%
    dplyr::mutate(
      wp = OffWinProb,
      vegas_wp = OffWinProb_spread,
      # for figuring out posteam on NA posteam lines
      tmp_posteam = .data$posteam
      ) %>%
    tidyr::fill(
      .data$wp, .direction = "up"
    ) %>%
    tidyr::fill(
      .data$vegas_wp, .direction = "up"
    ) %>%
    tidyr::fill(
      .data$tmp_posteam, .direction = "up"
    ) %>%
    dplyr::group_by(.data$game_id) %>%
    dplyr::mutate(
      #add columns for home WP
      home_wp = dplyr::if_else(.data$tmp_posteam == .data$home_team, .data$wp, 1 - .data$wp),
      vegas_home_wp = dplyr::if_else(.data$tmp_posteam == .data$home_team, .data$vegas_wp, 1 - .data$vegas_wp),

      # convenience to mark end of game
      end_game = ifelse(
        stringr::str_detect(tolower(.data$desc), "(end of game)|(end game)"),
        1, 0
        ),

      # convenience for marking home win prob on last line
      final_value = dplyr::case_when(
        .data$home_score > .data$away_score ~ 1,
        .data$away_score > .data$home_score ~ 0,
        .data$home_score == .data$away_score ~ .5
      ),

      #make 1 or 0 the final win prob
      vegas_home_wp = dplyr::if_else(
        .data$end_game == 1,
        .data$final_value,
        .data$vegas_home_wp
      ),

      # can we make this and the above into a function? feels like a lot of repitition
      home_wp = dplyr::if_else(
        .data$end_game == 1,
        .data$final_value,
        .data$home_wp
      ),

      away_wp = 1 - .data$home_wp,

      # make wp of posteam on last line NA because there's no posteam
      vegas_wp = dplyr::if_else(
        .data$end_game == 1,
        NA_real_,
        .data$vegas_wp
      ),

      wp = dplyr::if_else(
        .data$end_game == 1,
        NA_real_,
        .data$wp
      ),

      def_wp = 1 - .data$wp,

      # make wpa
      vegas_home_wpa = dplyr::lead(.data$vegas_home_wp) - .data$vegas_home_wp,
      vegas_wpa = dplyr::if_else(.data$tmp_posteam == .data$home_team, .data$vegas_home_wpa, -.data$vegas_home_wpa),
      vegas_wpa = dplyr::if_else(
        stringr::str_detect(tolower(.data$desc), "( kneels )|(end of game)|(end game)"), NA_real_, .data$vegas_wpa
        ),

      # home wpa isn't saved but needed for next line
      home_wpa = dplyr::lead(.data$home_wp) - .data$home_wp,
      wpa = dplyr::if_else(.data$tmp_posteam == .data$home_team, .data$home_wpa, -.data$home_wpa),
      wpa = dplyr::if_else(
        stringr::str_detect(tolower(.data$desc), "( kneels )|(end of game)|(end game)"), NA_real_, .data$wpa
      )
      ) %>%
    dplyr::ungroup()

  # Home and Away post:

  pbp_data$home_wp_post <- ifelse(pbp_data$posteam == pbp_data$home_team,
                                  pbp_data$home_wp + pbp_data$wpa,
                                  pbp_data$home_wp - pbp_data$wpa)
  pbp_data$away_wp_post <- ifelse(pbp_data$posteam == pbp_data$away_team,
                                  pbp_data$away_wp + pbp_data$wpa,
                                  pbp_data$away_wp - pbp_data$wpa)

  # If next thing is end of game, and post score differential is tied because it's
  # overtime then make both the home_wp_post and away_wp_post equal to 0:
  pbp_data <- pbp_data %>%
    dplyr::mutate(home_wp_post = dplyr::if_else(.data$qtr == 5 &
                                                  stringr::str_detect(tolower(dplyr::lead(.data$desc)),
                                                                      "(end of game)|(end game)") &
                                                  .data$score_differential_post == 0,
                                                0, .data$home_wp_post),
                  away_wp_post = dplyr::if_else(.data$qtr == 5 &
                                                  stringr::str_detect(tolower(dplyr::lead(.data$desc)),
                                                                      "(end of game)|(end game)") &
                                                  .data$score_differential_post == 0,
                                                0, .data$away_wp_post))


  # For plays with playtype of End of Game, use the previous play's WP_post columns
  # as the pre and post, since those are already set to be 1 and 0:

  pbp_data$home_wp_post <- with(pbp_data,
                                ifelse(stringr::str_detect(tolower(desc),
                                                           "(end of game)|(end game)"), dplyr::lag(home_wp_post),
                                       ifelse(dplyr::lag(play_type) == "no_play" & play_type == "no_play", dplyr::lag(home_wp_post),home_wp_post)))

  pbp_data$away_wp_post <- with(pbp_data,
                                ifelse(stringr::str_detect(tolower(desc),
                                                           "(end of game)|(end game)"), dplyr::lag(away_wp_post),
                                       ifelse(dplyr::lag(play_type) == "no_play" & play_type == "no_play", dplyr::lag(away_wp_post),away_wp_post)))



  # Now drop the unnecessary columns, rename variables back, and return:
  pbp_data %>%
    dplyr::group_by(.data$game_id) %>%
    dplyr::mutate(
                  # Generate columns to keep track of cumulative rushing and
                  # passing WPA values:
                  home_team_wpa = dplyr::if_else(.data$posteam == .data$home_team,
                                                 .data$wpa, -.data$wpa),
                  away_team_wpa = dplyr::if_else(.data$posteam == .data$away_team,
                                                 .data$wpa, -.data$wpa),
                  home_team_wpa = dplyr::if_else(is.na(.data$home_team_wpa),
                                                 0, .data$home_team_wpa),
                  away_team_wpa = dplyr::if_else(is.na(.data$away_team_wpa),
                                                 0, .data$away_team_wpa),
                  # Same thing but separating passing and rushing:
                  home_team_rush_wpa = dplyr::if_else(.data$play_type == "run",
                                                      .data$home_team_wpa, 0),
                  away_team_rush_wpa = dplyr::if_else(.data$play_type == "run",
                                                      .data$away_team_wpa, 0),
                  home_team_rush_wpa = dplyr::if_else(is.na(.data$home_team_rush_wpa),
                                                      0, .data$home_team_rush_wpa),
                  away_team_rush_wpa = dplyr::if_else(is.na(.data$away_team_rush_wpa),
                                                      0, .data$away_team_rush_wpa),
                  total_home_rush_wpa = cumsum(.data$home_team_rush_wpa),
                  total_away_rush_wpa = cumsum(.data$away_team_rush_wpa),
                  home_team_pass_wpa = dplyr::if_else(.data$play_type == "pass",
                                                      .data$home_team_wpa, 0),
                  away_team_pass_wpa = dplyr::if_else(.data$play_type == "pass",
                                                      .data$away_team_wpa, 0),
                  home_team_pass_wpa = dplyr::if_else(is.na(.data$home_team_pass_wpa),
                                                      0, .data$home_team_pass_wpa),
                  away_team_pass_wpa = dplyr::if_else(is.na(.data$away_team_pass_wpa),
                                                      0, .data$away_team_pass_wpa),
                  total_home_pass_wpa = cumsum(.data$home_team_pass_wpa),
                  total_away_pass_wpa = cumsum(.data$away_team_pass_wpa)) %>%
    dplyr::ungroup() %>%
    return()

}


# helper function to get expected score diff to time ratio
# needed after flipping teams in WP for getting PAT WP
add_esdtr <- function(data) {

  data %>%
    dplyr::mutate(
      Diff_Time_Ratio = .data$score_differential / (exp(-4 * .data$elapsed_share))
    ) %>%
    return()

}


#################################################################
# air and YAC EP:
# as with the rest, heavily borrowed from nflscrapR:
# https://github.com/maksimhorowitz/nflscrapR/blob/master/R/add_ep_wp_variables.R
add_air_yac_ep_variables <- function(pbp_data) {

  #testing
  #pbp_data <- g

  # Final all pass attempts that are not sacks:
  pass_plays_i <- which(!is.na(pbp_data$air_yards) & pbp_data$play_type == 'pass')
  pass_pbp_data <- pbp_data[pass_plays_i,]

  # Using the air_yards need to update the following:
  # - yrdline100
  # - TimeSecs_Remaining
  # - ydstogo
  # - down
  # - timeouts

  # Get everything set up for calculation
  pass_pbp_data <- pass_pbp_data %>%
    dplyr::mutate(
      posteam_timeouts_pre = .data$posteam_timeouts_remaining,
      defeam_timeouts_pre = .data$defteam_timeouts_remaining
    ) %>%
    # Rename the old columns to update for calculating the EP from the air:
    dplyr::rename(old_yrdline100 = .data$yardline_100,
                  old_ydstogo = .data$ydstogo,
                  old_TimeSecs_Remaining = .data$half_seconds_remaining,
                  old_down = .data$down) %>%
    dplyr::mutate(Turnover_Ind = dplyr::if_else(.data$old_down == 4 & .data$air_yards < .data$old_ydstogo,
                                                1, 0),
                  yardline_100 = dplyr::if_else(.data$Turnover_Ind == 0,
                                                .data$old_yrdline100 - .data$air_yards,
                                              100 - (.data$old_yrdline100 - .data$air_yards)),
                  ydstogo = dplyr::if_else(.data$air_yards >= .data$old_ydstogo |
                                             .data$Turnover_Ind == 1,
                                           10, .data$old_ydstogo - .data$air_yards),
                  down = dplyr::if_else(.data$air_yards >= .data$old_ydstogo |
                                          .data$Turnover_Ind == 1,
                                        1, as.numeric(.data$old_down) + 1),
                  half_seconds_remaining = .data$old_TimeSecs_Remaining - 5.704673,
                  down1 = dplyr::if_else(.data$down == 1, 1, 0),
                  down2 = dplyr::if_else(.data$down == 2, 1, 0),
                  down3 = dplyr::if_else(.data$down == 3, 1, 0),
                  down4 = dplyr::if_else(.data$down == 4, 1, 0),
                  posteam_timeouts_remaining = dplyr::if_else(.data$Turnover_Ind == 1,
                                                              .data$defeam_timeouts_pre,
                                                              .data$posteam_timeouts_pre),
                  defteam_timeouts_remaining = dplyr::if_else(.data$Turnover_Ind == 1,
                                                              .data$posteam_timeouts_pre,
                                                              .data$defeam_timeouts_pre)
                  )


  #get EP predictions
  pass_pbp_data_preds <- get_preds(pass_pbp_data)

  # Convert to air EP:
  pass_pbp_data_preds <- dplyr::mutate(pass_pbp_data_preds, airEP = (.data$Opp_Safety*-2) + (.data$Opp_Field_Goal*-3) +
                                         (.data$Opp_Touchdown*-7) + (.data$Safety*2) + (.data$Field_Goal*3) + (.data$Touchdown*7))

  # Return back to the passing data:
  pass_pbp_data$airEP <- pass_pbp_data_preds$airEP

  # For the plays that have TimeSecs_Remaining 0 or less, set airEP to 0:
  pass_pbp_data$airEP[which(pass_pbp_data$half_seconds_remaining <= 0)] <- 0

  # Calculate the airEPA based on 4 scenarios:
  pass_pbp_data$airEPA <- with(pass_pbp_data, ifelse(old_yrdline100 - air_yards <= 0,
                                                     7 - ep,
                                                     ifelse(old_yrdline100 - air_yards > 99,
                                                            -2 - ep,
                                                            ifelse(Turnover_Ind == 1,
                                                                   (-1*airEP) - ep,
                                                                   airEP - ep))))

  # If the play is a two-point conversion then change the airEPA to NA since
  # no air yards are provided:
  pass_pbp_data$airEPA <- with(pass_pbp_data, ifelse(two_point_attempt == 1,
                                                     NA, airEPA))
  # Calculate the yards after catch EPA:
  pass_pbp_data <- dplyr::mutate(pass_pbp_data, yacEPA = .data$epa - .data$airEPA)


  # if Yards after catch is 0 make yacEPA set to 0:
  pass_pbp_data$yacEPA <- ifelse(pass_pbp_data$penalty == 0 & pass_pbp_data$yards_after_catch == 0 & pass_pbp_data$complete_pass==1,
                                 0, pass_pbp_data$yacEPA)

  # if Yards after catch is 0 make airEPA set to EPA:
  pass_pbp_data$airEPA <- ifelse(pass_pbp_data$penalty == 0 & pass_pbp_data$yards_after_catch == 0 & pass_pbp_data$complete_pass == 1,
                                 pass_pbp_data$epa, pass_pbp_data$airEPA)

  # Now add airEPA and yacEPA to the original dataset:
  pbp_data$airEPA <- NA
  pbp_data$yacEPA <- NA
  pbp_data$airEPA[pass_plays_i] <- pass_pbp_data$airEPA
  pbp_data$yacEPA[pass_plays_i] <- pass_pbp_data$yacEPA

  # Now change the names to be the right style, calculate the completion form
  # of the variables, as well as the cumulative totals and return:
  pbp_data %>%
    dplyr::rename(air_epa = "airEPA",
                  yac_epa = "yacEPA") %>%
    dplyr::group_by(.data$game_id) %>%
    dplyr::mutate(comp_air_epa = dplyr::if_else(.data$complete_pass == 1,
                                                .data$air_epa, 0),
                  comp_yac_epa = dplyr::if_else(.data$complete_pass == 1,
                                                .data$yac_epa, 0),
                  home_team_comp_air_epa = dplyr::if_else(.data$posteam == .data$home_team,
                                                          .data$comp_air_epa, -.data$comp_air_epa),
                  away_team_comp_air_epa = dplyr::if_else(.data$posteam == .data$away_team,
                                                          .data$comp_air_epa, -.data$comp_air_epa),
                  home_team_comp_yac_epa = dplyr::if_else(.data$posteam == .data$home_team,
                                                          .data$comp_yac_epa, -.data$comp_yac_epa),
                  away_team_comp_yac_epa = dplyr::if_else(.data$posteam == .data$away_team,
                                                          .data$comp_yac_epa, -.data$comp_yac_epa),
                  home_team_comp_air_epa = dplyr::if_else(is.na(.data$home_team_comp_air_epa),
                                                          0, .data$home_team_comp_air_epa),
                  away_team_comp_air_epa = dplyr::if_else(is.na(.data$away_team_comp_air_epa),
                                                          0, .data$away_team_comp_air_epa),
                  home_team_comp_yac_epa = dplyr::if_else(is.na(.data$home_team_comp_yac_epa),
                                                          0, .data$home_team_comp_yac_epa),
                  away_team_comp_yac_epa = dplyr::if_else(is.na(.data$away_team_comp_yac_epa),
                                                          0, .data$away_team_comp_yac_epa),
                  total_home_comp_air_epa = cumsum(.data$home_team_comp_air_epa),
                  total_away_comp_air_epa = cumsum(.data$away_team_comp_air_epa),
                  total_home_comp_yac_epa = cumsum(.data$home_team_comp_yac_epa),
                  total_away_comp_yac_epa = cumsum(.data$away_team_comp_yac_epa),
                  # Same but for raw - not just completions:
                  home_team_raw_air_epa = dplyr::if_else(.data$posteam == .data$home_team,
                                                         .data$air_epa, -.data$air_epa),
                  away_team_raw_air_epa = dplyr::if_else(.data$posteam == .data$away_team,
                                                         .data$air_epa, -.data$air_epa),
                  home_team_raw_yac_epa = dplyr::if_else(.data$posteam == .data$home_team,
                                                         .data$yac_epa, -.data$yac_epa),
                  away_team_raw_yac_epa = dplyr::if_else(.data$posteam == .data$away_team,
                                                         .data$yac_epa, -.data$yac_epa),
                  home_team_raw_air_epa = dplyr::if_else(is.na(.data$home_team_raw_air_epa),
                                                         0, .data$home_team_raw_air_epa),
                  away_team_raw_air_epa = dplyr::if_else(is.na(.data$away_team_raw_air_epa),
                                                         0, .data$away_team_raw_air_epa),
                  home_team_raw_yac_epa = dplyr::if_else(is.na(.data$home_team_raw_yac_epa),
                                                         0, .data$home_team_raw_yac_epa),
                  away_team_raw_yac_epa = dplyr::if_else(is.na(.data$away_team_raw_yac_epa),
                                                         0, .data$away_team_raw_yac_epa),
                  total_home_raw_air_epa = cumsum(.data$home_team_raw_air_epa),
                  total_away_raw_air_epa = cumsum(.data$away_team_raw_air_epa),
                  total_home_raw_yac_epa = cumsum(.data$home_team_raw_yac_epa),
                  total_away_raw_yac_epa = cumsum(.data$away_team_raw_yac_epa)) %>%
    dplyr::ungroup() %>%
    return()
}


#################################################################
# air and YAC WP:
# as with the rest, heavily borrowed from nflscrapR:
# https://github.com/maksimhorowitz/nflscrapR/blob/master/R/add_ep_wp_variables.R
add_air_yac_wp_variables <- function(pbp_data) {

  #testing
  #pbp_data <- g

  # Change the names to reflect the old style - will update this later on:
  pbp_data <- pbp_data %>%
    dplyr::mutate(
                  posteam_timeouts_pre = .data$posteam_timeouts_remaining,
                  defeam_timeouts_pre = .data$defteam_timeouts_remaining
      )

  # Final all pass attempts that are not sacks:
  pass_plays_i <- which(!is.na(pbp_data$air_yards) & pbp_data$play_type == 'pass')
  pass_pbp_data <- pbp_data[pass_plays_i,]

  pass_pbp_data <- pass_pbp_data %>%
    dplyr::mutate(
                  half_seconds_remaining = .data$half_seconds_remaining - 5.704673,
                  game_seconds_remaining = .data$game_seconds_remaining - 5.704673,
                  Diff_Time_Ratio = .data$score_differential / (exp(-4 * .data$elapsed_share)),
                  Turnover_Ind = dplyr::if_else(.data$down == 4 & .data$air_yards < .data$ydstogo,
                                                1, 0),
                  Diff_Time_Ratio = dplyr::if_else(.data$Turnover_Ind == 1,
                                                           -1 * .data$Diff_Time_Ratio,
                                                           .data$Diff_Time_Ratio),
                  posteam_timeouts_remaining = dplyr::if_else(.data$Turnover_Ind == 1,
                                                              .data$defeam_timeouts_pre,
                                                        .data$posteam_timeouts_pre),
                  defteam_timeouts_remaining = dplyr::if_else(.data$Turnover_Ind == 1,
                                                              .data$posteam_timeouts_pre,
                                                              .data$defeam_timeouts_pre)
                  )

  # Calculate the airWP:
  pass_pbp_data$airWP <- get_preds_wp(pass_pbp_data)

  # Now for plays marked with Turnover_Ind, use 1 - airWP to flip back to the original
  # team with possession:
  pass_pbp_data$airWP <- ifelse(pass_pbp_data$Turnover_Ind == 1,
                                1 - pass_pbp_data$airWP, pass_pbp_data$airWP)

  # For the plays that have TimeSecs_Remaining 0 or less, set airWP to 0:
  pass_pbp_data$airWP[which(pass_pbp_data$half_seconds_remaining <= 0)] <- 0
  pass_pbp_data$airWP[which(pass_pbp_data$game_seconds_remaining <= 0)] <- 0

  # Calculate the airWPA and yacWPA:
  pass_pbp_data <- dplyr::mutate(pass_pbp_data, airWPA = .data$airWP - .data$wp,
                                 yacWPA = .data$wpa - .data$airWPA)


  # If the play is a two-point conversion then change the airWPA to NA since
  # no air yards are provided:
  pass_pbp_data$airWPA <- with(pass_pbp_data, ifelse(two_point_attempt == 1,
                                                     NA, airWPA))
  pass_pbp_data$yacWPA <- with(pass_pbp_data, ifelse(two_point_attempt == 1,
                                                     NA, yacWPA))

  # Check to see if there is any overtime plays, if so then need to calculate
  # by essentially taking the same process as the airEP calculation and using
  # the resulting probabilities for overtime:

  # First check if there's any overtime plays:
  if (any(pass_pbp_data$qtr == 5 | pass_pbp_data$qtr == 6)){
    # Find the rows that are overtime:
    pass_overtime_i <- which(pass_pbp_data$qtr == 5 | pass_pbp_data$qtr == 6)
    pass_overtime_df <- pass_pbp_data[pass_overtime_i,]

    # Find the rows that are overtime:

    # Need to generate same overtime scenario data as before in the wp function:
    # Find the rows that are overtime:
    overtime_i <- which(pbp_data$qtr == 5 | pbp_data$qtr == 6)

    overtime_df <- pbp_data[overtime_i,]

    # Separate routine for overtime:

    # Create a column that is just the first drive of overtime repeated:
    overtime_df$First_Drive <- rep(min(overtime_df$drive,
                                       na.rm = TRUE),
                                   nrow(overtime_df))

    # Calculate the difference in drive number
    overtime_df <- dplyr::mutate(overtime_df,
                                 Drive_Diff = .data$drive - .data$First_Drive)

    # Create an indicator column that means the posteam is losing by 3 and
    # its the second drive of overtime:
    overtime_df$One_FG_Game <- ifelse(overtime_df$score_differential == -3 &
                                        overtime_df$Drive_Diff == 1, 1, 0)

    # Now create a copy of the dataset to then make the EP predictions for when
    # a field goal is scored and its not sudden death:
    overtime_df_ko <- overtime_df

    overtime_df_ko$yardline_100 <- with(overtime_df_ko,
                                      ifelse(game_year < 2016 |
                                               (game_year == 2016 & game_month < 4),
                                             80, 75))

    # Now first down:
    overtime_df_ko$down1 <- rep(1,nrow(overtime_df_ko))
    overtime_df_ko$down2 <- rep(0,nrow(overtime_df_ko))
    overtime_df_ko$down3 <- rep(0,nrow(overtime_df_ko))
    overtime_df_ko$down4 <- rep(0,nrow(overtime_df_ko))
    # 10 ydstogo:
    overtime_df_ko$ydstogo <- rep(10,nrow(overtime_df_ko))

    # Get the predictions from the EP model and calculate the necessary probability:
    if (nrow(overtime_df_ko) > 1) {
      overtime_df_ko_preds <- get_preds(overtime_df_ko)
    } else{
      overtime_df_ko_preds <- get_preds(overtime_df_ko)
    }

    overtime_df_ko_preds <- dplyr::mutate(overtime_df_ko_preds,
                                          Win_Back = .data$No_Score + .data$Opp_Field_Goal + .data$Opp_Safety + .data$Opp_Touchdown)

    # Calculate the two possible win probability types, Sudden Death and one Field Goal:
    overtime_df$Sudden_Death_WP <- overtime_df$fg_prob + overtime_df$td_prob + overtime_df$safety_prob
    overtime_df$One_FG_WP <- overtime_df$td_prob + (overtime_df$fg_prob * overtime_df_ko_preds$Win_Back)

    # Find all Pass Attempts that are also actual plays in overtime:
    overtime_pass_plays_i <- which(overtime_df$play_type == "pass" &
                                     !is.na(overtime_df$air_yards))

    overtime_pass_df <- overtime_df[overtime_pass_plays_i,]
    overtime_df_ko_preds_pass <- overtime_df_ko_preds[overtime_pass_plays_i,]

    # Using the AirYards need to update the following:
    # - yardline_100
    # - half_seconds_remaining
    # - ydstogo
    # - down

    # First rename the old columns to update for calculating the EP from the air:
    overtime_pass_df <- dplyr::rename(overtime_pass_df,
                                      old_yrdline100 = "yardline_100",
                                      old_ydstogo = "ydstogo",
                                      old_TimeSecs_Remaining = "half_seconds_remaining",
                                      old_down = "down")

    # Create an indicator column for the air yards failing to convert the first down:
    overtime_pass_df$Turnover_Ind <- ifelse(overtime_pass_df$old_down == 4 &
                                              overtime_pass_df$air_yards < overtime_pass_df$old_ydstogo,
                                            1, 0)
    # Adjust the field position variables:
    overtime_pass_df$yardline_100 <- ifelse(overtime_pass_df$Turnover_Ind == 0,
                                          overtime_pass_df$old_yrdline100 - overtime_pass_df$air_yards,
                                          100 - (overtime_pass_df$old_yrdline100 - overtime_pass_df$air_yards))

    overtime_pass_df$ydstogo <- ifelse(overtime_pass_df$air_yards >= overtime_pass_df$old_ydstogo |
                                         overtime_pass_df$Turnover_Ind == 1,
                                       10, overtime_pass_df$old_ydstogo - overtime_pass_df$air_yards)

    overtime_pass_df$down <- ifelse(overtime_pass_df$air_yards >= overtime_pass_df$old_ydstogo |
                                      overtime_pass_df$Turnover_Ind == 1,
                                    1, as.numeric(overtime_pass_df$old_down) + 1)

    # Adjust the time with the average incomplete pass time:
    overtime_pass_df$half_seconds_remaining <- overtime_pass_df$old_TimeSecs_Remaining - 5.704673

    overtime_pass_df <- overtime_pass_df %>%
      dplyr::mutate(
        down1 = dplyr::if_else(.data$down == 1, 1, 0),
        down2 = dplyr::if_else(.data$down == 2, 1, 0),
        down3 = dplyr::if_else(.data$down == 3, 1, 0),
        down4 = dplyr::if_else(.data$down == 4, 1, 0)
      )

    # Get the predictions from the EP model and calculate the necessary probability:
    if (nrow(overtime_df_ko) > 1) {
      overtime_pass_data_preds <- get_preds(overtime_pass_df)
    } else{
      overtime_pass_data_preds <- get_preds(overtime_pass_df)
    }

    # For the turnover plays flip the scoring probabilities:
    overtime_pass_data_preds <- dplyr::mutate(overtime_pass_data_preds,
                                              old_Opp_Field_Goal = .data$Opp_Field_Goal,
                                              old_Opp_Safety = .data$Opp_Safety,
                                              old_Opp_Touchdown = .data$Opp_Touchdown,
                                              old_Field_Goal = .data$Field_Goal,
                                              old_Safety = .data$Safety,
                                              old_Touchdown = .data$Touchdown)
    overtime_pass_data_preds$Opp_Field_Goal <- ifelse(overtime_pass_df$Turnover_Ind == 1,
                                                      overtime_pass_data_preds$old_Field_Goal,
                                                      overtime_pass_data_preds$Opp_Field_Goal)
    overtime_pass_data_preds$Opp_Safety <- ifelse(overtime_pass_df$Turnover_Ind == 1,
                                                  overtime_pass_data_preds$old_Safety,
                                                  overtime_pass_data_preds$Opp_Safety)
    overtime_pass_data_preds$Opp_Touchdown <- ifelse(overtime_pass_df$Turnover_Ind == 1,
                                                     overtime_pass_data_preds$old_Touchdown,
                                                     overtime_pass_data_preds$Opp_Touchdown)
    overtime_pass_data_preds$Field_Goal <- ifelse(overtime_pass_df$Turnover_Ind == 1,
                                                  overtime_pass_data_preds$old_Opp_Field_Goal,
                                                  overtime_pass_data_preds$Field_Goal)
    overtime_pass_data_preds$Safety <- ifelse(overtime_pass_df$Turnover_Ind == 1,
                                              overtime_pass_data_preds$old_Opp_Safety,
                                              overtime_pass_data_preds$Safety)
    overtime_pass_data_preds$Touchdown <- ifelse(overtime_pass_df$Turnover_Ind == 1,
                                                 overtime_pass_data_preds$old_Opp_Touchdown,
                                                 overtime_pass_data_preds$Touchdown)

    # Calculate the two possible win probability types, Sudden Death and one Field Goal:
    pass_overtime_df$Sudden_Death_airWP <- with(overtime_pass_data_preds, Field_Goal + Touchdown + Safety)
    pass_overtime_df$One_FG_airWP <- overtime_pass_data_preds$Touchdown + (overtime_pass_data_preds$Field_Goal*overtime_df_ko_preds_pass$Win_Back)

    # Decide which win probability to use:
    pass_overtime_df$airWP <- ifelse(overtime_pass_df$game_year >= 2012  & (overtime_pass_df$Drive_Diff == 0 | (overtime_pass_df$Drive_Diff == 1 & overtime_pass_df$One_FG_Game == 1)),
                                     pass_overtime_df$One_FG_airWP, pass_overtime_df$Sudden_Death_airWP)

    # For the plays that have TimeSecs_Remaining 0 or less, set airWP to 0:
    pass_overtime_df$airWP[which(overtime_pass_df$half_seconds_remaining <= 0)] <- 0

    # Calculate the airWPA and yacWPA:
    pass_overtime_df <- dplyr::mutate(pass_overtime_df, airWPA = .data$airWP - .data$wp,
                                      yacWPA = .data$wpa - .data$airWPA)

    # If the play is a two-point conversion then change the airWPA to NA since
    # no air yards are provided:
    pass_overtime_df$airWPA <- with(pass_overtime_df, ifelse(two_point_attempt == 1,
                                                             NA, airWPA))
    pass_overtime_df$yacWPA <- with(pass_overtime_df, ifelse(two_point_attempt == 1,
                                                             NA, yacWPA))


    pass_overtime_df <- pass_pbp_data[pass_overtime_i,]

    # Now update the overtime rows in the original pass_pbp_data for airWPA and yacWPA:
    pass_pbp_data$airWPA[pass_overtime_i] <- pass_overtime_df$airWPA
    pass_pbp_data$yacWPA[pass_overtime_i] <- pass_overtime_df$yacWPA
  }

  # if Yards after catch is 0 make yacWPA set to 0:
  pass_pbp_data$yacWPA <- ifelse(pass_pbp_data$penalty == 0 & pass_pbp_data$yards_after_catch == 0 &
                                   pass_pbp_data$complete_pass == 1,
                                 0, pass_pbp_data$yacWPA)
  # if Yards after catch is 0 make airWPA set to WPA:
  pass_pbp_data$airWPA <- ifelse(pass_pbp_data$penalty == 0 & pass_pbp_data$yards_after_catch == 0 &
                                   pass_pbp_data$complete_pass == 1,
                                 pass_pbp_data$wpa, pass_pbp_data$airWPA)

  # Now add airWPA and yacWPA to the original dataset:
  pbp_data$airWPA <- NA
  pbp_data$yacWPA <- NA
  pbp_data$airWPA[pass_plays_i] <- pass_pbp_data$airWPA
  pbp_data$yacWPA[pass_plays_i] <- pass_pbp_data$yacWPA


  # Now change the names to be the right style, calculate the completion form
  # of the variables, as well as the cumulative totals and return:
  pbp_data %>%
    dplyr::rename(air_wpa = "airWPA",
                  yac_wpa = "yacWPA") %>%
    dplyr::group_by(.data$game_id) %>%
    dplyr::mutate(comp_air_wpa = dplyr::if_else(.data$complete_pass == 1,
                                                .data$air_wpa, 0),
                  comp_yac_wpa = dplyr::if_else(.data$complete_pass == 1,
                                                .data$yac_wpa, 0),
                  home_team_comp_air_wpa = dplyr::if_else(.data$posteam == .data$home_team,
                                                          .data$comp_air_wpa, -.data$comp_air_wpa),
                  away_team_comp_air_wpa = dplyr::if_else(.data$posteam == .data$away_team,
                                                          .data$comp_air_wpa, -.data$comp_air_wpa),
                  home_team_comp_yac_wpa = dplyr::if_else(.data$posteam == .data$home_team,
                                                          .data$comp_yac_wpa, -.data$comp_yac_wpa),
                  away_team_comp_yac_wpa = dplyr::if_else(.data$posteam == .data$away_team,
                                                          .data$comp_yac_wpa, -.data$comp_yac_wpa),
                  home_team_comp_air_wpa = dplyr::if_else(is.na(.data$home_team_comp_air_wpa),
                                                          0, .data$home_team_comp_air_wpa),
                  away_team_comp_air_wpa = dplyr::if_else(is.na(.data$away_team_comp_air_wpa),
                                                          0, .data$away_team_comp_air_wpa),
                  home_team_comp_yac_wpa = dplyr::if_else(is.na(.data$home_team_comp_yac_wpa),
                                                          0, .data$home_team_comp_yac_wpa),
                  away_team_comp_yac_wpa = dplyr::if_else(is.na(.data$away_team_comp_yac_wpa),
                                                          0, .data$away_team_comp_yac_wpa),
                  total_home_comp_air_wpa = cumsum(.data$home_team_comp_air_wpa),
                  total_away_comp_air_wpa = cumsum(.data$away_team_comp_air_wpa),
                  total_home_comp_yac_wpa = cumsum(.data$home_team_comp_yac_wpa),
                  total_away_comp_yac_wpa = cumsum(.data$away_team_comp_yac_wpa),
                  # Same but for raw - not just completions:
                  home_team_raw_air_wpa = dplyr::if_else(.data$posteam == .data$home_team,
                                                         .data$air_wpa, -.data$air_wpa),
                  away_team_raw_air_wpa = dplyr::if_else(.data$posteam == .data$away_team,
                                                         .data$air_wpa, -.data$air_wpa),
                  home_team_raw_yac_wpa = dplyr::if_else(.data$posteam == .data$home_team,
                                                         .data$yac_wpa, -.data$yac_wpa),
                  away_team_raw_yac_wpa = dplyr::if_else(.data$posteam == .data$away_team,
                                                         .data$yac_wpa, -.data$yac_wpa),
                  home_team_raw_air_wpa = dplyr::if_else(is.na(.data$home_team_raw_air_wpa),
                                                         0, .data$home_team_raw_air_wpa),
                  away_team_raw_air_wpa = dplyr::if_else(is.na(.data$away_team_raw_air_wpa),
                                                         0, .data$away_team_raw_air_wpa),
                  home_team_raw_yac_wpa = dplyr::if_else(is.na(.data$home_team_raw_yac_wpa),
                                                         0, .data$home_team_raw_yac_wpa),
                  away_team_raw_yac_wpa = dplyr::if_else(is.na(.data$away_team_raw_yac_wpa),
                                                         0, .data$away_team_raw_yac_wpa),
                  total_home_raw_air_wpa = cumsum(.data$home_team_raw_air_wpa),
                  total_away_raw_air_wpa = cumsum(.data$away_team_raw_air_wpa),
                  total_home_raw_yac_wpa = cumsum(.data$home_team_raw_yac_wpa),
                  total_away_raw_yac_wpa = cumsum(.data$away_team_raw_yac_wpa)) %>%
    dplyr::ungroup() %>%
    return()

}
