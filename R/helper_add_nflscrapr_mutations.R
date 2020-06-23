################################################################################
# Author: Sebastian Carl, Ben Baldwin (Code mostly extracted from nflscrapR)
# Purpose: Add variables mostly needed for ep(a) and wp(a) calculation
# Code Style Guide: styler::tidyverse_style()
################################################################################

add_nflscrapr_mutations <- function(pbp) {

  #testing only
  #pbp <- combined

  out <-
    pbp %>%
    dplyr::mutate(index = 1 : dplyr::n()) %>%
    # remove duplicate plays. can't do this with play_id because duplicate plays
    # sometimes have different play_ids
    dplyr::group_by(game_id, quarter, time, play_description) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      # Modify the time column for the quarter end:
      time = dplyr::if_else(quarter_end == 1, "00:00", time),
      time = dplyr::if_else(play_description == 'GAME', "15:00", time),
      # Create a column with the time in seconds remaining for the quarter:
      quarter_seconds_remaining = lubridate::period_to_seconds(lubridate::ms(time))
    ) %>%
    #put plays in the right order
    dplyr::group_by(game_id) %>%
    # the !is.na(drive), drive part is to make the initial GAME line show up first
    # https://stackoverflow.com/questions/43343590/how-to-sort-putting-nas-first-in-dplyr
    dplyr::arrange(quarter, !is.na(quarter_seconds_remaining), -quarter_seconds_remaining, !is.na(drive), drive, index, .by_group = TRUE) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      # Fill in the rows with missing posteam with the lag:
      posteam = dplyr::if_else(
        (quarter_end == 1 | posteam == ""),
        dplyr::lag(posteam),
        posteam),
      posteam_id = dplyr::if_else(
        (quarter_end == 1 | posteam_id == ""),
        dplyr::lag(posteam_id),
        posteam_id),
      # Denote whether the home or away team has possession:
      posteam_type = dplyr::if_else(posteam == home_team, "home", "away"),
      # Column denoting which team is on defense:
      defteam = dplyr::if_else(
        posteam_type == "home",
        away_team, home_team
      ),
      # Make the possession team for kickoffs be the return team, since that is
      # more intuitive from the EPA / WPA point of view:
      posteam = dplyr::if_else(
        kickoff_attempt == 1,
        dplyr::if_else(
          posteam_type == "home",
          away_team, home_team
        ),
        posteam
      ),
      defteam = dplyr::if_else(
        kickoff_attempt == 1,
        dplyr::if_else(
          posteam_type == "home",
          home_team, away_team
        ),
        defteam
      ),
      # Now flip the posteam_type as well:
      posteam_type = dplyr::if_else(
        kickoff_attempt == 1,
        dplyr::if_else(
          posteam_type == "home",
          "away", "home"
        ),
        posteam_type
      ),
      yardline = dplyr::if_else(yardline == "50", "MID 50", yardline),
      yardline = dplyr::if_else(
        nchar(yardline) == 0 | is.null(yardline) | yardline == "NULL" | is.na(yardline),
        dplyr::lag(yardline), yardline
      ),
      yardline_number = dplyr::if_else(
        yardline == "MID 50", 50, yardline_number
      ),
      yardline_100 = dplyr::if_else(
        yardline_side == posteam | yardline == "MID 50",
        100 - yardline_number, yardline_number
      ),
      # Create a column with the time in seconds remaining for each half:
      half_seconds_remaining = dplyr::if_else(
        quarter %in% c(1, 3),
        quarter_seconds_remaining + 900,
        quarter_seconds_remaining),
      # Create a column with the time in seconds remaining for the game:
      game_seconds_remaining = dplyr::if_else(
        quarter %in% c(1, 2, 3, 4),
        quarter_seconds_remaining + (900 * (4 - as.numeric(quarter))),
        quarter_seconds_remaining
      ),
      # Add column for replay or challenge:
      replay_or_challenge = stringr::str_detect(
        play_description, "(Replay Official reviewed)|( challenge(d)? )") %>%
        as.numeric(),
      # Result of replay or challenge:
      replay_or_challenge_result = dplyr::if_else(
        replay_or_challenge == 1,
        dplyr::if_else(
          stringr::str_detect(
            tolower(play_description),
            "( upheld)|( reversed)|( confirmed)"
          ),
          stringr::str_extract(
            tolower(play_description),
            "( upheld)|( reversed)|( confirmed)"
          ) %>%
            stringr::str_trim(), "denied"
        ),
        NA_character_
      ),
      # Using the various two point indicators, create a column denoting the result
      # outcome for two point conversions:
      two_point_conv_result = dplyr::if_else(
        (two_point_rush_good == 1 |
          two_point_pass_good == 1 |
          two_point_pass_reception_good == 1) &
          two_point_attempt == 1,
        "success", NA_character_
      ),
      two_point_conv_result = dplyr::if_else(
        (two_point_rush_failed == 1 |
          two_point_pass_failed == 1 |
          two_point_pass_reception_failed == 1) &
          two_point_attempt == 1,
        "failure", two_point_conv_result
      ),
      two_point_conv_result = dplyr::if_else(
        (two_point_rush_safety == 1 |
          two_point_pass_safety == 1) &
          two_point_attempt == 1,
        "safety", two_point_conv_result
      ),
      two_point_conv_result = dplyr::if_else(
        two_point_return == 1 &
          two_point_attempt == 1,
        "return", two_point_conv_result
      ),
      # If the result was a success, make the yards_gained to be 2:
      yards_gained = dplyr::if_else(
        !is.na(two_point_conv_result) &
          two_point_conv_result == "success",
        2, yards_gained
      ),
      # Extract the penalty type:
      penalty_type = dplyr::if_else(
        penalty == 1,
        play_description %>%
          stringr::str_extract("PENALTY on (.){2,35},.+, [0-9]{1,2} yard(s),") %>%
          stringr::str_extract(", (([:alpha:])+([:space:])?)+,") %>%
          stringr::str_remove_all(",") %>%
          stringr::str_trim(), NA_character_
      ),
      # Make plays marked with down == 0 as NA:
      down = dplyr::if_else(
        down == 0,
        NA_real_, down
      ),
      # Using the field goal indicators make a column with the field goal result:
      field_goal_result = dplyr::if_else(
        field_goal_attempt == 1 &
          field_goal_made == 1,
        "made", NA_character_
      ),
      field_goal_result = dplyr::if_else(
        field_goal_attempt == 1 &
          field_goal_missed == 1,
        "missed", field_goal_result
      ),
      field_goal_result = dplyr::if_else(
        field_goal_attempt == 1 &
          field_goal_blocked == 1,
        "blocked", field_goal_result
      ),
      # Set the kick_distance for extra points by adding 18 to the yardline_100:
      kick_distance = dplyr::if_else(
        extra_point_attempt == 1,
        yardline_100 + 18,
        kick_distance
      ),
      # Using the indicators make a column with the extra point result:
      extra_point_result = dplyr::if_else(
        extra_point_attempt == 1 &
          extra_point_good == 1,
        "good", NA_character_
      ),
      extra_point_result = dplyr::if_else(
        extra_point_attempt == 1 &
          extra_point_failed == 1,
        "failed", extra_point_result
      ),
      extra_point_result = dplyr::if_else(
        extra_point_attempt == 1 &
          extra_point_blocked == 1,
        "blocked", extra_point_result
      ),
      extra_point_result = dplyr::if_else(
        extra_point_attempt == 1 &
          extra_point_safety == 1,
        "safety", extra_point_result
      ),
      extra_point_result = dplyr::if_else(
        extra_point_attempt == 1 &
          extra_point_aborted == 1,
        "aborted", extra_point_result
      ),
      # Create the column denoting the categorical description of the pass length:
      pass_length = dplyr::if_else(
        two_point_attempt == 0 &
          sack == 0 &
          pass_attempt == 1,
        play_description %>% stringr::str_extract("pass (incomplete )?(short|deep)") %>%
          stringr::str_extract("short|deep"), NA_character_
      ),
      # Create the column denoting the categorical location of the pass:
      pass_location = dplyr::if_else(
        two_point_attempt == 0 &
          sack == 0 &
          pass_attempt == 1,
        play_description %>% stringr::str_extract("(short|deep) (left|middle|right)") %>%
          stringr::str_extract("left|middle|right"), NA_character_
      ),
      # Indicator columns for both QB kneels, spikes, scrambles,
      # no huddle, shotgun plays:
      qb_kneel = stringr::str_detect(play_description, " kneels ") %>% as.numeric(),
      qb_spike = stringr::str_detect(play_description, " spiked ") %>% as.numeric(),
      qb_scramble = stringr::str_detect(play_description, " scrambles ") %>% as.numeric(),
      shotgun = stringr::str_detect(play_description, "Shotgun") %>% as.numeric(),
      no_huddle = stringr::str_detect(play_description, "No Huddle") %>% as.numeric(),
      # Create a play type column: either pass, run, field_goal, extra_point,
      # kickoff, punt, qb_kneel, qb_spike, or no_play (which includes timeouts and
      # penalties):
      play_type = dplyr::if_else(
        (penalty == 0 |
          (penalty == 1 & penalty_fix == 1)) &
          (pass_attempt == 1 |
            incomplete_pass == 1 |
            two_point_pass_good == 1 |
            two_point_pass_failed == 1 |
            two_point_pass_safety == 1 |
            two_point_pass_reception_good == 1 |
            two_point_pass_reception_failed == 1 |
            pass_attempt == 1 |
            pass_touchdown == 1 |
            complete_pass == 1),
        "pass", "no_play"
      ),
      play_type = dplyr::if_else(
        (penalty == 0 |
          (penalty == 1 & penalty_fix == 1)) &
          (two_point_rush_good == 1 |
            two_point_rush_failed == 1 |
            two_point_rush_safety == 1 |
            rush_attempt == 1 |
            rush_touchdown == 1),
        "run", play_type
      ),
      play_type = dplyr::if_else(
        (penalty == 0 |
          (penalty == 1 & return_penalty_fix == 1) |
          (penalty == 1 & (punt_inside_twenty == 1 |
            punt_in_endzone == 1 |
            punt_out_of_bounds == 1 |
            punt_downed == 1 |
            punt_fair_catch == 1))) &
          punt_attempt == 1,
        "punt", play_type
      ),
      play_type = dplyr::if_else(
        (penalty == 0 |
          (penalty == 1 & return_penalty_fix == 1) |
          (penalty == 1 & (kickoff_inside_twenty == 1 |
            kickoff_in_endzone == 1 |
            kickoff_out_of_bounds == 1 |
            kickoff_downed == 1 |
            kickoff_fair_catch == 1))) &
          kickoff_attempt == 1,
        "kickoff", play_type
      ),
      play_type = dplyr::if_else(
        (penalty == 0 |
          (penalty == 1 & penalty_fix == 1)) & qb_spike == 1,
        "qb_spike", play_type
      ),
      play_type = dplyr::if_else(
        (penalty == 0 |
          (penalty == 1 & penalty_fix == 1)) & qb_kneel == 1,
        "qb_kneel", play_type
      ),
      play_type = dplyr::if_else(
        (penalty == 0 |
          (penalty == 1 & penalty_fix == 1)) & field_goal_attempt == 1,
        "field_goal", play_type
      ),
      play_type = dplyr::if_else(
        (penalty == 0 |
          (penalty == 1 & penalty_fix == 1)) & extra_point_attempt == 1,
        "extra_point", play_type
      ),
      # Indicator for QB dropbacks (exclude spikes and kneels):
      qb_dropback = dplyr::if_else(
        play_type == "pass" |
          (play_type == "run" &
            qb_scramble == 1),
        1, 0
      ),
      # Columns denoting the run location and gap:
      run_location = dplyr::if_else(
        two_point_attempt == 0 &
          rush_attempt == 1,
        play_description %>% stringr::str_extract(" (left|middle|right) ") %>%
          stringr::str_trim(), NA_character_
      ),
      run_gap = dplyr::if_else(
        two_point_attempt == 0 &
          rush_attempt == 1,
        play_description %>% stringr::str_extract(" (guard|tackle|end) ") %>%
          stringr::str_trim(), NA_character_
      ),
      game_half = dplyr::case_when(
        quarter %in% c(1, 2) ~ "Half1",
        quarter %in% c(3, 4) ~ "Half2",
        quarter >= 5 ~ "Overtime",
        FALSE ~ NA_character_
      ),
      # Create columns to denote the timeouts remaining for each team, making
      # columns for both home/away and pos/def (this will involve creating
      # temporary columns that will not be included):
      # Initialize both home and away to have 3 timeouts for each
      # half except overtime where they have 2:
      home_timeouts_remaining = dplyr::if_else(
        quarter %in% c(1, 2, 3, 4),
        3, 2
      ),
      away_timeouts_remaining = dplyr::if_else(
        quarter %in% c(1, 2, 3, 4),
        3, 2
      ),
      home_timeout_used = dplyr::if_else(
        timeout == 1 &
          timeout_team == home_team,
        1, 0
      ),
      away_timeout_used = dplyr::if_else(
        timeout == 1 &
          timeout_team == away_team,
        1, 0
      ),
      home_timeout_used = dplyr::if_else(
        is.na(home_timeout_used),
        0, home_timeout_used
      ),
      away_timeout_used = dplyr::if_else(
        is.na(away_timeout_used),
        0, away_timeout_used
      )
    ) %>%
    # Group by the game_half to then create cumulative timeouts used for both
    # the home and away teams:
    dplyr::group_by(game_id, game_half) %>%
    dplyr::mutate(
      total_home_timeouts_used = dplyr::if_else(cumsum(home_timeout_used) > 3, 3, cumsum(home_timeout_used)),
      total_away_timeouts_used = dplyr::if_else(cumsum(away_timeout_used) > 3, 3, cumsum(away_timeout_used))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(game_id) %>%
    # Now just take the difference between the timeouts remaining
    # columns and the total timeouts used, and create the columns for both
    # the pos and def team timeouts remaining:
    dplyr::mutate(
      home_timeouts_remaining = home_timeouts_remaining - total_home_timeouts_used,
      away_timeouts_remaining = away_timeouts_remaining - total_away_timeouts_used,
      posteam_timeouts_remaining = dplyr::if_else(
        posteam == home_team,
        home_timeouts_remaining,
        away_timeouts_remaining
      ),
      defteam_timeouts_remaining = dplyr::if_else(
        defteam == home_team,
        home_timeouts_remaining,
        away_timeouts_remaining
      ),
      # Same type of logic to calculate the score for each team and the score
      # differential in the game. First create columns to track how many points
      # were scored on a particular play based on various scoring indicators for
      # both the home and away teams:
      home_points_scored = dplyr::if_else(
        touchdown == 1 &
          td_team == home_team,
        6, 0
      ),
      home_points_scored = dplyr::if_else(
        posteam == home_team &
          field_goal_made == 1,
        3, home_points_scored
      ),
      home_points_scored = dplyr::if_else(
        posteam == home_team &
          (extra_point_good == 1 |
            extra_point_safety == 1 |
            two_point_rush_safety == 1 |
            two_point_pass_safety == 1),
        1, home_points_scored
      ),
      home_points_scored = dplyr::if_else(
        posteam == home_team &
          (two_point_rush_good == 1 |
            two_point_pass_good == 1 |
            two_point_pass_reception_good == 1),
        2, home_points_scored
      ),
      home_points_scored = dplyr::if_else(
        defteam == home_team &
          (safety == 1 | two_point_return == 1),
        2, home_points_scored
      ),
      away_points_scored = dplyr::if_else(
        touchdown == 1 &
          td_team == away_team,
        6, 0
      ),
      away_points_scored = dplyr::if_else(
        posteam == away_team &
          field_goal_made == 1,
        3, away_points_scored
      ),
      away_points_scored = dplyr::if_else(
        posteam == away_team &
          (extra_point_good == 1 |
            extra_point_safety == 1 |
            two_point_rush_safety == 1 |
            two_point_pass_safety == 1),
        1, away_points_scored
      ),
      away_points_scored = dplyr::if_else(
        posteam == away_team &
          (two_point_rush_good == 1 |
            two_point_pass_good == 1 |
            two_point_pass_reception_good == 1),
        2, away_points_scored
      ),
      away_points_scored = dplyr::if_else(
        defteam == away_team &
          (safety == 1 | two_point_return == 1),
        2, away_points_scored
      ),
      home_points_scored = dplyr::if_else(
        is.na(home_points_scored),
        0, home_points_scored
      ),
      away_points_scored = dplyr::if_else(
        is.na(away_points_scored),
        0, away_points_scored
      ),
      # Now create cumulative totals:
      total_home_score = cumsum(home_points_scored),
      total_away_score = cumsum(away_points_scored),
      posteam_score = dplyr::if_else(
        posteam == home_team,
        dplyr::lag(total_home_score),
        dplyr::lag(total_away_score)
      ),
      defteam_score = dplyr::if_else(
        defteam == home_team,
        dplyr::lag(total_home_score),
        dplyr::lag(total_away_score)
      ),
      score_differential = posteam_score - defteam_score,
      abs_score_differential = abs(score_differential),
      # Make post score differential columns to be used for the final
      # game indicators in the win probability calculations:
      posteam_score_post = dplyr::if_else(
        posteam == home_team,
        total_home_score,
        total_away_score
      ),
      defteam_score_post = dplyr::if_else(
        defteam == home_team,
        total_home_score,
        total_away_score
      ),
      score_differential_post = posteam_score_post - defteam_score_post,
      abs_score_differential_post = abs(posteam_score_post - defteam_score_post),
      # Create a variable for whether or not a touchback occurred, this
      # will apply to any type of play:
      touchback = as.numeric(stringr::str_detect(tolower(play_description), "touchback")),
      # There are a few plays with air_yards prior 2006 (most likely accidently)
      # To not crash the air_yac ep and wp calculation they are being set to NA
      air_yards = dplyr::if_else(season < 2006, NA_real_, air_yards)
    ) %>%
    dplyr::rename(
      ydstogo = yards_to_go,
      desc = play_description,
      yrdln = yardline,
      side_of_field = yardline_side,
      qtr = quarter
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(game_id = as.character(game_id), drive_real_start_time = as.character(drive_real_start_time)) %>%
    make_model_mutations()


  message("added nflscrapR variables")
  return(out)
}


##some steps to prepare the data for the EP/WP/CP/FG models
make_model_mutations <- function(pbp) {

  pbp <- pbp %>%
    dplyr::mutate(
      #for EP, CP, and WP model, xgb needs 0/1 for eras
      era0 = dplyr::if_else(season <= 2001, 1, 0),
      era1 = dplyr::if_else(season > 2001 & season <= 2005, 1, 0),
      era2 = dplyr::if_else(season > 2005 & season <= 2013, 1, 0),
      era3 = dplyr::if_else(season > 2013 & season <= 2017, 1, 0),
      era4 = dplyr::if_else(season > 2017, 1, 0),
      #for fg model, an era factor
      era = dplyr::case_when(
        era0 == 1 ~ 0,
        era1 == 1 ~ 1,
        era2 == 1 ~ 2,
        era3 | era4 == 1 ~ 3
      ),
      era = as.factor(era),
      #treat playoff games as week 17 as they aren't used for training
      model_week = dplyr::if_else(week > 17, as.integer(17), as.integer(week)),
      down1 = dplyr::if_else(down == 1, 1, 0),
      down2 = dplyr::if_else(down == 2, 1, 0),
      down3 = dplyr::if_else(down == 3, 1, 0),
      down4 = dplyr::if_else(down == 4, 1, 0),
      home = dplyr::if_else(posteam == home_team, 1, 0),
      model_roof = dplyr::if_else(is.na(roof) | roof == 'open' | roof == 'closed', as.character('retractable'), as.character(roof)),
      model_roof = as.factor(model_roof),
      retractable = dplyr::if_else(model_roof == 'retractable', 1, 0),
      dome = dplyr::if_else(model_roof == 'dome', 1, 0),
      outdoors = dplyr::if_else(model_roof == 'outdoors', 1, 0)
    )

  return(pbp)
}



