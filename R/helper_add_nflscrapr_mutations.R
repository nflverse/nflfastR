################################################################################
# Author: Sebastian Carl, Ben Baldwin (Code mostly extracted from nflscrapR)
# Purpose: Add variables mostly needed for ep(a) and wp(a) calculation
# Code Style Guide: styler::tidyverse_style()
################################################################################

add_nflscrapr_mutations <- function(pbp) {

  #testing only
  #pbp <- combined

  out <-
    pbp |>
    dplyr::mutate(index = 1 : dplyr::n()) |>
    # remove duplicate plays. can't do this with play_id because duplicate plays
    # sometimes have different play_ids
    dplyr::group_by(.data$game_id, .data$quarter, .data$time, .data$play_description, .data$down) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      # Modify the time column for the quarter end:
      time = dplyr::if_else(.data$quarter_end == 1 |
                              (.data$play_description == "END GAME" & is.na(.data$time)), "00:00", .data$time),
      time = dplyr::if_else(.data$play_description == 'GAME', "15:00", .data$time),
      # Create a column with the time in seconds remaining for the quarter:
      quarter_seconds_remaining = time_to_seconds(.data$time),
      play_description = dplyr::case_when(
        stringr::str_detect(.data$play_description, "(?<=kicks )[:alpha:]{1,}.[:alpha:]{1,}(?= yards)") ~
          stringr::str_replace(.data$play_description, "(?<=kicks )[:alpha:]{1,}.[:alpha:]{1,}(?= yards)", as.character(.data$kick_distance)),
        TRUE ~ .data$play_description
      )
    ) |>
    #put plays in the right order
    dplyr::group_by(.data$game_id) |>
    # the !is.na(drive), drive part is to make the initial GAME line show up first
    # https://stackoverflow.com/questions/43343590/how-to-sort-putting-nas-first-in-dplyr
    dplyr::arrange(.data$order_sequence, .data$quarter, !is.na(.data$quarter_seconds_remaining), -.data$quarter_seconds_remaining, !is.na(.data$drive), .data$drive, .data$index, .by_group = TRUE) |>
    dplyr::mutate(

      # Using the various two point indicators, create a column denoting the result
      # outcome for two point conversions:
      two_point_conv_result = dplyr::if_else(
        (.data$two_point_rush_good == 1 |
           .data$two_point_pass_good == 1 |
           .data$two_point_pass_reception_good == 1) &
          .data$two_point_attempt == 1,
        "success", NA_character_
      ),
      two_point_conv_result = dplyr::if_else(
        (.data$two_point_rush_failed == 1 |
           .data$two_point_pass_failed == 1 |
           .data$two_point_pass_reception_failed == 1) &
          .data$two_point_attempt == 1,
        "failure", .data$two_point_conv_result
      ),
      two_point_conv_result = dplyr::if_else(
        (.data$two_point_rush_safety == 1 |
           .data$two_point_pass_safety == 1) &
          .data$two_point_attempt == 1,
        "safety", .data$two_point_conv_result
      ),
      two_point_conv_result = dplyr::if_else(
        .data$two_point_return == 1 &
          .data$two_point_attempt == 1,
        "return", .data$two_point_conv_result
      ),
      # If the result was a success, make the yards_gained to be 2:
      yards_gained = dplyr::if_else(
        !is.na(.data$two_point_conv_result) &
          .data$two_point_conv_result == "success",
        2, .data$yards_gained
      ),
      # Fix yards_gained for plays with laterals
      yards_gained = dplyr::case_when(
        !is.na(.data$passing_yards) &
          .data$yards_gained != .data$passing_yards &
          .data$penalty == 0 ~ .data$passing_yards,
        !is.na(.data$rushing_yards) &
          !is.na(.data$lateral_rushing_yards) &
          .data$yards_gained != .data$rushing_yards &
          .data$penalty == 0 ~ .data$rushing_yards + .data$lateral_rushing_yards,
        TRUE ~ yards_gained
      ),
      # Extract the penalty type:
      penalty_type = dplyr::if_else(
        .data$penalty == 1,
        .data$play_description |>
          stringr::str_extract("(?<=PENALTY on .{1,50}, ).{1,50}(?=, [0-9]{1,2} yard)") |>
          # Face Mask penalties include the yardage as string (either 5 Yards or 15 Yards)
          # We remove the 15 Yards part and just keep the additional info if it's a
          # 5 yard Face Mask penalty
          stringr::str_remove("\\([0-9]{2}+ Yards\\)") |>
          stringr::str_squish(), NA_character_
      ),
      # The new "dynamic Kickoff" in the 2024 season introduces a new penalty type
      penalty_type = dplyr::if_else(
        .data$penalty == 1 & stringr::str_detect(tolower(.data$play_description), "kickoff short of landing zone"),
        "Kickoff Short of Landing Zone",
        .data$penalty_type
      ),
      # Make plays marked with down == 0 as NA:
      down = dplyr::if_else(
        .data$down == 0,
        NA_real_, .data$down
      ),
      # Using the field goal indicators make a column with the field goal result:
      field_goal_result = dplyr::if_else(
        .data$field_goal_attempt == 1 &
          .data$field_goal_made == 1,
        "made", NA_character_
      ),
      field_goal_result = dplyr::if_else(
        .data$field_goal_attempt == 1 &
          .data$field_goal_missed == 1,
        "missed", .data$field_goal_result
      ),
      field_goal_result = dplyr::if_else(
        .data$field_goal_attempt == 1 &
          .data$field_goal_blocked == 1,
        "blocked", .data$field_goal_result
      ),

      # Using the indicators make a column with the extra point result:
      extra_point_result = dplyr::if_else(
        .data$extra_point_attempt == 1 &
          .data$extra_point_good == 1,
        "good", NA_character_
      ),
      extra_point_result = dplyr::if_else(
        .data$extra_point_attempt == 1 &
          .data$extra_point_failed == 1,
        "failed", .data$extra_point_result
      ),
      extra_point_result = dplyr::if_else(
        .data$extra_point_attempt == 1 &
          .data$extra_point_blocked == 1,
        "blocked", .data$extra_point_result
      ),
      extra_point_result = dplyr::if_else(
        .data$extra_point_attempt == 1 &
          .data$extra_point_safety == 1,
        "safety", .data$extra_point_result
      ),
      extra_point_result = dplyr::if_else(
        .data$extra_point_attempt == 1 &
          .data$extra_point_aborted == 1,
        "aborted", .data$extra_point_result
      ),

      # find kickoffs with penalty: a play where the next play is a kickoff
      # and the prior play wasn't a safety or PAT
      lead_ko = case_when(
        dplyr::lead(.data$kickoff_attempt) == 1 &
          .data$game_id == dplyr::lead(.data$game_id) &
          !stringr::str_detect(tolower(.data$play_description), "(injured sf )|(tonight's attendance )|(injury update )|(end quarter)|(timeout)|( captains:)|( captains )|( captians:)|( humidity:)|(note - )|( deferred)|(game start )|( game has been suspended)") &
          !stringr::str_detect(.data$play_description, "GAME ") &
          !.data$play_description %in% c("GAME", "Two-Minute Warning", "The game has resumed.") &
          is.na(.data$two_point_conv_result) &
          is.na(.data$extra_point_result) &
          is.na(.data$field_goal_result) &
          (.data$safety == 0 | is.na(.data$safety)) &
          # because things too messed up before
             .data$season > 2000 ~ 1,
        TRUE ~ 0),

      kickoff_attempt = dplyr::if_else(
        .data$lead_ko == 1, 1, .data$kickoff_attempt
      ),

      # https://github.com/nflverse/nflfastR/issues/199#issuecomment-792321171
      kickoff_attempt = dplyr::if_else(
        .data$game_id == "2014_02_ATL_CIN" & .data$play_id == 3498, 1, .data$kickoff_attempt
      ),

      # Make the possession team for kickoffs be the return team, since that is
      # more intuitive from the EPA / WPA point of view:
      posteam = dplyr::case_when(
        # kickoff_finder is defined below
        (.data$lead_ko == 1 | .data$kickoff_attempt == 1 | stringr::str_detect(.data$play_description, kickoff_finder)) & .data$posteam == .data$home_team ~ .data$away_team,
        (.data$lead_ko == 1 | .data$kickoff_attempt == 1 | stringr::str_detect(.data$play_description, kickoff_finder)) & .data$posteam == .data$away_team ~ .data$home_team,
        TRUE ~ .data$posteam
      ),

      # Fill in the rows with missing posteam with the lead:
      posteam = dplyr::if_else(
        (.data$quarter_end == 1 | .data$posteam == ""),
        dplyr::lead(.data$posteam),
        .data$posteam),
      posteam_id = dplyr::if_else(
        (.data$quarter_end == 1 | .data$posteam_id == ""),
        dplyr::lead(.data$posteam_id),
        .data$posteam_id),

      # remove posteam from END Q2 plays or END Q4 plays (when game goes in OT)
      # because it doesn't make sense and breaks fixed_drive and fixed_drive_result
      posteam = dplyr::if_else(
        stringr::str_detect(.data$play_description, "(END QUARTER 2)|(END QUARTER 4)"),
        NA_character_, .data$posteam
      ),

      # Denote whether the home or away team has possession:
      posteam_type = dplyr::if_else(.data$posteam == .data$home_team, "home", "away"),

      # Column denoting which team is on defense:
      defteam = dplyr::if_else(
        .data$posteam == .data$home_team,
        .data$away_team, .data$home_team
      ),

      yardline = dplyr::if_else(stringr::str_detect(.data$yardline, "50"), "MID 50", .data$yardline),
      yardline = dplyr::if_else(
        nchar(.data$yardline) == 0 | is.null(.data$yardline) | .data$yardline == "NULL" | is.na(.data$yardline),
        dplyr::lead(.data$yardline), .data$yardline
      ),
      yardline_number = dplyr::if_else(
        .data$yardline == "MID 50", 50, .data$yardline_number
      ),
      yardline_100 = dplyr::if_else(
        .data$yardline_side == .data$posteam | .data$yardline == "MID 50",
        100 - .data$yardline_number, .data$yardline_number
      ),
      # Set the kick_distance for extra points by adding 18 to the yardline_100:
      kick_distance = dplyr::if_else(
        .data$extra_point_attempt == 1,
        .data$yardline_100 + 18,
        .data$kick_distance
      ),
      # Create a column with the time in seconds remaining for each half:
      half_seconds_remaining = dplyr::if_else(
        .data$quarter %in% c(1, 3),
        .data$quarter_seconds_remaining + 900,
        .data$quarter_seconds_remaining),
      # Create a column with the time in seconds remaining for the game:
      game_seconds_remaining = dplyr::if_else(
        .data$quarter %in% c(1, 2, 3, 4),
        .data$quarter_seconds_remaining + (900 * (4 - as.numeric(.data$quarter))),
        .data$quarter_seconds_remaining
      ),
      # Add column for replay or challenge:
      replay_or_challenge = stringr::str_detect(
        .data$play_description, "(Replay Official reviewed)|( challenge(d)? )|(Challenged)") |>
        as.numeric(),
      # Result of replay or challenge:
      replay_or_challenge_result = dplyr::if_else(
        .data$replay_or_challenge == 1,
        dplyr::if_else(
          stringr::str_detect(
            tolower(.data$play_description),
            "( upheld)|( reversed)|( confirmed)"
          ),
          stringr::str_extract(
            tolower(.data$play_description),
            "( upheld)|( reversed)|( confirmed)"
          ) |>
            stringr::str_trim(), "denied"
        ),
        NA_character_
      ),

      # Create the column denoting the categorical description of the pass length:
      pass_length = dplyr::if_else(
        .data$two_point_attempt == 0 &
          .data$sack == 0 &
          .data$pass_attempt == 1,
        .data$play_description |> stringr::str_extract("pass (incomplete )?(short|deep)") |>
          stringr::str_extract("short|deep"), NA_character_
      ),
      # Create the column denoting the categorical location of the pass:
      pass_location = dplyr::if_else(
        .data$two_point_attempt == 0 &
          .data$sack == 0 &
          .data$pass_attempt == 1,
        .data$play_description |> stringr::str_extract("(short|deep) (left|middle|right)") |>
          stringr::str_extract("left|middle|right"), NA_character_
      ),
      # Indicator columns for both QB kneels, spikes, scrambles,
      # no huddle, shotgun plays:
      qb_kneel = dplyr::if_else(stringr::str_detect(.data$play_description, " kneels ") & .data$kickoff_attempt != 1, 1, 0),
      qb_spike = stringr::str_detect(.data$play_description, " spiked ") |> as.numeric(),
      qb_scramble = stringr::str_detect(.data$play_description, " scrambles ") |> as.numeric(),
      shotgun = stringr::str_detect(.data$play_description, "Shotgun") |> as.numeric(),
      no_huddle = stringr::str_detect(.data$play_description, "No Huddle") |> as.numeric(),

      # Create a play type column: either pass, run, field_goal, extra_point,
      # kickoff, punt, qb_kneel, qb_spike, or no_play (which includes timeouts and
      # penalties):
      # but first reset the penalty fix variable in case it's trash
      penalty_fix = dplyr::if_else(.data$penalty == 1 & .data$play_type_nfl == "PENALTY", 0, .data$penalty_fix),

      play_type = dplyr::if_else(
        (.data$penalty == 0 |
          (.data$penalty == 1 & .data$penalty_fix == 1)) &
          (.data$pass_attempt == 1 |
             .data$incomplete_pass == 1 |
             .data$two_point_pass_good == 1 |
             .data$two_point_pass_failed == 1 |
             .data$two_point_pass_safety == 1 |
             .data$two_point_pass_reception_good == 1 |
             .data$two_point_pass_reception_failed == 1 |
             .data$pass_attempt == 1 |
             .data$pass_touchdown == 1 |
             .data$complete_pass == 1),
        "pass", "no_play"
      ),
      play_type = dplyr::if_else(
        (.data$penalty == 0 |
          (.data$penalty == 1 & .data$penalty_fix == 1)) &
          (.data$two_point_rush_good == 1 |
             .data$two_point_rush_failed == 1 |
             .data$two_point_rush_safety == 1 |
             .data$rush_attempt == 1 |
             .data$rush_touchdown == 1),
        "run", .data$play_type
      ),
      play_type = dplyr::if_else(
        (.data$penalty == 0 |
          (.data$penalty == 1 & .data$return_penalty_fix == 1) |
          (.data$penalty == 1 & (.data$punt_inside_twenty == 1 |
                                   .data$punt_in_endzone == 1 |
                                   .data$punt_out_of_bounds == 1 |
                                   .data$punt_downed == 1 |
                                   .data$punt_fair_catch == 1))) &
          .data$punt_attempt == 1,
        "punt", .data$play_type
      ),
      play_type = dplyr::if_else(
        (.data$penalty == 0 |
          (.data$penalty == 1 & .data$return_penalty_fix == 1) |
          (.data$penalty == 1 & (.data$kickoff_inside_twenty == 1 |
                                   .data$kickoff_in_endzone == 1 |
                                   .data$kickoff_out_of_bounds == 1 |
                                   .data$kickoff_downed == 1 |
                                   .data$kickoff_fair_catch == 1))) &
          .data$kickoff_attempt == 1,
        "kickoff", .data$play_type
      ),
      play_type = dplyr::if_else(
        (.data$penalty == 0 |
          (.data$penalty == 1 & .data$penalty_fix == 1)) & .data$field_goal_attempt == 1,
        "field_goal", .data$play_type
      ),
      play_type = dplyr::if_else(
        (.data$penalty == 0 |
          (.data$penalty == 1 & .data$penalty_fix == 1)) & .data$extra_point_attempt == 1,
        "extra_point", .data$play_type
      ),
      play_type = dplyr::if_else(
        (.data$penalty == 0 |
           (.data$penalty == 1 & .data$penalty_fix == 1)) & .data$qb_spike == 1,
        "qb_spike", .data$play_type
      ),
      play_type = dplyr::if_else(
        (.data$penalty == 0 |
           (.data$penalty == 1 & .data$penalty_fix == 1)) & .data$qb_kneel == 1,
        "qb_kneel", .data$play_type
      ),
      play_type = dplyr::if_else(
        is.na(.data$penalty) & is.na(.data$play_type) & stringr::str_detect(.data$play_description, " offsetting"), "no_play", .data$play_type
      ),
      # play_type can be no_play on special teams plays with penalties that don't
      # result in a replayed down. We fix this here using play_type_nfl (#281)
      play_type = dplyr::case_when(
        .data$play_type == "no_play" &
          !.data$play_type_nfl %in% c("PENALTY", "TIMEOUT") &
          !stringr::str_detect(.data$play_description, "No Play") ~ translate_play_type_nfl(.data$play_type_nfl),
        TRUE ~ .data$play_type
      ),
      # Indicator for QB dropbacks (exclude spikes and kneels):
      qb_dropback = dplyr::if_else(
        .data$play_type == "pass" |
          (.data$play_type == "run" &
             .data$qb_scramble == 1),
        1, 0
      ),
      # Columns denoting the run location and gap:
      run_location = dplyr::if_else(
        .data$two_point_attempt == 0 &
          .data$rush_attempt == 1,
        .data$play_description |> stringr::str_extract(" (left|middle|right) ") |>
          stringr::str_trim(), NA_character_
      ),
      run_gap = dplyr::if_else(
        .data$two_point_attempt == 0 &
          .data$rush_attempt == 1,
        .data$play_description |> stringr::str_extract(" (guard|tackle|end) ") |>
          stringr::str_trim(), NA_character_
      ),
      game_half = dplyr::case_when(
        .data$quarter %in% c(1, 2) ~ "Half1",
        .data$quarter %in% c(3, 4) ~ "Half2",
        .data$quarter >= 5 ~ "Overtime",
        FALSE ~ NA_character_
      ),
      # Create columns to denote the timeouts remaining for each team, making
      # columns for both home/away and pos/def (this will involve creating
      # temporary columns that will not be included):
      # Initialize both home and away to have 3 timeouts for each
      # half except overtime where they have 2:

      # extract timeouts from failed challenges when it's not otherwise there
      tmp_timeout = stringr::str_extract(.data$play_description, "(?<=by\\s)[:upper:]{2,3}(?=\\s)"),
      timeout_team = dplyr::if_else(
        .data$replay_or_challenge == 1 & .data$timeout == 1 & is.na(.data$timeout_team), .data$tmp_timeout, .data$timeout_team

      ),

      home_timeouts_remaining = dplyr::if_else(
        .data$quarter %in% c(1, 2, 3, 4),
        3, 2
      ),
      away_timeouts_remaining = dplyr::if_else(
        .data$quarter %in% c(1, 2, 3, 4),
        3, 2
      ),
      home_timeout_used = dplyr::if_else(
        .data$timeout == 1 &
          .data$timeout_team == .data$home_team,
        1, 0
      ),
      away_timeout_used = dplyr::if_else(
        .data$timeout == 1 &
          .data$timeout_team == .data$away_team,
        1, 0
      ),
      home_timeout_used = dplyr::if_else(
        is.na(.data$home_timeout_used),
        0, .data$home_timeout_used
      ),
      away_timeout_used = dplyr::if_else(
        is.na(.data$away_timeout_used),
        0, .data$away_timeout_used
      )
    ) |>
    # replace empty strings in yard line variables
    dplyr::mutate_at(
      .vars = c("yardline", "drive_start_yard_line" ,"drive_end_yard_line"),
      .funs = ~ dplyr::na_if(.x, "")
    ) |>
    # fix cases where a yardline variable misses the blank space between team name
    # and yard number. At the point of adding this, the only spot where this happened
    # was in the variable drive_start_yard_line in the games
    # "2000_01_CAR_WAS", "2000_02_NE_NYJ", and "2000_03_ATL_CAR"
    dplyr::mutate_at(
      .vars = c("yardline", "drive_start_yard_line" ,"drive_end_yard_line"),
      .funs = ~ dplyr::case_when(
        stringr::str_detect(.x, "[:upper:]{2,3}(?=[:digit:]{1,2})") ~
          stringr::str_c(stringr::str_extract(.x, "[:upper:]{2,3}"), stringr::str_extract(.x, "[:digit:]{1,2}"), sep = " "),
        TRUE ~ .x
      )
    ) |>
    # Group by the game_half to then create cumulative timeouts used for both
    # the home and away teams:
    dplyr::group_by(.data$game_id, .data$game_half) |>
    dplyr::mutate(
      total_home_timeouts_used = dplyr::if_else(cumsum(.data$home_timeout_used) > 3, 3, cumsum(.data$home_timeout_used)),
      total_away_timeouts_used = dplyr::if_else(cumsum(.data$away_timeout_used) > 3, 3, cumsum(.data$away_timeout_used))
    ) |>
    dplyr::ungroup() |>
    dplyr::group_by(.data$game_id) |>
    # Now just take the difference between the timeouts remaining
    # columns and the total timeouts used, and create the columns for both
    # the pos and def team timeouts remaining:
    dplyr::mutate(
      home_timeouts_remaining = .data$home_timeouts_remaining - .data$total_home_timeouts_used,
      away_timeouts_remaining = .data$away_timeouts_remaining - .data$total_away_timeouts_used,
      posteam_timeouts_remaining = dplyr::if_else(
        .data$posteam == .data$home_team,
        .data$home_timeouts_remaining,
        .data$away_timeouts_remaining
      ),
      defteam_timeouts_remaining = dplyr::if_else(
        .data$defteam == .data$home_team,
        .data$home_timeouts_remaining,
        .data$away_timeouts_remaining
      ),
      # Same type of logic to calculate the score for each team and the score
      # differential in the game. First create columns to track how many points
      # were scored on a particular play based on various scoring indicators for
      # both the home and away teams:
      home_points_scored = dplyr::if_else(
        .data$touchdown == 1 &
          .data$td_team == .data$home_team,
        6, 0
      ),
      home_points_scored = dplyr::if_else(
        .data$posteam == .data$home_team &
          .data$field_goal_made == 1,
        3, .data$home_points_scored
      ),
      home_points_scored = dplyr::if_else(
        .data$posteam == .data$home_team &
          (.data$extra_point_good == 1 |
             .data$extra_point_safety == 1 |
             .data$two_point_rush_safety == 1 |
             .data$two_point_pass_safety == 1),
        1, .data$home_points_scored
      ),
      home_points_scored = dplyr::if_else(
        .data$posteam == .data$home_team &
          (.data$two_point_rush_good == 1 |
             .data$two_point_pass_good == 1 |
             .data$two_point_pass_reception_good == 1),
        2, .data$home_points_scored
      ),
      home_points_scored = dplyr::if_else(
        .data$defteam == .data$home_team &
          (.data$two_point_return == 1 | .data$defensive_two_point_conv == 1),
        2, .data$home_points_scored
      ),
      home_points_scored = dplyr::if_else(
        .data$safety_team == .data$home_team & .data$safety == 1,
        2, .data$home_points_scored
      ),
      away_points_scored = dplyr::if_else(
        .data$touchdown == 1 &
          .data$td_team == .data$away_team,
        6, 0
      ),
      away_points_scored = dplyr::if_else(
        .data$posteam == .data$away_team &
          .data$field_goal_made == 1,
        3, .data$away_points_scored
      ),
      away_points_scored = dplyr::if_else(
        .data$posteam == .data$away_team &
          (.data$extra_point_good == 1 |
             .data$extra_point_safety == 1 |
             .data$two_point_rush_safety == 1 |
             .data$two_point_pass_safety == 1),
        1, .data$away_points_scored
      ),
      away_points_scored = dplyr::if_else(
        .data$posteam == .data$away_team &
          (.data$two_point_rush_good == 1 |
             .data$two_point_pass_good == 1 |
             .data$two_point_pass_reception_good == 1),
        2, .data$away_points_scored
      ),
      away_points_scored = dplyr::if_else(
        .data$defteam == .data$away_team &
          (.data$two_point_return == 1 | .data$defensive_two_point_conv == 1),
        2, .data$away_points_scored
      ),
      away_points_scored = dplyr::if_else(
        .data$safety_team == .data$away_team & .data$safety == 1,
        2, .data$away_points_scored
      ),
      home_points_scored = dplyr::if_else(
        is.na(.data$home_points_scored),
        0, .data$home_points_scored
      ),
      away_points_scored = dplyr::if_else(
        is.na(.data$away_points_scored),
        0, .data$away_points_scored
      ),
      # Now create cumulative totals:
      total_home_score = cumsum(.data$home_points_scored),
      total_away_score = cumsum(.data$away_points_scored),
      posteam_score = dplyr::if_else(
        .data$posteam == .data$home_team,
        dplyr::lag(.data$total_home_score),
        dplyr::lag(.data$total_away_score)
      ),
      defteam_score = dplyr::if_else(
        .data$defteam == .data$home_team,
        dplyr::lag(.data$total_home_score),
        dplyr::lag(.data$total_away_score)
      ),
      score_differential = .data$posteam_score - .data$defteam_score,
      abs_score_differential = abs(.data$score_differential),
      # Make post score differential columns to be used for the final
      # game indicators in the win probability calculations:
      posteam_score_post = dplyr::if_else(
        .data$posteam == .data$home_team,
        .data$total_home_score,
        .data$total_away_score
      ),
      defteam_score_post = dplyr::if_else(
        .data$defteam == .data$home_team,
        .data$total_home_score,
        .data$total_away_score
      ),
      score_differential_post = .data$posteam_score_post - .data$defteam_score_post,
      abs_score_differential_post = abs(.data$posteam_score_post - .data$defteam_score_post),
      # Create a variable for whether or not a touchback occurred, this
      # will apply to any type of play:
      touchback = as.numeric(stringr::str_detect(tolower(.data$play_description), "touchback")),
      # There are a few plays with air_yards prior 2006 (most likely accidently)
      # To not crash the air_yac ep and wp calculation they are being set to NA
      air_yards = dplyr::if_else(.data$season < 2006, NA_real_, .data$air_yards)
    ) |>
    dplyr::rename(
      ydstogo = "yards_to_go",
      desc = "play_description",
      yrdln = "yardline",
      side_of_field = "yardline_side",
      qtr = "quarter"
    ) |>
    dplyr::filter(
      !is.na(.data$desc),
      .data$desc != "",
      !is.na(.data$qtr)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      game_id = as.character(.data$game_id),
      # kick distance is NA on kickoffs and punts that result in touchbacks
      # (unless the kick/punt) was caught between endzones
      # we use yardline_100 to add it in those cases
      is_relevant_touchback = as.numeric(is.na(.data$kick_distance) & .data$touchback == 1 & .data$play_type %in% c("punt", "kickoff")),
      kick_distance = dplyr::case_when(
        .data$is_relevant_touchback == 1 & .data$kickoff_attempt == 0 ~ yardline_100,
        # gotta reverse yardline_100 on kickoffs
        .data$is_relevant_touchback == 1 & .data$kickoff_attempt == 1 ~ 100 - yardline_100,
        TRUE ~ .data$kick_distance
      ),
      # drop helper variable
      is_relevant_touchback = NULL
    ) |>
    fix_scrambles() |>
    make_model_mutations()


  user_message("added nflscrapR variables", "done")
  return(out)
}

# to help find kickoffs on plays with penalties
# otherwise win prob breaks down the road
kickoff_finder <- "(Offside on Free Kick)|(Delay of Kickoff)|(Onside Kick formation)|(kicks onside)|( kicks [:digit:]+ yards from)"


##some steps to prepare the data for the EP/WP/CP/FG models
make_model_mutations <- function(pbp) {

  pbp <- pbp |>
    dplyr::mutate(
      #for EP, CP, and WP model, xgb needs 0/1 for eras
      era0 = dplyr::if_else(.data$season <= 2001, 1, 0),
      era1 = dplyr::if_else(.data$season > 2001 & .data$season <= 2005, 1, 0),
      era2 = dplyr::if_else(.data$season > 2005 & .data$season <= 2013, 1, 0),
      era3 = dplyr::if_else(.data$season > 2013 & .data$season <= 2017, 1, 0),
      era4 = dplyr::if_else(.data$season > 2017, 1, 0),
      #for fg model, an era factor
      era = dplyr::case_when(
        .data$era0 == 1 ~ 0,
        .data$era1 == 1 ~ 1,
        .data$era2 == 1 ~ 2,
        .data$era3 == 1 | era4 == 1 ~ 3
      ),
      era = as.factor(.data$era),
      down1 = dplyr::if_else(.data$down == 1, 1, 0),
      down2 = dplyr::if_else(.data$down == 2, 1, 0),
      down3 = dplyr::if_else(.data$down == 3, 1, 0),
      down4 = dplyr::if_else(.data$down == 4, 1, 0),
      home = dplyr::if_else(.data$posteam == .data$home_team, 1, 0),
      model_roof = dplyr::if_else(is.na(.data$roof) | .data$roof == 'open' | .data$roof == 'closed', as.character('retractable'), as.character(.data$roof)),
      model_roof = as.factor(.data$model_roof),
      retractable = dplyr::if_else(.data$model_roof == 'retractable', 1, 0),
      dome = dplyr::if_else(.data$model_roof == 'dome', 1, 0),
      outdoors = dplyr::if_else(.data$model_roof == 'outdoors', 1, 0)
    )

  return(pbp)
}


fix_scrambles <- function(pbp) {
  # skip below code if <= 2005 is not in the data
  if (min(pbp$season) > 2005) return(pbp)

  pbp |>
    dplyr::mutate(
      scramble_id = paste0(.data$game_id, "_", .data$play_id),
      qb_scramble = dplyr::if_else(.data$scramble_id %in% scramble_fix, 1, .data$qb_scramble)
    ) |>
    dplyr::select(-"scramble_id")

  # Some notes on the scramble_fix:
  # This marks scrambles in the 1999 - 2005 season using charting data
  # Because NFL did not put scramble in play description during this season
  # Data from Aaron Schatz!
}

translate_play_type_nfl <- function(play_type_nfl){
  dplyr::case_when(
    play_type_nfl == "COMMENT" ~ "no_play",
    play_type_nfl == "END_GAME" ~ "no_play",
    play_type_nfl == "END_QUARTER" ~ "no_play",
    play_type_nfl == "FIELD_GOAL" ~ "field_goal",
    play_type_nfl == "FREE_KICK" ~ "kickoff",
    play_type_nfl == "GAME_START" ~ "no_play",
    play_type_nfl == "KICK_OFF" ~ "kickoff",
    play_type_nfl == "PASS" ~ "pass",
    play_type_nfl == "PAT2" ~ "extra_point",
    play_type_nfl == "PENALTY" ~ "no_play",
    play_type_nfl == "PUNT" ~ "punt",
    play_type_nfl == "RUSH" ~ "run",
    play_type_nfl == "SACK" ~ "pass",
    play_type_nfl == "TIMEOUT" ~ "no_play",
    play_type_nfl == "XP_KICK" ~ "extra_point",
    TRUE ~ NA_character_
  )
}
