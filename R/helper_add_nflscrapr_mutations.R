################################################################################
# Author: Sebastian Carl, Ben Baldwin (Code mostly extracted from nflscrapR)
# Purpose: Add variables mostly needed for ep(a) and wp(a) calculation
# Code Style Guide: styler::tidyverse_style()
################################################################################

add_nflscrapr_mutations <- function(pbp) {
  # All steps work on a list of column vectors instead of dplyr verbs (much
  # faster on single games). Grouped dplyr operations use the helpers in utils.R
  pbp$index <- seq_len(nrow(pbp))
  d <- as_column_list(drop_duplicated_plays(pbp))

  # Modify the time column for the quarter end:
  d$time <- if_else_vec(
    d$quarter_end == 1 |
      (d$play_description == "END GAME" & is.na(d$time)),
    "00:00",
    d$time
  )
  d$time <- if_else_vec(
    d$play_description == 'GAME',
    "15:00",
    d$time
  )
  # Create a column with the time in seconds remaining for the quarter:
  d$quarter_seconds_remaining <- time_to_seconds(d$time)
  d$play_description <- case_when_vec(
    list(
      stringr::str_detect(
        d$play_description,
        "(?<=kicks )[:alpha:]{1,}.[:alpha:]{1,}(?= yards)"
      ),
      stringr::str_replace(
        d$play_description,
        "(?<=kicks )[:alpha:]{1,}.[:alpha:]{1,}(?= yards)",
        as.character(d$kick_distance)
      )
    ),
    list(
      TRUE,
      d$play_description
    )
  )

  # put plays in the right order
  # the !is.na(drive), drive part is to make the initial GAME line show up first
  # https://stackoverflow.com/questions/43343590/how-to-sort-putting-nas-first-in-dplyr
  play_order <- order(
    d$game_id,
    d$order_sequence,
    d$quarter,
    !is.na(d$quarter_seconds_remaining),
    -d$quarter_seconds_remaining,
    !is.na(d$drive),
    d$drive,
    d$index,
    method = "radix"
  )
  d <- lapply(d, `[`, play_order)

  # helper, dropped below
  d$tmp_desc_lower <- tolower(d$play_description)
  # Using the various two point indicators, create a column denoting the result
  # outcome for two point conversions:
  d$two_point_conv_result <- if_else_vec(
    (d$two_point_rush_good == 1 |
      d$two_point_pass_good == 1 |
      d$two_point_pass_reception_good == 1) &
      d$two_point_attempt == 1,
    "success",
    NA_character_
  )
  d$two_point_conv_result <- if_else_vec(
    (d$two_point_rush_failed == 1 |
      d$two_point_pass_failed == 1 |
      d$two_point_pass_reception_failed == 1) &
      d$two_point_attempt == 1,
    "failure",
    d$two_point_conv_result
  )
  d$two_point_conv_result <- if_else_vec(
    (d$two_point_rush_safety == 1 |
      d$two_point_pass_safety == 1) &
      d$two_point_attempt == 1,
    "safety",
    d$two_point_conv_result
  )
  d$two_point_conv_result <- if_else_vec(
    d$two_point_return == 1 &
      d$two_point_attempt == 1,
    "return",
    d$two_point_conv_result
  )
  # If the result was a success, make the yards_gained to be 2:
  d$yards_gained <- if_else_vec(
    !is.na(d$two_point_conv_result) &
      d$two_point_conv_result == "success",
    2,
    d$yards_gained
  )
  # Fix yards_gained for plays with laterals
  d$yards_gained <- case_when_vec(
    list(
      !is.na(d$passing_yards) &
        d$yards_gained != d$passing_yards &
        d$penalty == 0,
      d$passing_yards
    ),
    list(
      !is.na(d$rushing_yards) &
        !is.na(d$lateral_rushing_yards) &
        d$yards_gained != d$rushing_yards &
        d$penalty == 0,
      d$rushing_yards +
        d$lateral_rushing_yards
    ),
    list(
      TRUE,
      d$yards_gained
    )
  )
  # Extract the penalty type:
  d$penalty_type <- apply_where(
    d$penalty == 1,
    d$play_description,
    function(x) {
      x |>
        stringr::str_extract(
          "(?<=PENALTY on .{1,50}, ).{1,50}(?=, [0-9]{1,2} yard)"
        ) |>
        # Face Mask penalties include the yardage as string (either 5 Yards or 15 Yards)
        # We remove the 15 Yards part and just keep the additional info if it's a
        # 5 yard Face Mask penalty
        stringr::str_remove("\\([0-9]{2}+ Yards\\)") |>
        stringr::str_squish()
    }
  )
  # The new "dynamic Kickoff" in the 2024 season introduces new penalty types
  d$penalty_type <- if_else_vec(
    d$penalty == 1 &
      stringr::str_detect(
        d$tmp_desc_lower,
        "kickoff short of landing zone"
      ),
    "Kickoff Short of Landing Zone",
    d$penalty_type
  )
  d$penalty_type <- if_else_vec(
    d$penalty == 1 &
      stringr::str_detect(
        d$tmp_desc_lower,
        "kickoff out of bounds"
      ),
    "Kickoff Out of Bounds",
    d$penalty_type
  )
  # Make plays marked with down == 0 as NA:
  d$down <- if_else_vec(
    d$down == 0,
    NA_real_,
    d$down
  )
  # Using the field goal indicators make a column with the field goal result:
  d$field_goal_result <- if_else_vec(
    d$field_goal_attempt == 1 &
      d$field_goal_made == 1,
    "made",
    NA_character_
  )
  d$field_goal_result <- if_else_vec(
    d$field_goal_attempt == 1 &
      d$field_goal_missed == 1,
    "missed",
    d$field_goal_result
  )
  d$field_goal_result <- if_else_vec(
    d$field_goal_attempt == 1 &
      d$field_goal_blocked == 1,
    "blocked",
    d$field_goal_result
  )
  # Using the indicators make a column with the extra point result:
  d$extra_point_result <- if_else_vec(
    d$extra_point_attempt == 1 &
      d$extra_point_good == 1,
    "good",
    NA_character_
  )
  d$extra_point_result <- if_else_vec(
    d$extra_point_attempt == 1 &
      d$extra_point_failed == 1,
    "failed",
    d$extra_point_result
  )
  d$extra_point_result <- if_else_vec(
    d$extra_point_attempt == 1 &
      d$extra_point_blocked == 1,
    "blocked",
    d$extra_point_result
  )
  d$extra_point_result <- if_else_vec(
    d$extra_point_attempt == 1 &
      d$extra_point_safety == 1,
    "safety",
    d$extra_point_result
  )
  d$extra_point_result <- if_else_vec(
    d$extra_point_attempt == 1 &
      d$extra_point_aborted == 1,
    "aborted",
    d$extra_point_result
  )
  # find kickoffs with penalty: a play where the next play is a kickoff
  # and the prior play wasn't a safety or PAT
  d$lead_ko <- case_when_vec(
    list(
      lead_in_game(d$kickoff_attempt, d$game_id) == 1 &
        d$game_id == lead_in_game(d$game_id, d$game_id) &
        !stringr::str_detect(
          d$tmp_desc_lower,
          "(injured sf )|(tonight's attendance )|(injury update )|(end quarter)|(timeout)|( captains:)|( captains )|( captians:)|( humidity:)|(note - )|( deferred)|(game start )|( game has been suspended)"
        ) &
        !stringr::str_detect(d$play_description, "GAME ") &
        !d$play_description %in%
          c("GAME", "Two-Minute Warning", "The game has resumed.") &
        is.na(d$two_point_conv_result) &
        is.na(d$extra_point_result) &
        is.na(d$field_goal_result) &
        (d$safety == 0 | is.na(d$safety)) &
        # because things too messed up before
        d$season > 2000,
      1
    ),
    list(
      TRUE,
      0
    )
  )
  # we overwrite kickoff_attempt for kickoffs with penalties because
  # those mess with ep/epa/wp/wpa. Since this is inconsistent compared to
  # all other *_attempt variables, we will restore kickoff_attempt after
  # models are applied. That's done with a temporary copy of kickoff_attempt.
  # See #556, #202, #199 for example
  d$copy_of_kickoff_attempt <- d$kickoff_attempt
  d$kickoff_attempt <- if_else_vec(
    d$lead_ko == 1,
    1,
    d$kickoff_attempt
  )
  # https://github.com/nflverse/nflfastR/issues/199#issuecomment-792321171
  d$kickoff_attempt <- if_else_vec(
    d$game_id == "2014_02_ATL_CIN" & d$play_id == 3498,
    1,
    d$kickoff_attempt
  )
  # Make the possession team for kickoffs be the return team, since that is
  # more intuitive from the EPA / WPA point of view:
  d$posteam <- case_when_vec(
    list(
      # kickoff_finder is defined below
      (d$lead_ko == 1 |
        d$kickoff_attempt == 1 |
        stringr::str_detect(d$play_description, kickoff_finder)) &
        d$posteam == d$home_team,
      d$away_team
    ),
    list(
      (d$lead_ko == 1 |
        d$kickoff_attempt == 1 |
        stringr::str_detect(d$play_description, kickoff_finder)) &
        d$posteam == d$away_team,
      d$home_team
    ),
    list(
      TRUE,
      d$posteam
    )
  )
  # Fill in the rows with missing posteam with the lead:
  d$posteam <- if_else_vec(
    (d$quarter_end == 1 | d$posteam == ""),
    lead_in_game(d$posteam, d$game_id),
    d$posteam
  )
  d$posteam_id <- if_else_vec(
    (d$quarter_end == 1 | d$posteam_id == ""),
    lead_in_game(d$posteam_id, d$game_id),
    d$posteam_id
  )
  # remove posteam from END Q2 plays or END Q4 plays (when game goes in OT)
  # because it doesn't make sense and breaks fixed_drive and fixed_drive_result
  d$posteam <- if_else_vec(
    stringr::str_detect(
      d$play_description,
      "(END QUARTER 2)|(END QUARTER 4)"
    ),
    NA_character_,
    d$posteam
  )
  # Denote whether the home or away team has possession:
  d$posteam_type <- if_else_vec(
    d$posteam == d$home_team,
    "home",
    "away"
  )
  # manual posteam adjustments for rare plays with issues related to game
  # delays.
  d$posteam <- case_when_vec(
    list(
      # 2025_01_CAR_JAX, 1317: Game resumed after weather delay
      # AND it was delayed right after a PAT.
      # Prior two plays were delay info that shouldn't have posteam in order
      # to get correct fixed drive results #529
      # https://github.com/nflverse/nflfastR/issues/529
      d$game_id == "2025_01_CAR_JAX" &
        d$play_id %in% c(1282, 1303),
      NA_character_
    ),
    list(
      TRUE,
      d$posteam
    )
  )
  # Column denoting which team is on defense:
  d$defteam <- if_else_vec(
    d$posteam == d$home_team,
    d$away_team,
    d$home_team
  )
  d$yardline <- if_else_vec(
    stringr::str_detect(d$yardline, "50"),
    "MID 50",
    d$yardline
  )
  d$yardline <- if_else_vec(
    nchar(d$yardline) == 0 |
      is.null(d$yardline) |
      d$yardline == "NULL" |
      is.na(d$yardline),
    lead_in_game(d$yardline, d$game_id),
    d$yardline
  )
  d$yardline_number <- if_else_vec(
    d$yardline == "MID 50",
    50,
    d$yardline_number
  )
  d$yardline_100 <- if_else_vec(
    d$yardline_side == d$posteam | d$yardline == "MID 50",
    100 - d$yardline_number,
    d$yardline_number
  )
  # Set the kick_distance for extra points by adding 18 to the yardline_100:
  d$kick_distance <- if_else_vec(
    d$extra_point_attempt == 1,
    d$yardline_100 + 18,
    d$kick_distance
  )
  # Create a column with the time in seconds remaining for each half:
  d$half_seconds_remaining <- if_else_vec(
    d$quarter %in% c(1, 3),
    d$quarter_seconds_remaining + 900,
    d$quarter_seconds_remaining
  )
  # Create a column with the time in seconds remaining for the game:
  d$game_seconds_remaining <- if_else_vec(
    d$quarter %in% c(1, 2, 3, 4),
    d$quarter_seconds_remaining +
      (900 * (4 - as.numeric(d$quarter))),
    d$quarter_seconds_remaining
  )
  # Add column for replay or challenge:
  d$replay_or_challenge <- stringr::str_detect(
    d$play_description,
    "(Replay Official reviewed)|( challenge(d)? )|(Challenged)"
  ) |>
    as.numeric()
  # Result of replay or challenge:
  d$replay_or_challenge_result <- if_else_vec(
    d$replay_or_challenge == 1,
    if_else_vec(
      stringr::str_detect(
        d$tmp_desc_lower,
        "( upheld)|( reversed)|( confirmed)"
      ),
      stringr::str_extract(
        d$tmp_desc_lower,
        "( upheld)|( reversed)|( confirmed)"
      ) |>
        stringr::str_trim(),
      "denied"
    ),
    NA_character_
  )
  # Create the column denoting the categorical description of the pass length:
  d$pass_length <- apply_where(
    d$two_point_attempt == 0 &
      d$sack == 0 &
      d$pass_attempt == 1,
    d$play_description,
    function(x) {
      x |>
        stringr::str_extract("pass (incomplete )?(short|deep)") |>
        stringr::str_extract("short|deep")
    }
  )
  # Create the column denoting the categorical location of the pass:
  d$pass_location <- apply_where(
    d$two_point_attempt == 0 &
      d$sack == 0 &
      d$pass_attempt == 1,
    d$play_description,
    function(x) {
      x |>
        stringr::str_extract("(short|deep) (left|middle|right)") |>
        stringr::str_extract("left|middle|right")
    }
  )
  # Indicator columns for both QB kneels, spikes, scrambles,
  # no huddle, shotgun plays:
  d$qb_kneel <- if_else_vec(
    stringr::str_detect(d$play_description, " kneels ") &
      d$kickoff_attempt != 1,
    1,
    0
  )
  d$qb_spike <- stringr::str_detect(d$play_description, " spiked ") |>
    as.numeric()
  d$qb_scramble <- stringr::str_detect(
    d$play_description,
    " scrambles "
  ) |>
    as.numeric()
  d$shotgun <- stringr::str_detect(d$play_description, "Shotgun") |>
    as.numeric()
  d$no_huddle <- stringr::str_detect(d$play_description, "No Huddle") |>
    as.numeric()
  # Create a play type column: either pass, run, field_goal, extra_point,
  # kickoff, punt, qb_kneel, qb_spike, or no_play (which includes timeouts and
  # penalties):
  d$play_type <- translate_play_type_nfl(
    d$play_type_nfl,
    qb_spike = d$qb_spike,
    qb_kneel = d$qb_kneel,
    pass_attempt = d$pass_attempt,
    rush_attempt = d$rush_attempt,
    punt_attempt = d$punt_attempt,
    field_goal_attempt = d$field_goal_attempt,
    penalty = d$penalty,
    is_penalty_enforced_between_downs = stringr::str_detect(
      d$tmp_desc_lower,
      "enforced between downs"
    )
  )
  # Indicator for QB dropbacks (exclude spikes and kneels):
  d$qb_dropback <- if_else_vec(
    d$play_type == "pass" |
      (d$play_type == "run" &
        d$qb_scramble == 1),
    1,
    0
  )
  # Columns denoting the run location and gap:
  d$run_location <- apply_where(
    d$two_point_attempt == 0 &
      d$rush_attempt == 1,
    d$play_description,
    function(x) {
      x |>
        stringr::str_extract(" (left|middle|right) ") |>
        stringr::str_trim()
    }
  )
  d$run_gap <- apply_where(
    d$two_point_attempt == 0 &
      d$rush_attempt == 1,
    d$play_description,
    function(x) {
      x |>
        stringr::str_extract(" (guard|tackle|end) ") |>
        stringr::str_trim()
    }
  )
  d$game_half <- case_when_vec(
    list(
      d$quarter %in% c(1, 2),
      "Half1"
    ),
    list(
      d$quarter %in% c(3, 4),
      "Half2"
    ),
    list(
      d$quarter >= 5,
      "Overtime"
    ),
    list(
      FALSE,
      NA_character_
    )
  )
  # Create columns to denote the timeouts remaining for each team, making
  # columns for both home/away and pos/def (this will involve creating
  # temporary columns that will not be included):
  # Initialize both home and away to have 3 timeouts for each
  # half except overtime where they have 2:
  # extract timeouts from failed challenges when it's not otherwise there
  d$tmp_timeout <- stringr::str_extract(
    d$play_description,
    "(?<=by\\s)[:upper:]{2,3}(?=\\s)"
  )
  d$timeout_team <- if_else_vec(
    d$replay_or_challenge == 1 &
      d$timeout == 1 &
      is.na(d$timeout_team),
    d$tmp_timeout,
    d$timeout_team
  )
  d$home_timeouts_remaining <- if_else_vec(
    d$quarter %in% c(1, 2, 3, 4),
    3,
    2
  )
  d$away_timeouts_remaining <- if_else_vec(
    d$quarter %in% c(1, 2, 3, 4),
    3,
    2
  )
  d$home_timeout_used <- if_else_vec(
    d$timeout == 1 &
      d$timeout_team == d$home_team,
    1,
    0
  )
  d$away_timeout_used <- if_else_vec(
    d$timeout == 1 &
      d$timeout_team == d$away_team,
    1,
    0
  )
  d$home_timeout_used <- if_else_vec(
    is.na(d$home_timeout_used),
    0,
    d$home_timeout_used
  )
  d$away_timeout_used <- if_else_vec(
    is.na(d$away_timeout_used),
    0,
    d$away_timeout_used
  )

  # replace empty strings in yard line variables
  # fix cases where a yardline variable misses the blank space between team name
  # and yard number. At the point of adding this, the only spot where this happened
  # was in the variable drive_start_yard_line in the games
  # "2000_01_CAR_WAS", "2000_02_NE_NYJ", and "2000_03_ATL_CAR"
  for (col in c("yardline", "drive_start_yard_line", "drive_end_yard_line")) {
    x <- d[[col]]
    x[!is.na(x) & x == ""] <- NA
    d[[col]] <- case_when_vec(
      list(
        stringr::str_detect(x, "[:upper:]{2,3}(?=[:digit:]{1,2})"),
        stringr::str_c(
          stringr::str_extract(x, "[:upper:]{2,3}"),
          stringr::str_extract(x, "[:digit:]{1,2}"),
          sep = " "
        )
      ),
      list(TRUE, x)
    )
  }

  # Create cumulative timeouts used for both the home and away teams per game
  # half:
  half <- group_key(d$game_id, d$game_half)
  d$total_home_timeouts_used <- if_else_vec(
    cumsum_by(d$home_timeout_used, half) > 3,
    3,
    cumsum_by(d$home_timeout_used, half)
  )
  d$total_away_timeouts_used <- if_else_vec(
    cumsum_by(d$away_timeout_used, half) > 3,
    3,
    cumsum_by(d$away_timeout_used, half)
  )

  # Now just take the difference between the timeouts remaining
  # columns and the total timeouts used, and create the columns for both
  # the pos and def team timeouts remaining:
  d$home_timeouts_remaining <- d$home_timeouts_remaining -
    d$total_home_timeouts_used
  d$away_timeouts_remaining <- d$away_timeouts_remaining -
    d$total_away_timeouts_used
  d$posteam_timeouts_remaining <- if_else_vec(
    d$posteam == d$home_team,
    d$home_timeouts_remaining,
    d$away_timeouts_remaining
  )
  d$defteam_timeouts_remaining <- if_else_vec(
    d$defteam == d$home_team,
    d$home_timeouts_remaining,
    d$away_timeouts_remaining
  )
  # Same type of logic to calculate the score for each team and the score
  # differential in the game. First create columns to track how many points
  # were scored on a particular play based on various scoring indicators for
  # both the home and away teams:
  d$home_points_scored <- if_else_vec(
    d$touchdown == 1 &
      d$td_team == d$home_team,
    6,
    0
  )
  d$home_points_scored <- if_else_vec(
    d$posteam == d$home_team &
      d$field_goal_made == 1,
    3,
    d$home_points_scored
  )
  d$home_points_scored <- if_else_vec(
    d$posteam == d$home_team &
      (d$extra_point_good == 1 |
        d$extra_point_safety == 1 |
        d$two_point_rush_safety == 1 |
        d$two_point_pass_safety == 1),
    1,
    d$home_points_scored
  )
  d$home_points_scored <- if_else_vec(
    d$posteam == d$home_team &
      (d$two_point_rush_good == 1 |
        d$two_point_pass_good == 1 |
        d$two_point_pass_reception_good == 1),
    2,
    d$home_points_scored
  )
  d$home_points_scored <- if_else_vec(
    d$defteam == d$home_team &
      (d$two_point_return == 1 | d$defensive_two_point_conv == 1),
    2,
    d$home_points_scored
  )
  d$home_points_scored <- if_else_vec(
    d$safety_team == d$home_team & d$safety == 1,
    2,
    d$home_points_scored
  )
  d$away_points_scored <- if_else_vec(
    d$touchdown == 1 &
      d$td_team == d$away_team,
    6,
    0
  )
  d$away_points_scored <- if_else_vec(
    d$posteam == d$away_team &
      d$field_goal_made == 1,
    3,
    d$away_points_scored
  )
  d$away_points_scored <- if_else_vec(
    d$posteam == d$away_team &
      (d$extra_point_good == 1 |
        d$extra_point_safety == 1 |
        d$two_point_rush_safety == 1 |
        d$two_point_pass_safety == 1),
    1,
    d$away_points_scored
  )
  d$away_points_scored <- if_else_vec(
    d$posteam == d$away_team &
      (d$two_point_rush_good == 1 |
        d$two_point_pass_good == 1 |
        d$two_point_pass_reception_good == 1),
    2,
    d$away_points_scored
  )
  d$away_points_scored <- if_else_vec(
    d$defteam == d$away_team &
      (d$two_point_return == 1 | d$defensive_two_point_conv == 1),
    2,
    d$away_points_scored
  )
  d$away_points_scored <- if_else_vec(
    d$safety_team == d$away_team & d$safety == 1,
    2,
    d$away_points_scored
  )
  d$home_points_scored <- if_else_vec(
    is.na(d$home_points_scored),
    0,
    d$home_points_scored
  )
  d$away_points_scored <- if_else_vec(
    is.na(d$away_points_scored),
    0,
    d$away_points_scored
  )
  # Now create cumulative totals:
  d$total_home_score <- group_cumsum(d$home_points_scored, d$game_id)
  d$total_away_score <- group_cumsum(d$away_points_scored, d$game_id)
  d$posteam_score <- if_else_vec(
    d$posteam == d$home_team,
    lag_in_game(d$total_home_score, d$game_id),
    lag_in_game(d$total_away_score, d$game_id)
  )
  d$defteam_score <- if_else_vec(
    d$defteam == d$home_team,
    lag_in_game(d$total_home_score, d$game_id),
    lag_in_game(d$total_away_score, d$game_id)
  )
  d$score_differential <- d$posteam_score - d$defteam_score
  d$abs_score_differential <- abs(d$score_differential)
  # Make post score differential columns to be used for the final
  # game indicators in the win probability calculations:
  d$posteam_score_post <- if_else_vec(
    d$posteam == d$home_team,
    d$total_home_score,
    d$total_away_score
  )
  d$defteam_score_post <- if_else_vec(
    d$defteam == d$home_team,
    d$total_home_score,
    d$total_away_score
  )
  d$score_differential_post <- d$posteam_score_post -
    d$defteam_score_post
  d$abs_score_differential_post <- abs(
    d$posteam_score_post - d$defteam_score_post
  )
  # Create a variable for whether or not a touchback occurred, this
  # will apply to any type of play:
  d$touchback <- as.numeric(stringr::str_detect(
    d$tmp_desc_lower,
    "touchback"
  ))
  # There are a few plays with air_yards prior 2006 (most likely accidently)
  # To not crash the air_yac ep and wp calculation they are being set to NA
  d$air_yards <- if_else_vec(d$season < 2006, NA_real_, d$air_yards)

  names(d)[match(
    c(
      "yards_to_go",
      "play_description",
      "yardline",
      "yardline_side",
      "quarter"
    ),
    names(d)
  )] <- c("ydstogo", "desc", "yrdln", "side_of_field", "qtr")
  keep <- is_true(!is.na(d$desc)) &
    is_true(d$desc != "") &
    is_true(!is.na(d$qtr))
  d <- lapply(d, `[`, keep)

  d$game_id <- as.character(d$game_id)
  # kick distance is NA on kickoffs and punts that result in touchbacks
  # (unless the kick/punt) was caught between endzones
  # we use yardline_100 to add it in those cases
  d$is_relevant_touchback <- as.numeric(
    is.na(d$kick_distance) &
      d$touchback == 1 &
      d$play_type %in% c("punt", "kickoff")
  )
  d$kick_distance <- case_when_vec(
    list(
      d$is_relevant_touchback == 1 &
        d$kickoff_attempt == 0,
      d$yardline_100
    ),
    list(
      # gotta reverse yardline_100 on kickoffs
      d$is_relevant_touchback == 1 & d$kickoff_attempt == 1,
      100 - d$yardline_100
    ),
    list(
      TRUE,
      d$kick_distance
    )
  )
  # drop helper variables
  d$is_relevant_touchback <- NULL
  d$tmp_desc_lower <- NULL

  out <- tibble::new_tibble(d, nrow = sum(keep)) |>
    fix_scrambles() |>
    make_model_mutations()

  user_message("added nflscrapR variables", "done")
  return(out)
}

# Remove duplicate plays. We can't do this with play_id because duplicate plays
# sometimes have different play_ids
drop_duplicated_plays <- function(pbp) {
  keys <- c("game_id", "quarter", "time", "play_description", "down")
  pbp[!duplicated(data.table::as.data.table(pbp[keys])), ]
}

# to help find kickoffs on plays with penalties
# otherwise win prob breaks down the road
kickoff_finder <- "(Offside on Free Kick)|(Delay of Kickoff)|(Onside Kick formation)|(kicks onside)|( kicks [:digit:]+ yards from)"


##some steps to prepare the data for the EP/WP/CP/FG models
make_model_mutations <- function(pbp) {
  #for EP, CP, and WP model, xgb needs 0/1 for eras
  pbp$era0 <- if_else_vec(pbp$season <= 2001, 1, 0)
  pbp$era1 <- if_else_vec(pbp$season > 2001 & pbp$season <= 2005, 1, 0)
  pbp$era2 <- if_else_vec(pbp$season > 2005 & pbp$season <= 2013, 1, 0)
  pbp$era3 <- if_else_vec(pbp$season > 2013 & pbp$season <= 2017, 1, 0)
  pbp$era4 <- if_else_vec(pbp$season > 2017, 1, 0)
  #for fg model, an era factor
  pbp$era <- case_when_vec(
    list(
      pbp$era0 == 1,
      0
    ),
    list(
      pbp$era1 == 1,
      1
    ),
    list(
      pbp$era2 == 1,
      2
    ),
    list(
      pbp$era3 == 1 | pbp$era4 == 1,
      3
    )
  )
  pbp$era <- as.factor(pbp$era)
  pbp$down1 <- if_else_vec(pbp$down == 1, 1, 0)
  pbp$down2 <- if_else_vec(pbp$down == 2, 1, 0)
  pbp$down3 <- if_else_vec(pbp$down == 3, 1, 0)
  pbp$down4 <- if_else_vec(pbp$down == 4, 1, 0)
  pbp$home <- if_else_vec(pbp$posteam == pbp$home_team, 1, 0)
  pbp$model_roof <- if_else_vec(
    is.na(pbp$roof) | pbp$roof == 'open' | pbp$roof == 'closed',
    as.character('retractable'),
    as.character(pbp$roof)
  )
  pbp$model_roof <- as.factor(pbp$model_roof)
  pbp$retractable <- if_else_vec(pbp$model_roof == 'retractable', 1, 0)
  pbp$dome <- if_else_vec(pbp$model_roof == 'dome', 1, 0)
  pbp$outdoors <- if_else_vec(pbp$model_roof == 'outdoors', 1, 0)

  return(pbp)
}


fix_scrambles <- function(pbp) {
  # skip below code if <= 2005 is not in the data
  if (min(pbp$season) > 2005) {
    return(pbp)
  }

  scramble_id <- paste0(pbp$game_id, "_", pbp$play_id)
  pbp$qb_scramble <- if_else_vec(
    scramble_id %in% scramble_fix,
    1,
    pbp$qb_scramble
  )
  pbp

  # Some notes on the scramble_fix:
  # This marks scrambles in the 1999 - 2005 season using charting data
  # Because NFL did not put scramble in play description during this season
  # Data from Aaron Schatz!
}

translate_play_type_nfl <- function(
  play_type_nfl,
  qb_spike,
  qb_kneel,
  pass_attempt,
  rush_attempt,
  punt_attempt,
  field_goal_attempt,
  penalty,
  is_penalty_enforced_between_downs
) {
  # I want the arg name to be descriptive, but I want a short variable name
  # for the code below
  x <- play_type_nfl

  out <- case_when_vec(
    list(
      x == "COMMENT",
      NA_character_
    ),
    list(
      x == "END_GAME",
      NA_character_
    ),
    list(
      x == "END_QUARTER",
      NA_character_
    ),
    list(
      x == "FIELD_GOAL",
      "field_goal"
    ),
    list(
      x == "FREE_KICK",
      "kickoff"
    ),
    list(
      x == "GAME_START",
      NA_character_
    ),
    list(
      x == "INTERCEPTION",
      "pass"
    ),
    list(
      x == "KICK_OFF",
      "kickoff"
    ),
    list(
      x == "PASS",
      "pass"
    ),
    list(
      x == "PAT2" & pass_attempt == 1,
      "pass"
    ),
    list(
      x == "PAT2" & rush_attempt == 1,
      "run"
    ),
    list(
      x == "PENALTY" &
        pass_attempt == 1 &
        is_penalty_enforced_between_downs,
      "pass"
    ),
    list(
      x == "PENALTY" &
        rush_attempt == 1 &
        is_penalty_enforced_between_downs,
      "run"
    ),
    list(
      x == "PENALTY",
      "no_play"
    ),
    list(
      x == "PUNT",
      "punt"
    ),
    list(
      x == "RUSH",
      "run"
    ),
    list(
      x == "SACK",
      "pass"
    ),
    list(
      x == "TIMEOUT",
      "no_play"
    ),
    list(
      x == "XP_KICK",
      "extra_point"
    ),
    list(
      # UNSPECIFIED is a mix of all sorts of weird plays
      x == "UNSPECIFIED" & penalty == 1,
      "no_play"
    ),
    list(
      # the following lines imply penalty == 0 because penalty == 1 triggers above
      x == "UNSPECIFIED" & pass_attempt == 1,
      "pass"
    ),
    list(
      x == "UNSPECIFIED" & rush_attempt == 1,
      "run"
    ),
    list(
      x == "UNSPECIFIED" & punt_attempt == 1,
      "punt"
    ),
    list(
      x == "UNSPECIFIED" & field_goal_attempt == 1,
      "field_goal"
    ),
    list(
      # most of the remaining UNSPECIFIED plays will be declined penalties
      # from punt or fg formation. These don't really count as play so we define
      # them as no_play
      x == "UNSPECIFIED",
      "no_play"
    ),
    list(
      # default
      TRUE,
      ""
    )
  )

  # every play_type_nfl that we do not catch in the above cases
  # will be an empty string. We try to resolve these as good as we can
  # also need to replace passes and runs that were spikes and kneel downs
  case_when_vec(
    list(
      out == "" & penalty == 1,
      "no_play"
    ),
    list(
      out == "" & pass_attempt == 1,
      "pass"
    ),
    list(
      out == "" & rush_attempt == 1,
      "run"
    ),
    list(
      out == "" & punt_attempt == 1,
      "punt"
    ),
    list(
      out == "" & field_goal_attempt == 1,
      "field_goal"
    ),
    list(
      qb_spike == 1 & out %in% c("pass", "run"),
      "qb_spike"
    ),
    list(
      qb_kneel == 1 & out %in% c("pass", "run"),
      "qb_kneel"
    ),
    list(
      TRUE,
      out
    )
  )
}

# we overwrite kickoff_attempt for kickoffs with penalties because
# those mess with ep/epa/wp/wpa. Since this is inconsistent compared to
# all other *_attempt variables, we will restore kickoff_attempt after
# models are applied. That's done with a temporary copy of kickoff_attempt.
# See #556, #202, #199 for example
restore_kickoff_attempt <- function(pbp) {
  pbp$kickoff_attempt <- pbp$copy_of_kickoff_attempt
  pbp$copy_of_kickoff_attempt <- NULL
  pbp
}
