################################################################################
# Author: Sebastian Carl, Ben Baldwin (Code mostly extracted from nflscrapR)
# Purpose: Add variables mostly needed for ep(a) and wp(a) calculation
# Code Style Guide: styler::tidyverse_style()
################################################################################

add_nflscrapr_mutations <- function(pbp) {
  # pbp is the data.table of build_nflfastR_pbp(), changed by reference
  d <- pbp
  data.table::set(d, j = "index", value = seq_len(nrow(d)))
  # remove duplicate plays. can't do this with play_id because duplicate plays
  # sometimes have different play_ids
  d <- unique(
    d,
    by = c("game_id", "quarter", "time", "play_description", "down")
  )

  # Modify the time column for the quarter end:
  data.table::set(
    d,
    j = "time",
    value = data.table::fifelse(
      d$quarter_end == 1 |
        (d$play_description == "END GAME" & is.na(d$time)),
      "00:00",
      d$time
    )
  )
  data.table::set(
    d,
    j = "time",
    value = data.table::fifelse(
      d$play_description == 'GAME',
      "15:00",
      d$time
    )
  )
  # Create a column with the time in seconds remaining for the quarter:
  data.table::set(
    d,
    j = "quarter_seconds_remaining",
    value = time_to_seconds(d$time)
  )
  data.table::set(
    d,
    j = "play_description",
    value = data.table::fcase(
      stringi::stri_detect_regex(d$play_description, "(?<=kicks )[:alpha:]{1,}.[:alpha:]{1,}(?= yards)")                                       ,
      stringi::stri_replace_first_regex(d$play_description, "(?<=kicks )[:alpha:]{1,}.[:alpha:]{1,}(?= yards)", as.character(d$kick_distance)) ,
      default = d$play_description
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
  d <- d[play_order]

  # helper, dropped below
  data.table::set(d, j = "tmp_desc_lower", value = tolower(d$play_description))
  # Using the various two point indicators, create a column denoting the result
  # outcome for two point conversions:
  data.table::set(
    d,
    j = "two_point_conv_result",
    value = data.table::fifelse(
      (d$two_point_rush_good == 1 |
        d$two_point_pass_good == 1 |
        d$two_point_pass_reception_good == 1) &
        d$two_point_attempt == 1,
      "success",
      NA_character_
    )
  )
  data.table::set(
    d,
    j = "two_point_conv_result",
    value = data.table::fifelse(
      (d$two_point_rush_failed == 1 |
        d$two_point_pass_failed == 1 |
        d$two_point_pass_reception_failed == 1) &
        d$two_point_attempt == 1,
      "failure",
      d$two_point_conv_result
    )
  )
  data.table::set(
    d,
    j = "two_point_conv_result",
    value = data.table::fifelse(
      (d$two_point_rush_safety == 1 |
        d$two_point_pass_safety == 1) &
        d$two_point_attempt == 1,
      "safety",
      d$two_point_conv_result
    )
  )
  data.table::set(
    d,
    j = "two_point_conv_result",
    value = data.table::fifelse(
      d$two_point_return == 1 &
        d$two_point_attempt == 1,
      "return",
      d$two_point_conv_result
    )
  )
  # If the result was a success, make the yards_gained to be 2:
  data.table::set(
    d,
    j = "yards_gained",
    value = data.table::fifelse(
      !is.na(d$two_point_conv_result) &
        d$two_point_conv_result == "success",
      2,
      d$yards_gained
    )
  )
  # Fix yards_gained for plays with laterals
  data.table::set(
    d,
    j = "yards_gained",
    value = data.table::fcase(
      !is.na(d$passing_yards) & d$yards_gained != d$passing_yards & d$penalty == 0                                   ,
      d$passing_yards                                                                                                ,
      !is.na(d$rushing_yards) & !is.na(d$lateral_rushing_yards) & d$yards_gained != d$rushing_yards & d$penalty == 0 ,
      d$rushing_yards + d$lateral_rushing_yards                                                                      ,
      default = d$yards_gained
    )
  )
  # Extract the penalty type:
  data.table::set(
    d,
    j = "penalty_type",
    value = apply_where(
      d$penalty == 1,
      d$play_description,
      function(x) {
        x |>
          stringi::stri_extract_first_regex(
            "(?<=PENALTY on .{1,50}, ).{1,50}(?=, [0-9]{1,2} yard)"
          ) |>
          # Face Mask penalties include the yardage as string (either 5 Yards or 15 Yards)
          # We remove the 15 Yards part and just keep the additional info if it's a
          # 5 yard Face Mask penalty
          stringi::stri_replace_first_regex("\\([0-9]{2}+ Yards\\)", "") |>
          squish()
      }
    )
  )
  # The new "dynamic Kickoff" in the 2024 season introduces new penalty types
  data.table::set(
    d,
    j = "penalty_type",
    value = data.table::fifelse(
      d$penalty == 1 &
        stringi::stri_detect_regex(
          d$tmp_desc_lower,
          "kickoff short of landing zone"
        ),
      "Kickoff Short of Landing Zone",
      d$penalty_type
    )
  )
  data.table::set(
    d,
    j = "penalty_type",
    value = data.table::fifelse(
      d$penalty == 1 &
        stringi::stri_detect_regex(
          d$tmp_desc_lower,
          "kickoff out of bounds"
        ),
      "Kickoff Out of Bounds",
      d$penalty_type
    )
  )
  # Make plays marked with down == 0 as NA:
  data.table::set(
    d,
    j = "down",
    value = data.table::fifelse(
      d$down == 0,
      NA_real_,
      d$down
    )
  )
  # Using the field goal indicators make a column with the field goal result:
  data.table::set(
    d,
    j = "field_goal_result",
    value = data.table::fifelse(
      d$field_goal_attempt == 1 &
        d$field_goal_made == 1,
      "made",
      NA_character_
    )
  )
  data.table::set(
    d,
    j = "field_goal_result",
    value = data.table::fifelse(
      d$field_goal_attempt == 1 &
        d$field_goal_missed == 1,
      "missed",
      d$field_goal_result
    )
  )
  data.table::set(
    d,
    j = "field_goal_result",
    value = data.table::fifelse(
      d$field_goal_attempt == 1 &
        d$field_goal_blocked == 1,
      "blocked",
      d$field_goal_result
    )
  )
  # Using the indicators make a column with the extra point result:
  data.table::set(
    d,
    j = "extra_point_result",
    value = data.table::fifelse(
      d$extra_point_attempt == 1 &
        d$extra_point_good == 1,
      "good",
      NA_character_
    )
  )
  data.table::set(
    d,
    j = "extra_point_result",
    value = data.table::fifelse(
      d$extra_point_attempt == 1 &
        d$extra_point_failed == 1,
      "failed",
      d$extra_point_result
    )
  )
  data.table::set(
    d,
    j = "extra_point_result",
    value = data.table::fifelse(
      d$extra_point_attempt == 1 &
        d$extra_point_blocked == 1,
      "blocked",
      d$extra_point_result
    )
  )
  data.table::set(
    d,
    j = "extra_point_result",
    value = data.table::fifelse(
      d$extra_point_attempt == 1 &
        d$extra_point_safety == 1,
      "safety",
      d$extra_point_result
    )
  )
  data.table::set(
    d,
    j = "extra_point_result",
    value = data.table::fifelse(
      d$extra_point_attempt == 1 &
        d$extra_point_aborted == 1,
      "aborted",
      d$extra_point_result
    )
  )
  # find kickoffs with penalty: a play where the next play is a kickoff
  # and the prior play wasn't a safety or PAT
  d[,
    lead_ko := data.table::fifelse(
      data.table::shift(kickoff_attempt, type = "lead") == 1 &
        !stringi::stri_detect_regex(
          tmp_desc_lower,
          "(injured sf )|(tonight's attendance )|(injury update )|(end quarter)|(timeout)|( captains:)|( captains )|( captians:)|( humidity:)|(note - )|( deferred)|(game start )|( game has been suspended)"
        ) &
        !stringi::stri_detect_regex(play_description, "GAME ") &
        !play_description %in%
          c("GAME", "Two-Minute Warning", "The game has resumed.") &
        is.na(two_point_conv_result) &
        is.na(extra_point_result) &
        is.na(field_goal_result) &
        (safety == 0 | is.na(safety)) &
        # because things too messed up before
        season > 2000,
      1,
      0,
      na = 0
    ),
    by = "game_id"
  ]
  # we overwrite kickoff_attempt for kickoffs with penalties because
  # those mess with ep/epa/wp/wpa. Since this is inconsistent compared to
  # all other *_attempt variables, we will restore kickoff_attempt after
  # models are applied. That's done with a temporary copy of kickoff_attempt.
  # See #556, #202, #199 for example
  data.table::set(d, j = "copy_of_kickoff_attempt", value = d$kickoff_attempt)
  data.table::set(
    d,
    j = "kickoff_attempt",
    value = data.table::fifelse(
      d$lead_ko == 1,
      1,
      d$kickoff_attempt
    )
  )
  # https://github.com/nflverse/nflfastR/issues/199#issuecomment-792321171
  data.table::set(
    d,
    j = "kickoff_attempt",
    value = data.table::fifelse(
      d$game_id == "2014_02_ATL_CIN" & d$play_id == 3498,
      1,
      d$kickoff_attempt
    )
  )
  # Make the possession team for kickoffs be the return team, since that is
  # more intuitive from the EPA / WPA point of view:
  data.table::set(
    d,
    j = "posteam",
    value = data.table::fcase(
      # kickoff_finder is defined below
      (d$lead_ko == 1 | d$kickoff_attempt == 1 | stringi::stri_detect_regex(d$play_description, kickoff_finder)) & d$posteam == d$home_team ,
      d$away_team                                                                                                                           ,
      (d$lead_ko == 1 | d$kickoff_attempt == 1 | stringi::stri_detect_regex(d$play_description, kickoff_finder)) & d$posteam == d$away_team ,
      d$home_team                                                                                                                           ,
      default = d$posteam
    )
  )
  # Fill in the rows with missing posteam with the lead:
  d[,
    posteam := data.table::fifelse(
      (quarter_end == 1 | posteam == ""),
      data.table::shift(posteam, type = "lead"),
      posteam
    ),
    by = "game_id"
  ]
  d[,
    posteam_id := data.table::fifelse(
      (quarter_end == 1 | posteam_id == ""),
      data.table::shift(posteam_id, type = "lead"),
      posteam_id
    ),
    by = "game_id"
  ]
  # remove posteam from END Q2 plays or END Q4 plays (when game goes in OT)
  # because it doesn't make sense and breaks fixed_drive and fixed_drive_result
  data.table::set(
    d,
    j = "posteam",
    value = data.table::fifelse(
      stringi::stri_detect_regex(
        d$play_description,
        "(END QUARTER 2)|(END QUARTER 4)"
      ),
      NA_character_,
      d$posteam
    )
  )
  # Denote whether the home or away team has possession:
  data.table::set(
    d,
    j = "posteam_type",
    value = data.table::fifelse(
      d$posteam == d$home_team,
      "home",
      "away"
    )
  )
  # manual posteam adjustments for rare plays with issues related to game
  # delays.
  data.table::set(
    d,
    j = "posteam",
    value = data.table::fcase(
      # 2025_01_CAR_JAX, 1317: Game resumed after weather delay
      # AND it was delayed right after a PAT.
      # Prior two plays were delay info that shouldn't have posteam in order
      # to get correct fixed drive results #529
      # https://github.com/nflverse/nflfastR/issues/529
      d$game_id == "2025_01_CAR_JAX" & d$play_id %in% c(1282, 1303) ,
      NA_character_                                                 ,
      default = d$posteam
    )
  )
  # Column denoting which team is on defense:
  data.table::set(
    d,
    j = "defteam",
    value = data.table::fifelse(
      d$posteam == d$home_team,
      d$away_team,
      d$home_team
    )
  )
  data.table::set(
    d,
    j = "yardline",
    value = data.table::fifelse(
      stringi::stri_detect_regex(d$yardline, "50"),
      "MID 50",
      d$yardline
    )
  )
  d[,
    yardline := data.table::fifelse(
      nchar(yardline) == 0 | yardline == "NULL" | is.na(yardline),
      data.table::shift(yardline, type = "lead"),
      yardline
    ),
    by = "game_id"
  ]
  data.table::set(
    d,
    j = "yardline_number",
    value = data.table::fifelse(
      d$yardline == "MID 50",
      50,
      d$yardline_number
    )
  )
  data.table::set(
    d,
    j = "yardline_100",
    value = data.table::fifelse(
      d$yardline_side == d$posteam | d$yardline == "MID 50",
      100 - d$yardline_number,
      d$yardline_number
    )
  )
  # Set the kick_distance for extra points by adding 18 to the yardline_100:
  data.table::set(
    d,
    j = "kick_distance",
    value = data.table::fifelse(
      d$extra_point_attempt == 1,
      d$yardline_100 + 18,
      d$kick_distance
    )
  )
  # Create a column with the time in seconds remaining for each half:
  data.table::set(
    d,
    j = "half_seconds_remaining",
    value = data.table::fifelse(
      d$quarter %in% c(1, 3),
      d$quarter_seconds_remaining + 900,
      d$quarter_seconds_remaining
    )
  )
  # Create a column with the time in seconds remaining for the game:
  data.table::set(
    d,
    j = "game_seconds_remaining",
    value = data.table::fifelse(
      d$quarter %in% c(1, 2, 3, 4),
      d$quarter_seconds_remaining +
        (900 * (4 - as.numeric(d$quarter))),
      d$quarter_seconds_remaining
    )
  )
  # Add column for replay or challenge:
  data.table::set(
    d,
    j = "replay_or_challenge",
    value = stringi::stri_detect_regex(
      d$play_description,
      "(Replay Official reviewed)|( challenge(d)? )|(Challenged)"
    ) |>
      as.numeric()
  )
  # Result of replay or challenge:
  data.table::set(
    d,
    j = "replay_or_challenge_result",
    value = data.table::fifelse(
      d$replay_or_challenge == 1,
      data.table::fifelse(
        stringi::stri_detect_regex(
          d$tmp_desc_lower,
          "( upheld)|( reversed)|( confirmed)"
        ),
        stringi::stri_extract_first_regex(
          d$tmp_desc_lower,
          "( upheld)|( reversed)|( confirmed)"
        ) |>
          stringi::stri_trim_both(),
        "denied"
      ),
      NA_character_
    )
  )
  # Create the column denoting the categorical description of the pass length:
  data.table::set(
    d,
    j = "pass_length",
    value = apply_where(
      d$two_point_attempt == 0 &
        d$sack == 0 &
        d$pass_attempt == 1,
      d$play_description,
      function(x) {
        x |>
          stringi::stri_extract_first_regex(
            "pass (incomplete )?(short|deep)"
          ) |>
          stringi::stri_extract_first_regex("short|deep")
      }
    )
  )
  # Create the column denoting the categorical location of the pass:
  data.table::set(
    d,
    j = "pass_location",
    value = apply_where(
      d$two_point_attempt == 0 &
        d$sack == 0 &
        d$pass_attempt == 1,
      d$play_description,
      function(x) {
        x |>
          stringi::stri_extract_first_regex(
            "(short|deep) (left|middle|right)"
          ) |>
          stringi::stri_extract_first_regex("left|middle|right")
      }
    )
  )
  # Indicator columns for both QB kneels, spikes, scrambles,
  # no huddle, shotgun plays:
  data.table::set(
    d,
    j = "qb_kneel",
    value = data.table::fifelse(
      stringi::stri_detect_regex(d$play_description, " kneels ") &
        d$kickoff_attempt != 1,
      1,
      0
    )
  )
  data.table::set(
    d,
    j = "qb_spike",
    value = stringi::stri_detect_regex(d$play_description, " spiked ") |>
      as.numeric()
  )
  data.table::set(
    d,
    j = "qb_scramble",
    value = stringi::stri_detect_regex(
      d$play_description,
      " scrambles "
    ) |>
      as.numeric()
  )
  data.table::set(
    d,
    j = "shotgun",
    value = stringi::stri_detect_regex(d$play_description, "Shotgun") |>
      as.numeric()
  )
  data.table::set(
    d,
    j = "no_huddle",
    value = stringi::stri_detect_regex(d$play_description, "No Huddle") |>
      as.numeric()
  )
  # Create a play type column: either pass, run, field_goal, extra_point,
  # kickoff, punt, qb_kneel, qb_spike, or no_play (which includes timeouts and
  # penalties):
  data.table::set(
    d,
    j = "play_type",
    value = translate_play_type_nfl(
      d$play_type_nfl,
      qb_spike = d$qb_spike,
      qb_kneel = d$qb_kneel,
      pass_attempt = d$pass_attempt,
      rush_attempt = d$rush_attempt,
      punt_attempt = d$punt_attempt,
      field_goal_attempt = d$field_goal_attempt,
      penalty = d$penalty,
      is_penalty_enforced_between_downs = stringi::stri_detect_regex(
        d$tmp_desc_lower,
        "enforced between downs"
      )
    )
  )
  # Indicator for QB dropbacks (exclude spikes and kneels):
  data.table::set(
    d,
    j = "qb_dropback",
    value = data.table::fifelse(
      d$play_type == "pass" |
        (d$play_type == "run" &
          d$qb_scramble == 1),
      1,
      0
    )
  )
  # Columns denoting the run location and gap:
  data.table::set(
    d,
    j = "run_location",
    value = apply_where(
      d$two_point_attempt == 0 &
        d$rush_attempt == 1,
      d$play_description,
      function(x) {
        x |>
          stringi::stri_extract_first_regex(" (left|middle|right) ") |>
          stringi::stri_trim_both()
      }
    )
  )
  data.table::set(
    d,
    j = "run_gap",
    value = apply_where(
      d$two_point_attempt == 0 &
        d$rush_attempt == 1,
      d$play_description,
      function(x) {
        x |>
          stringi::stri_extract_first_regex(" (guard|tackle|end) ") |>
          stringi::stri_trim_both()
      }
    )
  )
  data.table::set(
    d,
    j = "game_half",
    value = data.table::fcase(
      d$quarter %in% c(1, 2) ,
      "Half1"                ,
      d$quarter %in% c(3, 4) ,
      "Half2"                ,
      d$quarter >= 5         ,
      "Overtime"             ,
      FALSE                  ,
      NA_character_
    )
  )
  # Create columns to denote the timeouts remaining for each team, making
  # columns for both home/away and pos/def (this will involve creating
  # temporary columns that will not be included):
  # Initialize both home and away to have 3 timeouts for each
  # half except overtime where they have 2:
  # extract timeouts from failed challenges when it's not otherwise there
  data.table::set(
    d,
    j = "tmp_timeout",
    value = stringi::stri_extract_first_regex(
      d$play_description,
      "(?<=by\\s)[:upper:]{2,3}(?=\\s)"
    )
  )
  data.table::set(
    d,
    j = "timeout_team",
    value = data.table::fifelse(
      d$replay_or_challenge == 1 &
        d$timeout == 1 &
        is.na(d$timeout_team),
      d$tmp_timeout,
      d$timeout_team
    )
  )
  data.table::set(
    d,
    j = "home_timeouts_remaining",
    value = data.table::fifelse(
      d$quarter %in% c(1, 2, 3, 4),
      3,
      2
    )
  )
  data.table::set(
    d,
    j = "away_timeouts_remaining",
    value = data.table::fifelse(
      d$quarter %in% c(1, 2, 3, 4),
      3,
      2
    )
  )
  data.table::set(
    d,
    j = "home_timeout_used",
    value = data.table::fifelse(
      d$timeout == 1 &
        d$timeout_team == d$home_team,
      1,
      0
    )
  )
  data.table::set(
    d,
    j = "away_timeout_used",
    value = data.table::fifelse(
      d$timeout == 1 &
        d$timeout_team == d$away_team,
      1,
      0
    )
  )
  data.table::set(
    d,
    j = "home_timeout_used",
    value = data.table::fifelse(
      is.na(d$home_timeout_used),
      0,
      d$home_timeout_used
    )
  )
  data.table::set(
    d,
    j = "away_timeout_used",
    value = data.table::fifelse(
      is.na(d$away_timeout_used),
      0,
      d$away_timeout_used
    )
  )

  # replace empty strings in yard line variables
  # fix cases where a yardline variable misses the blank space between team name
  # and yard number. At the point of adding this, the only spot where this happened
  # was in the variable drive_start_yard_line in the games
  # "2000_01_CAR_WAS", "2000_02_NE_NYJ", and "2000_03_ATL_CAR"
  for (col in c("yardline", "drive_start_yard_line", "drive_end_yard_line")) {
    x <- d[[col]]
    x[!is.na(x) & x == ""] <- NA
    data.table::set(
      d,
      j = col,
      value = data.table::fcase(
        stringi::stri_detect_regex(x, "[:upper:]{2,3}(?=[:digit:]{1,2})")                                                                          ,
        stringi::stri_c(stringi::stri_extract_first_regex(x, "[:upper:]{2,3}"), stringi::stri_extract_first_regex(x, "[:digit:]{1,2}"), sep = " ") ,
        default = x
      )
    )
  }

  # Create cumulative timeouts used for both the home and away teams per game
  # half:
  d[,
    c("total_home_timeouts_used", "total_away_timeouts_used") := list(
      data.table::fifelse(
        cumsum(home_timeout_used) > 3,
        3,
        cumsum(home_timeout_used)
      ),
      data.table::fifelse(
        cumsum(away_timeout_used) > 3,
        3,
        cumsum(away_timeout_used)
      )
    ),
    by = c("game_id", "game_half")
  ]

  # Now just take the difference between the timeouts remaining
  # columns and the total timeouts used, and create the columns for both
  # the pos and def team timeouts remaining:
  data.table::set(
    d,
    j = "home_timeouts_remaining",
    value = d$home_timeouts_remaining -
      d$total_home_timeouts_used
  )
  data.table::set(
    d,
    j = "away_timeouts_remaining",
    value = d$away_timeouts_remaining -
      d$total_away_timeouts_used
  )
  data.table::set(
    d,
    j = "posteam_timeouts_remaining",
    value = data.table::fifelse(
      d$posteam == d$home_team,
      d$home_timeouts_remaining,
      d$away_timeouts_remaining
    )
  )
  data.table::set(
    d,
    j = "defteam_timeouts_remaining",
    value = data.table::fifelse(
      d$defteam == d$home_team,
      d$home_timeouts_remaining,
      d$away_timeouts_remaining
    )
  )
  # Same type of logic to calculate the score for each team and the score
  # differential in the game. First create columns to track how many points
  # were scored on a particular play based on various scoring indicators for
  # both the home and away teams:
  data.table::set(
    d,
    j = "home_points_scored",
    value = data.table::fifelse(
      d$touchdown == 1 &
        d$td_team == d$home_team,
      6,
      0
    )
  )
  data.table::set(
    d,
    j = "home_points_scored",
    value = data.table::fifelse(
      d$posteam == d$home_team &
        d$field_goal_made == 1,
      3,
      d$home_points_scored
    )
  )
  data.table::set(
    d,
    j = "home_points_scored",
    value = data.table::fifelse(
      d$posteam == d$home_team &
        (d$extra_point_good == 1 |
          d$extra_point_safety == 1 |
          d$two_point_rush_safety == 1 |
          d$two_point_pass_safety == 1),
      1,
      d$home_points_scored
    )
  )
  data.table::set(
    d,
    j = "home_points_scored",
    value = data.table::fifelse(
      d$posteam == d$home_team &
        (d$two_point_rush_good == 1 |
          d$two_point_pass_good == 1 |
          d$two_point_pass_reception_good == 1),
      2,
      d$home_points_scored
    )
  )
  data.table::set(
    d,
    j = "home_points_scored",
    value = data.table::fifelse(
      d$defteam == d$home_team &
        (d$two_point_return == 1 | d$defensive_two_point_conv == 1),
      2,
      d$home_points_scored
    )
  )
  data.table::set(
    d,
    j = "home_points_scored",
    value = data.table::fifelse(
      d$safety_team == d$home_team & d$safety == 1,
      2,
      d$home_points_scored
    )
  )
  data.table::set(
    d,
    j = "away_points_scored",
    value = data.table::fifelse(
      d$touchdown == 1 &
        d$td_team == d$away_team,
      6,
      0
    )
  )
  data.table::set(
    d,
    j = "away_points_scored",
    value = data.table::fifelse(
      d$posteam == d$away_team &
        d$field_goal_made == 1,
      3,
      d$away_points_scored
    )
  )
  data.table::set(
    d,
    j = "away_points_scored",
    value = data.table::fifelse(
      d$posteam == d$away_team &
        (d$extra_point_good == 1 |
          d$extra_point_safety == 1 |
          d$two_point_rush_safety == 1 |
          d$two_point_pass_safety == 1),
      1,
      d$away_points_scored
    )
  )
  data.table::set(
    d,
    j = "away_points_scored",
    value = data.table::fifelse(
      d$posteam == d$away_team &
        (d$two_point_rush_good == 1 |
          d$two_point_pass_good == 1 |
          d$two_point_pass_reception_good == 1),
      2,
      d$away_points_scored
    )
  )
  data.table::set(
    d,
    j = "away_points_scored",
    value = data.table::fifelse(
      d$defteam == d$away_team &
        (d$two_point_return == 1 | d$defensive_two_point_conv == 1),
      2,
      d$away_points_scored
    )
  )
  data.table::set(
    d,
    j = "away_points_scored",
    value = data.table::fifelse(
      d$safety_team == d$away_team & d$safety == 1,
      2,
      d$away_points_scored
    )
  )
  data.table::set(
    d,
    j = "home_points_scored",
    value = data.table::fifelse(
      is.na(d$home_points_scored),
      0,
      d$home_points_scored
    )
  )
  data.table::set(
    d,
    j = "away_points_scored",
    value = data.table::fifelse(
      is.na(d$away_points_scored),
      0,
      d$away_points_scored
    )
  )
  # Now create cumulative totals:
  d[, total_home_score := cumsum(home_points_scored), by = "game_id"]
  d[, total_away_score := cumsum(away_points_scored), by = "game_id"]
  d[,
    posteam_score := data.table::fifelse(
      posteam == home_team,
      data.table::shift(total_home_score),
      data.table::shift(total_away_score)
    ),
    by = "game_id"
  ]
  d[,
    defteam_score := data.table::fifelse(
      defteam == home_team,
      data.table::shift(total_home_score),
      data.table::shift(total_away_score)
    ),
    by = "game_id"
  ]
  data.table::set(
    d,
    j = "score_differential",
    value = d$posteam_score - d$defteam_score
  )
  data.table::set(
    d,
    j = "abs_score_differential",
    value = abs(d$score_differential)
  )
  # Make post score differential columns to be used for the final
  # game indicators in the win probability calculations:
  data.table::set(
    d,
    j = "posteam_score_post",
    value = data.table::fifelse(
      d$posteam == d$home_team,
      d$total_home_score,
      d$total_away_score
    )
  )
  data.table::set(
    d,
    j = "defteam_score_post",
    value = data.table::fifelse(
      d$defteam == d$home_team,
      d$total_home_score,
      d$total_away_score
    )
  )
  data.table::set(
    d,
    j = "score_differential_post",
    value = d$posteam_score_post -
      d$defteam_score_post
  )
  data.table::set(
    d,
    j = "abs_score_differential_post",
    value = abs(
      d$posteam_score_post - d$defteam_score_post
    )
  )
  # Create a variable for whether or not a touchback occurred, this
  # will apply to any type of play:
  data.table::set(
    d,
    j = "touchback",
    value = as.numeric(stringi::stri_detect_regex(
      d$tmp_desc_lower,
      "touchback"
    ))
  )
  # There are a few plays with air_yards prior 2006 (most likely accidently)
  # To not crash the air_yac ep and wp calculation they are being set to NA
  data.table::set(
    d,
    j = "air_yards",
    value = data.table::fifelse(d$season < 2006, NA_real_, d$air_yards)
  )

  data.table::setnames(
    d,
    c(
      "yards_to_go",
      "play_description",
      "yardline",
      "yardline_side",
      "quarter"
    ),
    c("ydstogo", "desc", "yrdln", "side_of_field", "qtr")
  )
  d <- d[!is.na(desc) & desc != "" & !is.na(qtr)]

  data.table::set(d, j = "game_id", value = as.character(d$game_id))
  # kick distance is NA on kickoffs and punts that result in touchbacks
  # (unless the kick/punt) was caught between endzones
  # we use yardline_100 to add it in those cases
  data.table::set(
    d,
    j = "is_relevant_touchback",
    value = as.numeric(
      is.na(d$kick_distance) &
        d$touchback == 1 &
        d$play_type %in% c("punt", "kickoff")
    )
  )
  data.table::set(
    d,
    j = "kick_distance",
    value = data.table::fcase(
      d$is_relevant_touchback == 1 & d$kickoff_attempt == 0 ,
      d$yardline_100                                        ,
      # gotta reverse yardline_100 on kickoffs
      d$is_relevant_touchback == 1 & d$kickoff_attempt == 1 ,
      100 - d$yardline_100                                  ,
      default = d$kick_distance
    )
  )
  # drop helper variables
  data.table::set(d, j = "is_relevant_touchback", value = NULL)
  data.table::set(d, j = "tmp_desc_lower", value = NULL)

  d <- fix_scrambles(d)
  d <- make_model_mutations(d)

  user_message("added nflscrapR variables", "done")
  d
}

# to help find kickoffs on plays with penalties
# otherwise win prob breaks down the road
kickoff_finder <- "(Offside on Free Kick)|(Delay of Kickoff)|(Onside Kick formation)|(kicks onside)|( kicks [:digit:]+ yards from)"


##some steps to prepare the data for the EP/WP/CP/FG models
make_model_mutations <- function(pbp) {
  #for EP, CP, and WP model, xgb needs 0/1 for eras
  era0 <- data.table::fifelse(pbp$season <= 2001, 1, 0)
  era1 <- data.table::fifelse(pbp$season > 2001 & pbp$season <= 2005, 1, 0)
  era2 <- data.table::fifelse(pbp$season > 2005 & pbp$season <= 2013, 1, 0)
  era3 <- data.table::fifelse(pbp$season > 2013 & pbp$season <= 2017, 1, 0)
  era4 <- data.table::fifelse(pbp$season > 2017, 1, 0)
  model_roof <- as.factor(data.table::fifelse(
    is.na(pbp$roof) | pbp$roof == "open" | pbp$roof == "closed",
    "retractable",
    as.character(pbp$roof)
  ))
  cols <- list(
    era0 = era0,
    era1 = era1,
    era2 = era2,
    era3 = era3,
    era4 = era4,
    #for fg model, an era factor
    era = as.factor(data.table::fcase(
      era0 == 1             ,
                          0 ,
      era1 == 1             ,
                          1 ,
      era2 == 1             ,
                          2 ,
      era3 == 1 | era4 == 1 ,
                          3
    )),
    down1 = data.table::fifelse(pbp$down == 1, 1, 0),
    down2 = data.table::fifelse(pbp$down == 2, 1, 0),
    down3 = data.table::fifelse(pbp$down == 3, 1, 0),
    down4 = data.table::fifelse(pbp$down == 4, 1, 0),
    home = data.table::fifelse(pbp$posteam == pbp$home_team, 1, 0),
    model_roof = model_roof,
    retractable = data.table::fifelse(model_roof == "retractable", 1, 0),
    dome = data.table::fifelse(model_roof == "dome", 1, 0),
    outdoors = data.table::fifelse(model_roof == "outdoors", 1, 0)
  )
  if (data.table::is.data.table(pbp)) {
    data.table::set(pbp, j = names(cols), value = cols)
  } else {
    pbp[names(cols)] <- cols
  }
  pbp
}


fix_scrambles <- function(pbp) {
  # skip below code if <= 2005 is not in the data
  if (min(pbp$season) > 2005) {
    return(pbp)
  }

  scramble_id <- paste0(pbp$game_id, "_", pbp$play_id)
  data.table::set(
    pbp,
    j = "qb_scramble",
    value = data.table::fifelse(
      scramble_id %in% scramble_fix,
      1,
      pbp$qb_scramble
    )
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

  out <- data.table::fcase(
    x == "COMMENT"                                                         ,
    NA_character_                                                          ,
    x == "END_GAME"                                                        ,
    NA_character_                                                          ,
    x == "END_QUARTER"                                                     ,
    NA_character_                                                          ,
    x == "FIELD_GOAL"                                                      ,
    "field_goal"                                                           ,
    x == "FREE_KICK"                                                       ,
    "kickoff"                                                              ,
    x == "GAME_START"                                                      ,
    NA_character_                                                          ,
    x == "INTERCEPTION"                                                    ,
    "pass"                                                                 ,
    x == "KICK_OFF"                                                        ,
    "kickoff"                                                              ,
    x == "PASS"                                                            ,
    "pass"                                                                 ,
    x == "PAT2" & pass_attempt == 1                                        ,
    "pass"                                                                 ,
    x == "PAT2" & rush_attempt == 1                                        ,
    "run"                                                                  ,
    x == "PENALTY" & pass_attempt == 1 & is_penalty_enforced_between_downs ,
    "pass"                                                                 ,
    x == "PENALTY" & rush_attempt == 1 & is_penalty_enforced_between_downs ,
    "run"                                                                  ,
    x == "PENALTY"                                                         ,
    "no_play"                                                              ,
    x == "PUNT"                                                            ,
    "punt"                                                                 ,
    x == "RUSH"                                                            ,
    "run"                                                                  ,
    x == "SACK"                                                            ,
    "pass"                                                                 ,
    x == "TIMEOUT"                                                         ,
    "no_play"                                                              ,
    x == "XP_KICK"                                                         ,
    "extra_point"                                                          ,
    # UNSPECIFIED is a mix of all sorts of weird plays
    x == "UNSPECIFIED" & penalty == 1                                      ,
    "no_play"                                                              ,
    # the following lines imply penalty == 0 because penalty == 1 triggers above
    x == "UNSPECIFIED" & pass_attempt == 1                                 ,
    "pass"                                                                 ,
    x == "UNSPECIFIED" & rush_attempt == 1                                 ,
    "run"                                                                  ,
    x == "UNSPECIFIED" & punt_attempt == 1                                 ,
    "punt"                                                                 ,
    x == "UNSPECIFIED" & field_goal_attempt == 1                           ,
    "field_goal"                                                           ,
    # most of the remaining UNSPECIFIED plays will be declined penalties
    # from punt or fg formation. These don't really count as play so we define
    # them as no_play
    x == "UNSPECIFIED"                                                     ,
    "no_play"                                                              ,
    # default
    default = ""
  )

  # every play_type_nfl that we do not catch in the above cases
  # will be an empty string. We try to resolve these as good as we can
  # also need to replace passes and runs that were spikes and kneel downs
  data.table::fcase(
    out == "" & penalty == 1                  ,
    "no_play"                                 ,
    out == "" & pass_attempt == 1             ,
    "pass"                                    ,
    out == "" & rush_attempt == 1             ,
    "run"                                     ,
    out == "" & punt_attempt == 1             ,
    "punt"                                    ,
    out == "" & field_goal_attempt == 1       ,
    "field_goal"                              ,
    qb_spike == 1 & out %in% c("pass", "run") ,
    "qb_spike"                                ,
    qb_kneel == 1 & out %in% c("pass", "run") ,
    "qb_kneel"                                ,
    default = out
  )
}

# we overwrite kickoff_attempt for kickoffs with penalties because
# those mess with ep/epa/wp/wpa. Since this is inconsistent compared to
# all other *_attempt variables, we will restore kickoff_attempt after
# models are applied. That's done with a temporary copy of kickoff_attempt.
# See #556, #202, #199 for example
restore_kickoff_attempt <- function(pbp) {
  data.table::set(
    pbp,
    j = "kickoff_attempt",
    value = pbp$copy_of_kickoff_attempt
  )
  data.table::set(pbp, j = "copy_of_kickoff_attempt", value = NULL)
  pbp
}
