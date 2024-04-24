################################################################################
# Author: Matthew Cooper
# Styleguide: styler::tidyverse_style()
################################################################################

#' Get Official Game Stats
#'
#' @param pbp A Data frame of NFL play-by-play data typically loaded with
#' [load_pbp()] or [build_nflfastR_pbp()]. If the data doesn't include the variable
#' `qb_epa`, the function `add_qb_epa()` will be called to add it.
#' @param weekly If `TRUE`, returns week-by-week stats, otherwise, stats
#' for the entire Data frame.
#' @description Build columns that aggregates a team's official passing, rushing, receiving,
#' and per-drive stats either at the game level or at the level of the entire data frame passed.
#' @return A data frame including the following columns:
#' \describe{
#' \item{team}{Name of the team. Use this to join to other sources.}
#' \item{season}{Season if `weekly` is `TRUE`}
#' \item{week}{Week if `weekly` is `TRUE`}
#' \item{season_type}{`REG` or `POST` if `weekly` is `TRUE`}
#' \item{opponent_team}{The team's opponent team if `weekly` is `TRUE`}
#' \item{completions}{The number of completed passes.}
#' \item{attempts}{The number of pass attempts as defined by the NFL.}
#' \item{passing_yards}{Yards gained on pass plays.}
#' \item{passing_tds}{The number of passing touchdowns.}
#' \item{interceptions}{The number of interceptions thrown.}
#' \item{sacks}{The Number of times sacked.}
#' \item{sack_yards}{Yards lost on sack plays.}
#' \item{sack_fumbles}{The number of sacks with a fumble.}
#' \item{sack_fumbles_lost}{The number of sacks with a lost fumble.}
#' \item{passing_air_yards}{Passing air yards (includes incomplete passes).}
#' \item{passing_yards_after_catch}{Yards after the catch gained on pass
#' plays (this is an unofficial stat and may differ slightly between different sources).}
#' \item{passing_first_downs}{First downs on pass attempts.}
#' \item{passing_epa}{Total expected points added on pass attempts and sacks.
#' NOTE: this uses the variable `qb_epa`, which gives QB credit for EPA for up
#' to the point where a receiver lost a fumble after a completed catch and makes
#' EPA work more like passing yards on plays with fumbles.}
#' \item{passing_2pt_conversions}{Two-point conversion passes.}
#' \item{pacr}{Passing Air Conversion Ratio. PACR = `passing_yards` / `passing_air_yards`}
#' \item{carries}{The number of official rush attempts (incl. scrambles and kneel downs).
#' Rushes after a lateral reception don't count as carry.}
#' \item{rushing_yards}{Yards gained when rushing with the ball (incl. scrambles and kneel downs).
#' Also includes yards gained after obtaining a lateral on a play that started
#' with a rushing attempt.}
#' \item{rushing_tds}{The number of rushing touchdowns (incl. scrambles).
#' Also includes touchdowns after obtaining a lateral on a play that started
#' with a rushing attempt.}
#' \item{rushing_fumbles}{The number of rushes with a fumble.}
#' \item{rushing_fumbles_lost}{The number of rushes with a lost fumble.}
#' \item{rushing_first_downs}{First downs on rush attempts (incl. scrambles).}
#' \item{rushing_epa}{Expected points added on rush attempts (incl. scrambles and kneel downs).}
#' \item{rushing_2pt_conversions}{Two-point conversion rushes}
#' \item{receiving_yards}{Yards gained after a pass reception. Includes yards
#' gained after receiving a lateral on a play that started as a pass play.}
#' \item{receiving_tds}{The number of touchdowns following a pass reception.
#' Also includes touchdowns after receiving a lateral on a play that started
#' as a pass play.}
#' \item{receiving_air_yards}{Receiving air yards (incl. incomplete passes).}
#' \item{receiving_yards_after_catch}{Yards after the catch gained on pass plays
#' by the team's receiver(s) (this is an unofficial stat and may differ slightly
#' between different sources).}
#' \item{receiving_fumbles}{The number of fumbles after a pass reception.}
#' \item{receiving_fumbles_lost}{The number of fumbles lost after a pass reception.}
#' \item{receiving_2pt_conversions}{Two-point conversion receptions}
#' \item{special_teams_tds}{The total number of touchdowns scored on special teams.}
#' \item{yards_per_drive}{The average number of yards a team gained on each drive at
#' the game level if `weekly` = `TRUE` or at the level of the entire data frame passed.}
#' \item{plays_per_drive}{The average number of plays a team ran on each drive at
#' the game level if `weekly` = `TRUE` or at the level of the entire data frame passed.}
#' \item{scoring_drive_percentage}{The percentage of drives that ended with an offensive score
#' at the game level if `weekly` = `TRUE` or at the level of the entire data frame passed.}
#' \item{inside20_percentage}{The percentage of drives in which the team was able to get inside
#' the opponents 20 yard line while on offense at the game level if `weekly` = `TRUE`}
#' or at the level of the entire data frame passed.}
#' \item{penalty_yards_per_drive}{The average number of yards the offense gained
#' or lost through penalties at the game level if `weekly` = `TRUE` or at the
#' level of the entire data frame passed.}
#' \item{points_per_drive}{The average number of offensive (non special teams) points scored
#' by the team on their offensive drives at the game level if `weekly` = `TRUE` or at the
#' level of the entire data frame passed.}
#' \item{drive_turnover_percentage}{The average number of offensive drives ending in a
#' turnover for the team either at the game level if `weekly` = `TRUE` or at the level
#' of the entire data frame passed.}
#' \item{passing_fantasy_points}{Total standard passing fantasy points.}
#' \item{rushing_fantasy_points}{Total standard rushing fantasy points.}
#' \item{receiving_fantasy_points}{Total standard receiving fantasy points.}
#' \item{receiving_fantasy_points_ppr}{Total ppr receiving fantasy points.}
#' @export

calculate_team_stats <- function(pbp, weekly = FALSE) {
  # Prepare data ------------------------------------------------------------

  # filter down to the 2 dfs we need
  suppressMessages({
    # 1. for "normal" plays: get plays that count in official stats
    data <- pbp %>%
      dplyr::filter(
        !is.na(.data$down),
        .data$play_type %in% c("pass", "qb_kneel", "qb_spike", "run")
      )

    if (!"qb_epa" %in% names(data)) data <- add_qb_epa(data)

    # 2. for 2pt conversions only, get those plays
    two_points <- pbp %>%
      dplyr::filter(.data$two_point_conv_result == "success") %>%
      dplyr::select(
        "week", "season", "posteam", "defteam",
        "pass_attempt", "rush_attempt"
      )
  })
  
  if (!"special" %in% names(pbp)) {
    # we need this column for the special teams tds
    pbp <- pbp %>%
      dplyr::mutate(
        special = dplyr::if_else(
          .data$play_type %in% c("extra_point","field_goal","kickoff","punt"),
          1, 0
        )
      )
  }
  
  s_type <- pbp %>%
    dplyr::select("season", "season_type", "week") %>%
    dplyr::distinct()

  # Passing stats -----------------------------------------------------------
  
  # Get passing stats (grouped by team instead of passer_player_name)
  pass_df <- data %>%
    dplyr::filter(.data$play_type %in% c("pass", "qb_spike")) %>%
    dplyr::group_by(.data$posteam, .data$week, .data$season) %>%
    dplyr::summarize(
      passing_yards_after_catch = sum((.data$passing_yards - .data$air_yards) * .data$complete_pass, na.rm = TRUE),
      opp_pass = dplyr::first(.data$defteam),
      passing_yards = sum(.data$passing_yards, na.rm = TRUE),
      passing_tds = sum(.data$touchdown == 1 & .data$td_team == .data$posteam & .data$complete_pass == 1),
      interceptions = sum(.data$interception),
      attempts = sum(.data$complete_pass == 1 | .data$incomplete_pass == 1 | .data$interception == 1),
      completions = sum(.data$complete_pass == 1),
      sack_fumbles = sum(.data$fumble == 1 & .data$fumbled_1_player_id == .data$passer_player_id),
      sack_fumbles_lost = sum(.data$fumble_lost == 1 & .data$fumbled_1_player_id == .data$passer_player_id & .data$fumble_recovery_1_team != .data$posteam),
      passing_air_yards = sum(.data$air_yards, na.rm = TRUE),
      sacks = sum(.data$sack),
      sack_yards = -1*sum(.data$yards_gained * .data$sack),
      passing_first_downs = sum(.data$first_down_pass),
      passing_epa = sum(.data$qb_epa, na.rm = TRUE),
      pacr = .data$passing_yards / .data$passing_air_yards,
      pacr = dplyr::case_when(
        is.nan(.data$pacr) ~ NA_real_,
        .data$passing_air_yards <= 0 ~ 0,
        TRUE ~ .data$pacr
      ),
    ) %>%
    dplyr::ungroup()
  
  
  # Add 2 point conversion passing stats
  pass_two_points <- two_points %>%
    dplyr::filter(.data$pass_attempt == 1) %>%
    dplyr::group_by(.data$posteam, .data$week, .data$season) %>%
    dplyr::summarise(
      opp_pass = custom_mode(.data$defteam),
      passing_2pt_conversions = dplyr::n()
    ) %>%
    dplyr::ungroup()

  
  pass_df <- pass_df %>%
    # Full join passing stats and 2pt conversion passing stats
    dplyr::full_join(pass_two_points, by = c("posteam", "week", "season", "opp_pass")) %>%
    dplyr::mutate(passing_2pt_conversions = dplyr::if_else(is.na(.data$passing_2pt_conversions), 0L, .data$passing_2pt_conversions)) %>%
    dplyr::filter(!is.na(.data$posteam))
  
  
  pass_df_nas <- is.na(pass_df)
  epa_index <- which(dimnames(pass_df_nas)[[2]] %in% c("passing_epa", "pacr"))
  pass_df_nas[,epa_index] <- c(FALSE)
  
  pass_df[pass_df_nas] <- 0
  
  # Rushing stats -----------------------------------------------------------
  rushes <- data %>%
    dplyr::filter(.data$play_type %in% c("run", "qb_kneel")) %>%
    dplyr::group_by(.data$posteam, .data$week, .data$season) %>%
    dplyr::summarize(
      opp_rush = dplyr::first(.data$defteam),
      rushing_yards = sum(.data$rushing_yards, .data$lateral_rushing_yards, na.rm = TRUE),
      rushing_tds = sum(.data$td_player_id == .data$rusher_player_id, .data$td_player_id == .data$lateral_rusher_player_id, na.rm = TRUE),
      carries = dplyr::n(),
      # Since we are grouping by team, no longer need to filter out plays with laterals for fumbles and first downs
      rushing_fumbles = sum(.data$fumble == 1),
      rushing_fumbles_lost = sum(.data$fumble_lost == 1 & .data$fumble_recovery_1_team != .data$posteam),
      rushing_first_downs = sum(.data$first_down_rush == 1),
      rushing_epa = sum(.data$epa, na.rm = TRUE)
    ) %>%
    dplyr::ungroup()
  
  rush_two_points <- two_points %>%
    dplyr::filter(.data$rush_attempt == 1) %>%
    dplyr::group_by(.data$posteam, .data$week, .data$season) %>%
    dplyr::summarise(
      opp_rush = custom_mode(.data$defteam),
      rushing_2pt_conversions = dplyr::n()
    ) %>%
    dplyr::ungroup()
  
  rush_df <- rushes %>%
    dplyr::full_join(rush_two_points, by = c("posteam", "week", "season", "opp_rush")) %>%
    dplyr::mutate(rushing_2pt_conversions = dplyr::if_else(is.na(.data$rushing_2pt_conversions), 0L, .data$rushing_2pt_conversions)) %>%
    dplyr::filter(!is.na(.data$posteam))
  
  rush_df_nas <- is.na(rush_df)
  epa_index <- which(dimnames(rush_df_nas)[[2]] == 'rushing_epa')
  rush_df_nas[,epa_index] <- c(FALSE)
  
  rush_df[rush_df_nas] <- 0

  # Receiving stats -----------------------------------------------------------
  
  # Receiving stats
  rec <- data %>%
    dplyr::filter(!is.na(.data$receiver_player_id)) %>%
    dplyr::group_by(.data$posteam, .data$week, .data$season) %>%
    dplyr::summarize(
      opp_receiver = dplyr::first(.data$defteam),
      receiving_yards = sum(.data$receiving_yards, .data$lateral_receiving_yards, na.rm = TRUE),
      receptions = sum(.data$complete_pass == 1),
      receiving_tds = sum(.data$td_player_id == .data$receiver_player_id, .data$td_player_id == .data$lateral_receiver_player_id, na.rm = TRUE),
      receiving_fumbles = sum(.data$fumble == 1),
      receiving_fumbles_lost = sum(.data$fumble_lost == 1 & .data$fumble_recovery_1_team != .data$posteam),
      receiving_air_yards = sum(.data$air_yards, na.rm = TRUE),
      receiving_yards_after_catch = sum(.data$yards_after_catch, na.rm = TRUE),
      receiving_first_downs = sum(.data$first_down_pass)
    ) %>%
    dplyr::ungroup()
  
  rec_two_points <- two_points %>%
    dplyr::filter(.data$pass_attempt == 1) %>%
    dplyr::group_by(.data$posteam, .data$week, .data$season) %>%
    dplyr::summarise(
      opp_receiver = custom_mode(.data$defteam),
      receiving_2pt_conversions = dplyr::n()
    ) %>%
    dplyr::ungroup()
  
  rec_df <- rec %>%
    dplyr::full_join(rec_two_points, by = c("posteam", "week", "season", "opp_receiver")) %>%
    dplyr::mutate(receiving_2pt_conversions = dplyr::if_else(is.na(.data$receiving_2pt_conversions), 0L, .data$receiving_2pt_conversions)) %>%
    dplyr::filter(!is.na(.data$posteam))
  
  # Special Teams
  st_tds <- pbp %>%
    dplyr::filter(.data$special == 1 & !is.na(.data$posteam) & !is.na(.data$td_player_id)) %>%
    dplyr::group_by(.data$posteam, .data$week, .data$season) %>%
    dplyr::summarise(
      opp_st = custom_mode(.data$defteam),
      special_teams_tds = sum(.data$touchdown, na.rm = TRUE)
    )
  
  # Per-drive statistics
  drive_df <- data %>%
    dplyr::filter(!is.na(.data$posteam)) %>%
    dplyr::group_by(.data$posteam, .data$season, .data$week) %>%
    dplyr::distinct(.data$drive, .keep_all = TRUE) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$posteam, .data$season, .data$week) %>%
    dplyr::summarise(
      yards_per_drive = mean(.data$ydsnet, na.rm = TRUE),
      plays_per_drive = mean(.data$drive_play_count, na.rm = TRUE),
      scoring_drive_percentage = mean(.data$drive_ended_with_score, na.rm = TRUE),
      inside20_percentage = mean(.data$drive_inside20, na.rm = TRUE),
      penalty_yards_per_drive = sum(.data$drive_yards_penalized, na.rm = TRUE),
      off_points = sum((6 * (.data$fixed_drive_result == 'Touchdown')) + (3 * (.data$fixed_drive_result == 'Field goal'))),
      off_turnovers = sum(.data$fixed_drive_result == 'Turnover'),
      num_drives = dplyr::n()
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      points_per_drive = dplyr::case_when(
        is.na(.data$off_points) | is.na(.data$num_drives) ~ NA_real_,
        .data$num_drives == 0 ~ 0,
        TRUE ~ .data$off_points / .data$num_drives
      ),
      drive_turnover_percentage = dplyr::case_when(
        is.na(.data$off_turnovers) | is.na(.data$num_drives) ~ NA_real_,
        .data$num_drives == 0 ~ 0,
        TRUE ~ .data$off_turnovers / .data$num_drives
      )
    )
  
  # Combine all stats -------------------------------------------------------

  # combine all the stats together
  team_df <- pass_df %>%
    dplyr::full_join(rush_df, by = c("posteam", "week", "season")) %>%
    dplyr::full_join(rec_df, by = c("posteam", "week", "season")) %>%
    dplyr::full_join(st_tds, by = c("posteam", "week", "season")) %>%
    dplyr::full_join(drive_df, by = c("posteam", "week", "season")) %>%
    dplyr::left_join(s_type, by = c("season", "week")) %>%
    dplyr::mutate(
      team = .data$posteam,
      opponent_team = dplyr::case_when(
        !is.na(.data$opp_pass) ~ .data$opp_pass,
        !is.na(.data$opp_rush) ~ .data$opp_rush,
        !is.na(.data$opp_receiver) ~ .data$opp_receiver,
        TRUE ~ .data$opp_st
      )
    ) %>%
    dplyr::select(tidyselect::any_of(c(
      "team", "season", "week", "season_type", "opponent_team",
      # passing stats
      "completions", "attempts", "passing_yards", "passing_tds", "interceptions",
      "sacks", "sack_yards", "sack_fumbles", "sack_fumbles_lost", "passing_air_yards", 
      "passing_yards_after_catch", "passing_first_downs", "passing_epa", 
      "passing_2pt_conversions", "pacr",
      
      # rushing stats
      "carries", "rushing_yards", "rushing_tds", "rushing_fumbles", "rushing_fumbles_lost",
      "rushing_first_downs", "rushing_epa", "rushing_2pt_conversions",
      
      # receiving stats
      "receptions", "receiving_yards", "receiving_tds", "receiving_fumbles",
      "receiving_fumbles_lost", "receiving_air_yards", "receiving_yards_after_catch",
      "receiving_first_downs", "receiving_2pt_conversions",
      
      # special teams
      "special_teams_tds",
      
      # drive stats
      "yards_per_drive", "plays_per_drive", "scoring_drive_percentage", "inside20_percentage", 
      "penalty_yards_per_drive", "points_per_drive", "drive_turnover_percentage"
      
    ))) %>%
    dplyr::filter(!is.na(.data$team))
  
  team_df_nas <- is.na(team_df)
  epa_index <- which(dimnames(team_df_nas)[[2]] %in% c("passing_epa", "rushing_epa", "pacr"))
  team_df_nas[,epa_index] <- c(FALSE)
  
  team_df[team_df_nas] <- 0
  
  # Fantasy points
  team_df <- team_df %>%
    dplyr::mutate(
      passing_fantasy_points =
        1 / 25 * .data$passing_yards +
        4 * .data$passing_tds +
        -2 * .data$interceptions +
        2 * (.data$passing_2pt_conversions) +
        -2 * (.data$sack_fumbles_lost),
      
      rushing_fantasy_points = 
        1 / 10 * (.data$rushing_yards) +
        6 * (.data$rushing_tds) +
        2 * (.data$rushing_2pt_conversions) +
        -2 * (.data$rushing_fumbles_lost),
      
      receiving_fantasy_points = 
        1 / 10 * (.data$receiving_yards) +
        6 * (.data$receiving_tds) +
        2 * (.data$receiving_2pt_conversions) +
        -2 * (.data$receiving_fumbles_lost),
      
      receiving_fantasy_points_ppr = (.data$receiving_fantasy_points + .data$receptions)
    ) %>%
    dplyr::arrange(.data$team, .data$season, .data$week)
  
  if (isFALSE(weekly)) {
    team_df <- team_df %>%
      dplyr::group_by(.data$team) %>%
      dplyr::summarise(
        games = dplyr::n(),
        
        # passing
        completions = sum(.data$completions),
        attempts = sum(.data$attempts),
        passing_yards = sum(.data$passing_yards),
        passing_tds = sum(.data$passing_tds),
        interceptions = sum(.data$interceptions),
        sacks = sum(.data$sacks),
        sack_yards = sum(.data$sack_yards),
        sack_fumbles = sum(.data$sack_fumbles),
        sack_fumbles_lost = sum(.data$sack_fumbles_lost),
        passing_air_yards = sum(.data$passing_air_yards),
        passing_yards_after_catch = sum(.data$passing_yards_after_catch),
        passing_first_downs = sum(.data$passing_first_downs),
        passing_epa = dplyr::if_else(all(is.na(.data$passing_epa)), NA_real_, sum(.data$passing_epa, na.rm = TRUE)),
        passing_2pt_conversions = sum(.data$passing_2pt_conversions),
        pacr = .data$passing_yards / .data$passing_air_yards,
        
        # rushing
        carries = sum(.data$carries),
        rushing_yards = sum(.data$rushing_yards),
        rushing_tds = sum(.data$rushing_tds),
        rushing_fumbles = sum(.data$rushing_fumbles),
        rushing_fumbles_lost = sum(.data$rushing_fumbles_lost),
        rushing_first_downs = sum(.data$rushing_first_downs),
        rushing_epa = dplyr::if_else(all(is.na(.data$rushing_epa)), NA_real_, sum(.data$rushing_epa, na.rm = TRUE)),
        rushing_2pt_conversions = sum(.data$rushing_2pt_conversions),
        
        # receiving
        receptions = sum(.data$receptions),
        receiving_yards = sum(.data$receiving_yards),
        receiving_tds = sum(.data$receiving_tds),
        receiving_fumbles = sum(.data$receiving_fumbles),
        receiving_fumbles_lost = sum(.data$receiving_fumbles_lost),
        receiving_air_yards = sum(.data$receiving_air_yards),
        receiving_yards_after_catch = sum(.data$receiving_yards_after_catch),
        receiving_first_downs = sum(.data$receiving_first_downs),
        receiving_2pt_conversions = sum(.data$receiving_2pt_conversions),
        
        # special teams
        special_teams_tds = sum(.data$special_teams_tds),
        
        # per drive stats
        yards_per_drive = mean(.data$yards_per_drive),
        plays_per_drive = mean(.data$plays_per_drive),
        scoring_drive_percentage = mean(.data$scoring_drive_percentage),
        inside20_percentage = mean(.data$inside20_percentage),
        penalty_yards_per_drive = mean(.data$penalty_yards_per_drive),
        points_per_drive = mean(.data$points_per_drive),
        drive_turnover_percentage = mean(.data$drive_turnover_percentage),
        
        # fantasy
        passing_fantasy_points = sum(.data$passing_fantasy_points),
        rushing_fantasy_points = sum(.data$rushing_fantasy_points),
        receiving_fantasy_points = sum(.data$receiving_fantasy_points),
        receiving_fantasy_points_ppr = sum(.data$receiving_fantasy_points_ppr)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        pacr = dplyr::case_when(
          is.nan(.data$pacr) ~ NA_real_,
          .data$passing_air_yards <= 0 ~ 0,
          TRUE ~ .data$pacr
        )
      )
  }
  return(team_df)
}
