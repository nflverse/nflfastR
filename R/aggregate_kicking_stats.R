#' Summarize Kicking Stats
#'
#' @description Build columns that aggregate kicking stats at the game level.
#'
#' @param pbp A Data frame of NFL play-by-play data typically loaded with
#' [load_pbp()] or [build_nflfastR_pbp()].
#' @param weekly If `TRUE`, returns week-by-week stats, otherwise, stats for
#' the entire Data frame.
#'
#' @examples
#'
#' \donttest{
#' pbp <- nflreadr::load_pbp(2021)
#' calculate_kicking_stats(pbp, weekly = TRUE)
#' }
#'
#' @return a dataframe of kicking stats
#' @seealso <https://nflreadr.nflverse.com/reference/load_player_stats.html> for the nflreadr function to download this from repo (`stat_type = "kicking"`)
#' @export
calculate_kicking_stats <- function(pbp, weekly = FALSE){

  # First, creating a grouping variable object to toggle the weekly argument w/
  grp_vars <- list("season", "season_type", "week", "team", "player_id")
  if(!weekly) {
    grp_vars <- setdiff(grp_vars, "week")
  }
  grp_vars <- lapply(grp_vars, as.symbol)

  # Filtering down / creating a base dataset
  df_fg_or_pat = pbp %>%
    dplyr::filter(field_goal_attempt == 1 | extra_point_attempt == 1) %>%
    dplyr::select(
      game_id,
      season,
      week,
      season_type,
      team = posteam,
      player_name = kicker_player_name,
      player_id = kicker_player_id,
      dist = kick_distance,
      field_goal_attempt,
      fg_res = field_goal_result,
      extra_point_attempt,
      pat_res = extra_point_result,
      fixed_drive,
      score_differential
    )

  # Field-goal relevant columns
  df_field_goals <- df_fg_or_pat %>%
    dplyr::filter(field_goal_attempt == 1) %>%
    dplyr::group_by(!!!grp_vars) %>%
    dplyr::mutate(temp_made_idx = fg_res == "made",
                  temp_miss_idx = fg_res == "missed",
                  temp_block_idx = fg_res == "blocked") %>%
    dplyr::summarise(
      fg_made = sum(temp_made_idx, na.rm = TRUE),
      fg_att = sum(field_goal_attempt, na.rm = TRUE),
      fg_missed = sum(temp_miss_idx, na.rm = TRUE),
      fg_blocked = sum(temp_block_idx, na.rm = TRUE),
      fg_long = if(any(temp_made_idx, na.rm = TRUE)) max(dist[temp_made_idx], na.rm = TRUE) else NA_real_,
      fg_pct = round(fg_made / fg_att, 3L),
      fg_made_0_19 = sum(dplyr::between(dist[temp_made_idx], 0, 19), na.rm = TRUE),
      fg_made_20_29 = sum(dplyr::between(dist[temp_made_idx], 20, 29), na.rm = TRUE),
      fg_made_30_39 = sum(dplyr::between(dist[temp_made_idx], 30, 39), na.rm = TRUE),
      fg_made_40_49 = sum(dplyr::between(dist[temp_made_idx], 40, 49), na.rm = TRUE),
      fg_made_50_59 = sum(dplyr::between(dist[temp_made_idx], 50, 59), na.rm = TRUE),
      fg_made_60_ = sum(dist[temp_made_idx] >= 60, na.rm = TRUE),
      fg_missed_0_19 = sum(dplyr::between(dist[temp_miss_idx], 0, 19), na.rm = TRUE),
      fg_missed_20_29 = sum(dplyr::between(dist[temp_miss_idx], 20, 29), na.rm = TRUE),
      fg_missed_30_39 = sum(dplyr::between(dist[temp_miss_idx], 30, 39), na.rm = TRUE),
      fg_missed_40_49 = sum(dplyr::between(dist[temp_miss_idx], 40, 49), na.rm = TRUE),
      fg_missed_50_59 = sum(dplyr::between(dist[temp_miss_idx], 50, 59), na.rm = TRUE),
      fg_missed_60_ = sum(dist[temp_miss_idx] >= 60, na.rm = TRUE),
      fg_made_list = paste(na.omit(dist[temp_made_idx]), collapse = ";"),
      fg_missed_list = paste(na.omit(dist[temp_miss_idx]), collapse = ";"),
      fg_blocked_list = paste(na.omit(dist[temp_block_idx]), collapse = ";"),
      fg_made_distance = sum(dist[temp_made_idx], na.rm = TRUE),
      fg_missed_distance = sum(dist[temp_miss_idx], na.rm = TRUE),
      fg_blocked_distance = sum(dist[temp_block_idx], na.rm = TRUE),
      .groups = "drop"
    )


  # Extra points
  df_pat <- df_fg_or_pat %>%
    dplyr::filter(extra_point_attempt == 1) %>%
    dplyr::group_by(!!!grp_vars) %>%
    dplyr::summarise(
      pat_made = sum(pat_res == "good", na.rm = TRUE),
      pat_att = sum(extra_point_attempt, na.rm = TRUE),
      pat_missed = sum(pat_res == "failed", na.rm = TRUE),
      pat_blocked = sum(pat_res == "blocked", na.rm = TRUE),
      pat_pct = round(pat_made / pat_att, 3L),
      .groups = "drop"
    )


  # The Game Winning kicks distance include up to one value at the weekly level
  # but can include multiple across the season. This is one way to account for that.
  # the downside is that the column names change depending on if it is weekly vs
  # seasonal.
  if(weekly) {
    gw_dist_name <- "gwfg_distance"
  } else {
    gw_dist_name <- "gwfg_distance_list"
  }

  # See the above note. I wonder if this shoudl also include field goals that tie
  # the game but I kept the filter dplyr::between(score_differential, -2, 0) the way
  # that is was previously. If you do include field goals that send the game into OT,
  # then you'll probably need to include the gwfg_distance AND gwfg_distance_list columns
  # in the weekly data
  # Also, unusual one for me: An in-line if() statement.
  game_winners <- df_fg_or_pat %>%
    dplyr::group_by(game_id, team) %>%
    dplyr::filter(fixed_drive == max(fixed_drive, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(
      field_goal_attempt == 1,
      dplyr::between(score_differential, -2, 0)
    ) %>%
    dplyr::group_by(!!!grp_vars) %>%
    dplyr::summarise(
      gwfg_att = n(),
      !!gw_dist_name := if(weekly) dist else paste(na.omit(dist), collapse = ";"),
      gwfg_made = sum(fg_res == "made", na.rm = TRUE),
      gwfg_missed = sum(fg_res == "missed", na.rm = TRUE),
      gwfg_blocked = sum(fg_res == "blocked", na.rm = TRUE),
      .groups = "drop"
    )

  # Prepping data to merge-in player names. Not 100% sure how you wanted this
  # done so feel free to scrap the way I did this
  df_player_names <- df_fg_or_pat %>%
    dplyr::select(!!!grp_vars, player_name) %>%
    dplyr::distinct()

  # Joining all the data together and grabbing the first few columns.
  # I was not sure what order you want the columns or if you want them arranged (and how)
  full_kicks <- df_field_goals %>%
    dplyr::full_join(df_pat, as.character(grp_vars)) %>%
    dplyr::full_join(game_winners, as.character(grp_vars)) %>%
    dplyr::left_join(df_player_names, as.character(grp_vars)) %>%
    dplyr::select(season, any_of("week"), season_type, team, player_name, player_id, everything())

  # This just replaced NAs with 0. I personally prefer the NA's but saw you explicity change
  # NA to zero in the PR. Also, I changed character values with zero length to 0 as well
  full_kicks[] <- lapply(full_kicks, function(x) {
    replace(x, is.na(x) | nchar(x) == 0, 0)
  })

  if(weekly) {
    full_kicks %>%
      dplyr::arrange(season, week, team, player_name)
  } else {
    full_kicks %>%
      dplyr::arrange(season, desc(season_type), team, player_name)
  }

}
