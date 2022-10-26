#' Summarize Kicking Stats
#'
#' @description Build columns that aggregate kicking stats at the game level.
#'
#' @param pbp A Data frame of NFL play-by-play data typically loaded with
#' [load_pbp()] or [build_nflfastR_pbp()].
#' @param weekly If `TRUE`, returns week-by-week stats, otherwise, stats for
#' the entire data frame in argument `pbp`.
#'
#' @examples
#' \donttest{
#' try({
#'     pbp <- nflreadr::load_pbp(2021)
#'     weekly <- calculate_player_stats_kicking(pbp, weekly = TRUE)
#'     dplyr::glimpse(weekly)
#'
#'     overall <- calculate_player_stats_kicking(pbp, weekly = FALSE)
#'     dplyr::glimpse(overall)
#' })
#' }
#'
#' @return a dataframe of kicking stats
#' @seealso <https://nflreadr.nflverse.com/reference/load_player_stats.html> for the nflreadr function to download this from repo (`stat_type = "kicking"`)
#' @export
calculate_player_stats_kicking <- function(pbp, weekly = FALSE) {

  # need newer version of nflreadr to use load_players
  rlang::check_installed("nflreadr (>= 1.3.0)")

  # First, creating a grouping variable object to toggle the weekly argument w/
  grp_vars <- if (isTRUE(weekly)){
    list("season", "week", "season_type", "player_id", "team")
  } else if (isFALSE(weekly)){
    list("player_id", "team")
  }
  grp_vars <- lapply(grp_vars, as.symbol)

  # Filtering down / creating a base dataset
  df_fg_or_pat <- pbp %>%
    dplyr::group_by(.data$game_id, .data$posteam) %>%
    dplyr::filter(
      .data$field_goal_attempt == 1 |
        .data$extra_point_attempt == 1 |
        .data$fixed_drive == max(.data$fixed_drive, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(.data$kicker_player_id)) %>%
    dplyr::select(
      "game_id",
      "season",
      "week",
      "season_type",
      "team" = "posteam",
      "player_name" = "kicker_player_name",
      "player_id" = "kicker_player_id",
      "dist" = "kick_distance",
      "field_goal_attempt",
      "fg_res" = "field_goal_result",
      "extra_point_attempt",
      "pat_res" = "extra_point_result",
      "fixed_drive",
      "score_differential"
    )

  # Field-goal relevant columns
  df_field_goals <- df_fg_or_pat %>%
    dplyr::filter(.data$field_goal_attempt == 1) %>%
    dplyr::group_by(!!!grp_vars) %>%
    dplyr::mutate(
      temp_made_idx = .data$fg_res == "made",
      temp_miss_idx = .data$fg_res == "missed",
      temp_block_idx = .data$fg_res == "blocked"
    ) %>%
    dplyr::summarise(
      games_fg = list(unique(.data$game_id)),
      fg_made = sum(.data$temp_made_idx, na.rm = TRUE),
      fg_att = sum(.data$field_goal_attempt, na.rm = TRUE),
      fg_missed = sum(.data$temp_miss_idx, na.rm = TRUE),
      fg_blocked = sum(.data$temp_block_idx, na.rm = TRUE),
      fg_long = if (any(.data$temp_made_idx, na.rm = TRUE)) max(.data$dist[.data$temp_made_idx], na.rm = TRUE) else NA_real_,
      fg_pct = round(.data$fg_made / .data$fg_att, 3L),
      fg_made_0_19 = sum(dplyr::between(.data$dist[.data$temp_made_idx], 0, 19), na.rm = TRUE),
      fg_made_20_29 = sum(dplyr::between(.data$dist[.data$temp_made_idx], 20, 29), na.rm = TRUE),
      fg_made_30_39 = sum(dplyr::between(.data$dist[.data$temp_made_idx], 30, 39), na.rm = TRUE),
      fg_made_40_49 = sum(dplyr::between(.data$dist[.data$temp_made_idx], 40, 49), na.rm = TRUE),
      fg_made_50_59 = sum(dplyr::between(.data$dist[.data$temp_made_idx], 50, 59), na.rm = TRUE),
      fg_made_60_ = sum(.data$dist[.data$temp_made_idx] >= 60, na.rm = TRUE),
      fg_missed_0_19 = sum(dplyr::between(.data$dist[.data$temp_miss_idx], 0, 19), na.rm = TRUE),
      fg_missed_20_29 = sum(dplyr::between(.data$dist[.data$temp_miss_idx], 20, 29), na.rm = TRUE),
      fg_missed_30_39 = sum(dplyr::between(.data$dist[.data$temp_miss_idx], 30, 39), na.rm = TRUE),
      fg_missed_40_49 = sum(dplyr::between(.data$dist[.data$temp_miss_idx], 40, 49), na.rm = TRUE),
      fg_missed_50_59 = sum(dplyr::between(.data$dist[.data$temp_miss_idx], 50, 59), na.rm = TRUE),
      fg_missed_60_ = sum(.data$dist[.data$temp_miss_idx] >= 60, na.rm = TRUE),
      fg_made_list = paste(na.omit(.data$dist[.data$temp_made_idx]), collapse = ";"),
      fg_missed_list = paste(na.omit(.data$dist[.data$temp_miss_idx]), collapse = ";"),
      fg_blocked_list = paste(na.omit(.data$dist[.data$temp_block_idx]), collapse = ";"),
      fg_made_distance = sum(.data$dist[.data$temp_made_idx], na.rm = TRUE),
      fg_missed_distance = sum(.data$dist[.data$temp_miss_idx], na.rm = TRUE),
      fg_blocked_distance = sum(.data$dist[.data$temp_block_idx], na.rm = TRUE),
      .groups = "drop"
    )


  # Extra points
  df_pat <- df_fg_or_pat %>%
    dplyr::filter(.data$extra_point_attempt == 1) %>%
    dplyr::group_by(!!!grp_vars) %>%
    dplyr::summarise(
      games_pat = list(unique(.data$game_id)),
      pat_made = sum(.data$pat_res == "good", na.rm = TRUE),
      pat_att = sum(.data$extra_point_attempt, na.rm = TRUE),
      pat_missed = sum(.data$pat_res == "failed", na.rm = TRUE),
      pat_blocked = sum(.data$pat_res == "blocked", na.rm = TRUE),
      pat_pct = round(.data$pat_made / .data$pat_att, 3L),
      .groups = "drop"
    )


  # The Game Winning kicks distance include up to one value at the weekly level
  # but can include multiple across the season. This is one way to account for that.
  # the downside is that the column names change depending on if it is weekly vs
  # seasonal.
  if (weekly) {
    gw_dist_name <- "gwfg_distance"
  } else {
    gw_dist_name <- "gwfg_distance_list"
  }

  # See the above note. I wonder if this should also include field goals that tie
  # the game but I kept the filter dplyr::between(score_differential, -2, 0) the way
  # that is was previously. If you do include field goals that send the game into OT,
  # then you'll probably need to include the gwfg_distance AND gwfg_distance_list columns
  # in the weekly data
  game_winners <- df_fg_or_pat %>%
    dplyr::group_by(.data$game_id, .data$team) %>%
    dplyr::filter(.data$fixed_drive == max(.data$fixed_drive, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$field_goal_attempt == 1, dplyr::between(.data$score_differential, -2, 0)) %>%
    dplyr::group_by(!!!grp_vars) %>%
    dplyr::summarise(
      games_gwfg = list(unique(.data$game_id)),
      gwfg_att = dplyr::n(),
      !!gw_dist_name := if (weekly) .data$dist else paste(na.omit(.data$dist), collapse = ";"),
      gwfg_made = sum(.data$fg_res == "made", na.rm = TRUE),
      gwfg_missed = sum(.data$fg_res == "missed", na.rm = TRUE),
      gwfg_blocked = sum(.data$fg_res == "blocked", na.rm = TRUE),
      .groups = "drop"
    )

  # Prepping data to merge-in player names
  df_player_names <- nflreadr::load_players() %>%
    dplyr::select(
      "player_id" = "gsis_id",
      "player_display_name" = "display_name",
      "player_name" = "short_name",
      "position",
      "position_group",
      "headshot_url" = "headshot"
    )

  # Joining all the data together and organizing the first few columns.
  full_kicks <- df_field_goals %>%
    dplyr::full_join(df_pat, as.character(grp_vars)) %>%
    dplyr::full_join(game_winners, as.character(grp_vars)) %>%
    dplyr::left_join(df_player_names, "player_id") %>%
    dplyr::group_by(!!!grp_vars) %>%
    dplyr::mutate(games = length(unique(unlist(c(.data$games_fg, .data$games_pat, .data$games_gwfg))))) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      dplyr::any_of(c("season", "week", "season_type")), "player_id",
      "team", "player_name", "player_display_name", "games", "position",
      "position_group", "headshot_url", dplyr::everything(),
      -c("games_fg", "games_pat", "games_gwfg")
    ) %>%
    # replace "" with NA
    dplyr::mutate_all(~replace(.x, nchar(.x) == 0 | is.nan(.x), NA)) %>%
    # replace NA in attempt columns with 0
    dplyr::mutate_at(c("fg_att", "pat_att", "gwfg_att"), ~tidyr::replace_na(.x, 0))


  if (weekly) {
    full_kicks %>%
      dplyr::select(-"games") %>%
      dplyr::arrange(.data$player_id, .data$season, .data$week)
  } else {
    full_kicks %>%
      dplyr::arrange(.data$player_id)
  }
}
