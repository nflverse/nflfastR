#' Summarize Weekly Kicking stats
#'
#' @description Build columns that aggregate kicking stats at the game level.
#'
#' @param pbp A Data frame of NFL play-by-play data typically loaded with
#' [load_pbp()] or [build_nflfastR_pbp()].
#'
#' @examples
#'
#' \donttest{
#' pbp <- nflreadr::load_pbp(2021)
#' calculate_kicking_stats(pbp)
#' }
#'
#' @return a dataframe of kicking stats
#' @seealso <https://nflreadr.nflverse.com/reference/load_player_stats.html> for the nflreadr function to download this from repo (`stat_type = "kicking"`)
#' @export
calculate_kicking_stats <- function(pbp){

  base_kicks <- pbp %>%
    dplyr::filter(.data$field_goal_attempt == 1 | .data$extra_point_attempt == 1) %>%
    dplyr::select(
      "season",
      "week",
      "season_type",
      "team" = "posteam",
      "player_name" = "kicker_player_name",
      "player_id" = "kicker_player_id",
      "kick_distance",
      "field_goal_attempt",
      "extra_point_attempt",
      "field_goal_result",
      "extra_point_result"
    ) %>%
    dplyr::group_by(.data$season,.data$week,.data$season_type,.data$team,.data$player_name,.data$player_id) %>%
    dplyr::summarise(
      fg_made = sum(.data$field_goal_result == "made", na.rm = TRUE),
      fg_missed = sum(.data$field_goal_result == "missed", na.rm = TRUE),
      fg_blocked = sum(.data$field_goal_result == "blocked", na.rm = TRUE),
      fg_long = max(as.numeric(.data$field_goal_result == "made") * .data$kick_distance,0, na.rm = TRUE),
      fg_att = sum(.data$field_goal_attempt,na.rm = TRUE),
      fg_pct = round(.data$fg_made /.data$fg_att, 3),
      pat_made = sum(.data$extra_point_result == "good", na.rm = TRUE),
      pat_missed = sum(.data$extra_point_result == "failed", na.rm = TRUE),
      pat_blocked = sum(.data$extra_point_result == "blocked", na.rm = TRUE),
      pat_att = sum(.data$extra_point_attempt, na.rm = TRUE),
      pat_pct = round(.data$pat_made/.data$pat_att, 3),
      fg_made_distance = sum(as.numeric(.data$field_goal_result == "made") * .data$kick_distance, na.rm = TRUE),
      fg_missed_distance = sum(as.numeric(.data$field_goal_result == "missed") * .data$kick_distance, na.rm = TRUE),
      fg_blocked_distance = sum(as.numeric(.data$field_goal_result == "blocked") * .data$kick_distance, na.rm = TRUE),
      fg_made_0_19 = sum(as.numeric(.data$field_goal_result == "made") * dplyr::between(.data$kick_distance,0,19), na.rm = TRUE),
      fg_made_20_29 = sum(as.numeric(.data$field_goal_result == "made") * dplyr::between(.data$kick_distance,20,29), na.rm = TRUE),
      fg_made_30_39 = sum(as.numeric(.data$field_goal_result == "made") * dplyr::between(.data$kick_distance,30,39), na.rm = TRUE),
      fg_made_40_49 = sum(as.numeric(.data$field_goal_result == "made") * dplyr::between(.data$kick_distance,40,49), na.rm = TRUE),
      fg_made_50_59 = sum(as.numeric(.data$field_goal_result == "made") * dplyr::between(.data$kick_distance,50,59), na.rm = TRUE),
      fg_made_60_ = sum(as.numeric(.data$field_goal_result == "made") * (.data$kick_distance >=60), na.rm = TRUE),
      fg_missed_0_19 = sum(as.numeric(.data$field_goal_result == "missed") * dplyr::between(.data$kick_distance,0,19), na.rm = TRUE),
      fg_missed_20_29 = sum(as.numeric(.data$field_goal_result == "missed") * dplyr::between(.data$kick_distance,20,29), na.rm = TRUE),
      fg_missed_30_39 = sum(as.numeric(.data$field_goal_result == "missed") * dplyr::between(.data$kick_distance,30,39), na.rm = TRUE),
      fg_missed_40_49 = sum(as.numeric(.data$field_goal_result == "missed") * dplyr::between(.data$kick_distance,40,49), na.rm = TRUE),
      fg_missed_50_59 = sum(as.numeric(.data$field_goal_result == "missed") * dplyr::between(.data$kick_distance,50,59), na.rm = TRUE),
      fg_missed_60_ = sum(as.numeric(.data$field_goal_result == "missed") * (.data$kick_distance >=60), na.rm = TRUE),
      fg_made_list = .data$kick_distance[.data$field_goal_result == "made"] %>% na.omit() %>%  paste(collapse = ";"),
      fg_missed_list = .data$kick_distance[.data$field_goal_result == "missed"] %>% na.omit() %>% paste(collapse = ";"),
      fg_blocked_list = .data$kick_distance[.data$field_goal_result == "blocked"] %>% na.omit() %>% paste(collapse = ";")
    ) %>%
    dplyr::ungroup()

  game_winners <- pbp %>%
    dplyr::group_by(.data$game_id,.data$posteam) %>%
    dplyr::filter(.data$fixed_drive == max(.data$fixed_drive)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$field_goal_attempt == 1, dplyr::between(.data$score_differential,-2,0)) %>%
    dplyr::select(
      "season",
      "week",
      "season_type",
      "team" = "posteam",
      "qtr",
      "game_seconds_remaining",
      "score_differential",
      "desc",
      "player_name" = "kicker_player_name",
      "player_id" = "kicker_player_id",
      "kick_distance",
      "field_goal_attempt",
      "extra_point_attempt",
      "field_goal_result",
      "extra_point_result"
    ) %>%
    dplyr::group_by(.data$season,.data$week,.data$season_type,.data$team,.data$player_name,.data$player_id) %>%
    dplyr::summarise(
      gwfg_att = n(),
      gwfg_distance = .data$kick_distance,
      gwfg_made = sum(.data$field_goal_result == "made", na.rm = TRUE),
      gwfg_missed = sum(.data$field_goal_result == "missed", na.rm = TRUE),
      gwfg_blocked = sum(.data$field_goal_result == "blocked", na.rm = TRUE),
    ) %>%
    dplyr::ungroup()

  full_kicks <- base_kicks %>%
    dplyr::left_join(
      game_winners,
      by = c("season", "week", "season_type", "team", "player_name", "player_id")) %>%
    dplyr::mutate(
      dplyr::across(dplyr::starts_with("gwfg"),tidyr::replace_na,0)
    ) %>%
    dplyr::relocate(
      dplyr::starts_with("gwfg"),
      .after = "fg_blocked_distance"
    )

  return(full_kicks)
}
