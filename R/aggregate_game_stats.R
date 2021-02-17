################################################################################
# Author: Ben Baldwin, Sebastian Carl
# Stlyeguide: styler::tidyverse_style()
################################################################################

#' Get Official Game Stats
#'
#' @param pbp A Data frame of NFL play-by-play data typically loaded with
#' [load_pbp()] or [build_nflfastR_pbp()].
#' @param weekly If `TRUE`, returns week-by-week stats, otherwise, stats
#' for the entire Data frame.
#' @description Build columns that aggregate official passing, rushing, and receiving stats
#' either at the game level or at the level of the entire data frame passed.
#' @return A data frame including the following columns (all ID columns are
#' decoded to the gsis ID format):
#' \describe{
#' \item{player_id}{ID of the player. Use this to join to other sources.}
#' \item{player_name}{Name of the player}
#' \item{games}{The number of games where the player recorded passing, rushing or receiving stats.}
#' \item{recent_team}{Most recent team player appears in `pbp` with.}
#' \item{season}{Season if `weekly` is `TRUE`}
#' \item{week}{Week if `weekly` is `TRUE`}
#' \item{completions}{The number of completed passes.}
#' \item{attempts}{The number of pass attempts as defined by the NFL.}
#' \item{passing_yards}{Yards gained on pass plays.}
#' \item{passing_tds}{The number of passing touchdowns.}
#' \item{interceptions}{The number of interceptions thrown.}
#' \item{sack_fumbles_lost}{The number of sacks with a lost fumble.}
#' \item{passing_air_yards}{Passing air yards (includes incomplete passes).}
#' \item{passing_yards_after_catch}{Yards after the catch gained on plays in
#' which player was the passer (this is an unofficial stat and may differ slightly
#' between different sources).}
#' \item{carries}{The number of official rush attempts (incl. scrambles and kneel downs).
#' Rushes after a lateral reception don't count as carry.}
#' \item{rushing_yards}{Yards gained when rushing with the ball (incl. scrambles and kneel downs).
#' Also includes yards gained after obtaining a lateral on a play that started
#' with a rushing attempt.}
#' \item{rushing_tds}{The number of rushing touchdowns (incl. scrambles).
#' Also includes touchdowns after obtaining a lateral on a play that started
#' with a rushing attempt.}
#' \item{rushing_fumbles_lost}{The number of rushes with a lost fumble.}
#' \item{receptions}{The number of pass receptions. Lateral receptions officially
#' don't count as reception.}
#' \item{targets}{The number of pass plays where the player was the targeted receiver.}
#' \item{receiving_yards}{Yards gained after a pass reception. Includes yards
#' gained after receiving a lateral on a play that started as a pass play.}
#' \item{receiving_tds}{The number of touchdowns following a pass reception.
#' Also includes touchdowns after receiving a lateral on a play that started
#' as a pass play.}
#' \item{receiving_air_yards}{Receiving air yards (includes incomplete passes).}
#' \item{receiving_yards_after_catch}{Yards after the catch gained on plays in
#' which player was receiver (this is an unofficial stat and may differ slightly
#' between different sources).}
#' \item{receiving_fumbles_lost}{The number of fumbles after a pass reception.}
#' }
#' @export
#' @examples
#' \donttest{
#' pbp <- nflfastR::load_pbp(2020)
#'
#' weekly <- calculate_player_stats(pbp)
#' dplyr::glimpse(weekly)
#'
#' overall <- calculate_player_stats(pbp, weekly = FALSE)
#' dplyr::glimpse(overall)
#' }
calculate_player_stats <- function(pbp, weekly = FALSE) {

  # load plays with multiple laterals
  con <- url("https://github.com/mrcaseb/nfl-data/blob/master/data/lateral_yards/multiple_lateral_yards.rds?raw=true")
  mult_lats <- readRDS(con) %>%
    dplyr::mutate(
      season = substr(.data$game_id, 1, 4) %>% as.integer(),
      week = substr(.data$game_id, 6, 7) %>% as.integer()
    ) %>%
    dplyr::filter(.data$yards != 0) %>%
    # the list includes all plays with multiple laterals
    # and all receivers. Since the last one already is in the
    # pbp data, we have to drop him here so the entry isn't duplicated
    dplyr::group_by(.data$game_id, .data$play_id) %>%
    dplyr::slice(seq_len(dplyr::n() - 1)) %>%
    dplyr::ungroup()
  close(con)

  # get down to plays that count in official stats
  suppressMessages({
    data <- pbp %>%
      dplyr::filter(
        !is.na(.data$down),
        .data$play_type %in% c("pass", "qb_kneel", "qb_spike", "run")
      ) %>%
      decode_player_ids()
  })

  # get passing stats
  pass_df <- data %>%
    dplyr::filter(.data$play_type %in% c("pass", "qb_spike")) %>%
    dplyr::group_by(.data$passer_player_id, .data$week, .data$season) %>%
    dplyr::summarize(
      passing_yards_after_catch = sum((.data$passing_yards - .data$air_yards) * .data$complete_pass, na.rm = TRUE),
      name_pass = dplyr::first(.data$passer_player_name),
      team_pass = dplyr::first(.data$posteam),
      passing_yards = sum(.data$passing_yards, na.rm = TRUE),
      passing_tds = sum(.data$touchdown == 1 & .data$td_team == .data$posteam & .data$complete_pass == 1),
      interceptions = sum(.data$interception),
      attempts = sum(.data$complete_pass == 1 | .data$incomplete_pass == 1 | .data$interception == 1),
      completions = sum(.data$complete_pass == 1),
      sack_fumbles_lost = sum(.data$fumble_lost),
      passing_air_yards = sum(.data$air_yards, na.rm = TRUE)
    ) %>%
    dplyr::rename(player_id = .data$passer_player_id) %>%
    dplyr::ungroup()

  # rush df 1: primary rusher
  rushes <- data %>%
    dplyr::filter(.data$play_type %in% c("run", "qb_kneel")) %>%
    dplyr::group_by(.data$rusher_player_id, .data$week, .data$season) %>%
    dplyr::summarize(
      name_rush = dplyr::first(.data$rusher_player_name),
      team_rush = dplyr::first(.data$posteam),
      yards = sum(.data$rushing_yards, na.rm = TRUE),
      tds = sum(.data$td_player_id == .data$rusher_player_id, na.rm = TRUE),
      carries = dplyr::n(),
      rushing_fumbles_lost = sum(.data$fumble_lost == 1 & is.na(.data$lateral_rusher_player_id))
    ) %>%
    dplyr::ungroup()

  # rush df 2: lateral
  laterals <- data %>%
    dplyr::filter(!is.na(.data$lateral_rusher_player_id)) %>%
    dplyr::group_by(.data$lateral_rusher_player_id, .data$week, .data$season) %>%
    dplyr::summarize(
      lateral_yards = sum(.data$lateral_rushing_yards, na.rm = TRUE),
      lateral_tds = sum(.data$td_player_id == .data$lateral_rusher_player_id, na.rm = TRUE),
      lateral_att = dplyr::n()
    ) %>%
    dplyr::ungroup() %>%
    dplyr::rename(rusher_player_id = .data$lateral_rusher_player_id) %>%
    dplyr::bind_rows(
      mult_lats %>%
        dplyr::filter(
          .data$type == "lateral_rushing" & .data$season %in% data$season & .data$week %in% data$week
        ) %>%
        dplyr::select("season", "week", "rusher_player_id" = .data$gsis_player_id, "lateral_yards" = .data$yards) %>%
        dplyr::mutate(lateral_tds = 0L, lateral_att = 1L)
    )

  # rush df: join
  rush_df <- rushes %>%
    dplyr::left_join(laterals, by = c("rusher_player_id", "week", "season")) %>%
    dplyr::mutate(
      lateral_yards = dplyr::if_else(is.na(.data$lateral_yards), 0, .data$lateral_yards),
      lateral_tds = dplyr::if_else(is.na(.data$lateral_tds), 0L, .data$lateral_tds)
    ) %>%
    dplyr::mutate(rushing_yards = .data$yards + .data$lateral_yards, rushing_tds = .data$tds + .data$lateral_tds) %>%
    dplyr::rename(player_id = .data$rusher_player_id) %>%
    dplyr::select("player_id", "week", "season", "name_rush", "team_rush", "rushing_yards", "carries", "rushing_tds", "rushing_fumbles_lost") %>%
    dplyr::ungroup()


  # receiver df 1: primary receiver
  rec <- data %>%
    dplyr::filter(!is.na(.data$receiver_player_id)) %>%
    dplyr::group_by(.data$receiver_player_id, .data$week, .data$season) %>%
    dplyr::summarize(
      name_receiver = dplyr::first(.data$receiver_player_name),
      team_receiver = dplyr::first(.data$posteam),
      yards = sum(.data$receiving_yards, na.rm = TRUE),
      receptions = sum(.data$complete_pass == 1),
      targets = dplyr::n(),
      tds = sum(.data$td_player_id == .data$receiver_player_id, na.rm = TRUE),
      receiving_fumbles_lost = sum(.data$fumble_lost == 1 & is.na(.data$lateral_receiver_player_id)),
      receiving_air_yards = sum(.data$air_yards, na.rm = TRUE),
      receiving_yards_after_catch = sum(.data$yards_after_catch, na.rm = TRUE)
    ) %>%
    dplyr::ungroup()

  # receiver df 2: lateral
  laterals <- data %>%
    dplyr::filter(!is.na(.data$lateral_receiver_player_id)) %>%
    dplyr::group_by(.data$lateral_receiver_player_id, .data$week, .data$season) %>%
    dplyr::summarize(
      lateral_yards = sum(.data$lateral_receiving_yards, na.rm = TRUE),
      lateral_tds = sum(.data$td_player_id == .data$lateral_receiver_player_id, na.rm = TRUE),
      lateral_att = dplyr::n()
    ) %>%
    dplyr::ungroup() %>%
    dplyr::rename(receiver_player_id = .data$lateral_receiver_player_id) %>%
    dplyr::bind_rows(
      mult_lats %>%
        dplyr::filter(
          .data$type == "lateral_receiving" & .data$season %in% data$season & .data$week %in% data$week
        ) %>%
        dplyr::select("season", "week", "receiver_player_id" = .data$gsis_player_id, "lateral_yards" = .data$yards) %>%
        dplyr::mutate(lateral_tds = 0L, lateral_att = 1L)
    )

  # rec df: join
  rec_df <- rec %>%
    dplyr::left_join(laterals, by = c("receiver_player_id", "week", "season")) %>%
    dplyr::mutate(
      lateral_yards = dplyr::if_else(is.na(.data$lateral_yards), 0, .data$lateral_yards),
      lateral_tds = dplyr::if_else(is.na(.data$lateral_tds), 0L, .data$lateral_tds)
    ) %>%
    dplyr::mutate(
      receiving_yards = .data$yards + .data$lateral_yards,
      receiving_tds = .data$tds + .data$lateral_tds,
      receiving_yards_after_catch = .data$receiving_yards_after_catch + .data$lateral_yards
      ) %>%
    dplyr::rename(player_id = .data$receiver_player_id) %>%
    dplyr::select("player_id", "week", "season", "name_receiver", "team_receiver", "receiving_yards", "receiving_air_yards", "receiving_yards_after_catch", "receptions", "targets", "receiving_tds", "receiving_fumbles_lost")

  # combine all the stats together
  player_df <- pass_df %>%
    dplyr::full_join(rush_df, by = c("player_id", "week", "season")) %>%
    dplyr::full_join(rec_df, by = c("player_id", "week", "season")) %>%
    dplyr::mutate(
      player_name = dplyr::case_when(
        !is.na(.data$name_pass) ~ .data$name_pass,
        !is.na(.data$name_rush) ~ .data$name_rush,
        TRUE ~ .data$name_receiver
      ),
      recent_team = dplyr::case_when(
        !is.na(.data$team_pass) ~ .data$team_pass,
        !is.na(.data$team_rush) ~ .data$team_rush,
        TRUE ~ .data$team_receiver
      )
    ) %>%
    dplyr::select(
      "player_id", "player_name", "recent_team", "season", "week", "completions", "attempts", "passing_yards", "passing_tds", "interceptions", "passing_air_yards", "passing_yards_after_catch", "sack_fumbles_lost",
      "carries", "rushing_yards", "rushing_tds", "rushing_fumbles_lost",
      "receptions", "targets", "receiving_yards", "receiving_tds", "receiving_air_yards", "receiving_yards_after_catch", "receiving_fumbles_lost"
    ) %>%
    dplyr::arrange(.data$player_id, .data$season, .data$week)

  player_df[is.na(player_df)] <- 0

  # if user doesn't want week-by-week input, aggregate the whole df
  if (isFALSE(weekly)) {
    player_df <- player_df %>%
      dplyr::group_by(.data$player_id, .data$player_name) %>%
      dplyr::summarise(
        games = dplyr::n(),
        recent_team = dplyr::last(.data$recent_team),
        completions = sum(.data$completions),
        attempts = sum(.data$attempts),
        passing_yards = sum(.data$passing_yards),
        passing_tds = sum(.data$passing_tds),
        interceptions = sum(.data$interceptions),
        passing_air_yards = sum(.data$passing_air_yards),
        passing_yards_after_catch = sum(.data$passing_yards_after_catch),
        sack_fumbles_lost = sum(.data$sack_fumbles_lost),
        carries = sum(.data$carries),
        rushing_yards = sum(.data$rushing_yards),
        rushing_tds = sum(.data$rushing_tds),
        rushing_fumbles_lost = sum(.data$rushing_fumbles_lost),
        receptions = sum(.data$receptions),
        targets = sum(.data$targets),
        receiving_yards = sum(.data$receiving_yards),
        receiving_tds = sum(.data$receiving_tds),
        receiving_air_yards = sum(.data$receiving_air_yards),
        receiving_yards_after_catch = sum(.data$receiving_yards_after_catch),
        receiving_fumbles_lost = sum(.data$receiving_fumbles_lost)
      ) %>%
      dplyr::ungroup()
  }

  return(player_df)
}
