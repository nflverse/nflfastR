################################################################################
# Author: Ben Baldwin, Sebastian Carl
# Stlyeguide: styler::tidyverse_style()
################################################################################

#' Get Official Game Stats
#'
#' @param pbp A Data frame of NFL play-by-play data **with decoded player IDs**
#' typically loaded with [load_pbp()] or [build_nflfastR_pbp()].
#' For ID decoding please see [decode_player_ids()].
#' @param weekly If `TRUE`, returns week-by-week stats, otherwise, stats
#' for the entire Data frame.
#' @description Build columns that aggregate official passing, rushing, and receiving stats
#' either at the game level or at the level of the entire data frame passed.
#' @return A data frame including the following columns:
#' \describe{
#' \item{player_id}{ID of the player. Use this to join to other sources.}
#' \item{player_name}{Name of the player}
#' \item{season}{Season if `weekly` is `TRUE`}
#' \item{week}{Week if `weekly` is `TRUE`}
#' \item{other_things}{fill this out}
#' }
#' @export
#' @examples
#' \donttest{
#' pbp <- nflfastR::load_pbp(2020)
#' get_player_stats(pbp)
#' }
get_player_stats <- function(pbp, weekly = TRUE) {

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
  data <- pbp %>%
    dplyr::filter(
      !is.na(.data$down),
      .data$play_type %in% c("pass", "qb_kneel", "qb_spike", "run")
    )

  # get passing stats
  pass_df <- data %>%
    dplyr::filter(.data$play_type %in% c("pass", "qb_spike")) %>%
    dplyr::group_by(.data$passer_player_id, .data$week, .data$season) %>%
    dplyr::summarize(
      name_pass = dplyr::first(.data$passer_player_name),
      passing_yards = sum(.data$passing_yards, na.rm = TRUE),
      pass_tds = sum(.data$touchdown == 1 & .data$td_team == .data$posteam & .data$complete_pass == 1),
      ints = sum(.data$interception),
      att = sum(.data$complete_pass == 1 | .data$incomplete_pass == 1 | .data$interception == 1),
      cmp = sum(.data$complete_pass == 1),
      fumble_lost_sack = sum(.data$fumble_lost)
    ) %>%
    dplyr::rename(player_id = .data$passer_player_id) %>%
    dplyr::ungroup()

  # rush df 1: primary rusher
  rushes <- data %>%
    dplyr::filter(.data$play_type %in% c("run", "qb_kneel")) %>%
    dplyr::group_by(.data$rusher_player_id, .data$week, .data$season) %>%
    dplyr::summarize(
      name_rush = dplyr::first(.data$rusher_player_name),
      yards = sum(.data$rushing_yards, na.rm = TRUE),
      tds = sum(.data$touchdown == 1 & .data$td_team == .data$posteam),
      carries = dplyr::n(),
      fumble_lost_rush = sum(.data$fumble_lost)
    ) %>%
    dplyr::ungroup()

  # rush df 2: lateral
  laterals <- data %>%
    dplyr::filter(!is.na(.data$lateral_rusher_player_id)) %>%
    dplyr::group_by(.data$lateral_rusher_player_id, .data$week, .data$season) %>%
    dplyr::summarize(
      lateral_yards = sum(.data$lateral_rushing_yards, na.rm = TRUE),
      lateral_tds = sum(.data$touchdown == 1 & .data$td_team == .data$posteam),
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
    dplyr::mutate(rushing_yards = .data$yards + .data$lateral_yards, tds_rush = .data$tds + .data$lateral_tds) %>%
    dplyr::rename(player_id = .data$rusher_player_id) %>%
    dplyr::select("player_id", "week", "season", "name_rush", "rushing_yards", "carries", "tds_rush", "fumble_lost_rush") %>%
    dplyr::ungroup()


  # receiver df 1: primary receiver
  rec <- data %>%
    dplyr::filter(!is.na(.data$receiver_player_id)) %>%
    dplyr::group_by(.data$receiver_player_id, .data$week, .data$season) %>%
    dplyr::summarize(
      name_receiver = dplyr::first(.data$receiver_player_name),
      yards = sum(.data$receiving_yards, na.rm = TRUE),
      rec = sum(.data$complete_pass == 1),
      tgt = dplyr::n(),
      tds = sum(.data$touchdown == 1 & .data$td_team == .data$posteam),
      fumble_lost_rec = sum(.data$fumble_lost)
    ) %>%
    dplyr::ungroup()

  # receiver df 2: lateral
  laterals <- data %>%
    dplyr::filter(!is.na(.data$lateral_receiver_player_id)) %>%
    dplyr::group_by(.data$lateral_receiver_player_id, .data$week, .data$season) %>%
    dplyr::summarize(
      lateral_yards = sum(.data$lateral_receiving_yards, na.rm = TRUE),
      lateral_tds = sum(.data$touchdown == 1 & .data$td_team == .data$posteam),
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
    dplyr::mutate(receiving_yards = .data$yards + .data$lateral_yards, tds_rec = .data$tds + .data$lateral_tds) %>%
    dplyr::rename(player_id = .data$receiver_player_id) %>%
    dplyr::select("player_id", "week", "season", "name_receiver", "receiving_yards", "rec", "tgt", "tds_rec", "fumble_lost_rec")

  # combine all the stats together
  player_df <- pass_df %>%
    dplyr::full_join(rush_df, by = c("player_id", "week", "season")) %>%
    dplyr::full_join(rec_df, by = c("player_id", "week", "season")) %>%
    dplyr::mutate(
      player_name = dplyr::case_when(
        !is.na(.data$name_pass) ~ .data$name_pass,
        !is.na(.data$name_rush) ~ .data$name_rush,
        TRUE ~ .data$name_receiver
      )
    ) %>%
    dplyr::select(
      "player_id", "player_name", "season", "week", "cmp", "att", "passing_yards", "pass_tds", "ints", "fumble_lost_sack",
      "carries", "rushing_yards", "tds_rush", "fumble_lost_rush",
      "rec", "tgt", "receiving_yards", "tds_rec", "fumble_lost_rec"
    )

  player_df[is.na(player_df)] <- 0

  # if user doesn't want week-by-week input, aggregate the whole df
  if (isFALSE(weekly)) {
    player_df <- player_df %>%
      dplyr::group_by(.data$player_id, .data$player_name) %>%
      dplyr::summarise(
        cmp = sum(.data$cmp),
        att = sum(.data$att),
        passing_yards = sum(.data$passing_yards),
        pass_tds = sum(.data$pass_tds),
        ints = sum(.data$ints),
        fumble_lost_sack = sum(.data$fumble_lost_sack),
        carries = sum(.data$carries),
        rushing_yards = sum(.data$rushing_yards),
        tds_rush = sum(.data$tds_rush),
        fumble_lost_rush = sum(.data$fumble_lost_rush),
        rec = sum(.data$rec),
        tgt = sum(.data$tgt),
        receiving_yards = sum(.data$receiving_yards),
        tds_rec = sum(.data$tds_rec),
        fumble_lost_rec = sum(.data$fumble_lost_rec)
      ) %>%
      dplyr::ungroup()
  }

  return(player_df)
}
