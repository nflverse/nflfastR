################################################################################
# Author: Ben Baldwin, Sebastian Carl
# Stlyeguide: styler::tidyverse_style()
################################################################################

#' Get official game stats
#'
#' @param df is a Data frame of play-by-play data scraped using [fast_scraper()].
#' @param weekly Defaults to TRUE. If true, returns week-by-week stats, otherwise, stats
#' for the entire dataframe.
#' @details Build columns that aggregate official passing, rushing, and receiving stats
#' at the game level or at the level of the entire df passed.
#' @return The following columns
#' \describe{
#' \item{player_id}{ID of the player. Use this to join to other sources.}
#' \item{player_name}{Name of the player}
#' \item{season}{Season if `weekly` is TRUE}
#' \item{week}{Week if `weekly` is TRUE}
#' \item{other_things}{fill this out}
#' }
#' @export
get_player_stats <- function(df, weekly = TRUE) {

  # get down to plays that count in official stats
  data <- df %>%
    dplyr::filter(
      !is.na(down),
      play_type %in% c("pass", "qb_kneel", "qb_spike", "run")
    )

  # get passing stats
  pass_df <- data %>%
    dplyr::filter(play_type %in% c("pass", "qb_spike")) %>%
    dplyr::group_by(passer_player_id, week, season) %>%
    dplyr::summarize(
      name_pass = dplyr::first(passer_player_name),
      passing_yards = sum(passing_yards, na.rm = T),
      pass_tds = sum(touchdown == 1 & td_team == posteam & complete_pass == 1),
      ints = sum(interception),
      att = sum(complete_pass == 1 | incomplete_pass == 1 | interception == 1),
      cmp = sum(complete_pass == 1),
      fumble_lost_sack = sum(fumble_lost)
    ) %>%
    dplyr::rename(player_id = passer_player_id) %>%
    dplyr::ungroup()

  # rush df 1: primary rusher
  rushes <- data %>%
    dplyr::filter(play_type %in% c("run", "qb_kneel")) %>%
    dplyr::group_by(rusher_player_id, week, season) %>%
    dplyr::summarize(
      name_rush = dplyr::first(rusher_player_name),
      yards = sum(rushing_yards, na.rm = T),
      tds = sum(touchdown == 1 & td_team == posteam),
      carries = dplyr::n(),
      fumble_lost_rush = sum(fumble_lost)
    ) %>%
    dplyr::ungroup()

  # rush df 2: lateral
  laterals <- data %>%
    dplyr::filter(!is.na(lateral_rusher_player_id)) %>%
    dplyr::group_by(lateral_rusher_player_id, week, season) %>%
    dplyr::summarize(
      lateral_yards = sum(lateral_rushing_yards, na.rm = T),
      lateral_tds = sum(touchdown == 1 & td_team == posteam),
      lateral_att = dplyr::n()
    ) %>%
    dplyr::ungroup() %>%
    dplyr::rename(
      rusher_player_id = lateral_rusher_player_id
    )

  # rush df: join
  rush_df <- rushes %>%
    dplyr::left_join(laterals, by = c("rusher_player_id", "week", "season")) %>%
    dplyr::mutate(
      lateral_yards = dplyr::if_else(is.na(lateral_yards), 0, lateral_yards),
      lateral_tds = dplyr::if_else(is.na(lateral_tds), 0L, lateral_tds)
    ) %>%
    dplyr::mutate(rushing_yards = yards + lateral_yards, tds_rush = tds + lateral_tds) %>%
    dplyr::rename(player_id = rusher_player_id) %>%
    dplyr::select(player_id, week, season, name_rush, rushing_yards, carries, tds_rush, fumble_lost_rush) %>%
    dplyr::ungroup()


  # receiver df 1: primary receiver
  rec <- data %>%
    dplyr::filter(!is.na(receiver_player_id)) %>%
    dplyr::group_by(receiver_player_id, week, season) %>%
    dplyr::summarize(
      name_receiver = dplyr::first(receiver_player_name),
      yards = sum(receiving_yards, na.rm = T),
      rec = sum(complete_pass ==1),
      tgt = dplyr::n(),
      tds = sum(touchdown == 1 & td_team == posteam),
      fumble_lost_rec = sum(fumble_lost)
    ) %>%
    dplyr::ungroup()

  # receiver df 2: lateral
  laterals <- data %>%
    dplyr::filter(!is.na(lateral_receiver_player_id)) %>%
    dplyr::group_by(lateral_receiver_player_id, week, season) %>%
    dplyr::summarize(
      lateral_yards = sum(lateral_receiving_yards, na.rm = T),
      lateral_tds = sum(touchdown == 1 & td_team == posteam),
      lateral_att = dplyr::n()
    ) %>%
    dplyr::ungroup() %>%
    dplyr::rename(
      receiver_player_id = lateral_receiver_player_id
    )

  # rec df: join
  rec_df <- rec %>%
    dplyr::left_join(laterals, by = c("receiver_player_id", "week", "season")) %>%
    dplyr::mutate(
      lateral_yards = dplyr::if_else(is.na(lateral_yards), 0, lateral_yards),
      lateral_tds = dplyr::if_else(is.na(lateral_tds), 0L, lateral_tds)
    ) %>%
    dplyr::mutate(receiving_yards = yards + lateral_yards, tds_rec = tds + lateral_tds) %>%
    dplyr::rename(player_id = receiver_player_id) %>%
    dplyr::select(player_id, week, season, name_receiver, receiving_yards, rec, tgt, tds_rec, fumble_lost_rec)

  # combine all the stats together
  player_df <- pass_df %>%
    dplyr::full_join(rush_df, by = c("player_id", "week", "season")) %>%
    dplyr::full_join(rec_df, by = c("player_id", "week", "season")) %>%
    dplyr::mutate(
      player_name = dplyr::case_when(
        !is.na(name_pass) ~ name_pass,
        !is.na(name_rush) ~ name_rush,
        TRUE ~ name_receiver
      )
    ) %>%
    dplyr::select(
      player_id, player_name, season, week, cmp, att, passing_yards, pass_tds, ints, fumble_lost_sack,
      carries, rushing_yards, tds_rush, fumble_lost_rush,
      rec, tgt, receiving_yards, tds_rec, fumble_lost_rec
    ) %>%
    replace(is.na(.), 0) %>%
    dplyr::ungroup()

  # if user doesn't want week-by-week input, aggregate the whole df
  if (weekly == FALSE) {
    player_df <- player_df %>%
      dplyr::group_by(player_id, player_name) %>%
      dplyr::summarise(
        cmp = sum(cmp),
        att = sum(att),
        passing_yards = sum(passing_yards),
        pass_tds = sum(pass_tds),
        ints = sum(ints),
        fumble_lost_sack = sum(fumble_lost_sack),
        carries = sum(carries),
        rushing_yards = sum(rushing_yards),
        tds_rush = sum(tds_rush),
        fumble_lost_rush = sum(fumble_lost_rush),
        rec = sum(rec),
        tgt = sum(tgt),
        receiving_yards = sum(receiving_yards),
        tds_rec = sum(tds_rec),
        fumble_lost_rec = sum(fumble_lost_rec)
      ) %>%
      dplyr::ungroup()
  }

  return(player_df)

}
