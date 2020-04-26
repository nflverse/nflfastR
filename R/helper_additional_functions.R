################################################################################
# Author: Ben Baldwin
# Stlyeguide: styler::tidyverse_style()
################################################################################

#' Clean Play by Play Data
#'
#' @param pbp is a dataframe of play-by-play data scraped using \code{\link{fast_scraper}}.
#' @details Build columns that capture what happens on all plays, including
#' penalties, using string extraction from play description. The created 'name'
#' column denotes the dropback player on dropbacks or the rusher on rush attempts.
#' Loosely based on Ben's nflscrapR guide (https://gist.github.com/guga31bb/5634562c5a2a7b1e9961ac9b6c568701)
#' but updated to work with the RS data, which has a different player format in
#' the play description; e.g. 24-M.Lynch instead of M.Lynch.
#' The function also standardizes team abbreviations so that, for example,
#' the Chargers are always represented by 'LAC' regardless of which year it was
#' @export
clean_pbp <- function(pbp) {
  r <- pbp %>%
    dplyr::mutate(
      pass = dplyr::if_else(stringr::str_detect(desc, "( pass)|(sacked)|(scramble)"), 1, 0),
      rush = dplyr::if_else(stringr::str_detect(desc, "(left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)") & pass == 0, 1, 0),
      success = dplyr::if_else(is.na(epa), NA_real_, dplyr::if_else(epa > 0, 1, 0)),
      rusher_player_name =
        stringr::str_extract(desc, "(?<=)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?(?=\\s((left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)))"),
      receiver_player_name =
        stringr::str_extract(desc, "(?<=((to)|(for))\\s[:digit:]{0,2}\\-{0,1})[A-Z][A-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?"),
      passer_player_name =
        stringr::str_extract(desc, "(?<=)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?(?=\\s((pass)|(sack)|(scramble)))"),
      name = dplyr::if_else(!is.na(passer_player_name), passer_player_name, rusher_player_name),
      first_down = dplyr::if_else(first_down_rush == 1 | first_down_pass == 1 | first_down_penalty == 1, 1, 0)
    ) %>%
    #standardize team names (eg Chargers are always LAC even when they were playing in SD)
    dplyr::mutate_at(vars(posteam, defteam, home_team, away_team, timeout_team, td_team, return_team, penalty_team), team_name_fn)

  return(r)
}

#just a function to help with standardizing team abbreviations used in clean_pbp()
team_name_fn <- function(var) {
  dplyr::case_when(
    var %in% "JAC" ~ "JAX",
    var %in% "STL" ~ "LA",
    var %in% "SL" ~ "LA",
    var %in% "ARZ" ~ "ARI",
    var %in% "BLT" ~ "BAL",
    var %in% "CLV" ~ "CLE",
    var %in% "HST" ~ "HOU",
    var %in% "SD" ~ "LAC",
    var %in% "OAK" ~ "LV",
    TRUE ~ var
  )
}

#' Compute QB epa
#'
#' @param d is a dataframe of play-by-play data scraped using \code{\link{fast_scraper}}.
#' @details Add the variable 'qb_epa', which gives QB credit for EPA for up to the point where
#' a receiver lost a fumble after a completed catch and makes EPA work more
#' like passing yards on plays with fumbles
#' @export
fix_fumbles <- function(d) {
  fumbles_df <- d %>%
    dplyr::filter(complete_pass == 1 & fumble_lost == 1 & !is.na(epa)) %>%
    dplyr::select(desc, game_id, play_id, epa, posteam, half_seconds_remaining, yardline_100, down, ydstogo, yards_gained, goal_to_go, ep) %>%
    dplyr::mutate(
      down = as.numeric(down),
      # save old stuff for testing/checking
      down_old = down, ydstogo_old = ydstogo, epa_old = epa,
      # update yard line, down, yards to go from play result
      yardline_100 = yardline_100 - yards_gained, down = dplyr::if_else(yards_gained >= ydstogo, 1, down + 1),
      # if the fumble spot would have resulted in turnover on downs, need to give other team the ball and fix
      change = dplyr::if_else(down == 5, 1, 0), down = dplyr::if_else(down == 5, 1, down),
      # yards to go is 10 if its a first down, update otherwise
      ydstogo = dplyr::if_else(down == 1, 10, ydstogo - yards_gained),
      # fix yards to go for goal line (eg can't have 1st & 10 inside opponent 10 yard line)
      ydstogo = dplyr::if_else(yardline_100 < ydstogo, yardline_100, ydstogo),
      # 10 yards to go if possession change
      ydstogo = dplyr::if_else(change == 1, 10, ydstogo),
      # flip field for possession change
      yardline_100 = dplyr::if_else(change == 1, 100 - yardline_100, yardline_100),
      goal_to_go = dplyr::if_else(yardline_100 == ydstogo, 1, 0),
      ep_old = ep
    ) %>%
    dplyr::select(-ep, -epa)

  new_ep_df <- nflscrapR::calculate_expected_points(
    fumbles_df, "half_seconds_remaining", "yardline_100",
    "down", "ydstogo", "goal_to_go"
  ) %>%
    dplyr::mutate(ep = dplyr::if_else(change == 1, -ep, ep), fixed_epa = ep - ep_old) %>%
    dplyr::select(game_id, play_id, fixed_epa)

  d <- d %>%
    dplyr::left_join(new_ep_df, by = c("game_id", "play_id")) %>%
    dplyr::mutate(qb_epa = dplyr::if_else(!is.na(fixed_epa), fixed_epa, epa)) %>%
    dplyr::select(-fixed_epa)

  return(d)
}
