################################################################################
# Author: Ben Baldwin, Sebastian Carl
# Stlyeguide: styler::tidyverse_style()
################################################################################

#' Clean Play by Play Data
#'
#' @param pbp is a Data frame of play-by-play data scraped using \code{\link{fast_scraper}}.
#' @details Build columns that capture what happens on all plays, including
#' penalties, using string extraction from play description.
#' Loosely based on Ben's nflfastR guide (\url{https://mrcaseb.github.io/nflfastR/articles/beginners_guide.html})
#' but updated to work with the RS data, which has a different player format in
#' the play description; e.g. 24-M.Lynch instead of M.Lynch.
#' The function also standardizes team abbreviations so that, for example,
#' the Chargers are always represented by 'LAC' regardless of which year it was.
#' @return The input Data Frame of the paramter 'pbp' with the following columns
#' added:
#' \describe{
#' \item{success}{Binary indicator wheter epa > 0 in the given play. }
#' \item{passer}{Name of the dropback player (scrambles included) including plays with penalties.}
#' \item{rusher}{Name of the rusher (no scrambles) including plays with penalties.}
#' \item{receiver}{Name of the receiver including plays with penalties.}
#' \item{pass}{Binary indicator if the play was a pass play (sacks and scrambles included).}
#' \item{rush}{Binary indicator if the play was a rushing play.}
#' \item{special}{Binary indicator if the play was a special teams play.}
#' \item{first_down}{Binary indicator if the play ended in a first down.}
#' \item{play}{Binary indicator: 1 if the play was a 'normal' play (including penalties), 0 otherwise.}
#' \item{passer_id}{ID of the player in the 'passer' column (NOTE: ids vary pre and post 2011)}
#' \item{rusher_id}{ID of the player in the 'rusher' column (NOTE: ids vary pre and post 2011)}
#' \item{receiver_id}{ID of the player in the 'receiver' column (NOTE: ids vary pre and post 2011)}
#' \item{name}{Name of the 'passer' if it is not 'NA', or name of the 'rusher' otherwise.}
#' \item{id}{ID of the player in the 'name' column.}
#' \item{qb_epa}{Gives QB credit for EPA for up to the point where a receiver lost a fumble after a completed catch and makes EPA work more like passing yards on plays with fumbles.}
#' }
#' @export
clean_pbp <- function(pbp) {
  message('Cleaning up play-by-play. If you run this with a lot of seasons this could take a few minutes.')
  r <- pbp %>%
    dplyr::mutate(
      #get rid of extraneous spaces that mess with player name finding
      #if there is a space or dash, and then a capital letter, and then a period, and then a space, take out the space
      desc = stringr::str_replace_all(desc, "(((\\s)|(\\-))[A-Z]\\.)\\s+", "\\1"),
      success = dplyr::if_else(is.na(epa), NA_real_, dplyr::if_else(epa > 0, 1, 0)),
      passer = stringr::str_extract(desc, glue::glue('{big_parser}{pass_finder}')),
      rusher = stringr::str_extract(desc, glue::glue('{big_parser}{rush_finder}')),
      #get rusher_player_name as a measure of last resort
      #finds things like aborted snaps and "F.Last to NYG 44."
      rusher = dplyr::if_else(
        is.na(rusher) & is.na(passer) & !is.na(rusher_player_name), rusher_player_name, rusher
      ),
      receiver = stringr::str_extract(desc, glue::glue('{receiver_finder}{big_parser}')),
      #overwrite all these weird plays messing with the parser
      receiver = dplyr::case_when(
        stringr::str_detect(desc, glue::glue('{abnormal_play}')) ~ receiver_player_name,
        TRUE ~ receiver
      ),
      rusher = dplyr::case_when(
        stringr::str_detect(desc, glue::glue('{abnormal_play}')) ~ rusher_player_name,
        TRUE ~ rusher
      ),
      passer = dplyr::case_when(
        stringr::str_detect(desc, glue::glue('{abnormal_play}')) ~ passer_player_name,
        TRUE ~ passer
      ),
      #finally, for rusher, if there was already a passer (eg from scramble), set rusher to NA
      rusher = dplyr::if_else(
        !is.na(passer), NA_character_, rusher
      ),
      #if no pass is thrown, there shouldn't be a receiver
      receiver = dplyr::if_else(
        stringr::str_detect(desc, ' pass'), receiver, NA_character_
      ),
      #if there's a pass, sack, or scramble, it's a pass play
      pass = dplyr::if_else(stringr::str_detect(desc, "( pass)|(sacked)|(scramble)"), 1, 0),
      #if there's a rusher and it wasn't a QB kneel or pass play, it's a run play
      rush = dplyr::if_else(!is.na(rusher) & qb_kneel == 0 & pass == 0, 1, 0),
      #fix some common QBs with inconsistent names
      passer = dplyr::case_when(
        passer == "Jos.Allen" ~ "J.Allen",
        passer == "Alex Smith" | passer == "Ale.Smith" ~ "A.Smith",
        passer == "Ryan" & posteam == "ATL" ~ "M.Ryan",
        passer == "Tr.Brown" ~ "T.Brown",
        passer == "Sh.Hill" ~ "S.Hill",
        passer == "Matt.Moore" | passer == "Mat.Moore" ~ "M.Moore",
        passer == "Jo.Freeman" ~ "J.Freeman",
        passer == "G.Minshew" ~ "G.Minshew II",
        passer == "R.Griffin" ~ "R.Griffin III",
        passer == "Randel El" ~ "A.Randle El",
        passer == "Randle El" ~ "A.Randle El",
        passer == "Van Pelt" ~ "A.Van Pelt",
        passer == "Dom.Davis" ~ "D.Davis",
        TRUE ~ passer
      ),
      rusher = dplyr::case_when(
        rusher == "Jos.Allen" ~ "J.Allen",
        rusher == "Alex Smith" | rusher == "Ale.Smith" ~ "A.Smith",
        rusher == "Ryan" & posteam == "ATL" ~ "M.Ryan",
        rusher == "Tr.Brown" ~ "T.Brown",
        rusher == "Sh.Hill" ~ "S.Hill",
        rusher == "Matt.Moore" | rusher == "Mat.Moore" ~ "M.Moore",
        rusher == "Jo.Freeman" ~ "J.Freeman",
        rusher == "G.Minshew" ~ "G.Minshew II",
        rusher == "R.Griffin" ~ "R.Griffin III",
        rusher == "Randel El" ~ "A.Randle El",
        rusher == "Randle El" ~ "A.Randle El",
        rusher == "Van Pelt" ~ "A.Van Pelt",
        rusher == "Dom.Davis" ~ "D.Davis",
        TRUE ~ rusher
      ),
      receiver = dplyr::case_when(
        receiver == "F.R" ~ "F.Jones",
        TRUE ~ receiver
      ),
      first_down = dplyr::if_else(first_down_rush == 1 | first_down_pass == 1 | first_down_penalty == 1, 1, 0),
      # easy filter: play is 1 if a "special teams" play, or 0 otherwise
      # with thanks to Lee Sharpe for the code
      special=dplyr::if_else(play_type %in%
                       c("extra_point","field_goal","kickoff","punt"), 1, 0),
      # easy filter: play is 1 if a "normal" play (including penalties), or 0 otherwise
      # with thanks to Lee Sharpe for the code
      play=dplyr::if_else(!is.na(epa) & !is.na(posteam) &
                    desc != "*** play under review ***" &
                    substr(desc,1,8) != "Timeout " &
                    play_type %in% c("no_play","pass","run"),1,0)
    ) %>%
    #standardize team names (eg Chargers are always LAC even when they were playing in SD)
    dplyr::mutate_at(dplyr::vars(posteam, defteam, home_team, away_team, timeout_team, td_team, return_team, penalty_team), team_name_fn) %>%
    #Seb's stuff for fixing player ids
    dplyr::mutate(index = 1 : dplyr::n()) %>% # to re-sort after all the group_bys

    dplyr::group_by(passer, posteam, season) %>%
    dplyr::mutate(passer_id = dplyr::if_else(is.na(passer), NA_character_, custom_mode(passer_player_id))) %>%

    dplyr::group_by(passer_id) %>%
    dplyr::mutate(passer = dplyr::if_else(is.na(passer_id), NA_character_, custom_mode(passer))) %>%

    dplyr::group_by(rusher, posteam, season) %>%
    dplyr::mutate(rusher_id = dplyr::if_else(is.na(rusher), NA_character_, custom_mode(rusher_player_id))) %>%

    dplyr::group_by(rusher_id) %>%
    dplyr::mutate(rusher = dplyr::if_else(is.na(rusher_id), NA_character_, custom_mode(rusher))) %>%

    dplyr::group_by(receiver, posteam, season) %>%
    dplyr::mutate(receiver_id = dplyr::if_else(is.na(receiver), NA_character_, custom_mode(receiver_player_id))) %>%

    dplyr::group_by(receiver_id) %>%
    dplyr::mutate(receiver = dplyr::if_else(is.na(receiver_id), NA_character_, custom_mode(receiver))) %>%

    dplyr::ungroup() %>%
    dplyr::mutate(
      name = dplyr::if_else(!is.na(passer), passer, rusher),
      id = dplyr::if_else(!is.na(passer_id), passer_id, rusher_id)
    ) %>%
    dplyr::arrange(index) %>%
    dplyr::select(-index)

  return(r)
}

#these things are used in clean_pbp() above

#look for First[period or space]Last[maybe - or ' in last][maybe more letters in last][maybe Jr. or II or IV]
  big_parser = "(?<=)[A-Z][A-z]*(\\.|\\s)+[A-Z][A-z]*\\'*\\-*[A-Z]*[a-z]*(\\s((Jr.)|(Sr.)|I{2,3})|(IV))?"
#maybe some spaces and letters, and then a rush direction unless they fumbled
  rush_finder = '(?=\\s*[a-z]*\\s*((FUMBLES) | (left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)))'
#maybe some spaces and leters, and then pass / sack / scramble
  pass_finder = "(?=\\s*[a-z]*\\s*(( pass)|(sack)|(scramble)))"
#to or for, maybe a jersey number and a dash
  receiver_finder = "(?<=((to)|(for))\\s[:digit:]{0,2}\\-{0,1})"
#weird play finder
  abnormal_play = "(Lateral)|(lateral)|(pitches to)|(Direct snap to)|(New quarterback for)|(Aborted)|(backwards pass)|(Pass back to)|(Flea-flicker)"

# custom mode function from https://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode/8189441
custom_mode <- function(x, na.rm = TRUE) {
  if(na.rm){x = x[!is.na(x)]}
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
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

# custom mode function from https://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode/8189441
custom_mode <- function(x, na.rm = TRUE) {
  if(na.rm){x = x[!is.na(x)]}
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

#' Compute QB epa
#'
#' @param d is a Data frame of play-by-play data scraped using \code{\link{fast_scraper}}.
#' @details Add the variable 'qb_epa', which gives QB credit for EPA for up to the point where
#' a receiver lost a fumble after a completed catch and makes EPA work more
#' like passing yards on plays with fumbles
#' @export
add_qb_epa <- function(d) {

  fumbles_df <- d %>%
    dplyr::filter(complete_pass == 1 & fumble_lost == 1 & !is.na(epa)) %>%
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
      ep_old = ep
    ) %>%
    dplyr::select(-ep, -epa)

  if (nrow(fumbles_df) > 0) {
    new_ep_df <- calculate_expected_points(fumbles_df) %>%
      dplyr::mutate(ep = dplyr::if_else(change == 1, -ep, ep), fixed_epa = ep - ep_old) %>%
      dplyr::select(game_id, play_id, fixed_epa)

    d <- d %>%
      dplyr::left_join(new_ep_df, by = c("game_id", "play_id")) %>%
      dplyr::mutate(qb_epa = dplyr::if_else(!is.na(fixed_epa), fixed_epa, epa)) %>%
      dplyr::select(-fixed_epa)
  } else {
    d <- d %>% dplyr::mutate(qb_epa = epa)
  }

  return(d)
}

