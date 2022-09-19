################################################################################
# Author: Ben Baldwin, Sebastian Carl, Tan Ho
# Stlyeguide: styler::tidyverse_style()
################################################################################

#' Clean Play by Play Data
#'
#' @param pbp is a Data frame of play-by-play data scraped using [fast_scraper()].
#' @param ... Additional arguments passed to a message function (for internal use).
#' @details Build columns that capture what happens on all plays, including
#' penalties, using string extraction from play description.
#' Loosely based on Ben's nflfastR guide (<https://www.nflfastr.com/articles/beginners_guide.html>)
#' but updated to work with the RS data, which has a different player format in
#' the play description; e.g. 24-M.Lynch instead of M.Lynch.
#' The function also standardizes team abbreviations so that, for example,
#' the Chargers are always represented by 'LAC' regardless of which year it was.
#' Starting in 2022, play-by-play data was missing gsis player IDs of rookies.
#' This functions tries to fix as many as possible.
#' @seealso For information on parallel processing and progress updates please
#' see [nflfastR].
#' @return The input Data Frame of the parameter 'pbp' with the following columns
#' added:
#' \describe{
#' \item{success}{Binary indicator wheter epa > 0 in the given play. }
#' \item{passer}{Name of the dropback player (scrambles included) including plays with penalties.}
#' \item{passer_jersey_number}{Jersey number of the passer.}
#' \item{rusher}{Name of the rusher (no scrambles) including plays with penalties.}
#' \item{rusher_jersey_number}{Jersey number of the rusher.}
#' \item{receiver}{Name of the receiver including plays with penalties.}
#' \item{receiver_jersey_number}{Jersey number of the receiver.}
#' \item{pass}{Binary indicator if the play was a pass play (sacks and scrambles included).}
#' \item{rush}{Binary indicator if the play was a rushing play.}
#' \item{special}{Binary indicator if the play was a special teams play.}
#' \item{first_down}{Binary indicator if the play ended in a first down.}
#' \item{aborted_play}{Binary indicator if the play description indicates "Aborted".}
#' \item{play}{Binary indicator: 1 if the play was a 'normal' play (including penalties), 0 otherwise.}
#' \item{passer_id}{ID of the player in the 'passer' column.}
#' \item{rusher_id}{ID of the player in the 'rusher' column.}
#' \item{receiver_id}{ID of the player in the 'receiver' column.}
#' \item{name}{Name of the 'passer' if it is not 'NA', or name of the 'rusher' otherwise.}
#' \item{fantasy}{Name of the rusher on rush plays or receiver on pass plays.}
#' \item{fantasy_id}{ID of the rusher on rush plays or receiver on pass plays.}
#' \item{fantasy_player_name}{Name of the rusher on rush plays or receiver on pass plays (from official stats).}
#' \item{fantasy_player_id}{ID of the rusher on rush plays or receiver on pass plays (from official stats).}
#' \item{jersey_number}{Jersey number of the player listed in the 'name' column.}
#' \item{id}{ID of the player in the 'name' column.}
#' \item{out_of_bounds}{= 1 if play description contains "ran ob", "pushed ob", or "sacked ob"; = 0 otherwise.}
#' \item{home_opening_kickoff}{= 1 if the home team received the opening kickoff, 0 otherwise.}
#' }
#' @export
clean_pbp <- function(pbp, ...) {
  if (nrow(pbp) == 0) {
    user_message("Nothing to clean. Return passed data frame.", "info")
    r <- pbp
  } else {
    user_message("Cleaning up play-by-play...", "todo")

    if(any(pbp$season >= 2022)){

      # user_message("Loading pbp player ID patch files", "info")

      patch_seasons <- unique(pbp$season[pbp$season >= 2022])

      patch_ids <- nflreadr::load_from_url(
        glue::glue("https://github.com/nflverse/nflverse-data/releases/download/misc/pbp_patch_ids_{patch_seasons}.rds")
      ) %>% suppressMessages()

      patchable_ids <- pbp  %>%
        dplyr::select(
          dplyr::any_of(c(
            "game_id", "play_id",
            "passer_id", "passer_name" = "passer",
            "receiver_id", "receiver_name" = "receiver",
            "rusher_id", "rusher_name" = "rusher",
            "fantasy_id", "fantasy_name" = "fantasy",
            "fantasy_player_name"
          )),
          dplyr::matches("player_id|player_name")
        )  %>%
        tidyr::pivot_longer(
          cols = -c("game_id","play_id"),
          names_to = c("stat",".value"),
          names_pattern = c("(.+)_(id|name)"),
          values_drop_na = TRUE
        )  %>%
        dplyr::filter(is.na(.data$id)) %>%
        dplyr::left_join(patch_ids, by = c("game_id","play_id","name"))  %>%
        dplyr::mutate(
          id = dplyr::coalesce(.data$id,.data$gsis_id),
          gsis_id = NULL,
          club_code = NULL,
          name = NULL
        )  %>%
        tidyr::pivot_wider(
          names_from = "stat",
          values_from = "id",
          names_glue = "{stat}_id"
        )

      if(nrow(patchable_ids) > 0){
        pbp <- tibble::tibble(pbp)  %>%
          dplyr::rows_patch(patchable_ids, by = c("game_id","play_id"))
      }

      # cli::cli_alert_success("{my_time()} | Patched {nrow(patchable_ids)} missing gsis_id field{?s}")

    }

    # drop existing values of clean_pbp
    pbp <- pbp %>% dplyr::select(-tidyselect::any_of(drop.cols))

    r <- pbp %>%
      dplyr::mutate(
        aborted_play = dplyr::if_else(stringr::str_detect(.data$desc, 'Aborted'), 1, 0),
        #get rid of extraneous spaces that mess with player name finding
        #if there is a space or dash, and then a capital letter, and then a period, and then a space, take out the space
        desc = stringr::str_replace_all(.data$desc, "(((\\s)|(\\-))[A-Z]\\.)\\s+", "\\1"),
        success = dplyr::if_else(is.na(.data$epa), NA_real_, dplyr::if_else(.data$epa > 0, 1, 0)),
        passer = stringr::str_extract(.data$desc, glue::glue('{big_parser}{pass_finder}')),
        passer_jersey_number = stringr::str_extract(stringr::str_extract(.data$desc, glue::glue('{number_parser}{big_parser}{pass_finder}')), "[:digit:]*") %>% as.integer(),
        rusher = stringr::str_extract(.data$desc, glue::glue('{big_parser}{rush_finder}')),
        rusher_jersey_number = stringr::str_extract(stringr::str_extract(.data$desc, glue::glue('{number_parser}{big_parser}{rush_finder}')), "[:digit:]*") %>% as.integer(),
        #get rusher_player_name as a measure of last resort
        #finds things like aborted snaps and "F.Last to NYG 44."
        rusher = dplyr::if_else(
          is.na(.data$rusher) & is.na(.data$passer) & !is.na(.data$rusher_player_name), .data$rusher_player_name, .data$rusher
        ),
        receiver = stringr::str_extract(.data$desc, glue::glue('{receiver_finder}{big_parser}')),
        receiver_jersey_number = stringr::str_extract(stringr::str_extract(.data$desc, glue::glue('{receiver_number}{big_parser}')), "[:digit:]*") %>% as.integer(),
        #overwrite all these weird plays messing with the parser
        receiver = dplyr::case_when(
          stringr::str_detect(.data$desc, glue::glue('{abnormal_play}')) ~ .data$receiver_player_name,
          TRUE ~ .data$receiver
        ),
        rusher = dplyr::case_when(
          stringr::str_detect(.data$desc, glue::glue('{abnormal_play}')) ~ .data$rusher_player_name,
          TRUE ~ .data$rusher
        ),
        passer = dplyr::case_when(
          stringr::str_detect(.data$desc, glue::glue('{abnormal_play}')) ~ .data$passer_player_name,
          TRUE ~ .data$passer
        ),
        # fix the plays where scramble was fixed using charting data in 2005
        passer = dplyr::case_when(
          is.na(.data$passer) & .data$qb_scramble == 1 & !is.na(.data$rusher) & .data$season == 2005 ~ .data$rusher,
          TRUE ~ .data$passer
        ),
        # finally, for rusher, if there was already a passer (eg from scramble), set rusher to NA
        rusher = dplyr::if_else(
          !is.na(.data$passer), NA_character_, .data$rusher
        ),
        # if no pass is thrown, there shouldn't be a receiver
        receiver = dplyr::if_else(
          stringr::str_detect(.data$desc, ' pass '), .data$receiver, NA_character_
        ),
        # if there's a pass, sack, or scramble, it's a pass play...
        pass = dplyr::if_else(stringr::str_detect(.data$desc, "( pass )|(sacked)|(scramble)") | .data$qb_scramble == 1, 1, 0),
        # ...unless it says "backwards pass" and there's a rusher
        pass = dplyr::if_else(
          stringr::str_detect(.data$desc, "(backward pass)|(Backward pass)") & !is.na(.data$rusher),
          0, .data$pass
        ),
        # and make sure there's no pass on a kickoff (sometimes there's forward pass on kickoff but that's not a pass play)
        pass = dplyr::case_when(
          .data$kickoff_attempt == 1 ~ 0,
          TRUE ~ .data$pass
        ),
        #if there's a rusher and it wasn't a QB kneel or pass play, it's a run play
        rush = dplyr::if_else(!is.na(.data$rusher) & .data$qb_kneel == 0 & .data$pass == 0, 1, 0),
        #fix some common QBs with inconsistent names
        passer = dplyr::case_when(
          passer == "Jos.Allen" ~ "J.Allen",
          passer == "Alex Smith" | passer == "Ale.Smith" ~ "A.Smith",
          passer == "Ryan" & .data$posteam == "ATL" ~ "M.Ryan",
          passer == "Tr.Brown" ~ "T.Brown",
          passer == "Sh.Hill" ~ "S.Hill",
          passer == "Matt.Moore" | passer == "Mat.Moore" ~ "M.Moore",
          passer == "Jo.Freeman" ~ "J.Freeman",
          passer == "G.Minshew" ~ "G.Minshew II",
          passer == "R.Griffin" ~ "R.Griffin III",
          passer == "Randel El" ~ "A.Randle El",
          passer == "Randle El" ~ "A.Randle El",
          season <= 2003 & passer == "Van Pelt" ~ "A.Van Pelt",
          season > 2003 & passer == "Van Pelt" ~ "B.Van Pelt",
          passer == "Dom.Davis" ~ "D.Davis",
          TRUE ~ .data$passer
        ),
        rusher = dplyr::case_when(
          rusher == "D.Johnson" & posteam == "HOU" & season == 2020 & rusher_jersey_number == 31 ~ "Da.Johnson",
          rusher == "D.Johnson" & posteam == "HOU" & season == 2020 & rusher_jersey_number == 25 ~ "Du.Johnson",
          rusher == "Jos.Allen" ~ "J.Allen",
          rusher == "Alex Smith" | rusher == "Ale.Smith" ~ "A.Smith",
          rusher == "Ryan" & .data$posteam == "ATL" ~ "M.Ryan",
          rusher == "Tr.Brown" ~ "T.Brown",
          rusher == "Sh.Hill" ~ "S.Hill",
          rusher == "Matt.Moore" | rusher == "Mat.Moore" ~ "M.Moore",
          rusher == "Jo.Freeman" ~ "J.Freeman",
          rusher == "G.Minshew" ~ "G.Minshew II",
          rusher == "R.Griffin" ~ "R.Griffin III",
          rusher == "Randel El" ~ "A.Randle El",
          rusher == "Randle El" ~ "A.Randle El",
          season <= 2003 & rusher == "Van Pelt" ~ "A.Van Pelt",
          season > 2003 & rusher == "Van Pelt" ~ "B.Van Pelt",
          rusher == "Dom.Davis" ~ "D.Davis",
          TRUE ~ rusher
        ),
        receiver = dplyr::case_when(
          receiver == "F.R" ~ "F.Jones",
          receiver_player_name == "D.Wells" & receiver_player_id == "00-0017421" ~ "D.Wells",
          receiver_player_name == "D.Hayes" & receiver_player_id == "00-0007144" ~ "D.Hayes",
          receiver_player_name == "DanielThomas" ~ "D.Thomas",
          receiver_player_name == "JulioJones" ~ "J.Jones",
          receiver_player_name == "Andre' Davis" ~ "A.Davis",
          receiver_player_name == "A.al-Jabbar" ~ "A.al-Jabbar",
          receiver_player_name == "A.St. Brown" ~ "A.St. Brown",
          TRUE ~ receiver
        ),
        first_down = dplyr::if_else(.data$first_down_rush == 1 | .data$first_down_pass == 1 | .data$first_down_penalty == 1, 1, 0),
        # easy filter: play is 1 if a "special teams" play, or 0 otherwise
        # with thanks to Lee Sharpe for the code
        special = dplyr::if_else(.data$play_type %in%
                                   c("extra_point","field_goal","kickoff","punt"), 1, 0),
        # easy filter: play is 1 if a "normal" play (including penalties), or 0 otherwise
        # with thanks to Lee Sharpe for the code
        play = dplyr::if_else(!is.na(.data$epa) & !is.na(.data$posteam) &
                                .data$desc != "*** play under review ***" &
                                substr(.data$desc,1,8) != "Timeout " &
                                .data$play_type %in% c("no_play","pass","run"),1,0)
      ) %>%
      #standardize team names (eg Chargers are always LAC even when they were playing in SD)
      dplyr::mutate_at(dplyr::vars(
        "posteam", "defteam", "home_team", "away_team", "timeout_team", "td_team", "return_team", "penalty_team",
        "side_of_field", "forced_fumble_player_1_team", "forced_fumble_player_2_team",
        "solo_tackle_1_team", "solo_tackle_2_team",
        "assist_tackle_1_team", "assist_tackle_2_team", "assist_tackle_3_team", "assist_tackle_4_team",
        "tackle_with_assist_1_team", "tackle_with_assist_2_team",
        "fumbled_1_team", "fumbled_2_team", "fumble_recovery_1_team", "fumble_recovery_2_team",
        "yrdln", "end_yard_line", "drive_start_yard_line", "drive_end_yard_line"
      ), team_name_fn) %>%

      #Seb's stuff for fixing player ids
      dplyr::mutate(index = 1 : dplyr::n()) %>% # to re-sort after all the group_bys

      dplyr::group_by(.data$passer, .data$posteam, .data$season) %>%
      dplyr::mutate(
        passer_id = dplyr::if_else(is.na(.data$passer), NA_character_, custom_mode(.data$passer_player_id))
      ) %>%

      dplyr::group_by(.data$passer_id) %>%
      dplyr::mutate(passer = dplyr::if_else(is.na(.data$passer_id), NA_character_, custom_mode(.data$passer))) %>%

      dplyr::group_by(.data$rusher, .data$posteam, .data$season) %>%
      dplyr::mutate(
        rusher_id = dplyr::if_else(is.na(.data$rusher), NA_character_, custom_mode(.data$rusher_player_id))
      ) %>%

      dplyr::group_by(.data$rusher_id) %>%
      dplyr::mutate(rusher = dplyr::if_else(is.na(.data$rusher_id), NA_character_, custom_mode(.data$rusher))) %>%

      dplyr::group_by(.data$receiver, .data$posteam, .data$season) %>%
      dplyr::mutate(
        receiver_id = dplyr::if_else(is.na(.data$receiver), NA_character_, custom_mode(.data$receiver_player_id))
      ) %>%

      dplyr::group_by(.data$receiver_id) %>%
      dplyr::mutate(receiver = dplyr::if_else(is.na(.data$receiver_id), NA_character_, custom_mode(.data$receiver))) %>%

      dplyr::ungroup() %>%
      dplyr::mutate(
        # if there's an aborted snap and qb didn't get a pass off,
        # then charge it to whoever charged with the fumble
        # this has to go after all the custom_mode stuff or it gets messed up
        rusher = dplyr::if_else(
          .data$aborted_play == 1 & is.na(.data$passer) & !is.na(.data$fumbled_1_player_name),
          .data$fumbled_1_player_name, .data$rusher
        ),
        rusher_id = dplyr::if_else(
          .data$aborted_play == 1 & is.na(.data$passer) & !is.na(.data$fumbled_1_player_id),
          .data$fumbled_1_player_id, .data$rusher_id
        ),

        name = dplyr::if_else(!is.na(.data$passer), .data$passer, .data$rusher),
        jersey_number = dplyr::if_else(!is.na(.data$passer_jersey_number), .data$passer_jersey_number, .data$rusher_jersey_number),
        id = dplyr::if_else(!is.na(.data$passer_id), .data$passer_id, .data$rusher_id)
      ) %>%
      dplyr::arrange(.data$index) %>%
      dplyr::select(-"index") %>%
      # add action player
      dplyr::mutate(
        fantasy_player_name = case_when(
          !is.na(.data$rusher_player_name) ~ .data$rusher_player_name,
          is.na(.data$rusher_player_name) & !is.na(.data$receiver_player_name) ~ .data$receiver_player_name,
          TRUE ~ NA_character_
        ),
        fantasy_player_id = case_when(
          !is.na(.data$rusher_player_id) ~ .data$rusher_player_id,
          is.na(.data$rusher_player_id) & !is.na(.data$receiver_player_id) ~ .data$receiver_player_id,
          TRUE ~ NA_character_
        ),
        fantasy = case_when(
          !is.na(.data$rusher) ~ .data$rusher,
          is.na(.data$rusher) & !is.na(.data$receiver) ~ .data$receiver,
          .data$qb_scramble == 1 ~ .data$passer,
          TRUE ~ NA_character_
        ),
        fantasy_id = case_when(
          !is.na(.data$rusher_id) ~ .data$rusher_id,
          is.na(.data$rusher_id) & !is.na(.data$receiver_id) ~ .data$receiver_id,
          .data$qb_scramble == 1 ~ .data$passer_id,
          TRUE ~ NA_character_
        ),
        out_of_bounds = dplyr::if_else(
          stringr::str_detect(.data$desc, "(ran ob)|(pushed ob)|(sacked ob)"), 1, 0
        )
      ) %>%
      dplyr::group_by(.data$game_id) %>%
      dplyr::mutate(
        home_opening_kickoff = dplyr::if_else(.data$home_team == dplyr::first(stats::na.omit(.data$posteam)), 1, 0)
      ) %>%
      dplyr::ungroup()

  }

  message_completed("Cleaning completed", ...)

  return(r)
}

#these things are used in clean_pbp() above

# look for First[period or space]Last[maybe - or ' in last][maybe more letters in last][maybe Jr. or II or IV]
big_parser <- "(?<=)[A-Z][A-z]*+(\\.|\\s)+[A-Z][A-z]*+\\'*\\-*[A-Z]*+[a-z]*+(\\s((Jr.)|(Sr.)|I{2,3})|(IV))?"
# maybe some spaces and letters, and then a rush direction unless they fumbled
rush_finder <- "(?=\\s*[a-z]*+\\s*((FUMBLES) | (left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)))"
# maybe some spaces and leters, and then pass / sack / scramble
pass_finder <- "(?=\\s*[a-z]*+\\s*(( pass)|(sack)|(scramble)))"
# to or for, maybe a jersey number and a dash
receiver_finder <- "(?<=((to)|(for))\\s[:digit:]{0,2}\\-{0,1})"
# weird play finder
abnormal_play <- "(Lateral)|(lateral)|(pitches to)|(Direct snap to)|(New quarterback for)|(Aborted)|(backwards pass)|(Pass back to)|(Flea-flicker)"
# look for 1-2 numbers before a dash
number_parser <- "((?<=)[:digit:]{1,2}(-))?"
# special case for receivers
receiver_number <- "(?<=((to)|(for))\\s)[:digit:]{0,2}\\-{0,1}"

# These columns are being generated by clean_pbp and the function tries to drop
# them in case it is being used on a pbp dataset where the columns already exist
drop.cols <- c(
  "success", "passer", "rusher", "receiver", "pass", "rush", "special",
  "first_down", "play", "passer_id", "rusher_id", "receiver_id", "name", "id",
  "passer_jersey_number", "rusher_jersey_number", "receiver_jersey_number",
  "jersey_number", "aborted_play", "fantasy", "fantasy_id", "fantasy_player_name",
  "fantasy_player_id", "out_of_bounds"
)

# fixes team names on columns with yard line
# example: 'SD 49' --> 'LAC 49'
# thanks to awgymer for the contribution:
# https://github.com/nflverse/nflfastR/issues/29#issuecomment-654592195
team_name_fn <- function(var) {
  stringr::str_replace_all(
    var,
    c(
      "JAC" = "JAX",
      "STL" = "LA",
      "SL" = "LA",
      "ARZ" = "ARI",
      "BLT" = "BAL",
      "CLV" = "CLE",
      "HST" = "HOU",
      "SD" = "LAC",
      "OAK" = "LV"
    )
  )
}

#' Compute QB epa
#'
#' @inheritParams clean_pbp
#' @details Add the variable 'qb_epa', which gives QB credit for EPA for up to the point where
#' a receiver lost a fumble after a completed catch and makes EPA work more
#' like passing yards on plays with fumbles
#' @export
add_qb_epa <- function(pbp, ...) {

  if (nrow(pbp) == 0) {
    user_message("Nothing to do. Return passed data frame.", "info")
  } else {
    # drop existing values of clean_pbp
    pbp <- pbp %>% dplyr::select(-tidyselect::any_of("qb_epa"))

    fumbles_df <- pbp %>%
      dplyr::filter(.data$complete_pass == 1 & .data$fumble_lost == 1 & !is.na(.data$epa) & !is.na(.data$down)) %>%
      dplyr::mutate(
        half_seconds_remaining = dplyr::if_else(
          .data$half_seconds_remaining <= 6,
          0,
          .data$half_seconds_remaining - 6
        ),
        down = as.numeric(.data$down),
        # save old stuff for testing/checking
        posteam_timeouts_pre = .data$posteam_timeouts_remaining,
        defeam_timeouts_pre = .data$defteam_timeouts_remaining,
        down_old = .data$down,
        ydstogo_old = .data$ydstogo,
        epa_old = .data$epa,
        # update yard line, down, yards to go from play result
        yardline_100 = .data$yardline_100 - .data$yards_gained,
        down = dplyr::if_else(.data$yards_gained >= .data$ydstogo, 1, .data$down + 1),
        # if the fumble spot would have resulted in turnover on downs, need to give other team the ball and fix
        change = dplyr::if_else(.data$down == 5, 1, 0), down = dplyr::if_else(.data$down == 5, 1, .data$down),
        # yards to go is 10 if its a first down, update otherwise
        ydstogo = dplyr::if_else(.data$down == 1, 10, .data$ydstogo - .data$yards_gained),
        # 10 yards to go if possession change
        ydstogo = dplyr::if_else(.data$change == 1, 10, .data$ydstogo),
        # flip field and timeouts for possession change
        yardline_100 = dplyr::if_else(.data$change == 1, 100 - .data$yardline_100, .data$yardline_100),
        posteam_timeouts_remaining = dplyr::if_else(.data$change == 1,
                                                    .data$defeam_timeouts_pre,
                                                    .data$posteam_timeouts_pre),
        defteam_timeouts_remaining = dplyr::if_else(.data$change == 1,
                                                    .data$posteam_timeouts_pre,
                                                    .data$defeam_timeouts_pre),
        # fix yards to go for goal line (eg can't have 1st & 10 inside opponent 10 yard line)
        ydstogo = dplyr::if_else(.data$yardline_100 < .data$ydstogo, .data$yardline_100, .data$ydstogo),
        ep_old = .data$ep
      ) %>%
      dplyr::select(
        "game_id", "play_id",
        "season", "home_team", "posteam", "roof", "half_seconds_remaining",
        "yardline_100", "down", "ydstogo",
        "posteam_timeouts_remaining", "defteam_timeouts_remaining",
        "down_old", "ep_old", "change"
      )

    if (nrow(fumbles_df) > 0) {
      new_ep_df <- calculate_expected_points(fumbles_df) %>%
        dplyr::mutate(ep = dplyr::if_else(.data$change == 1, -.data$ep, .data$ep), fixed_epa = .data$ep - .data$ep_old) %>%
        dplyr::select("game_id", "play_id", "fixed_epa")

      pbp <- pbp %>%
        dplyr::left_join(new_ep_df, by = c("game_id", "play_id")) %>%
        dplyr::mutate(qb_epa = dplyr::if_else(!is.na(.data$fixed_epa), .data$fixed_epa, .data$epa)) %>%
        dplyr::select(-"fixed_epa")
    } else {
      pbp <- pbp %>% dplyr::mutate(qb_epa = .data$epa)
    }
  }

  message_completed("added qb_epa", ...)

  return(pbp)
}

