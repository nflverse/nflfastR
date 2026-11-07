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
#' Loosely based on Ben's nflfastR guide (<https://nflfastr.com/articles/beginners_guide.html>)
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

    # drop existing values of clean_pbp
    pbp <- pbp[!names(pbp) %in% drop.cols]

    # All steps work on a list of column vectors instead of dplyr verbs (much
    # faster on single games). Grouped dplyr operations use the helpers in
    # utils.R
    d <- as_column_list(pbp)
    d$aborted_play <- if_else_vec(
      stringr::str_detect(d$desc, 'Aborted'),
      1,
      0
    )
    #get rid of extraneous spaces that mess with player name finding
    #if there is a space or dash, and then a capital letter, and then a period, and then a space, take out the space
    d$desc <- stringr::str_replace_all(
      d$desc,
      "(((\\s)|(\\-))[A-Z]\\.)\\s+",
      "\\1"
    )
    d$success <- if_else_vec(
      is.na(d$epa),
      NA_real_,
      if_else_vec(d$epa > 0, 1, 0)
    )
    new_cols <- extract_name_and_number(d$desc, pass_finder, "passer")
    d[names(new_cols)] <- as.list(new_cols)
    new_cols <- extract_name_and_number(d$desc, rush_finder, "rusher")
    d[names(new_cols)] <- as.list(new_cols)
    #get rusher_player_name as a measure of last resort
    #finds things like aborted snaps and "F.Last to NYG 44."
    d$rusher <- if_else_vec(
      is.na(d$rusher) &
        is.na(d$passer) &
        !is.na(d$rusher_player_name),
      d$rusher_player_name,
      d$rusher
    )
    d$receiver <- stringr::str_extract(
      d$desc,
      glue::glue('{receiver_finder}{big_parser}')
    )
    d$receiver_jersey_number <- stringr::str_extract(
      stringr::str_extract(
        d$desc,
        glue::glue('{receiver_number}{big_parser}')
      ),
      "[:digit:]*"
    ) |>
      as.integer()
    #overwrite all these weird plays messing with the parser
    d$receiver <- case_when_vec(
      list(
        stringr::str_detect(d$desc, glue::glue('{abnormal_play}')) &
          !is.na(d$receiver_player_name),
        d$receiver_player_name
      ),
      list(
        TRUE,
        d$receiver
      )
    )
    d$rusher <- case_when_vec(
      list(
        stringr::str_detect(d$desc, glue::glue('{abnormal_play}')) &
          !is.na(d$rusher_player_name),
        d$rusher_player_name
      ),
      list(
        TRUE,
        d$rusher
      )
    )
    d$passer <- case_when_vec(
      list(
        stringr::str_detect(d$desc, glue::glue('{abnormal_play}')) &
          !is.na(d$passer_player_name),
        d$passer_player_name
      ),
      list(
        TRUE,
        d$passer
      )
    )
    # fix the plays where scramble was fixed using charting data from 1999 to 2005
    d$passer <- case_when_vec(
      list(
        is.na(d$passer) &
          d$qb_scramble == 1 &
          !is.na(d$rusher) &
          d$season <= 2005,
        d$rusher
      ),
      list(
        TRUE,
        d$passer
      )
    )
    # finally, for rusher, if there was already a passer (eg from scramble), set rusher to NA
    d$rusher <- if_else_vec(
      !is.na(d$passer),
      NA_character_,
      d$rusher
    )
    # if no pass is thrown, there shouldn't be a receiver
    d$receiver <- if_else_vec(
      stringr::str_detect(d$desc, ' pass '),
      d$receiver,
      NA_character_
    )
    # if there's a pass, sack, or scramble, it's a pass play...
    d$pass <- if_else_vec(
      stringr::str_detect(d$desc, "( pass )|(sacked)|(scramble)") |
        d$qb_scramble == 1,
      1,
      0
    )
    # ...unless it says "backward(s) pass" or "lateral pass" and there's a rusher
    d$pass <- if_else_vec(
      stringr::str_detect(
        stringr::str_to_lower(d$desc),
        "(backward pass)|(backwards pass)|(lateral pass)"
      ) &
        !is.na(d$rusher),
      0,
      d$pass
    )
    # and make sure there's no pass on a kickoff (sometimes there's forward pass on kickoff but that's not a pass play)
    d$pass <- case_when_vec(
      list(
        d$kickoff_attempt == 1,
        0
      ),
      list(
        TRUE,
        d$pass
      )
    )
    # in very rare cases, the pass logic can fail. We do a hard coded overwrite here because it's not worth the time
    # to overthink the logic to catch weird play descriptions.
    d$pass <- fix_weird_pass_plays(d$pass, d$game_id, d$play_id)
    #if there's a rusher and it wasn't a QB kneel or pass play, it's a run play
    d$rush <- if_else_vec(
      !is.na(d$rusher) & d$qb_kneel == 0 & d$pass == 0,
      1,
      0
    )
    #fix some common QBs with inconsistent names
    d$passer <- case_when_vec(
      list(
        d$passer == "Jos.Allen",
        "J.Allen"
      ),
      list(
        d$passer == "Alex Smith" | d$passer == "Ale.Smith",
        "A.Smith"
      ),
      list(
        d$passer == "Ryan" & d$posteam == "ATL",
        "M.Ryan"
      ),
      list(
        d$passer == "Tr.Brown",
        "T.Brown"
      ),
      list(
        d$passer == "Sh.Hill",
        "S.Hill"
      ),
      list(
        d$passer == "Matt.Moore" | d$passer == "Mat.Moore",
        "M.Moore"
      ),
      list(
        d$passer == "Jo.Freeman",
        "J.Freeman"
      ),
      list(
        d$passer == "G.Minshew",
        "G.Minshew II"
      ),
      list(
        d$passer == "R.Griffin",
        "R.Griffin III"
      ),
      list(
        d$passer == "Randel El",
        "A.Randle El"
      ),
      list(
        d$passer == "Randle El",
        "A.Randle El"
      ),
      list(
        d$season <= 2003 & d$passer == "Van Pelt",
        "A.Van Pelt"
      ),
      list(
        d$season > 2003 & d$passer == "Van Pelt",
        "B.Van Pelt"
      ),
      list(
        d$passer == "Dom.Davis",
        "D.Davis"
      ),
      list(
        TRUE,
        d$passer
      )
    )
    d$rusher <- case_when_vec(
      list(
        d$rusher == "D.Johnson" &
          d$posteam == "HOU" &
          d$season == 2020 &
          d$rusher_jersey_number == 31,
        "Da.Johnson"
      ),
      list(
        d$rusher == "D.Johnson" &
          d$posteam == "HOU" &
          d$season == 2020 &
          d$rusher_jersey_number == 25,
        "Du.Johnson"
      ),
      list(
        d$rusher == "Jos.Allen",
        "J.Allen"
      ),
      list(
        d$rusher == "Alex Smith" | d$rusher == "Ale.Smith",
        "A.Smith"
      ),
      list(
        d$rusher == "Ryan" & d$posteam == "ATL",
        "M.Ryan"
      ),
      list(
        d$rusher == "Tr.Brown",
        "T.Brown"
      ),
      list(
        d$rusher == "Sh.Hill",
        "S.Hill"
      ),
      list(
        d$rusher == "Matt.Moore" | d$rusher == "Mat.Moore",
        "M.Moore"
      ),
      list(
        d$rusher == "Jo.Freeman",
        "J.Freeman"
      ),
      list(
        d$rusher == "G.Minshew",
        "G.Minshew II"
      ),
      list(
        d$rusher == "R.Griffin",
        "R.Griffin III"
      ),
      list(
        d$rusher == "Randel El",
        "A.Randle El"
      ),
      list(
        d$rusher == "Randle El",
        "A.Randle El"
      ),
      list(
        d$season <= 2003 & d$rusher == "Van Pelt",
        "A.Van Pelt"
      ),
      list(
        d$season > 2003 & d$rusher == "Van Pelt",
        "B.Van Pelt"
      ),
      list(
        d$rusher == "Dom.Davis",
        "D.Davis"
      ),
      list(
        TRUE,
        d$rusher
      )
    )
    d$receiver <- case_when_vec(
      list(
        d$receiver == "F.R",
        "F.Jones"
      ),
      list(
        d$receiver_player_name == "D.Wells" &
          d$receiver_player_id == "00-0017421",
        "D.Wells"
      ),
      list(
        d$receiver_player_name == "D.Hayes" &
          d$receiver_player_id == "00-0007144",
        "D.Hayes"
      ),
      list(
        d$receiver_player_name == "DanielThomas",
        "D.Thomas"
      ),
      list(
        d$receiver_player_name == "JulioJones",
        "J.Jones"
      ),
      list(
        d$receiver_player_name == "Andre' Davis",
        "A.Davis"
      ),
      list(
        d$receiver_player_name == "A.al-Jabbar",
        "A.al-Jabbar"
      ),
      list(
        d$receiver_player_name == "A.St. Brown",
        "A.St. Brown"
      ),
      list(
        TRUE,
        d$receiver
      )
    )
    d$first_down <- if_else_vec(
      d$first_down_rush == 1 |
        d$first_down_pass == 1 |
        d$first_down_penalty == 1,
      1,
      0
    )
    # easy filter: play is 1 if a "special teams" play, or 0 otherwise
    # with thanks to Lee Sharpe for the code
    d$special <- if_else_vec(
      d$play_type %in%
        c("extra_point", "field_goal", "kickoff", "punt"),
      1,
      0
    )
    # easy filter: play is 1 if a "normal" play (including penalties), or 0 otherwise
    # with thanks to Lee Sharpe for the code
    d$play <- if_else_vec(
      !is.na(d$epa) &
        !is.na(d$posteam) &
        d$desc != "*** play under review ***" &
        substr(d$desc, 1, 8) != "Timeout " &
        d$play_type %in% c("no_play", "pass", "run"),
      1,
      0
    )

    #standardize team names (eg Chargers are always LAC even when they were playing in SD)
    for (col in c(
      "posteam",
      "defteam",
      "home_team",
      "away_team",
      "timeout_team",
      "td_team",
      "return_team",
      "penalty_team",
      "side_of_field",
      "forced_fumble_player_1_team",
      "forced_fumble_player_2_team",
      "solo_tackle_1_team",
      "solo_tackle_2_team",
      "assist_tackle_1_team",
      "assist_tackle_2_team",
      "assist_tackle_3_team",
      "assist_tackle_4_team",
      "tackle_with_assist_1_team",
      "tackle_with_assist_2_team",
      "fumbled_1_team",
      "fumbled_2_team",
      "fumble_recovery_1_team",
      "fumble_recovery_2_team",
      "yrdln",
      "end_yard_line",
      "drive_start_yard_line",
      "drive_end_yard_line"
    )) {
      d[[col]] <- team_name_fn(d[[col]])
    }

    #Seb's stuff for fixing player ids
    # the most common id of a name (per team and season), then the most common
    # name of an id
    g <- group_key(d$passer, d$posteam, d$season)
    d$passer_id <- if_else_vec(
      is.na(d$passer),
      NA_character_,
      mode_by(d$passer_player_id, g)
    )
    d$passer <- if_else_vec(
      is.na(d$passer_id),
      NA_character_,
      mode_by(d$passer, group_key(d$passer_id))
    )
    g <- group_key(d$rusher, d$posteam, d$season)
    d$rusher_id <- if_else_vec(
      is.na(d$rusher),
      NA_character_,
      mode_by(d$rusher_player_id, g)
    )
    d$rusher <- if_else_vec(
      is.na(d$rusher_id),
      NA_character_,
      mode_by(d$rusher, group_key(d$rusher_id))
    )
    g <- group_key(d$receiver, d$posteam, d$season)
    d$receiver_id <- if_else_vec(
      is.na(d$receiver),
      NA_character_,
      mode_by(d$receiver_player_id, g)
    )
    d$receiver <- if_else_vec(
      is.na(d$receiver_id),
      NA_character_,
      mode_by(d$receiver, group_key(d$receiver_id))
    )

    # if there's an aborted snap and qb didn't get a pass off,
    # then charge it to whoever charged with the fumble
    # this has to go after all the custom_mode stuff or it gets messed up
    d$rusher <- if_else_vec(
      d$aborted_play == 1 &
        is.na(d$passer) &
        !is.na(d$fumbled_1_player_name),
      d$fumbled_1_player_name,
      d$rusher
    )
    d$rusher_id <- if_else_vec(
      d$aborted_play == 1 &
        is.na(d$passer) &
        !is.na(d$fumbled_1_player_id),
      d$fumbled_1_player_id,
      d$rusher_id
    )
    d$name <- if_else_vec(!is.na(d$passer), d$passer, d$rusher)
    d$jersey_number <- if_else_vec(
      !is.na(d$passer_jersey_number),
      d$passer_jersey_number,
      d$rusher_jersey_number
    )
    d$id <- if_else_vec(
      !is.na(d$passer_id),
      d$passer_id,
      d$rusher_id
    )

    # add action player
    d$fantasy_player_name <- case_when_vec(
      list(
        !is.na(d$rusher_player_name),
        d$rusher_player_name
      ),
      list(
        is.na(d$rusher_player_name) &
          !is.na(d$receiver_player_name),
        d$receiver_player_name
      ),
      list(
        TRUE,
        NA_character_
      )
    )
    d$fantasy_player_id <- case_when_vec(
      list(
        !is.na(d$rusher_player_id),
        d$rusher_player_id
      ),
      list(
        is.na(d$rusher_player_id) &
          !is.na(d$receiver_player_id),
        d$receiver_player_id
      ),
      list(
        TRUE,
        NA_character_
      )
    )
    d$fantasy <- case_when_vec(
      list(
        !is.na(d$rusher),
        d$rusher
      ),
      list(
        is.na(d$rusher) & !is.na(d$receiver),
        d$receiver
      ),
      list(
        d$qb_scramble == 1,
        d$passer
      ),
      list(
        TRUE,
        NA_character_
      )
    )
    d$fantasy_id <- case_when_vec(
      list(
        !is.na(d$rusher_id),
        d$rusher_id
      ),
      list(
        is.na(d$rusher_id) &
          !is.na(d$receiver_id),
        d$receiver_id
      ),
      list(
        d$qb_scramble == 1,
        d$passer_id
      ),
      list(
        TRUE,
        NA_character_
      )
    )
    d$out_of_bounds <- if_else_vec(
      stringr::str_detect(d$desc, "(ran ob)|(pushed ob)|(sacked ob)"),
      1,
      0
    )

    first_posteam <- first_non_na_by(d$posteam, group_key(d$game_id))
    d$home_opening_kickoff <- if_else_vec(d$home_team == first_posteam, 1, 0)

    r <- tibble::new_tibble(d, nrow = nrow(pbp))
  }

  message_completed("Cleaning completed", ...)

  return(r)
}

#these things are used in clean_pbp() above

# look for First[period or space]Last[maybe - or ' in last][maybe more letters in last][maybe Jr. or II or IV]
big_parser <- "(?<=)[A-Z][A-z]*+(\\.|\\s)+[A-Z][A-z]*+\\'*\\-*[A-Z]*+[a-z]*+(\\s((Jr.)|(Sr.)|I{2,3})|(IV))?"
# maybe some spaces and letters, and then a rush direction unless they fumbled
rush_finder <- "(?=\\s*[a-z]*+\\s*((FUMBLES) | (left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)))"
# maybe some spaces and letters, and then pass / sack / scramble
pass_finder <- "(?=\\s*[a-z]*+\\s*(( pass)|(sack)|(scramble)))"
# to or for, maybe a jersey number and a dash
receiver_finder <- "(?<=((to)|(for))\\s[:digit:]{0,2}\\-{0,1})"
# weird play finder
abnormal_play <- "(Lateral)|(lateral)|(pitches to)|(Direct snap to)|(New quarterback for)|(Aborted)|(backwards pass)|(Pass back to)|(Flea-flicker)"
# look for 1-2 numbers before a dash
number_parser <- "((?<=)[:digit:]{1,2}(-))?"
# special case for receivers
receiver_number <- "(?<=((to)|(for))\\s)[:digit:]{0,2}\\-{0,1}"

# Extract a player name and the jersey number in front of it (e.g. "12-T.Brady")
# with one regex pass. Returns a tibble with the columns <prefix> and
# <prefix>_jersey_number that dplyr::mutate() unpacks.
# The name is the same as with the pattern '{big_parser}{finder}' because the
# optional number in front can only match digits and a dash.
extract_name_and_number <- function(desc, finder, prefix) {
  # number_parser has 2 groups, so the name is the 3rd group (4th column)
  m <- stringr::str_match(
    desc,
    glue::glue('{number_parser}({big_parser}){finder}')
  )
  out <- tibble::tibble(
    name = m[, 4],
    number = as.integer(stringr::str_extract(m[, 2], "[:digit:]*"))
  )
  names(out) <- c(prefix, paste0(prefix, "_jersey_number"))
  out
}

# These columns are being generated by clean_pbp and the function tries to drop
# them in case it is being used on a pbp dataset where the columns already exist
drop.cols <- c(
  "success",
  "passer",
  "rusher",
  "receiver",
  "pass",
  "rush",
  "special",
  "first_down",
  "play",
  "passer_id",
  "rusher_id",
  "receiver_id",
  "name",
  "id",
  "passer_jersey_number",
  "rusher_jersey_number",
  "receiver_jersey_number",
  "jersey_number",
  "aborted_play",
  "fantasy",
  "fantasy_id",
  "fantasy_player_name",
  "fantasy_player_id",
  "out_of_bounds"
)

# fixes team names on columns with yard line
# example: 'SD 49' --> 'LAC 49'
# thanks to awgymer for the contribution:
# https://github.com/nflverse/nflfastR/issues/29#issuecomment-654592195
team_name_replacements <- c(
  "JAC" = "JAX",
  "STL" = "LA",
  "SL" = "LA",
  "LAR" = "LA",
  "ARZ" = "ARI",
  "AZ" = "ARI",
  "BLT" = "BAL",
  "CLV" = "CLE",
  "HST" = "HOU",
  "SD" = "LAC",
  "OAK" = "LV"
)

team_name_pattern <- paste(names(team_name_replacements), collapse = "|")

team_name_fn <- function(var) {
  # Most columns contain no old abbreviation, so we skip the replacements for
  # them. No replacement creates an old abbreviation, so the result is the same.
  if (
    is.character(var) &&
      !any(stringr::str_detect(var, team_name_pattern), na.rm = TRUE)
  ) {
    return(var)
  }
  stringr::str_replace_all(var, team_name_replacements)
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
    pbp <- pbp |> dplyr::select(-dplyr::any_of("qb_epa"))

    # completed passes with a lost fumble, there are none in most games
    fumble_rows <- is_true(
      pbp$complete_pass == 1 &
        pbp$fumble_lost == 1 &
        !is.na(pbp$epa) &
        !is.na(pbp$down)
    )
    if (!any(fumble_rows)) {
      pbp$qb_epa <- pbp$epa
    } else {
      fumbles_df <- pbp |>
        dplyr::filter(
          .data$complete_pass == 1 &
            .data$fumble_lost == 1 &
            !is.na(.data$epa) &
            !is.na(.data$down)
        ) |>
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
          down = dplyr::if_else(
            .data$yards_gained >= .data$ydstogo,
            1,
            .data$down + 1
          ),
          # if the fumble spot would have resulted in turnover on downs, need to give other team the ball and fix
          change = dplyr::if_else(.data$down == 5, 1, 0),
          down = dplyr::if_else(.data$down == 5, 1, .data$down),
          # yards to go is 10 if its a first down, update otherwise
          ydstogo = dplyr::if_else(
            .data$down == 1,
            10,
            .data$ydstogo - .data$yards_gained
          ),
          # 10 yards to go if possession change
          ydstogo = dplyr::if_else(.data$change == 1, 10, .data$ydstogo),
          # flip field and timeouts for possession change
          yardline_100 = dplyr::if_else(
            .data$change == 1,
            100 - .data$yardline_100,
            .data$yardline_100
          ),
          posteam_timeouts_remaining = dplyr::if_else(
            .data$change == 1,
            .data$defeam_timeouts_pre,
            .data$posteam_timeouts_pre
          ),
          defteam_timeouts_remaining = dplyr::if_else(
            .data$change == 1,
            .data$posteam_timeouts_pre,
            .data$defeam_timeouts_pre
          ),
          # fix yards to go for goal line (eg can't have 1st & 10 inside opponent 10 yard line)
          ydstogo = dplyr::if_else(
            .data$yardline_100 < .data$ydstogo,
            .data$yardline_100,
            .data$ydstogo
          ),
          ep_old = .data$ep
        ) |>
        dplyr::select(
          "game_id",
          "play_id",
          "season",
          "home_team",
          "posteam",
          "roof",
          "half_seconds_remaining",
          "yardline_100",
          "down",
          "ydstogo",
          "posteam_timeouts_remaining",
          "defteam_timeouts_remaining",
          "down_old",
          "ep_old",
          "change"
        )

      new_ep_df <- calculate_expected_points(fumbles_df) |>
        dplyr::mutate(
          ep = dplyr::if_else(.data$change == 1, -.data$ep, .data$ep),
          fixed_epa = .data$ep - .data$ep_old
        ) |>
        dplyr::select("game_id", "play_id", "fixed_epa")

      pbp <- pbp |>
        dplyr::left_join(new_ep_df, by = c("game_id", "play_id")) |>
        dplyr::mutate(
          qb_epa = dplyr::if_else(
            !is.na(.data$fixed_epa),
            .data$fixed_epa,
            .data$epa
          )
        ) |>
        dplyr::select(-"fixed_epa")
    }
  }

  message_completed("added qb_epa", ...)

  return(pbp)
}

# Function that fixes false "pass" positives in some hard coded plays where
# the parser logic reached its limit
fix_weird_pass_plays <- function(pass, game_id, play_id) {
  combined_id <- paste(game_id, play_id, sep = "_")
  false_positives <- c(
    "1999_01_ARI_PHI_1611",
    "1999_01_SF_JAX_1788",
    "1999_01_SF_JAX_2081",
    "1999_11_ATL_TB_1740",
    "2001_09_MIN_PHI_1307",
    "2001_14_NE_BUF_452",
    "2002_16_PIT_TB_527",
    "2003_02_HOU_NO_3924",
    "2003_15_PIT_NYJ_873",
    "2004_05_BUF_NYJ_2555",
    "2005_07_SD_PHI_321",
    "2011_02_STL_NYG_1369",
    "2016_05_NE_CLE_912",
    "2016_06_CAR_NO_2690",
    "2020_10_BAL_NE_2013"
  )
  data.table::fifelse(combined_id %chin% false_positives, 0, pass, pass)
}
