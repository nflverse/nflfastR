################################################################################
# Author: Sebastian Carl, Ben Baldwin
# Purpose: Function for scraping pbp data from the new NFL web site
# Code Style Guide: styler::tidyverse_style()
################################################################################

# Build a tidy version of scraped NFL data as a data.table
#
# @param id Specifies the game
get_pbp_nfl <- function(
  id,
  dir = getOption("nflfastR.raw_directory", default = NULL),
  ...
) {
  season <- substr(id, 1, 4)
  week <- as.integer(substr(id, 6, 7))

  raw_data <- fetch_raw(game_id = id, dir = dir)

  season_type <- if (
    (season <= 2020 && week <= 17) || (season >= 2021 && week <= 18)
  ) {
    "REG"
  } else {
    "POST"
  }

  game_id <- raw_data$data$viewer$gameDetail$id
  home_team <- raw_data$data$viewer$gameDetail$homeTeam$abbreviation
  away_team <- raw_data$data$viewer$gameDetail$visitorTeam$abbreviation
  home_team <- data.table::fcase(
    home_team == "JAC",
    "JAX",
    home_team == "SD",
    "LAC",
    default = home_team
  )
  away_team <- data.table::fcase(
    away_team == "JAC",
    "JAX",
    away_team == "SD",
    "LAC",
    default = away_team
  )

  # if home team and away team are the same, the game is messed up and needs fixing
  bad_game <- home_team == away_team
  if (bad_game) {
    # get correct home and away from the game ID
    id_parts <- stringi::stri_split_fixed(id, "_")
    away_team <- id_parts[[1]][3]
    home_team <- id_parts[[1]][4]
  }

  weather <- raw_data$data$viewer$gameDetail$weather$shortDescription %||%
    NA_character_
  stadium <- raw_data$data$viewer$gameDetail$stadium %||% NA_character_
  start_time <- raw_data$data$viewer$gameDetail$startTime

  game_info <- list(
    game_id = as.character(game_id),
    home_team = home_team,
    away_team = away_team,
    weather = weather,
    stadium = stadium,
    start_time = start_time
  )

  # The raw data has three description columns. We only need the one with
  # jersey numbers, so we drop the others before they go through all steps.
  plays <- raw_data$data$viewer$gameDetail$plays
  data.table::setDT(plays)
  plays[, playDescription := playDescriptionWithJerseyNumbers]
  plays[, c("playDescriptionWithJerseyNumbers", "shortDescription") := NULL]

  # We have this issue https://github.com/nflverse/nflfastR/issues/309 with 2013 postseason games
  # where the driveSequenceNumber in the plays df is NA for all plays. That prevents drive information
  # from being joined.
  # In this case, we compute our own driveSequenceNumber by incrementing a counter depending on the
  # value of driveTimeOfPossession.
  # driveTimeOfPossession will be a constant value during a drive so this should actually be accurate
  if (all(is.na(plays$driveSequenceNumber))) {
    plays[,
      driveSequenceNumber := {
        prev_top <- data.table::shift(driveTimeOfPossession)
        # a new drive starts with the first play of the first drive and
        # whenever driveTimeOfPossession changes
        drive_trigger <- data.table::fcase(
          is.na(prev_top) & !is.na(driveTimeOfPossession),
          1,
          prev_top != driveTimeOfPossession,
          1,
          default = 0
        )
        # driveSequenceNumber should be NA on plays where driveTimeOfPossession is NA
        data.table::fifelse(
          is.na(driveTimeOfPossession),
          NA_real_,
          cumsum(drive_trigger)
        )
      }
    ]
  }

  drives <- raw_data$data$viewer$gameDetail$drives
  data.table::setDT(drives)
  drives[, ydsnet := yards + yardsPenalized]
  # these are already in plays
  drives[,
    c(
      "possessionTeam.abbreviation",
      "possessionTeam.nickName",
      "possessionTeam.franchise.currentLogo.url"
    ) := NULL
  ]
  data.table::setnames(
    drives,
    paste0("drive_", clean_names_cached(names(drives)))
  )

  stats <- unnest_play_stats(plays)

  # there was a penalty on this play so these stat IDs shouldn't exist
  if (id == "2020_10_DEN_LV") {
    stats <- stats[
      which(!(stats$playId == 979 & stats$statId %in% c(8, 10, 79))),
    ]
  }

  pbp_stats <- tidy_play_stats(stats)

  # game info in the first columns, then plays, drives and play stats
  combined <- plays
  combined[, playStats := NULL]
  combined[, names(game_info) := game_info]
  data.table::setcolorder(combined, names(game_info))

  # Left joins: the rows of drives and play stats that match the plays (NA
  # without a match, like dplyr::left_join()), added to combined by reference.
  # The joins fail if a key isn't unique, because then the row numbers don't
  # fit the plays.
  drive_rows <- drives[
    combined,
    on = c(drive_order_sequence = "driveSequenceNumber"),
    which = TRUE
  ]
  drive_cols <- setdiff(names(drives), "drive_order_sequence")
  drives <- drives[drive_rows, ..drive_cols]
  combined[, (drive_cols) := drives]
  stat_rows <- pbp_stats[combined, on = c(play_id = "playId"), which = TRUE]
  stat_cols <- setdiff(names(pbp_stats), "play_id")
  pbp_stats <- pbp_stats[stat_rows, ..stat_cols]
  combined[, (stat_cols) := pbp_stats]

  normalize_raw_columns(combined)
  combined[,
    c("drive_play_count", "drive_time_of_possession", "next_play_type") := NULL
  ]
  data.table::setnames(
    combined,
    c(
      "clock_time",
      "play_type",
      "possession_team_abbreviation",
      "yard_line",
      "scoring_play",
      "drive_sequence_number",
      "game_id",
      "drive_play_count_2",
      "drive_time_of_possession_2",
      "drive_ydsnet"
    ),
    c(
      "time",
      "play_type_nfl",
      "posteam",
      "yardline",
      "sp",
      "drive",
      "nfl_api_id",
      "drive_play_count",
      "drive_time_of_possession",
      "ydsnet"
    )
  )

  # env avoids that season, week and game_id refer to columns
  combined[,
    `:=`(
      posteam_id = posteam,
      # have to do all this nonsense to make goal_to_go and yardline_side for compatibility with later functions
      yardline_side = str_split_and_extract(yardline, " ", 1),
      yardline_number = as.numeric(str_split_and_extract(yardline, " ", 2)),
      quarter_end = as.numeric(
        stringi::stri_detect_fixed(play_description, "END QUARTER")
      ),
      game_year = SEASON,
      season = SEASON,
      # this is only needed for epa and dropped later
      game_month = 11L,
      game_id = GAME_ID,
      week = WEEK,
      season_type = SEASON_TYPE,
      play_clock = as.character(play_clock),
      st_play_type = as.character(st_play_type),
      drive_real_start_time = as.character(drive_real_start_time),
      # get the safety team to ensure the correct team gets the points
      # usage of base ifelse is important here for non-scoring games (i.e. early live games)
      safety_team = ifelse(
        safety == 1,
        scoring_team_abbreviation,
        NA_character_
      ),
      # can't trust the goal_to_go variable so we overwrite it here
      goal_to_go = as.numeric(stringi::stri_detect_fixed(
        tolower(pre_play_by_play),
        "goal"
      ))
    ),
    env = list(
      SEASON = as.integer(season),
      WEEK = week,
      GAME_ID = I(id),
      SEASON_TYPE = I(season_type)
    )
  ]

  fix <- td_team_fixes[td_team_fixes$game_id == id, ]
  if (nrow(fix) > 0) {
    combined[
      play_id %in% fix$play_id & td_team != "JAX",
      td_team := fix$td_team[1]
    ]
  }

  if (id == "2012_04_NO_GB") {
    combined[play_id == 1085, time := "3:34"]
  } else if (id == "2012_16_BUF_MIA") {
    combined[play_id == 2571, time := "8:31"]
  }

  empty_strings_to_na(combined)

  # Data in 2023 pbp introduced separate "plays" for TV timeouts and two minute
  # warnings, and data in 2024 pbp separate "plays" for injury updates. These
  # mess up some of our logic. Since they are useless, we remove them here.
  # Plays with a missing description and no timeout team are removed as well.
  useless <- combined[, {
    desc <- tolower(play_description)
    is.na(timeout_team) &
      (stringi::stri_detect_regex(desc, "timeout at|two-minute") |
        stringi::stri_startswith_fixed(desc, "** injury update:"))
  }]
  if (!all(useless %in% FALSE)) {
    combined <- combined[useless %in% FALSE]
  }

  fix_posteams(combined)

  # fix for games where home_team == away_team and fields are messed up
  if (bad_game) {
    fix_bad_games(combined)
  }

  # nfl didn't fill in first downs on this game
  if (id == "2018_01_ATL_PHI") {
    combined[pass_attempt == 1 & first_down == 1, first_down_pass := 1]
    combined[rush_attempt == 1 & first_down == 1, first_down_rush := 1]
    combined[first_down == 1 & down == 3, third_down_converted := 1]
    combined[first_down == 1 & down == 4, fourth_down_converted := 1]
    combined[first_down == 0 & down == 3, third_down_failed := 1]
    combined[
      first_down == 0 &
        down == 4 &
        play_type_nfl != "FIELD_GOAL" &
        play_type_nfl != "PUNT" &
        play_type_nfl != "PENALTY",
      fourth_down_failed := 1
    ]
  }

  # remove duplicate plays. can't do this with play_id because duplicate plays
  # sometimes have different play_ids. Doing it here for every game is much
  # cheaper than for the bound data of all games.
  duplicate <- duplicated(
    combined,
    by = c("quarter", "time", "play_description", "down")
  )
  if (any(duplicate)) {
    combined <- combined[!duplicate]
  }

  # Columns of the raw data that no step uses and that aren't part of the
  # output. select_variables() would drop them at the end, dropping them here
  # saves memory in all steps.
  unused <- c(
    grep(
      "^(drive_)?(possession|scoring)_team_(id|nick_name|franchise_.+)$",
      names(combined),
      value = TRUE
    ),
    "drive_net_yards",
    "drive_yards",
    "is_big_play",
    "latest_play",
    "next_play_is_goal_to_go",
    "penalty_on_play",
    "play_review_status",
    "scoring_play_type"
  )
  unused <- intersect(unused, names(combined))
  if (length(unused) > 0) {
    combined[, (unused) := NULL]
  }
  combined[]
}

# Wrong td_team in old JAX games: a muffed punt TD and kickoff return TDs
td_team_fixes <- data.frame(
  game_id = c(
    "2011_14_TB_JAX",
    "2006_14_IND_JAX",
    "2007_17_JAX_HOU",
    "2007_17_JAX_HOU",
    "2008_09_JAX_CIN",
    "2009_15_IND_JAX",
    "2010_15_JAX_IND"
  ),
  play_id = c(1343, 2078, 1907, 2042, 3145, 1088, 3848),
  td_team = c("JAX", "JAX", "HOU", "HOU", "JAX", "IND", "IND")
)

# helper function to manually fill in fields for problematic games. Changes pbp
# by reference.
fix_bad_games <- function(pbp) {
  # (fifelse() because a missing drive_how_ended_description makes td_team
  # missing)
  pbp[,
    td_team := {
      #if team has the ball and scored, make them the scoring team
      td <- data.table::fifelse(
        drive_how_ended_description == "Touchdown" & !is.na(td_team),
        posteam,
        td_team
      )
      #if team defensive team score, fill in the right team (the team that
      #doesn't have the ball)
      data.table::fifelse(
        drive_how_ended_description != "Touchdown" & !is.na(td),
        data.table::fifelse(posteam == home_team, away_team, home_team),
        td
      )
    }
  ]
  # fill in return team. this is before we flip posteam for kickoffs
  pbp[
    !is.na(return_team),
    return_team := data.table::fifelse(
      posteam == home_team,
      away_team,
      home_team
    )
  ]
  # assign possession based on fumble_lost
  pbp[
    !is.na(fumble_recovery_1_team),
    fumble_recovery_1_team := data.table::fcase(
      fumble_lost == 1 & posteam == home_team,
      away_team,
      fumble_lost == 1 & posteam == away_team,
      home_team,
      fumble_lost == 0 & posteam == home_team,
      home_team,
      fumble_lost == 0 & posteam == away_team,
      away_team
    )
  ]
  # extract timeouts in the affected seasons from the play description
  pbp[
    !is.na(timeout_team),
    timeout_team := stringi::stri_extract_first_regex(
      play_description,
      "(?<=Timeout #[1-3] by )[:upper:]+"
    )
  ]
  pbp[]
}

# Changes pbp by reference
fix_posteams <- function(pbp) {
  # Data source switch in 2023 introduced new problems
  # 1. Definition of posteam on kick offs changed to receiving team. That's our
  #    definition and we swap teams later.
  # 2. Posteam doesn't change on the PAT after defensive TD
  #
  # We adjust both things here
  # We need the variable pre_play_by_play which usually looks like "KC  1-10  NYJ 40"
  if ("pre_play_by_play" %chin% names(pbp)) {
    # Let's be as explicit as possible about what we want to extract from the string
    # It's really only the first valid team abbreviation followed by a blank space
    valid_team_abbrs <- paste(
      c(nflfastR::teams_colors_logos$team_abbr, "OAK", "SD", "STL"),
      collapse = " |"
    )
    posteam_regex <- paste0("^", valid_team_abbrs, "(?=[:space:])")

    pbp[,
      posteam := {
        parsed_posteam <- stringi::stri_extract_first_regex(
          pre_play_by_play,
          posteam_regex
        ) |>
          stringi::stri_trim_both()
        data.table::fcase(
          stringi::stri_startswith_fixed(play_description, "Timeout "),
          NA_character_,
          is.na(parsed_posteam),
          posteam,
          play_description == "GAME",
          NA_character_,
          default = parsed_posteam
        )
      }
    ]
  }

  pbp[]
}

# Convert logical and integer columns to double and factors to character,
# normalize team abbreviations and white space in character columns, and
# clean the column names. Same result as a chain of dplyr::mutate_if() calls
# and janitor::clean_names(), but much faster on wide data with few rows.
# Changes the data.table dt by reference.
normalize_raw_columns <- function(dt) {
  for (j in seq_along(dt)) {
    x <- dt[[j]]
    if (is.logical(x) || is.integer(x)) {
      data.table::set(dt, j = j, value = as.numeric(x))
    } else if (is.factor(x) || is.character(x)) {
      # str_squish() also turns line breaks into blanks
      x <- squish(team_name_fn(as.character(x)))
      data.table::set(dt, j = j, value = x)
    }
  }
  data.table::setnames(dt, clean_names_cached(names(dt)))
  dt[]
}

# One row per stat with the columns tidy_play_stats() needs, like
# tidyr::unnest() of plays$playStats but without carrying all play columns
# along. Plays without stats have no rows. A game without any stats yet (before
# kickoff) gives an empty table.
unnest_play_stats <- function(plays) {
  ps <- plays$playStats
  n <- vapply(ps, NROW, integer(1))
  has <- which(n > 0)
  if (length(has) == 0) {
    return(tibble::tibble(
      playId = plays$playId[0],
      statId = numeric(),
      yards = integer(),
      teamAbbr = character(),
      player.displayName = character(),
      player.esbId = character()
    ))
  }
  stats <- data.table::rbindlist(ps[has], fill = TRUE)
  pick <- function(col) {
    stats[[col]] %||% rep(NA, nrow(stats))
  }
  tibble::tibble(
    playId = rep(plays$playId[has], n[has]),
    statId = as.numeric(pick("statId")),
    yards = as.integer(pick("yards")),
    teamAbbr = as.character(pick("team.abbreviation")),
    player.displayName = pick("playerName"),
    player.esbId = pick("gsisPlayer.id")
  )
}

# janitor::make_clean_names() is slow and the raw data of most games has the
# same column names, so we remember the results
clean_names_cached <- function(x) {
  key <- paste0("clean_names_", rlang::hash(x))
  if (is.null(nflfastR_cache[[key]])) {
    assign(key, janitor::make_clean_names(x), envir = nflfastR_cache)
  }
  nflfastR_cache[[key]]
}

# Changes the data.table dt by reference
empty_strings_to_na <- function(dt) {
  for (col in names(dt)) {
    x <- dt[[col]]
    if (is.character(x)) {
      i <- which(x == "")
      if (length(i) > 0) {
        data.table::set(dt, i = i, j = col, value = NA_character_)
      }
    }
  }
  dt[]
}
