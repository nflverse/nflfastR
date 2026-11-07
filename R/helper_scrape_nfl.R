################################################################################
# Author: Sebastian Carl, Ben Baldwin
# Purpose: Function for scraping pbp data from the new NFL web site
# Code Style Guide: styler::tidyverse_style()
################################################################################

# Build a tidy version of scraped NFL data
#
# @param id Specifies the game
get_pbp_nfl <- function(
  id,
  dir = getOption("nflfastR.raw_directory", default = NULL),
  ...
) {
  #testing
  #id = '2022_01_PHI_DET'
  # id = '2015_01_CAR_JAX'
  #id = '2011_01_NO_GB'

  season <- substr(id, 1, 4)
  week <- as.integer(substr(id, 6, 7))

  raw_data <- fetch_raw(game_id = id, dir = dir)

  season_type <- dplyr::case_when(
    season <= 2020 & week <= 17 ~ "REG",
    season >= 2021 & week <= 18 ~ "REG",
    TRUE ~ "POST"
  )

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
  if (home_team == away_team) {
    # get correct home and away from the game ID
    id_parts <- stringi::stri_split_fixed(id, "_")
    away_team <- id_parts[[1]][3]
    home_team <- id_parts[[1]][4]
    bad_game <- 1
  } else {
    bad_game <- 0
  }

  weather <- ifelse(
    is.null(raw_data$data$viewer$gameDetail$weather$shortDescription),
    NA_character_,
    raw_data$data$viewer$gameDetail$weather$shortDescription
  )
  stadium <- ifelse(
    is.null(raw_data$data$viewer$gameDetail$stadium),
    NA_character_,
    raw_data$data$viewer$gameDetail$stadium
  )
  start_time <- raw_data$data$viewer$gameDetail$startTime

  game_info <- tibble::tibble(
    game_id = as.character(game_id),
    home_team,
    away_team,
    weather,
    stadium,
    start_time
  )

  # The raw data has three description columns. We only need the one with
  # jersey numbers, so we drop the others before they go through all steps.
  plays <- raw_data$data$viewer$gameDetail$plays
  plays$playDescription <- plays$playDescriptionWithJerseyNumbers
  plays$playDescriptionWithJerseyNumbers <- NULL
  plays$shortDescription <- NULL
  plays <- dplyr::mutate(plays, game_id = as.character(game_id))

  # We have this issue https://github.com/nflverse/nflfastR/issues/309 with 2013 postseason games
  # where the driveSequenceNumber in the plays df is NA for all plays. That prevents drive information
  # from being joined.
  # In this case, we compute our own driveSequenceNumber by incrementing a counter depending on the
  # value of driveTimeOfPossession.
  # driveTimeOfPossession will be a constant value during a drive so this should actually be accurate
  if (all(is.na(plays$driveSequenceNumber))) {
    plays <- plays |>
      dplyr::mutate(
        # First, create a trigger for cumsum
        drive_trigger = dplyr::case_when(
          # this is the first play of the first drive
          is.na(dplyr::lag(.data$driveTimeOfPossession)) &
            !is.na(.data$driveTimeOfPossession) ~ 1,
          # if driveTimeOfPossession changes, there is a new drive
          dplyr::lag(.data$driveTimeOfPossession) !=
            .data$driveTimeOfPossession ~ 1,
          TRUE ~ 0
        ),
        # Now create the drive number by accumulationg triggers
        driveSequenceNumber = cumsum(.data$drive_trigger),
        # driveSequenceNumber should be NA on plays where driveTimeOfPossession is NA
        driveSequenceNumber = ifelse(
          is.na(.data$driveTimeOfPossession),
          NA_real_,
          .data$driveSequenceNumber
        ),
        # drop the helper
        drive_trigger = NULL
      )
  }

  drives <- raw_data$data$viewer$gameDetail$drives |>
    dplyr::mutate(ydsnet = .data$yards + .data$yardsPenalized) |>
    # these are already in plays
    dplyr::select(
      -"possessionTeam.abbreviation",
      -"possessionTeam.nickName",
      -"possessionTeam.franchise.currentLogo.url"
    )
  colnames(drives) <- paste0("drive_", clean_names_cached(colnames(drives)))

  stats <- unnest_play_stats(plays)

  # there was a penalty on this play so these stat IDs shouldn't exist
  if (id == "2020_10_DEN_LV") {
    stats <- stats |>
      dplyr::filter(!(.data$playId == 979 & .data$statId %in% c(8, 10, 79)))
  }

  pbp_stats <- tidy_play_stats(stats) |> tibble::as_tibble()

  combined <- game_info |>
    dplyr::bind_cols(plays |> dplyr::select(-"playStats", -"game_id")) |>
    dplyr::left_join(
      drives,
      by = c("driveSequenceNumber" = "drive_order_sequence")
    ) |>
    dplyr::left_join(pbp_stats, by = c("playId" = "play_id")) |>
    normalize_raw_columns() |>
    dplyr::select(
      -"drive_play_count",
      -"drive_time_of_possession",
      -"next_play_type"
    ) |>
    dplyr::rename(
      time = "clock_time",
      play_type_nfl = "play_type",
      posteam = "possession_team_abbreviation",
      yardline = "yard_line",
      sp = "scoring_play",
      drive = "drive_sequence_number",
      nfl_api_id = "game_id",
      drive_play_count = "drive_play_count_2",
      drive_time_of_possession = "drive_time_of_possession_2",
      ydsnet = "drive_ydsnet"
    ) |>
    dplyr::mutate(
      posteam_id = .data$posteam,
      # have to do all this nonsense to make goal_to_go and yardline_side for compatibility with later functions
      yardline_side = str_split_and_extract(.data$yardline, " ", 1),
      yardline_number = as.numeric(str_split_and_extract(
        .data$yardline,
        " ",
        2
      )),
      quarter_end = dplyr::if_else(
        stringi::stri_detect_fixed(.data$play_description, "END QUARTER"),
        1,
        0
      ),
      game_year = as.integer(season),
      season = as.integer(season),
      # this is only needed for epa and dropped later
      game_month = as.integer(11),
      game_id = id,
      week = week,
      season_type = season_type,
      play_clock = as.character(.data$play_clock),
      st_play_type = as.character(.data$st_play_type),

      # fix muffed punt td in JAC game
      td_team = dplyr::if_else(
        id == "2011_14_TB_JAX" & .data$play_id == 1343 & .data$td_team != "JAX",
        'JAX',
        .data$td_team
      ),

      # kickoff return TDs in old JAC games
      td_team = dplyr::if_else(
        id == "2006_14_IND_JAX" &
          .data$play_id == 2078 &
          .data$td_team != "JAX",
        'JAX',
        .data$td_team
      ),
      td_team = dplyr::if_else(
        id == "2007_17_JAX_HOU" &
          .data$play_id %in% c(1907, 2042) &
          .data$td_team != "JAX",
        'HOU',
        .data$td_team
      ),
      td_team = dplyr::if_else(
        id == "2008_09_JAX_CIN" &
          .data$play_id == 3145 &
          .data$td_team != "JAX",
        'JAX',
        .data$td_team
      ),
      td_team = dplyr::if_else(
        id == "2009_15_IND_JAX" &
          .data$play_id == 1088 &
          .data$td_team != "JAX",
        'IND',
        .data$td_team
      ),
      td_team = dplyr::if_else(
        id == "2010_15_JAX_IND" &
          .data$play_id == 3848 &
          .data$td_team != "JAX",
        'IND',
        .data$td_team
      ),

      time = dplyr::case_when(
        id == '2012_04_NO_GB' & .data$play_id == 1085 ~ '3:34',
        id == '2012_16_BUF_MIA' & .data$play_id == 2571 ~ '8:31',
        TRUE ~ .data$time
      ),
      drive_real_start_time = as.character(.data$drive_real_start_time),
      # get the safety team to ensure the correct team gets the points
      # usage of base ifelse is important here for non-scoring games (i.e. early live games)
      safety_team = ifelse(
        .data$safety == 1,
        .data$scoring_team_abbreviation,
        NA_character_
      ),

      # can't trust the goal_to_go variable so we overwrite it here
      goal_to_go = as.numeric(stringi::stri_detect_fixed(
        tolower(.data$pre_play_by_play),
        "goal"
      ))
    ) |>
    empty_strings_to_na() |>
    # Data in 2023 pbp introduced separate "plays" for TV timeouts and two minute warnings
    # These mess up some of our logic. Since they are useless, we remove them here
    dplyr::filter(
      !(is.na(.data$timeout_team) &
        stringi::stri_detect_regex(
          tolower(.data$play_description),
          "timeout at|two-minute"
        ))
    ) |>
    # Data in 2024 pbp introduced separate "plays" for injury updates
    # These mess up some of our logic. Since they are useless, we remove them here
    dplyr::filter(
      !(is.na(.data$timeout_team) &
        stringi::stri_startswith_fixed(
          tolower(.data$play_description),
          "** injury update:"
        ))
    ) |>
    fix_posteams()

  # fix for games where home_team == away_team and fields are messed up
  if (bad_game == 1) {
    combined <- combined |>
      fix_bad_games()
  }

  # nfl didn't fill in first downs on this game
  if (id == '2018_01_ATL_PHI') {
    combined <- combined |>
      dplyr::mutate(
        first_down_pass = dplyr::if_else(
          .data$pass_attempt == 1 & .data$first_down == 1,
          1,
          .data$first_down_pass
        ),
        first_down_rush = dplyr::if_else(
          .data$rush_attempt == 1 & .data$first_down == 1,
          1,
          .data$first_down_rush
        ),

        third_down_converted = dplyr::if_else(
          .data$first_down == 1 & .data$down == 3,
          1,
          .data$third_down_converted
        ),
        fourth_down_converted = dplyr::if_else(
          .data$first_down == 1 & .data$down == 4,
          1,
          .data$fourth_down_converted
        ),

        third_down_failed = dplyr::if_else(
          .data$first_down == 0 & .data$down == 3,
          1,
          .data$third_down_failed
        ),
        fourth_down_failed = dplyr::if_else(
          .data$first_down == 0 &
            .data$down == 4 &
            .data$play_type_nfl != "FIELD_GOAL" &
            .data$play_type_nfl != "PUNT" &
            .data$play_type_nfl != "PENALTY",
          1,
          .data$fourth_down_failed
        )
      )
  }

  # remove duplicate plays. can't do this with play_id because duplicate plays
  # sometimes have different play_ids. Doing it here for every game is much
  # cheaper than for the bound data of all games.
  duplicate <- duplicated(data.table::as.data.table(
    combined[c("quarter", "time", "play_description", "down")]
  ))
  if (any(duplicate)) {
    combined <- combined[!duplicate, ]
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
  combined[setdiff(names(combined), unused)]
}

# helper function to manually fill in fields for problematic games
fix_bad_games <- function(pbp) {
  fixed <- pbp |>
    dplyr::mutate(
      #if team has the ball and scored, make them the scoring team
      td_team = dplyr::if_else(
        .data$drive_how_ended_description == 'Touchdown' &
          !is.na(.data$td_team),
        .data$posteam,
        .data$td_team
      ),
      #if team defensive team score, fill in the right team
      td_team = dplyr::if_else(
        #game involving the jags
        #defensive TD
        .data$drive_how_ended_description != 'Touchdown' &
          !is.na(.data$td_team),
        #if home team has ball, then away team scored, otherwise home team scored
        dplyr::if_else(
          .data$posteam == .data$home_team,
          .data$away_team,
          .data$home_team
        ),
        .data$td_team
      ),
      # fill in return team
      return_team = dplyr::if_else(
        !is.na(.data$return_team),
        dplyr::if_else(
          # if the home team has the ball, return team is away team (this is before we flip posteam for kickoffs)
          .data$posteam == .data$home_team,
          .data$away_team,
          .data$home_team
        ),
        .data$return_team
      ),
      fumble_recovery_1_team = dplyr::if_else(
        !is.na(.data$fumble_recovery_1_team),
        # assign possession based on fumble_lost
        dplyr::case_when(
          .data$fumble_lost == 1 &
            .data$posteam == .data$home_team ~ .data$away_team,
          .data$fumble_lost == 1 &
            .data$posteam == .data$away_team ~ .data$home_team,
          .data$fumble_lost == 0 &
            .data$posteam == .data$home_team ~ .data$home_team,
          .data$fumble_lost == 0 &
            .data$posteam == .data$away_team ~ .data$away_team
        ),
        .data$fumble_recovery_1_team
      ),
      timeout_team = dplyr::if_else(
        # if there's a timeout in the affected seasons
        !is.na(.data$timeout_team),
        # extract from play description
        stringi::stri_extract_first_regex(
          .data$play_description,
          "(?<=Timeout #[1-3] by )[:upper:]+"
        ),
        .data$timeout_team
      )
    )

  return(fixed)
}

fix_posteams <- function(pbp) {
  # Data source switch in 2023 introduced new problems
  # 1. Definition of posteam on kick offs changed to receiving team. That's our
  #    definition and we swap teams later.
  # 2. Posteam doesn't change on the PAT after defensive TD
  #
  # We adjust both things here
  # We need the variable pre_play_by_play which usually looks like "KC  1-10  NYJ 40"
  if ("pre_play_by_play" %in% names(pbp)) {
    # Let's be as explicit as possible about what we want to extract from the string
    # It's really only the first valid team abbreviation followed by a blank space
    valid_team_abbrs <- paste(
      c(nflfastR::teams_colors_logos$team_abbr, "OAK", "SD", "STL"),
      collapse = " |"
    )
    posteam_regex <- paste0("^", valid_team_abbrs, "(?=[:space:])")

    pbp <- pbp |>
      dplyr::mutate(
        parsed_posteam = stringi::stri_extract_first_regex(
          .data$pre_play_by_play,
          posteam_regex
        ) |>
          stringi::stri_trim_both(),
        posteam = dplyr::case_when(
          stringi::stri_detect_regex(
            .data$play_description,
            "^Timeout "
          ) ~ NA_character_,
          is.na(.data$parsed_posteam) ~ .data$posteam,
          .data$play_description == "GAME" ~ NA_character_,
          TRUE ~ .data$parsed_posteam
        ),
        # drop helper
        parsed_posteam = NULL
      )
  }

  pbp
}

# Convert logical and integer columns to double and factors to character,
# normalize team abbreviations and white space in character columns, and
# clean the column names. Same result as a chain of dplyr::mutate_if() calls
# and janitor::clean_names(), but much faster on wide data with few rows.
normalize_raw_columns <- function(df) {
  n <- nrow(df)
  cols <- as.list(df)
  for (j in seq_along(cols)) {
    x <- cols[[j]]
    if (is.logical(x) || is.integer(x)) {
      x <- as.numeric(x)
    } else if (is.factor(x)) {
      x <- as.character(x)
    }
    if (is.character(x)) {
      # str_squish() also turns line breaks into blanks
      x <- squish(team_name_fn(x))
    }
    cols[[j]] <- x
  }
  names(cols) <- clean_names_cached(names(cols))
  tibble::new_tibble(cols, nrow = n)
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
    if (is.null(stats[[col]])) rep(NA, nrow(stats)) else stats[[col]]
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

empty_strings_to_na <- function(df) {
  n <- nrow(df)
  cols <- as.list(df)
  for (j in seq_along(cols)) {
    x <- cols[[j]]
    if (is.character(x)) {
      x[which(x == "")] <- NA_character_
      cols[[j]] <- x
    }
  }
  tibble::new_tibble(cols, nrow = n)
}
