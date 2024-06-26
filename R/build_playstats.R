build_playstats <- function(seasons = nflreadr::most_recent_season(),
                            stat_ids = 1:1000,
                            dir = getOption("nflfastR.raw_directory", default = NULL),
                            skip_local = FALSE){

  if (is_sequential()) {
    cli::cli_alert_info(
      "It is recommended to use parallel processing when using this function. \\
        Please consider running {.code future::plan(\"multisession\")}! \\
        Will go on sequentially...", wrap = TRUE)
  }

  games <- nflreadr::load_schedules(seasons = seasons) %>%
    dplyr::filter(!is.na(.data$result)) %>%
    dplyr::pull(.data$game_id)

  p <- progressr::progressor(along = games)

  l <- furrr::future_map(
    games,
    function(id, p = NULL, dir, skip_local){
      if (id %in% c("2000_03_SD_KC", "2000_06_BUF_MIA", "1999_01_BAL_STL")){
        cli::cli_alert_warning("We are missing raw game data of {.val {id}}. Skipping.")
        return(data.frame())
      }
      season <- substr(id, 1, 4)
      raw_data <- load_raw_game(id, dir = dir, skip_local = skip_local)
      if (season <= 2000){
        drives <- raw_data[[1]][["drives"]] %>%
          purrr::keep(is.list)
        out <- tibble::tibble(d = drives) %>%
          tidyr::unnest_wider(.data$d) %>%
          tidyr::unnest_longer(.data$plays) %>%
          tidyr::unnest_wider(.data$plays, names_sep = "_") %>%
          dplyr::select("playId" = "plays_id", "playStats" = "plays_players") %>%
          tidyr::unnest_longer(.data$playStats) %>%
          tidyr::unnest_longer(.data$playStats) %>%
          tidyr::unnest_wider(.data$playStats) %>%
          dplyr::mutate(
            playId = as.integer(.data$playId),
            statId = as.integer(.data$statId),
            yards = as.integer(.data$yards),
            team.id = NA_character_
          ) %>%
          dplyr::select(-"sequence") %>%
          dplyr::rename(
            team.abbreviation = "clubcode",
            gsis.Player.id = "playStats_id"
          ) %>%
          tidyr::nest(
            playStats = c(
              .data$statId,
              .data$yards,
              .data$playerName,
              .data$team.id,
              .data$team.abbreviation,
              .data$gsis.Player.id
            )
          )
      } else {
        out <- raw_data$data$viewer$gameDetail$plays[, c("playId", "playStats")]
      }
      out$game_id <- as.character(id)
      p(sprintf("ID=%s", as.character(id)))
      out
    },
    p = p,
    dir = dir,
    skip_local = skip_local
  )

  out <- data.table::rbindlist(l) %>%
    tidyr::unnest(cols = c("playStats")) %>%
    janitor::clean_names() %>%
    dplyr::filter(.data$stat_id %in% stat_ids) %>%
    dplyr::mutate(
      season = as.integer(substr(.data$game_id, 1, 4)),
      week = as.integer(substr(.data$game_id, 6, 7))
    ) %>%
    decode_player_ids() %>%
    dplyr::select(
      "game_id",
      "season",
      "week",
      "play_id",
      "stat_id",
      "yards",
      "team_abbr" = "team_abbreviation",
      "player_name",
      "gsis_player_id",
    ) %>%
    dplyr::mutate_if(
      .predicate = is.character,
      .funs = ~dplyr::na_if(.x, "")
    )
  out
}
