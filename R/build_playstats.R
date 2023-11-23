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

  games <- nflreadr::load_schedules() %>%
    dplyr::filter(!is.na(.data$result), .data$season %in% seasons) %>%
    dplyr::pull(.data$game_id)

  p <- progressr::progressor(along = games)

  l <- furrr::future_map(
    games,
    function(id, p, dir, skip_local){
      raw_data <- load_raw_game(id, dir = dir, skip_local = skip_local)
      out <- raw_data$data$viewer$gameDetail$plays[, c("playId", "playStats")]
      out$game_id <- as.character(id)
      # out$desc <- raw_data$data$viewer$gameDetail$plays$playDescriptionWithJerseyNumbers
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
      # "desc"
    ) %>%
    dplyr::mutate_if(
      .predicate = is.character,
      .funs = ~dplyr::na_if(.x, "")
    )
  out
}
