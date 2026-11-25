# Not called inside the package, but needed: the nflverse workflows call
# nflfastR:::build_playstats() to build the play stats that calculate_stats()
# loads with load_playstats() (release "playstats" of nflverse/nflverse-pbp).
# jarl-ignore unused_function: called by the nflverse workflows
build_playstats <- function(
  seasons = nflreadr::most_recent_season(),
  stat_ids = 1:1000,
  dir = getOption("nflfastR.raw_directory", default = NULL),
  skip_local = FALSE
) {
  if (is_sequential()) {
    cli::cli_alert_info(
      "It is recommended to use parallel processing when using this function. \\
        Please consider running {.code future::plan(\"multisession\")}! \\
        Will go on sequentially...",
      wrap = TRUE
    )
  }

  games <- nflreadr::load_schedules(seasons = seasons)
  games <- games$game_id[!is.na(games$result)]
  # The play stats of the 1999 and 2000 seasons are built with nflfastR v5.2.0
  games <- check_for_dropped_seasons(games)
  if (length(games) == 0) {
    cli::cli_abort("There are no finished games to build play stats from.")
  }

  p <- progressr::progressor(along = games)

  out <- furrr::future_map(
    games,
    playstats_of_game,
    p = p,
    stat_ids = stat_ids,
    dir = dir,
    skip_local = skip_local
  ) |>
    data.table::rbindlist()

  out[, season := as.integer(substr(game_id, 1, 4))]
  out[, week := as.integer(substr(game_id, 6, 7))]
  data.table::setcolorder(out, c("game_id", "season", "week"))
  # The raw data has empty strings instead of missing values
  for (col in c("team_abbr", "player_name", "gsis_player_id")) {
    data.table::set(
      out,
      i = which(out[[col]] == ""),
      j = col,
      value = NA_character_
    )
  }
  out <- decode_player_ids(out)
  # clean team abbrs for consistency in calculate_stats
  out[, team_abbr := team_name_fn(team_abbr)]
  data.table::setDF(out)
  out
}

# One row per stat of a play, after the drop_play and drop_stat patches (see
# inst/pbp_patches.dcf), so the play stats agree with the pbp. Plays without
# stats have no rows.
playstats_of_game <- function(id, p, stat_ids, dir, skip_local) {
  plays <- load_raw_game(
    id,
    dir = dir,
    skip_local = skip_local
  )$data$viewer$gameDetail$plays
  plays <- patch_drop_plays(data.table::as.data.table(plays), id)
  stats <- patch_drop_stats(unnest_play_stats(plays), id)
  out <- data.table::data.table(
    game_id = id,
    play_id = stats$playId,
    stat_id = as.integer(stats$statId),
    yards = stats$yards,
    team_abbr = stats$teamAbbr,
    player_name = stats$player.displayName,
    gsis_player_id = stats$player.esbId
  )
  p(sprintf("ID=%s", id))
  out[stat_id %in% stat_ids]
}
