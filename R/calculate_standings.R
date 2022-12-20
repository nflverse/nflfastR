#' Compute Division Standings and Conference Seeds from Play by Play
#'
#' @description This function calculates division standings as well as playoff
#'   seeds per conference based on either nflverse play-by-play data or nflverse
#'   schedule data.
#'
#' @param nflverse_object Data object of class `nflverse_data`. Either schedules
#'   as returned by [`fast_scraper_schedules()`] or [`nflreadr::load_schedules()`].
#'   Or play-by-play data as returned by [`load_pbp()`], [`build_nflfastR_pbp()`], or
#'  [`fast_scraper()`].
#' @param playoff_seeds Number of playoff teams per conference. If `NULL` (the
#'   default), the function will try to split `nflverse_object` into seasons prior
#'   2020 (6 seeds) and 2020ff (7 seeds). If set to a numeric, it will be used
#'   for all seasons in `nflverse_object`!
#' @inheritParams nflseedR::compute_conference_seeds
#'
#' @return A tibble with NFL regular season standings
#' @export
#'
#' @examples
#' \donttest{
#' try({# to avoid CRAN test problems
#'   # load nflverse data both schedules and pbp
#'   scheds <- fast_scraper_schedules(2014)
#'   pbp <- load_pbp(c(2018, 2021))
#'
#'   # calculate standings based on pbp
#'   calculate_standings(pbp)
#'
#'   # calculate standings based on schedules
#'   calculate_standings(scheds)
#' })
#' }
calculate_standings <- function(nflverse_object,
                                tiebreaker_depth = 3,
                                playoff_seeds = NULL){

  if(!inherits(nflverse_object, "nflverse_data")){
    cli::cli_abort("The function argument {.arg nflverse_object} has to be
                   of class {.cls nflverse_data}")
  }

  rlang::check_installed("nflseedR", "to compute standings.",
                         compare = ">=",
                         version = "1.0.2"
                         )

  type <- attr(nflverse_object, "nflverse_type")

  if (type == "play by play data"){
    .standings_from_pbp(nflverse_object,
                        tiebreaker_depth = tiebreaker_depth,
                        playoff_seeds = playoff_seeds)
  } else if (type == "games and schedules"){
    .standings_from_games(nflverse_object,
                          tiebreaker_depth = tiebreaker_depth,
                          playoff_seeds = playoff_seeds)
  } else {
    cli::cli_abort("Can only handle nflverse_type {.val play by play data} or
                   {.val games and schedules} and not {.val {type}}")
  }
}

.standings_from_pbp <- function(pbp, tiebreaker_depth, playoff_seeds){
  g <- pbp %>%
    dplyr::filter(.data$season_type == "REG") %>%
    dplyr::group_by(.data$game_id) %>%
    dplyr::summarise(
      sim = dplyr::first(.data$season),
      game_type = dplyr::first(.data$season_type),
      week = dplyr::first(.data$week),
      away_team = dplyr::first(.data$away_team),
      home_team = dplyr::first(.data$home_team),
      result = dplyr::last(.data$home_score) - dplyr::last(.data$away_score)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-"game_id")

  if(is.null(playoff_seeds)){
    g6 <- g %>%
      dplyr::filter(.data$sim %in% 1999:2019)
    g7 <- g %>%
      dplyr::filter(.data$sim >= 2020)
    dplyr::bind_rows(
      .compute_standings(g6, tiebreaker_depth = tiebreaker_depth, playoff_seeds = 6),
      .compute_standings(g7, tiebreaker_depth = tiebreaker_depth, playoff_seeds = 7)
    )
  } else {
    .compute_standings(g,
                       tiebreaker_depth = tiebreaker_depth,
                       playoff_seeds = playoff_seeds)
  }
}

.standings_from_games <- function(games, tiebreaker_depth, playoff_seeds){
  g <- games %>%
    dplyr::filter(.data$game_type == "REG", !is.na(.data$result)) %>%
    dplyr::select(
      "sim" = "season", "game_type", "week", "away_team", "home_team", "result"
    )

  if(is.null(playoff_seeds)){
    g6 <- g %>%
      dplyr::filter(.data$sim %in% 1999:2019)
    g7 <- g %>%
      dplyr::filter(.data$sim >= 2020)
    dplyr::bind_rows(
      .compute_standings(g6, tiebreaker_depth = tiebreaker_depth, playoff_seeds = 6),
      .compute_standings(g7, tiebreaker_depth = tiebreaker_depth, playoff_seeds = 7)
    )
  } else {
    .compute_standings(g,
                       tiebreaker_depth = tiebreaker_depth,
                       playoff_seeds = playoff_seeds)
  }
}

.compute_standings <- function(games, tiebreaker_depth, playoff_seeds){
  if(nrow(games) == 0) return(data.frame())
  suppressMessages({
    div <- nflseedR::compute_division_ranks(games,
                                            tiebreaker_depth = tiebreaker_depth)
    conf <- nflseedR::compute_conference_seeds(div,
                                               h2h = div$h2h,
                                               tiebreaker_depth = tiebreaker_depth,
                                               playoff_seeds = playoff_seeds)
  })
  conf$standings %>%
    dplyr::select(-"exit", -"wins") %>%
    dplyr::select("sim":"division", "div_rank", "seed", dplyr::everything()) %>%
    dplyr::rename("season" = "sim", "wins" = "true_wins") %>%
    dplyr::arrange(.data$season, .data$division, .data$div_rank, .data$seed) %>%
    tibble::as_tibble()
}
