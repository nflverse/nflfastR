#' Compute Division Standings and Conference Seeds from Play by Play
#'
#' @param pbp nflfastR play-by-play data either loaded via `load_pbp()` or
#'   computed via `build_nflfastR_pbp()`.
#' @param playoff_seeds Number of playoff teams per conference. If `NULL` (the
#'   default), the function will try to split `pbp` in seasons prior 2020 (6
#'   seeds) and 2020ff (7 seeds). If set to a numeric, it will be used for all
#'   seasons in `pbp`!
#' @inheritParams nflseedR::compute_conference_seeds
#'
#' @return A tibble with NFL regular season standings
#' @export
#'
#' @examples
#' \donttest{
#' pbp <- nflreadr::load_pbp(c(2018, 2021))
#' calculate_standings(pbp)
#' }
calculate_standings <- function(pbp,
                                tiebreaker_depth = 3,
                                playoff_seeds = NULL){

  rlang::check_installed("nflseedR", "to compute standings.",
                         compare = ">=",
                         version = "1.0.2"
                         )

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
      dplyr::filter(sim %in% 1999:2019)
    g7 <- g %>%
      dplyr::filter(sim >= 2020)
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
    dplyr::arrange(.data$season, .data$division, .data$div_rank, .data$seed)
}
