#' Compute Division Standings and Conference Seeds from Play by Play
#'
#' @param pbp nflfastR play-by-play data either loaded via `load_pbp()` or
#'   computed via `build_nflfastR_pbp()`.
#' @inheritParams nflseedR::compute_conference_seeds
#'
#' @return A tibble with NFL regular season standings
#' @export
#'
#' @examples
#' \donttest{
#' pbp <- nflreadr::load_pbp(2020:2021)
#' calculate_standings(pbp)
#' }
calculate_standings <- function(pbp,
                                tiebreaker_depth = 3,
                                playoff_seeds = 7){

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

  div <- nflseedR::compute_division_ranks(g,
                                          tiebreaker_depth = tiebreaker_depth)

  conf <- nflseedR::compute_conference_seeds(div,
                                             h2h = div$h2h,
                                             tiebreaker_depth = tiebreaker_depth,
                                             playoff_seeds = playoff_seeds)
  conf$standings %>%
    dplyr::select(-"exit", -"wins") %>%
    dplyr::rename("season" = "sim", "wins" = "true_wins")
}
