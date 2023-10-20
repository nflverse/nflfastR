#' Compute expected points
#'
#' for provided plays. Returns the data with
#' probabilities of each scoring event and EP added. The following columns
#' must be present: season, home_team, posteam, roof (coded as 'open',
#' 'closed', or 'retractable'), half_seconds_remaining, yardline_100,
#' ydstogo, posteam_timeouts_remaining, defteam_timeouts_remaining
#'
#' @param pbp_data Play-by-play dataset to estimate expected points for.
#' @details Computes expected points for provided plays. Returns the data with
#' probabilities of each scoring event and EP added. The following columns
#' must be present:
#' \itemize{
#' \item{season}
#' \item{home_team}
#' \item{posteam}
#' \item{roof (coded as 'outdoors', 'dome', or 'open'/'closed'/NA (retractable))}
#' \item{half_seconds_remaining}
#' \item{yardline_100}
#' \item{down}
#' \item{ydstogo}
#' \item{posteam_timeouts_remaining}
#' \item{defteam_timeouts_remaining}
#' }
#' @return The original pbp_data with the following columns appended to it:
#' \describe{
#' \item{ep}{expected points.}
#' \item{no_score_prob}{probability of no more scoring this half.}
#' \item{opp_fg_prob}{probability next score opponent field goal this half.}
#' \item{opp_safety_prob}{probability next score opponent safety  this half.}
#' \item{opp_td_prob}{probability of next score opponent touchdown this half.}
#' \item{fg_prob}{probability next score field goal this half.}
#' \item{safety_prob}{probability next score safety this half.}
#' \item{td_prob}{probability text score touchdown this half.}
#' }
#' @export
#' @examples
#' \donttest{
#' try({# to avoid CRAN test problems
#' library(dplyr)
#' data <- tibble::tibble(
#' "season" = 1999:2019,
#' "home_team" = "SEA",
#' "posteam" = "SEA",
#' "roof" = "outdoors",
#' "half_seconds_remaining" = 1800,
#' "yardline_100" = c(rep(80, 17), rep(75, 4)),
#' "down" = 1,
#' "ydstogo" = 10,
#' "posteam_timeouts_remaining" = 3,
#' "defteam_timeouts_remaining" = 3
#' )
#'
#' nflfastR::calculate_expected_points(data) %>%
#'   dplyr::select(season, yardline_100, td_prob, ep)
#' })
#' }
calculate_expected_points <- function(pbp_data) {

   # drop existing values of ep and the probs before making new ones
  pbp_data <- pbp_data %>% dplyr::select(-tidyselect::any_of(drop.cols))

  suppressWarnings(
    model_data <- pbp_data %>%
      make_model_mutations() %>%
      ep_model_select()
  )

  preds <- as.data.frame(
    matrix(stats::predict(fastrmodels::ep_model, as.matrix(model_data)), ncol = 7, byrow = TRUE)
  )

  colnames(preds) <- c(
    "td_prob", "opp_td_prob", "fg_prob", "opp_fg_prob",
    "safety_prob", "opp_safety_prob", "no_score_prob"
  )

  preds <- preds %>%
    dplyr::mutate(
      ep =
        (-3 * .data$opp_fg_prob) +
        (-2 * .data$opp_safety_prob) +
        (-7 * .data$opp_td_prob) +
        (3 * .data$fg_prob) +
        (2 * .data$safety_prob) +
        (7 * .data$td_prob)
    ) %>%
    dplyr::bind_cols(pbp_data)

  return(preds)
}

# helper column for ep calculator
drop.cols <- c(
  "ep", "td_prob", "opp_td_prob", "fg_prob", "opp_fg_prob",
  "safety_prob", "opp_safety_prob", "no_score_prob"
)


#' Compute win probability
#'
#' for provided plays. Returns the data with
#' probabilities of winning the game. The following columns
#' must be present: receive_h2_ko (1 if game is in 1st half and possession
#' team will receive 2nd half kickoff, 0 otherwise),
#' home_team, posteam, half_seconds_remaining, game_seconds_remaining,
#' spread_line (how many points home team was favored by), down, ydstogo,
#' yardline_100, posteam_timeouts_remaining, defteam_timeouts_remaining
#'
#' @param pbp_data Play-by-play dataset to estimate win probability for.
#' @details Computes win probability for provided plays. Returns the data with
#' spread and non-spread-adjusted win probabilities. The following columns
#' must be present:
#' \itemize{
#' \item{receive_2h_ko (1 if game is in 1st half and possession team will receive 2nd half kickoff, 0 otherwise)}
#' \item{score_differential}
#' \item{home_team}
#' \item{posteam}
#' \item{half_seconds_remaining}
#' \item{game_seconds_remaining}
#' \item{spread_line (how many points home team was favored by)}
#' \item{down}
#' \item{ydstogo}
#' \item{yardline_100}
#' \item{posteam_timeouts_remaining}
#' \item{defteam_timeouts_remaining}
#' }
#' @return The original pbp_data with the following columns appended to it:
#' \describe{
#' \item{wp}{win probability.}
#' \item{vegas_wp}{win probability taking into account pre-game spread.}
#' }
#' @export
#' @examples
#' \donttest{
#' try({# to avoid CRAN test problems
#' library(dplyr)
#' data <- tibble::tibble(
#' "receive_2h_ko" = 0,
#' "home_team" = "SEA",
#' "posteam" = "SEA",
#' "score_differential" = 0,
#' "half_seconds_remaining" = 1800,
#' "game_seconds_remaining" = 3600,
#' "spread_line" = c(1, 3, 4, 7, 14),
#' "down" = 1,
#' "ydstogo" = 10,
#' "yardline_100" = 75,
#' "posteam_timeouts_remaining" = 3,
#' "defteam_timeouts_remaining" = 3
#' )
#'
#' nflfastR::calculate_win_probability(data) %>%
#'   dplyr::select(spread_line, wp, vegas_wp)
#' })
#' }
calculate_win_probability <- function(pbp_data) {

  # drop existing values of ep and the probs before making new ones
  pbp_data <- pbp_data %>% dplyr::select(-tidyselect::any_of(drop.cols.wp))

  suppressWarnings(
    model_data <- pbp_data %>%
      dplyr::mutate(
        home = dplyr::if_else(.data$posteam == .data$home_team, 1, 0),
        posteam_spread = dplyr::if_else(.data$home == 1, .data$spread_line, -1 * .data$spread_line),
        elapsed_share = (3600 - .data$game_seconds_remaining) / 3600,
        spread_time = .data$posteam_spread * exp(-4 * .data$elapsed_share),
        Diff_Time_Ratio = .data$score_differential / (exp(-4 * .data$elapsed_share))
      )
  )

  wp <- get_preds_wp(model_data) %>%
    tibble::as_tibble() %>%
    dplyr::rename(wp = "value")
  wp_spread <- get_preds_wp_spread(model_data) %>%
    tibble::as_tibble() %>%
    dplyr::rename(vegas_wp = "value")

  preds <- dplyr::bind_cols(
    pbp_data,
    wp,
    wp_spread
  )

  return(preds)
}

# helper column for wp calculator
drop.cols.wp <- c(
  "wp", "vegas_wp"
)
