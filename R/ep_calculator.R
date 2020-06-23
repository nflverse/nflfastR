#' Compute expected points
#'
#' for provided plays. Returns the data with
#' probabilities of each scoring event and EP added. The following columns
#' must be present: season, week, home_team, posteam, roof (coded as 'open',
#' 'closed', or 'retractable'), half_seconds_remaining, yardline_100,
#' ydstogo, posteam_timeouts_remaining, defteam_timeouts_remaining
#'
#' @param pbp_data Play-by-play dataset to estimate expected points for.
#' @details Computes expected points for provided plays. Returns the data with
#' probabilities of each scoring event and EP added. The following columns
#' must be present:
#' \itemize{
#' \item{season}
#' \item{week}
#' \item{home_team}
#' \item{posteam}
#' \item{roof (coded as 'open', 'closed', or 'retractable')}
#' \item{half_seconds_remaining}
#' \item{yardline_100}
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

calculate_expected_points <- function(pbp_data) {

  suppressWarnings(
    model_data <- pbp_data %>%
      #drop existing values of ep and the probs before making new ones
      dplyr::select(-one_of(drop.cols)) %>%
      make_model_mutations() %>%
      ep_model_select()
  )


  preds <- as.data.frame(
    matrix(stats::predict(ep_model, as.matrix(model_data)), ncol=7, byrow=TRUE)
  )

  colnames(preds) <- c("td_prob","opp_td_prob","fg_prob","opp_fg_prob",
                       "safety_prob","opp_safety_prob","no_score_prob")

  preds <- preds %>%
    dplyr::mutate(
      ep =
        (-3 * opp_fg_prob) +
        (-2 * opp_safety_prob) +
        (-7 * opp_td_prob) +
        (3 * fg_prob) +
        (2 * safety_prob) +
        (7 * td_prob)
    ) %>%
    dplyr::bind_cols(pbp_data)

  return(preds)

}


drop.cols <- c(
  'ep', 'td_prob', 'opp_td_prob', 'fg_prob', 'opp_fg_prob',
  'safety_prob', 'opp_safety_prob', 'no_score_prob'
)


