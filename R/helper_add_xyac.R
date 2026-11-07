################################################################################
# Author: Ben Baldwin, Sebastian Carl
# Purpose: Function to add expected yac variables.
# Code Style Guide: styler::tidyverse_style()
################################################################################
#' Add expected yards after completion (xyac) variables
#'
#' @inheritParams clean_pbp
#' @details Build columns that capture what we should expect after the catch.
#' @return The input Data Frame of the parameter 'pbp' with the following columns
#' added:
#' \describe{
#' \item{xyac_epa}{Expected value of EPA gained after the catch, starting from where the catch was made. Zero yards after the catch would be listed as zero EPA.}
#' \item{xyac_success}{Probability play earns positive EPA (relative to where play started) based on where ball was caught.}
#' \item{xyac_fd}{Probability play earns a first down based on where the ball was caught.}
#' \item{xyac_mean_yardage}{Average expected yards after the catch based on where the ball was caught.}
#' \item{xyac_median_yardage}{Median expected yards after the catch based on where the ball was caught.}
#' }
#' @export
add_xyac <- function(pbp, ...) {
  if (nrow(pbp) == 0) {
    user_message("Nothing to do. Return passed data frame.", "info")
  } else {
    pbp <- pbp |> dplyr::select(-dplyr::any_of(drop.cols.xyac))

    # for joining at the end
    pbp <- pbp |>
      dplyr::mutate(index = 1:dplyr::n())

    # prepare_xyac_data helper function shown below
    passes <- prepare_xyac_data(pbp) |>
      dplyr::filter(.data$valid_pass == 1, .data$distance_to_goal != 0)

    if (!nrow(passes) == 0) {
      user_message("Computing xyac...", "todo")

      preds <- stats::predict(
        load_model("xyac"),
        as.matrix(passes |> xyac_model_select())
      )

      # xgboost v3 returns a matrix of predictions (one row per pass) but the
      # below code is designed to work with a vector as returned by xgboost v1
      if (is.matrix(preds)) {
        preds <- preds |>
          t() |>
          as.vector("numeric")
      }

      xyac_vars <- summarize_xyac(passes, preds)

      pbp <- pbp |>
        dplyr::left_join(xyac_vars, by = "index") |>
        dplyr::select(-"index")

      message_completed("added xyac variables", ...)
    } else {
      # means no valid pass plays in the pbp
      pbp <- pbp |>
        dplyr::mutate(
          xyac_epa = NA_real_,
          xyac_mean_yardage = NA_real_,
          xyac_median_yardage = NA_real_,
          xyac_success = NA_real_,
          xyac_fd = NA_real_
        ) |>
        dplyr::select(-"index")
      user_message(
        "No non-NA values for xyac calculation detected. xyac variables set to NA",
        "info"
      )
    }
  }

  return(pbp)
}

# Turn the predicted yac distribution of each pass into the xyac variables.
#
# Every pass gets one row for each possible yac value (-5 to 70 yards). All
# vectors below have one element per row and the rows of a pass are adjacent.
# Rows that are impossible on the field are removed, the field position after
# the catch is computed and evaluated with the EP model, and the results are
# summarized per pass.
summarize_xyac <- function(passes, preds) {
  n_yac <- 76L
  n_pass <- nrow(passes)
  each <- function(x) rep(x, each = n_yac)

  index <- each(passes$index)
  yac <- rep_len(-5:70, n_pass * n_yac)
  prob <- preds
  distance_to_goal <- each(passes$distance_to_goal)

  # truncate probs at losses bigger than possible and gains bigger than possible
  max_loss <- data.table::fifelse(
    distance_to_goal < 95,
    -5,
    distance_to_goal - 99
  )
  max_gain <- data.table::fifelse(distance_to_goal > 70, 70, distance_to_goal)
  cum_prob <- group_cumsum(prob, n_yac)
  lag_cum_prob <- c(NA_real_, cum_prob[-length(cum_prob)])
  lag_cum_prob[yac == -5L] <- NA_real_
  is_gain <- yac == max_gain
  prob[is_gain] <- 1 - lag_cum_prob[is_gain]
  is_loss <- yac == max_loss
  prob[is_loss] <- cum_prob[is_loss]

  keep <- yac >= max_loss & yac <= max_gain
  index <- index[keep]
  yac <- yac[keep]
  prob <- prob[keep]
  pass_row <- each(seq_len(n_pass))[keep]
  per_pass <- function(x) x[pass_row]

  # get updated end result for each possibility
  half_seconds_remaining <- per_pass(passes$half_seconds_remaining)
  half_seconds_remaining <- data.table::fifelse(
    half_seconds_remaining <= 6,
    0,
    half_seconds_remaining - 6
  )
  down <- as.integer(per_pass(passes$down))
  ydstogo <- as.integer(per_pass(passes$ydstogo))
  original_ydstogo <- ydstogo
  original_spot <- per_pass(passes$yardline_100)
  posteam_timeouts_pre <- per_pass(passes$posteam_timeouts_remaining)
  defteam_timeouts_pre <- per_pass(passes$defteam_timeouts_remaining)

  yardline_100 <- per_pass(passes$distance_to_goal) - yac
  gain <- original_spot - yardline_100
  turnover <- data.table::fifelse(down == 4 & gain < ydstogo, 1L, 0L)
  first_down <- gain >= ydstogo
  new_down <- data.table::fifelse(first_down, 1, down + 1)
  new_ydstogo <- data.table::fifelse(first_down, 10, ydstogo - gain)
  # possession change if 4th down failed
  down <- data.table::fifelse(turnover == 1, 1L, as.integer(new_down))
  ydstogo <- data.table::fifelse(turnover == 1, 10L, as.integer(new_ydstogo))
  # save yardline_100 for yards gained calculation
  yardline_100_noflip <- yardline_100
  # flip yardline_100 and timeouts for turnovers for EP calculation
  yardline_100 <- data.table::fifelse(
    turnover == 1,
    as.integer(100 - yardline_100),
    as.integer(yardline_100)
  )
  posteam_timeouts_remaining <- data.table::fifelse(
    turnover == 1,
    defteam_timeouts_pre,
    posteam_timeouts_pre
  )
  defteam_timeouts_remaining <- data.table::fifelse(
    turnover == 1,
    posteam_timeouts_pre,
    defteam_timeouts_pre
  )
  # ydstogo can't be bigger than yardline
  ydstogo <- data.table::fifelse(
    ydstogo >= yardline_100,
    as.integer(yardline_100),
    as.integer(ydstogo)
  )

  # a touchdown is worth 7 points, no need to predict it
  touchdown <- yardline_100 == 0 & !is.na(yardline_100)
  ep <- rep(NA_real_, length(yac))
  ep[!touchdown] <- predict_ep(
    season = per_pass(passes$season)[!touchdown],
    half_seconds_remaining = half_seconds_remaining[!touchdown],
    yardline_100 = yardline_100[!touchdown],
    home = data.table::fifelse(
      per_pass(passes$posteam) == per_pass(passes$home_team),
      1,
      0
    )[!touchdown],
    roof = per_pass(passes$roof)[!touchdown],
    ydstogo = ydstogo[!touchdown],
    down = down[!touchdown],
    posteam_timeouts_remaining = posteam_timeouts_remaining[!touchdown],
    defteam_timeouts_remaining = defteam_timeouts_remaining[!touchdown]
  )
  is_turnover <- turnover == 1 & !is.na(turnover)
  ep[is_turnover] <- -1 * ep[is_turnover]
  ep[touchdown] <- 7

  original_ep <- per_pass(passes$ep)
  epa <- ep - original_ep
  wt_epa <- epa * prob
  wt_yardln <- yardline_100_noflip * prob
  first_row <- c(TRUE, pass_row[-1] != pass_row[-length(pass_row)])
  csum <- group_cumsum(prob, pass_row)
  below_half <- csum < .5
  lag_below_half <- c(NA, below_half[-length(below_half)])
  lag_below_half[first_row] <- NA
  med <- data.table::fifelse(csum > .5 & lag_below_half, yac, 0L)

  # passes without any possible yac value don't get xyac variables
  present <- sort(unique(pass_row))
  group <- factor(pass_row, levels = present)
  group_sum <- function(x) unname(vapply(split(x, group), sum, numeric(1)))
  tibble::tibble(
    index = passes$index[present],
    xyac_epa = group_sum(wt_epa) - passes$air_epa[present],
    xyac_mean_yardage = (passes$yardline_100[present] -
      passes$air_yards[present]) -
      group_sum(wt_yardln),
    xyac_median_yardage = as.numeric(vapply(
      split(med, group),
      max,
      integer(1)
    )),
    xyac_success = group_sum((ep > original_ep) * prob),
    xyac_fd = group_sum((gain >= original_ydstogo) * prob)
  ) |>
    dplyr::arrange(.data$index)
}

# EP of the possession team, computed like calculate_expected_points() but from
# plain vectors and with one prediction per unique combination of features
predict_ep <- function(
  season,
  half_seconds_remaining,
  yardline_100,
  home,
  roof,
  ydstogo,
  down,
  posteam_timeouts_remaining,
  defteam_timeouts_remaining
) {
  if (length(season) == 0) {
    return(numeric())
  }
  indicator <- function(x) data.table::fifelse(x, 1, 0)
  model_roof <- data.table::fifelse(
    is.na(roof) | roof == "open" | roof == "closed",
    "retractable",
    roof
  )
  features <- tibble::tibble(
    half_seconds_remaining = as.numeric(half_seconds_remaining),
    yardline_100 = as.numeric(yardline_100),
    home = as.numeric(home),
    retractable = indicator(model_roof == "retractable"),
    dome = indicator(model_roof == "dome"),
    outdoors = indicator(model_roof == "outdoors"),
    ydstogo = as.numeric(ydstogo),
    era0 = indicator(season <= 2001),
    era1 = indicator(season > 2001 & season <= 2005),
    era2 = indicator(season > 2005 & season <= 2013),
    era3 = indicator(season > 2013 & season <= 2017),
    era4 = indicator(season > 2017),
    down1 = indicator(down == 1),
    down2 = indicator(down == 2),
    down3 = indicator(down == 3),
    down4 = indicator(down == 4),
    posteam_timeouts_remaining = as.numeric(posteam_timeouts_remaining),
    defteam_timeouts_remaining = as.numeric(defteam_timeouts_remaining)
  )
  # Many rows share the same features. Predictions don't depend on other
  # rows, so we predict every unique row once.
  combo <- features |>
    dplyr::group_by(dplyr::across(dplyr::everything())) |>
    dplyr::group_indices()
  unique_rows <- !duplicated(combo)
  preds <- stats::predict(
    load_model("ep"),
    as.matrix(features[unique_rows, ])
  )
  if (is.vector(preds)) {
    preds <- matrix(preds, ncol = 7, byrow = TRUE)
  }
  preds <- preds[match(combo, combo[unique_rows]), , drop = FALSE]
  # same order of operations as in calculate_expected_points()
  (-3 * preds[, 4]) +
    (-2 * preds[, 6]) +
    (-7 * preds[, 2]) +
    (3 * preds[, 3]) +
    (2 * preds[, 5]) +
    (7 * preds[, 1])
}

### helper function for getting the data ready
prepare_xyac_data <- function(pbp) {
  # valid pass play: at least -15 air yards, less than 70 air yards, has intended receiver, has pass location
  passes <- pbp |>
    make_model_mutations() |>
    dplyr::mutate(
      # all conditions of a valid pass except the receiver (helper)
      tmp_valid = (.data$complete_pass == 1 |
        .data$incomplete_pass == 1 |
        .data$interception == 1) &
        !is.na(.data$air_yards) &
        .data$air_yards >= -15 &
        .data$air_yards < 70 &
        !is.na(.data$pass_location),
      # the receiver only matters if the other conditions aren't FALSE
      receiver_player_name = apply_where(
        !(.data$tmp_valid %in% FALSE),
        .data$desc,
        function(x) {
          stringi::stri_extract_first_regex(
            x,
            glue::glue('{receiver_finder}{big_parser}')
          )
        }
      ),
      pass_middle = dplyr::if_else(.data$pass_location == "middle", 1, 0),
      air_is_zero = dplyr::if_else(.data$air_yards == 0, 1, 0),
      distance_to_sticks = .data$air_yards - .data$ydstogo,
      distance_to_goal = .data$yardline_100 - .data$air_yards,
      valid_pass = dplyr::if_else(
        .data$tmp_valid & !is.na(.data$receiver_player_name),
        1,
        0
      ),
      tmp_valid = NULL
    )
  return(passes)
}

### another helper function for getting the data ready
xyac_model_select <- function(pbp) {
  pbp |>
    dplyr::select(
      "air_yards",
      "yardline_100",
      "ydstogo",
      "distance_to_goal",
      "down1",
      "down2",
      "down3",
      "down4",
      "air_is_zero",
      "pass_middle",
      "era2",
      "era3",
      "era4",
      "qb_hit",
      "home",
      "outdoors",
      "retractable",
      "dome",
      "distance_to_sticks"
    )
}

# These columns are being generated by add_xyac and the function tries to drop
# them in case it is being used on a pbp dataset where the columns already exist
drop.cols.xyac <- c(
  "xyac_epa",
  "xyac_mean_yardage",
  "xyac_median_yardage",
  "xyac_success",
  "xyac_fd"
)
