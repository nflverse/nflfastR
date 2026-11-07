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
    return(pbp)
  }
  by_ref <- in_pipeline(pbp, ...)
  d <- if (by_ref) pbp else as_pbp_dt(pbp)
  drop <- intersect(drop.cols.xyac, names(d))
  if (length(drop) > 0) {
    data.table::set(d, j = drop, value = NULL)
  }

  passes <- prepare_xyac_data(d)
  data.table::set(d, j = drop.cols.xyac, value = NA_real_)

  if (length(passes$index) > 0) {
    user_message("Computing xyac...", "todo")

    preds <- stats::predict(
      load_model("xyac"),
      model_matrix(passes, xyac_model_features)
    )

    # xgboost v3 returns a matrix of predictions (one row per pass) but the
    # below code is designed to work with a vector as returned by xgboost v1
    if (is.matrix(preds)) {
      preds <- preds |>
        t() |>
        as.vector("numeric")
    }

    xyac_vars <- summarize_xyac(passes, preds)
    data.table::set(
      d,
      i = xyac_vars$index,
      j = drop.cols.xyac,
      value = xyac_vars[drop.cols.xyac]
    )

    message_completed("added xyac variables", ...)
  } else {
    # means no valid pass plays in the pbp
    user_message(
      "No non-NA values for xyac calculation detected. xyac variables set to NA",
      "info"
    )
  }

  if (by_ref) d else as_input_type(d, pbp)
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
  n_pass <- length(passes$index)
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
  # cumulative probabilities within each pass (the rows of a pass are adjacent,
  # so the grouped results are in row order)
  dist <- data.table::data.table(
    pass = rep(seq_len(n_pass), each = n_yac),
    prob
  )
  dist[, cum_prob := cumsum(prob), by = "pass"]
  dist[, lag_cum_prob := data.table::shift(cum_prob), by = "pass"]
  cum_prob <- dist$cum_prob
  lag_cum_prob <- dist$lag_cum_prob
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
  outcomes <- data.table::data.table(
    pass_row,
    yac,
    prob,
    wt_epa,
    wt_yardln,
    success = (ep > original_ep) * prob,
    first_down = (gain >= original_ydstogo) * prob
  )
  outcomes[, csum := cumsum(prob), by = "pass_row"]
  outcomes[, lag_below_half := data.table::shift(csum < 0.5), by = "pass_row"]
  outcomes[, med := data.table::fifelse(csum > 0.5 & lag_below_half, yac, 0L)]

  # one row per pass; passes without any possible yac value don't get xyac
  # variables
  per_pass_sums <- outcomes[,
    list(
      wt_epa = sum(wt_epa),
      wt_yardln = sum(wt_yardln),
      med = max(med),
      success = sum(success),
      first_down = sum(first_down)
    ),
    by = "pass_row"
  ]
  # pass_row is in increasing order, so the result is ordered by index
  present <- per_pass_sums$pass_row
  list(
    index = passes$index[present],
    xyac_epa = per_pass_sums$wt_epa - passes$air_epa[present],
    xyac_mean_yardage = (passes$yardline_100[present] -
      passes$air_yards[present]) -
      per_pass_sums$wt_yardln,
    xyac_median_yardage = as.numeric(per_pass_sums$med),
    xyac_success = per_pass_sums$success,
    xyac_fd = per_pass_sums$first_down
  )
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
  features <- data.table::data.table(
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
  features[, combo := .GRP, by = ep_model_features]
  combo <- features$combo
  unique_rows <- !duplicated(combo)
  preds <- stats::predict(
    load_model("ep"),
    model_matrix(features[unique_rows], ep_model_features)
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
# The valid passes (with a distance to the goal line after the air yards) as
# list of columns with the model features and the row index of the pass in pbp
prepare_xyac_data <- function(pbp) {
  # a list of columns, so make_model_mutations() doesn't change pbp
  passes <- make_model_mutations(as.list(pbp))
  passes$index <- seq_len(nrow(pbp))
  valid <- is_valid_pass(passes)
  # the receiver only matters if the other conditions aren't FALSE
  receiver_player_name <- apply_where(
    !(valid %in% FALSE),
    passes$desc,
    function(x) {
      stringi::stri_extract_first_regex(
        x,
        paste0(receiver_finder, big_parser)
      )
    }
  )
  passes$pass_middle <- data.table::fifelse(
    passes$pass_location == "middle",
    1,
    0
  )
  passes$air_is_zero <- data.table::fifelse(passes$air_yards == 0, 1, 0)
  passes$distance_to_sticks <- passes$air_yards - passes$ydstogo
  passes$distance_to_goal <- passes$yardline_100 - passes$air_yards
  # a valid pass also has an intended receiver
  lapply(
    passes,
    `[`,
    which(valid & !is.na(receiver_player_name) & passes$distance_to_goal != 0)
  )
}

### another helper function for getting the data ready
xyac_model_features <- c(
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

# These columns are being generated by add_xyac and the function tries to drop
# them in case it is being used on a pbp dataset where the columns already exist
drop.cols.xyac <- c(
  "xyac_epa",
  "xyac_mean_yardage",
  "xyac_median_yardage",
  "xyac_success",
  "xyac_fd"
)
