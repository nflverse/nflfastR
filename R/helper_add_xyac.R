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
    # testing only
    # pbp <- g

    pbp <- pbp %>% dplyr::select(-tidyselect::any_of(drop.cols.xyac))

    # for joining at the end
    pbp <- pbp %>%
      dplyr::mutate(index = 1:dplyr::n())

    # prepare_xyac_data helper function shown below
    passes <- prepare_xyac_data(pbp) %>%
      dplyr::filter(.data$valid_pass == 1, .data$distance_to_goal != 0)

    if (!nrow(passes) == 0) {
      user_message("Computing xyac...", "todo")
      join_data <- passes %>%
        dplyr::select(
          "index", "distance_to_goal", "season", "week", "home_team", "posteam", "roof",
          "half_seconds_remaining", "down", "ydstogo",
          "posteam_timeouts_remaining", "defteam_timeouts_remaining",
          "original_spot" = "yardline_100", "original_ep" = "ep", "air_epa", "air_yards"
        ) %>%
        dplyr::mutate(
          down = as.integer(.data$down),
          ydstogo = as.integer(.data$ydstogo),
          original_ydstogo = .data$ydstogo
        ) %>%
        dplyr::select("index":"ydstogo", "original_ydstogo", dplyr::everything())

      xyac_vars <-
        stats::predict(
          fastrmodels::xyac_model,
          as.matrix(passes %>% xyac_model_select())
        ) %>%
        tibble::as_tibble() %>%
        dplyr::rename(prob = "value") %>%
        dplyr::bind_cols(
          tibble::tibble(
            "yac" = rep_len(-5:70, length.out = nrow(passes) * 76),
            "index" = rep(passes$index, times = rep_len(76, length.out = nrow(passes)))
          ) %>%
            dplyr::left_join(join_data, by = "index") %>%
            dplyr::mutate(
              half_seconds_remaining = dplyr::if_else(
                .data$half_seconds_remaining <= 6,
                0,
                .data$half_seconds_remaining - 6
              )
            )
        ) %>%
        dplyr::group_by(.data$index) %>%
        dplyr::mutate(
          max_loss = dplyr::if_else(.data$distance_to_goal < 95, -5, .data$distance_to_goal - 99),
          max_gain = dplyr::if_else(.data$distance_to_goal > 70, 70, .data$distance_to_goal),
          cum_prob = cumsum(.data$prob),
          prob = dplyr::case_when(
            # truncate probs at loss greater than max loss
            .data$yac == .data$max_loss ~ .data$cum_prob,
            # same for gains bigger than possible
            .data$yac == .data$max_gain ~ 1 - dplyr::lag(.data$cum_prob, 1),
            TRUE ~ .data$prob
          ),
          # get updated end result for each possibility
          yardline_100 = .data$distance_to_goal - .data$yac
        ) %>%
        dplyr::filter(.data$yac >= .data$max_loss, .data$yac <= .data$max_gain) %>%
        dplyr::select(-"cum_prob") %>%
        dplyr::mutate(
          posteam_timeouts_pre = .data$posteam_timeouts_remaining,
          defeam_timeouts_pre = .data$defteam_timeouts_remaining,
          gain = .data$original_spot - .data$yardline_100,
          turnover = dplyr::if_else(.data$down == 4 & .data$gain < .data$ydstogo, as.integer(1), as.integer(0)),
          down = dplyr::if_else(.data$gain >= .data$ydstogo, 1, .data$down + 1),
          ydstogo = dplyr::if_else(.data$gain >= .data$ydstogo, 10, .data$ydstogo - .data$gain),
          # possession change if 4th down failed
          down = dplyr::if_else(.data$turnover == 1, as.integer(1), as.integer(.data$down)),
          ydstogo = dplyr::if_else(.data$turnover == 1, as.integer(10), as.integer(.data$ydstogo)),
          # save yardline_100 for yards gained calculation
          yardline_100_noflip = .data$yardline_100,
          # flip yardline_100 and timeouts for turnovers for EP calculation
          yardline_100 = dplyr::if_else(.data$turnover == 1, as.integer(100 - .data$yardline_100), as.integer(.data$yardline_100)),
          posteam_timeouts_remaining = dplyr::if_else(
            .data$turnover == 1,
            .data$defeam_timeouts_pre,
            .data$posteam_timeouts_pre
          ),
          defteam_timeouts_remaining = dplyr::if_else(
            .data$turnover == 1,
            .data$posteam_timeouts_pre,
            .data$defeam_timeouts_pre
          ),
          # ydstogo can't be bigger than yardline
          ydstogo = dplyr::if_else(.data$ydstogo >= .data$yardline_100, as.integer(.data$yardline_100), as.integer(.data$ydstogo))
        ) %>%
        dplyr::ungroup() %>%
        nflfastR::calculate_expected_points() %>%
        dplyr::group_by(.data$index) %>%
        dplyr::mutate(
          ep = dplyr::case_when(
            .data$yardline_100 == 0 ~ 7,
            .data$turnover == 1 ~ -1 * .data$ep,
            TRUE ~ ep
          ),
          epa = .data$ep - .data$original_ep,
          wt_epa = .data$epa * .data$prob,
          wt_yardln = .data$yardline_100_noflip * .data$prob,
          med = dplyr::if_else(
            cumsum(.data$prob) > .5 & dplyr::lag(cumsum(.data$prob) < .5), .data$yac, as.integer(0)
          )
        ) %>%
        dplyr::summarise(
          xyac_epa = sum(.data$wt_epa) - dplyr::first(.data$air_epa),
          xyac_mean_yardage = (dplyr::first(.data$original_spot) - dplyr::first(.data$air_yards)) - sum(.data$wt_yardln),
          xyac_median_yardage = max(.data$med),
          xyac_success = sum((.data$ep > .data$original_ep) * .data$prob),
          xyac_fd = sum((.data$gain >= .data$original_ydstogo) * .data$prob)
        ) %>%
        dplyr::ungroup()

      pbp <- pbp %>%
        dplyr::left_join(xyac_vars, by = "index") %>%
        dplyr::select(-"index")

      message_completed("added xyac variables", ...)
    } else { # means no valid pass plays in the pbp
      pbp <- pbp %>%
        dplyr::mutate(
          xyac_epa = NA_real_,
          xyac_mean_yardage = NA_real_,
          xyac_median_yardage = NA_real_,
          xyac_success = NA_real_,
          xyac_fd = NA_real_
        ) %>%
        dplyr::select(-"index")
      user_message("No non-NA values for xyac calculation detected. xyac variables set to NA", "info")
    }
  }

  return(pbp)
}


### helper function for getting the data ready
prepare_xyac_data <- function(pbp) {

  # valid pass play: at least -15 air yards, less than 70 air yards, has intended receiver, has pass location
  passes <- pbp %>%
    make_model_mutations() %>%
    dplyr::mutate(
      receiver_player_name =
        stringr::str_extract(.data$desc, glue::glue('{receiver_finder}{big_parser}')),
      pass_middle = dplyr::if_else(.data$pass_location == "middle", 1, 0),
      air_is_zero = dplyr::if_else(.data$air_yards == 0, 1, 0),
      distance_to_sticks = .data$air_yards - .data$ydstogo,
      distance_to_goal = .data$yardline_100 - .data$air_yards,
      valid_pass = dplyr::if_else(
        (.data$complete_pass == 1 | .data$incomplete_pass == 1 | .data$interception == 1) &
          !is.na(.data$air_yards) & .data$air_yards >= -15 & .data$air_yards < 70 &
          !is.na(.data$receiver_player_name) & !is.na(.data$pass_location),
        1, 0
      )
    )
  return(passes)
}

### another helper function for getting the data ready
xyac_model_select <- function(pbp) {
  pbp %>%
    dplyr::select(
      "air_yards", "yardline_100", "ydstogo", "distance_to_goal",
      "down1", "down2", "down3", "down4", "air_is_zero", "pass_middle",
      "era2", "era3", "era4", "qb_hit", "home",
      "outdoors", "retractable", "dome", "distance_to_sticks"
    )
}

# These columns are being generated by add_xyac and the function tries to drop
# them in case it is being used on a pbp dataset where the columns already exist
drop.cols.xyac <- c(
  "xyac_epa", "xyac_mean_yardage", "xyac_median_yardage", "xyac_success", "xyac_fd"
)


