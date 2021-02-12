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
    usethis::ui_info("Nothing to do. Return passed data frame.")
  } else {
    # testing only
    # pbp <- g

    rlang::inform(paste0("\033[31m*\033[39m", " Computing xyac... (", "\033[33mi\033[39m", " This is a heavy task, please be patient)"))

    pbp <- pbp %>% dplyr::select(-tidyselect::any_of(drop.cols.xyac))

    # for joining at the end
    pbp <- pbp %>%
      dplyr::mutate(index = 1:dplyr::n())

    # prepare_xyac_data helper function shown below
    passes <- prepare_xyac_data(pbp) %>%
      dplyr::filter(.data$valid_pass == 1, .data$distance_to_goal != 0)

    if (!nrow(passes) == 0) {
      # initialize xyac_model to avoid R CMD check note
      xyac_model <- NULL
      suppressWarnings(
        # load the model from github because it is too big for the package
        try(
          load(url("https://github.com/guga31bb/nflfastR-data/blob/master/models/xyac_model.Rdata?raw=true")),
          silent = TRUE
        )
      )

      if (!is.null(xyac_model)) {
        xyac_vars <-
          stats::predict(
            xyac_model,
            as.matrix(passes %>% xyac_model_select())
          ) %>%
          tibble::as_tibble() %>%
          dplyr::rename(prob = "value") %>%
          dplyr::bind_cols(
            furrr::future_map_dfr(seq_along(passes$index), function(x) {
              tibble::tibble(
                "yac" = -5:70,
                "index" = passes$index[[x]],
                "distance_to_goal" = passes$distance_to_goal[[x]],
                "season" = passes$season[[x]],
                "week" = passes$week[[x]],
                "home_team" = passes$home_team[[x]],
                "posteam" = passes$posteam[[x]],
                "roof" = passes$roof[[x]],
                "half_seconds_remaining" = dplyr::if_else(
                  passes$half_seconds_remaining[[x]] <= 6,
                  0,
                  passes$half_seconds_remaining[[x]] - 6
                ),
                "down" = as.integer(passes$down[[x]]),
                "ydstogo" = as.integer(passes$ydstogo[[x]]),
                "original_ydstogo" = as.integer(passes$ydstogo[[x]]),
                "posteam_timeouts_remaining" = passes$posteam_timeouts_remaining[[x]],
                "defteam_timeouts_remaining" = passes$defteam_timeouts_remaining[[x]],
                "original_spot" = passes$yardline_100[[x]],
                "original_ep" = passes$ep[[x]],
                "air_epa" = passes$air_epa[[x]],
                "air_yards" = passes$air_yards[[x]]
              )
            })
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
            # get end result for each possibility
            yardline_100 = .data$distance_to_goal - .data$yac
          ) %>%
          dplyr::filter(.data$yac >= .data$max_loss, .data$yac <= .data$max_gain) %>%
          dplyr::select(-.data$cum_prob) %>%
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
            # flip yardline_100 and timeouts for turnovers
            yardline_100 = dplyr::if_else(.data$turnover == 1, as.integer(100 - .data$yardline_100), as.integer(.data$yardline_100)),
            posteam_timeouts_remaining = dplyr::if_else(.data$turnover == 1,
                                                        .data$defeam_timeouts_pre,
                                                        .data$posteam_timeouts_pre),
            defteam_timeouts_remaining = dplyr::if_else(.data$turnover == 1,
                                                        .data$posteam_timeouts_pre,
                                                        .data$defeam_timeouts_pre),
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
            wt_yardln = .data$yardline_100 * .data$prob,
            med = dplyr::if_else(
              cumsum(.data$prob) > .5 & dplyr::lag(cumsum(.data$prob) < .5), .data$yac, as.integer(0)
            )
          ) %>%
          dplyr::summarise(
            xyac_epa = sum(.data$wt_epa) - dplyr::first(.data$air_epa),
            xyac_mean_yardage = (dplyr::first(.data$original_spot) - dplyr::first(.data$air_yards)) - sum(.data$wt_yardln),
            xyac_median_yardage = max(.data$med),
            xyac_success = sum((.data$ep > .data$original_ep) * .data$prob),
            xyac_fd = sum((.data$gain >= .data$original_ydstogo) * .data$prob),
            .groups = "drop_last"
          ) %>%
          dplyr::ungroup()

        pbp <- pbp %>%
          dplyr::left_join(xyac_vars, by = "index") %>%
          dplyr::select(-.data$index)

        message_completed("added xyac variables", ...)

      } else {# means xyac_model isn't available
        usethis::ui_oops("This function needs to download the model data from GitHub. Please check your Internet connection and try again!")
        pbp <- pbp %>% dplyr::select(-.data$index)
      }
    } else {# means no valid pass plays in the pbp
      pbp <- pbp %>%
        dplyr::mutate(
          xyac_epa = NA_real_,
          xyac_mean_yardage = NA_real_,
          xyac_median_yardage = NA_real_,
          xyac_success = NA_real_,
          xyac_fd = NA_real_
        ) %>%
        dplyr::select(-.data$index)
      usethis::ui_info("No non-NA values for xyac calculation detected. xyac variables set to NA")
    }

    # on old versions of dplyr, a .groups column is created, which we don't want
    pbp <- pbp %>% dplyr::select(-tidyselect::any_of(".groups"))
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
        stringr::str_extract(.data$desc, "(?<=((to)|(for))\\s[:digit:]{0,2}\\-{0,1})[A-Z][A-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?"),
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
  "xyac_epa", "xyac_mean_yardage", "xyac_median_yardage", "xyac_success", "xyac_fd", ".groups"
)


