#' Compute Series Conversion Information from Play by Play
#'
#' @description A "Series" begins on a 1st and 10 and each team attempts to either earn
#'   a new 1st down (on offense) or prevent the offense from converting a new
#'   1st down (on defense). Series conversion rate represents how many series
#'   have been either converted to a new 1st down or ended in a touchdown.
#'   This function computes series conversion rates on offense and defense from
#'   nflverse play-by-play data along with other series results.
#'   The function automatically removes series that ended in a QB kneel down.
#'
#' @param pbp Play-by-play data as returned by [`load_pbp()`], [`build_nflfastR_pbp()`], or
#'   [`fast_scraper()`].
#' @param weekly If `TRUE`, returns week-by-week stats, otherwise,
#'   season-by-season stats in argument `pbp`.
#'
#' @return A data frame of series information including the following columns:
#' \describe{
#' \item{season}{The NFL season}
#' \item{team}{NFL team abbreviation}
#' \item{week}{Week if `weekly` is `TRUE`}
#' \item{off_n}{The number of series the offense played (excludes QB kneel
#' downs, kickoffs, extra point/two point conversion attempts, non-plays, and
#' plays that do not list a "posteam")}
#' \item{off_scr}{The rate at which a series ended in either new 1st down or
#' touchdown while the offense was on the field}
#' \item{off_scr_1st}{The rate at which an offense earned a 1st down
#' or scored a touchdown on 1st down}
#' \item{off_scr_2nd}{The rate at which an offense earned a 1st down
#' or scored a touchdown on 2nd down}
#' \item{off_scr_3rd}{The rate at which an offense earned a 1st down
#' or scored a touchdown on 3rd down}
#' \item{off_scr_4th}{The rate at which an offense earned a 1st down
#' or scored a touchdown on 4th down}
#' \item{off_1st}{The rate of series that ended in a new 1st down while the
#' offense was on the field (does not include offensive touchdown)}
#' \item{off_td}{The rate of series that ended in an offensive touchdown while the
#' offense was on the field}
#' \item{off_fg}{The rate of series that ended in a field goal attempt while the
#' offense was on the field}
#' \item{off_punt}{The rate of series that ended in a punt while the
#' offense was on the field}
#' \item{off_to}{The rate of series that ended in a turnover (including on downs), in an
#' opponent score, or at the end of half (or game) while the
#' offense was on the field}
#' \item{def_n}{The number of series the defense played (excludes QB kneel
#' downs, kickoffs, extra point/two point conversion attempts, non-plays, and
#' plays that do not list a "posteam")}
#' \item{def_scr}{The rate at which a series ended in either new 1st down or
#' touchdown while the defense was on the field}
#' \item{def_scr_1st}{The rate at which a defense allowed a
#' 1st down or touchdown on 1st down}
#' \item{def_scr_2nd}{The rate at which a defense allowed a
#' 1st down or touchdown on 2nd down}
#' \item{def_scr_3rd}{The rate at which a defense allowed a
#' 1st down or touchdown on 3rd down}
#' \item{def_scr_4th}{The rate at which a defense allowed a
#' 1st down or touchdown on 4th down}
#' \item{def_1st}{The rate of series that ended in a new 1st down while the
#' defense was on the field (does not include offensive touchdown)}
#' \item{def_td}{The rate of series that ended in an offensive touchdown while the
#' defense was on the field}
#' \item{def_fg}{The rate of series that ended in a field goal attempt while the
#' defense was on the field}
#' \item{def_punt}{The rate of series that ended in a punt while the
#' defense was on the field}
#' \item{def_to}{The rate of series that ended in a turnover (including on downs), in an
#' opponent score, or at the end of half (or game) while the
#' defense was on the field}
#' }
#' @export
#'
#' @examples
#' \donttest{
#'   pbp <- nflfastR::load_pbp(2021)
#'
#'   weekly <- calculate_series_conversion_rates(pbp, weekly = TRUE)
#'   dplyr::glimpse(weekly)
#'
#'   overall <- calculate_series_conversion_rates(pbp, weekly = FALSE)
#'   dplyr::glimpse(overall)
#' }
calculate_series_conversion_rates <- function(pbp,
                                              weekly = FALSE){
  # For tests
  # pbp <- nflreadr::load_pbp()

# Offense -----------------------------------------------------------------

  off_series <- pbp %>%
    dplyr::filter(
      !is.na(.data$down),
      .data$series_result != "QB kneel"
      # .data$rush == 1 | .data$pass == 1
    ) %>%
    dplyr::group_by(.data$season, .data$week, team = .data$posteam, .data$series) %>%
    dplyr::summarise(
      conversion = dplyr::first(.data$series_success),
      result = dplyr::first(.data$series_result),
      last_down = dplyr::last(.data$down),
      .groups = "drop"
    )

  offense <- off_series %>%
    dplyr::group_by(.data$season, .data$team, .data$week) %>%
    dplyr::summarise(
      off_n = dplyr::n(),
      off_scr = mean(.data$conversion),
      off_scr_1st = mean(.data$last_down == 1 * .data$conversion),
      off_scr_2nd = mean(.data$last_down == 2 * .data$conversion),
      off_scr_3rd = mean(.data$last_down == 3 * .data$conversion),
      off_scr_4th = mean(.data$last_down == 4 * .data$conversion),
      off_1st = mean(.data$result == "First down"),
      off_td = mean(.data$result == "Touchdown"),
      off_fg = mean(.data$result %in% c("Field goal", "Missed field goal")),
      off_punt = mean(.data$result == "Punt"),
      off_to = mean(
        .data$result %in% c("Turnover on downs", "Turnover", "Opp touchdown", "Safety", "End of half")
      ),
      .groups = "drop"
    )

# Defense -----------------------------------------------------------------

  def_series <- pbp %>%
    dplyr::filter(
      !is.na(.data$down),
      .data$series_result != "QB kneel"
      # .data$rush == 1 | .data$pass == 1
    ) %>%
    dplyr::group_by(.data$season, .data$week, team = .data$defteam, .data$series) %>%
    dplyr::summarise(
      conversion = dplyr::first(.data$series_success),
      result = dplyr::first(.data$series_result),
      last_down = dplyr::last(.data$down),
      .groups = "drop"
    )

  defense <- def_series %>%
    dplyr::group_by(.data$season, .data$team, .data$week) %>%
    dplyr::summarise(
      def_n = dplyr::n(),
      def_scr = mean(.data$conversion),
      def_scr_1st = mean(.data$last_down == 1 * .data$conversion),
      def_scr_2nd = mean(.data$last_down == 2 * .data$conversion),
      def_scr_3rd = mean(.data$last_down == 3 * .data$conversion),
      def_scr_4th = mean(.data$last_down == 4 * .data$conversion),
      def_1st = mean(.data$result == "First down"),
      def_td = mean(.data$result == "Touchdown"),
      def_fg = mean(.data$result %in% c("Field goal", "Missed field goal")),
      def_punt = mean(.data$result == "Punt"),
      def_to = mean(
        .data$result %in% c("Turnover on downs", "Turnover", "Opp touchdown", "Safety", "End of half")
      ),
      .groups = "drop"
    )


# Offense + Defense -------------------------------------------------------

  combined <- dplyr::left_join(offense, defense, by = c("season", "team", "week"))

  if (isFALSE(weekly)){
    combined <- combined %>%
      dplyr::select(-"week") %>%
      dplyr::group_by(.data$season, .data$team) %>%
      dplyr::summarise(
        dplyr::across(.cols = dplyr::ends_with("_n"), .fns = sum),
        dplyr::across(.cols = !dplyr::ends_with("_n"), .fns = mean),
        .groups = "drop"
      ) %>%
      dplyr::relocate("def_n", .after = "off_to")
  }

  combined
}
