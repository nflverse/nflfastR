#' Compute Series Conversion Information from Play by Play
#'
#' @description A "Series" is a series of downs a team plays to either convert
#'   to a new 1st down (on offense) or prevent the offense from converting a new
#'   1st down (on defense). The series conversion rate describes how many series
#'   have been either converted to a new 1st down or ended in a touchdown.
#'   This function computes series conversion rates on offense and defense from
#'   nflverse play-by-play data along with other series result rates.
#'
#' @param pbp Play-by-play data as returned by [`load_pbp()`], [`build_nflfastR_pbp()`], or
#'   [`fast_scraper()`].
#' @param weekly If `TRUE`, returns week-by-week stats, otherwise,
#'   season-by-season stats in argument `pbp`.
#'
#' @return A data frame of series conversion results
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
