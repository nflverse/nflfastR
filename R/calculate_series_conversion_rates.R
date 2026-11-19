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
#' try({# to avoid CRAN test problems
#'   pbp <- nflfastR::load_pbp(2021)
#'
#'   weekly <- calculate_series_conversion_rates(pbp, weekly = TRUE)
#'   str(
#'     weekly,
#'     list.len = ncol(weekly),
#'     vec.len = 10,
#'     give.attr = FALSE,
#'     strict.width = "cut"
#'   )
#'
#'   overall <- calculate_series_conversion_rates(pbp, weekly = FALSE)
#'   str(
#'     overall,
#'     list.len = ncol(overall),
#'     vec.len = 10,
#'     give.attr = FALSE,
#'     strict.width = "cut"
#'   )
#' })
#' }
calculate_series_conversion_rates <- function(pbp, weekly = FALSE) {
  if (isTRUE(weekly)) {
    grp <- c("season", "team", "week")
  } else if (isFALSE(weekly)) {
    grp <- c("season", "team")
  }

  missing <- setdiff(series_pbp_columns(), names(pbp))
  if (length(missing) > 0) {
    cli::cli_abort(
      "{.arg pbp} is missing the required variable{?s} {.val {missing}}."
    )
  }

  # A data.table of the required columns only, filtered right away, so it
  # never changes the user's pbp by reference
  plays <- as_pbp_dt(.subset(pbp, series_pbp_columns()))
  plays <- plays[!is.na(down) & series_result != "QB kneel"]

  offense <- series_rates(plays, "posteam", "off", grp)
  defense <- series_rates(plays, "defteam", "def", grp)

  # Like a full join: the rows of offense, followed by the defense rows without
  # an offense row
  out <- merge(offense, defense, by = grp, all = TRUE, sort = FALSE)

  # a tibble without importing tibble
  data.table::setDF(out)
  data.table::setattr(out, "class", c("tbl_df", "tbl", "data.frame"))
  out
}

series_pbp_columns <- function() {
  c(
    "season",
    "week",
    "posteam",
    "defteam",
    "series",
    "series_success",
    "series_result",
    "down"
  )
}

# The series rates of the team in column team_col (posteam or defteam), in
# columns with the prefix side (off or def). The rows are ordered by grp with
# missing teams last, like dplyr.
series_rates <- function(plays, team_col, side, grp) {
  series <- plays[,
    list(
      conversion = series_success[1],
      result = series_result[1],
      last_down = down[.N]
    ),
    by = c("season", "week", team_col, "series")
  ]
  data.table::setnames(series, team_col, "team")

  # The rates are sums divided by the number of series: GForce optimizes the
  # sums, and k / n is the exactly rounded rate. mean() can differ in the last
  # digit (base R for doubles like conversion, data.table's own mean()).
  series[, `:=`(
    scr = conversion,
    scr_1st = (last_down == 1) * conversion,
    scr_2nd = (last_down == 2) * conversion,
    scr_3rd = (last_down == 3) * conversion,
    scr_4th = (last_down == 4) * conversion,
    `1st` = result == "First down",
    td = result == "Touchdown",
    fg = result %chin% c("Field goal", "Missed field goal"),
    punt = result == "Punt",
    to = result %chin%
      c(
        "Turnover on downs",
        "Turnover",
        "Opp touchdown",
        "Safety",
        "End of half"
      )
  )]
  stat_cols <- c(
    "scr",
    "scr_1st",
    "scr_2nd",
    "scr_3rd",
    "scr_4th",
    "1st",
    "td",
    "fg",
    "punt",
    "to"
  )
  out <- series[,
    c(list(n = .N), lapply(.SD, sum)),
    by = grp,
    .SDcols = stat_cols
  ]
  out[, (stat_cols) := lapply(.SD, function(x) x / n), .SDcols = stat_cols]
  data.table::setnames(
    out,
    c("n", stat_cols),
    paste(side, c("n", stat_cols), sep = "_")
  )
  data.table::setorderv(out, grp, na.last = TRUE)
  out
}
