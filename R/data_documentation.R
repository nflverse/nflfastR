################################################################################
# Author: Sebastian Carl
# Purpose: Documenting Data Files
# Code Style Guide: styler::tidyverse_style()
################################################################################

#' NFL Team names, colors and logo urls.
#'
#' @docType data
#' @format A data frame with 36 rows and 10 variables containing NFL team level
#' information, including franchises in multiple cities:
#' \describe{
#'   \item{team_abbr}{Team abbreviation}
#'   \item{team_name}{Complete Team name}
#'   \item{team_id}{Team id used in the roster function}
#'   \item{team_nick}{Nickname}
#'   \item{team_conf}{Conference}
#'   \item{team_division}{Division}
#'   \item{team_color}{Primary color}
#'   \item{team_color2}{Secondary color}
#'   \item{team_color3}{Tertiary color}
#'   \item{team_color4}{Quaternary color}
#'   \item{team_logo_wikipedia}{Url to Team logo on wikipedia}
#'   \item{team_logo_espn}{Url to higher quality logo on espn}
#'   \item{team_wordmark}{Url to team wordmarks}
#'   \item{team_conference_logo}{Url to AFC and NFC logos}
#'   \item{team_league_logo}{Url to NFL logo}
#' }
#' The primary and secondary colors have been taken from nfl.com with some modifications
#' for better team distinction and most recent team color themes.
#' The tertiary and quaternary colors are taken from Lee Sharpe's teamcolors.csv
#' who has taken them from the `teamcolors` package created by Ben Baumer and
#' Gregory Matthews. The Wikipeadia logo urls are taken from Lee Sharpe's logos.csv
#' Team wordmarks from nfl.com
#' @examples
#' \donttest{
#' teams_colors_logos
#' }
"teams_colors_logos"

#' nflfastR Field Descriptions
#'
#' @docType data
#' @format A data frame including names and descriptions of all variables in
#' an nflfastR dataset.
#' @seealso The searchable table on the
#' [nflfastR website](https://www.nflfastr.com/articles/field_descriptions.html)
#' @examples
#' \donttest{
#' field_descriptions
#' }
"field_descriptions"

#' NFL Stat IDs and their Meanings
#'
#' @docType data
#' @format A data frame including NFL stat IDs, names and descriptions used in
#' an nflfastR dataset.
#' @source \url{http://www.nflgsis.com/gsis/Documentation/Partners/StatIDs.html}
#' @examples
#' \donttest{
#' stat_ids
#' }
"stat_ids"
