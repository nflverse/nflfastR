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
#'   \item{team_color}{Primary color}
#'   \item{team_color2}{Secondary color}
#'   \item{team_color3}{Tertiary color}
#'   \item{team_color4}{Quaternary color}
#'   \item{team_logo_wikipedia}{Url to Team logo on wikipedia}
#'   \item{team_logo_espn}{Url to higher quality logo on espn}
#' }
#' The colors are taken from Lee Sharpe's teamcolors.csv who has taken them from the
#' `teamcolors` package created by Ben Baumer and Gregory Matthews.
#' The Wikipeadia logo urls are taken from Lee Sharpe's logos.csv
#'
"teams_colors_logos"
