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
#'   \item{team_wordmark}{Url to team wordmarks}
#' }
#' The colors are taken from Lee Sharpe's teamcolors.csv who has taken them from the
#' `teamcolors` package created by Ben Baumer and Gregory Matthews.
#' The Wikipeadia logo urls are taken from Lee Sharpe's logos.csv
#' Team wordmarks from nfl.com
#'
"teams_colors_logos"

#' PBP column descriptions
#'
#' This dataframe describes the columns of the play-by-play data created by the \code{build_nflfastr_pbp()} function.
#'
#' @docType data
#' @format  A dataframe with two columns: field, and description.
#'
#' \describe{
#'  \item{field}{Name of column as shown in PBP dataframe}
#'  \item{description}{Description of column}
#' }
#'
"field_descriptions"
