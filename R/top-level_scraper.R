################################################################################
# Author: Sebastian Carl
# Purpose: Top-Level functions which will be made availabe through the package
# Code Style Guide: styler::tidyverse_style()
################################################################################

#' Get NFL Play by Play Data
#'
#' @param game_ids Vector of character ids (see details for further information).
#' @param source Character - \code{nfl} for the NFL.com page or \code{old} for the old gamecenter. For \code{old}, old_game_id must be supplied
#' @param pp Logical - either \code{TRUE} or \code{FALSE} (see details for further information)
#' @param ... Additional arguments passed to the scraping functions (for internal use)
#' @param in_builder If \code{TRUE}, the final message will be suppressed (for usage inside of \code{\link{build_nflfastR_pbp}}).
#' @details To load valid game_ids please use the package function \code{\link{fast_scraper_schedules}}.
#'
#' The \code{source} parameter controls from which source the data is being
#' loaded. The old parameters \code{rs} as well as \code{gc}
#' are not valid anymore. Please use \code{nfl} or \code{old}.
#'
#' The \code{pp} parameter controls if the scraper should use parallel processing.
#' Please note that the initiating process takes a few seconds which means it
#' may be better to set \code{pp = FALSE} if you are scraping just a few games.
#' @return Data frame where each individual row represents a single play for
#' all passed game_ids containing the following
#'
#' @seealso \code{\link{field_descriptions}}, \url{https://nflfastr.com/articles/field_descriptions.html}
#'
#' @export
#' @examples
#' \donttest{
#' # Get pbp data for two games
#' fast_scraper(c("2019_01_GB_CHI", "2013_21_SEA_DEN"))
#' }
fast_scraper <- function(game_ids, source = "nfl", pp = FALSE, ..., in_builder = FALSE) {

  # Error handling to correct source type
  if (!source %in% c("nfl", "old")) {
    usethis::ui_stop("You tried to specify a source that isn't the new NFL web page or the old source.\nPlease remove source from your request, use {usethis::ui_code('source = \"nfl\"')}, or {usethis::ui_code('source = \"old\"')}.")
  }

  # No parallel processing demanded -> use purrr
  if (pp == FALSE) {
    suppressWarnings({
      progressr::with_progress({
        p <- progressr::progressor(along = game_ids)
        pbp <- purrr::map_dfr(game_ids, function(x, ...){
          if (substr(x, 1, 4) < 2011 & source == "nfl") {
            plays <- get_pbp_gc(x, ...)
          } else if (source == "nfl") {
            plays <- get_pbp_nfl(x, ...)
          } else {
            plays <- get_pbp_cdns(x, ...)
          }
          p(sprintf("x=%s", as.character(x)))
          return(plays)
        }, ...)
      })

      if(purrr::is_empty(pbp) == FALSE) {
        usethis::ui_done("Download finished. Adding variables...")
        pbp <- pbp %>%
          add_game_data(source) %>%
          add_nflscrapr_mutations() %>%
          add_ep() %>%
          add_air_yac_ep() %>%
          add_wp() %>%
          add_air_yac_wp() %>%
          add_cp() %>%
          add_drive_results() %>%
          add_series_data() %>%
          select_variables()
      }
    })
  }

  # User wants parallel processing: check if the required package is installed.
  # Stop and Error when missing
  else if (pp == TRUE & !requireNamespace("furrr", quietly = TRUE)) {
    usethis::ui_stop("Package {usethis::ui_value('furrr')} needed for parallel processing. Please install it with {usethis::ui_code('install.packages(\"furrr\")')}.")
  }
  else {
    if (length(game_ids)<=4){
      usethis::ui_info("You have passed only {length(game_ids)} GameID(s) to parallel processing.\nPlease note that the initiating process takes a few seconds\nand consider using {usethis::ui_code('pp = FALSE')} for a small number of games.\n")
    }
    suppressWarnings({
      progressr::with_progress({
        p <- progressr::progressor(along = game_ids)
        future::plan("multiprocess")
        pbp <- furrr::future_map_dfr(game_ids, function(x, ...){
          if (substr(x, 1, 4) < 2011 & source == "nfl") {
            plays <- get_pbp_gc(x, ...)
          } else if (source == "nfl") {
            plays <- get_pbp_nfl(x, ...)
          } else {
            plays <- get_pbp_cdns(x, ...)
          }
          p(sprintf("x=%s", as.character(x)))
          return(plays)
        }, ...)
      })

      if(purrr::is_empty(pbp) == FALSE) {
        usethis::ui_done("Download finished. Adding variables...")
        pbp <- pbp %>%
          add_game_data(source) %>%
          add_nflscrapr_mutations() %>%
          add_ep() %>%
          add_air_yac_ep() %>%
          add_wp() %>%
          add_air_yac_wp() %>%
          add_cp() %>%
          add_drive_results() %>%
          add_series_data() %>%
          select_variables()
      }
    })
  }
  if (!in_builder) {usethis::ui_done("{usethis::ui_field('Procedure completed.')}")}
  return(pbp)
}


#' Get NFL Play by Play Highlight Clips
#'
#' @param game_ids Vector of numeric or character ids
#' @param pp Logical - either \code{TRUE} or \code{FALSE} (see details for further information)
#' @details To load valid game_ids please use the package function \code{\link{fast_scraper_schedules}}.
#' The \code{pp} parameter controls if the scraper should use parallel processing.
#' Please note that the initiating process takes a few seconds which means it
#' may be better to set \code{pp = FALSE} if you are scraping just a few games.
#' @return Data frame containing game_id, play_id for all plays with available
#' highlightclip and the clip url
# @export
#' @noRd
#' @examples
#' \donttest{
#' # Get highlight clips for two 2019 games using parallel processing
#' # game_ids <- c("2019090804", "2019101700")
#' # clips <- fast_scraper_clips(game_ids, pp = TRUE)
#' }
fast_scraper_clips <- function(game_ids, pp = FALSE) {
  stop("The NFL removed the public available data feed. We are working on a new solution.\n Meanwhile please check https://github.com/guga31bb/nflfastR-data/tree/master/legacy-data for data of the seasons 2000-2019")

  scraper_func <- get_pbp_highlights

  # No parallel processing demanded -> use purrr
  if (pp == FALSE) {
    suppressWarnings(
      clips <- purrr::map_dfr(game_ids, scraper_func)
    )
  }

  # User wants parallel processing: check if the required package is installed.
  # Stop and Error when missing
  else if (pp == TRUE & !requireNamespace("furrr", quietly = TRUE)) {
    stop("Package \"furrr\" needed for parallel processing. Please install/load it.")
  }
  else {
    if (length(game_ids)<=4){
      message(glue::glue("You have passed only {length(game_ids)} GameIDs to parallel processing.\nPlease note that the initiating process takes a few seconds\nand consider using pp=FALSE for a small number of games."))
    }
    suppressWarnings({
      future::plan("multiprocess")
      clips <- furrr::future_map_dfr(game_ids, scraper_func, .progress = TRUE)
    })
  }
  return(clips)
}

#' Get team rosters for multiple seasons
#'
#' Given years return a dataset with each player listed as part of the roster.
#'
#' @param seasons A vector of 4-digit years associated with given NFL seasons
#' @param pp Logical - either \code{TRUE} or \code{FALSE} (see details for further information)
#' @details The roster data is accessed via the free to use Sleeper API.
#' The \code{pp} parameter controls if the scraper should use parallel processing.
#' Please note that the initiating process takes a few seconds which means it
#' may be better to set \code{pp = FALSE} if you are scraping just a few seasons.
#' @return Data frame where each individual row represents a player in
#' the roster of the given team and season containing the following information:
#' \describe{
#' \item{season}{4 digit season year.}
#' \item{team}{Team abbreviation.}
#' \item{position}{Abbreviation of the player's position (e.g. "QB", "WR", "RB", "CB"...).}
#' \item{depth_chart_position}{Starting with the 2020 season: the abbreviation of the players depth_chart_position.}
#' \item{jersey_number}{The player's 2 digit jersey number.}
#' \item{status}{String indicating the status of the player (e.g. "Active", "Inactive", "Injured Reserve"...) at the update time \code{update_dt} (see below)}
#' \item{full_name}{Full name of the player.}
#' \item{first_name}{First name of the player.}
#' \item{last_name}{Last name of the player.}
#' \item{birth_date}{Birth date of the player.}
#' \item{height}{Height of the player.}
#' \item{weight}{Weight of the player.}
#' \item{college}{Name of the college the player has attended.}
#' \item{high_school}{Name of the High School the player has attended (only non-NA for players who were listed in the 2020 season).}
#' \item{gsis_id}{The player's NFL GSIS ID, which can be used to link the player to play-by-play data.}
#' \item{espn_id}{The player's ESPN ID (only non-NA for players who were listed in the 2020 season).}
#' \item{sportradar_id}{The player's Sportradar ID (only non-NA for players who were listed in the 2020 season).}
#' \item{yahoo_id}{The player's Yahoo Sports ID (only non-NA for players who were listed in the 2020 season).}
#' \item{rotowire_id}{The player's Rotowire ID (only non-NA for players who were listed in the 2020 season).}
#' \item{update_dt}{Date and time when the current entry was last updated (starting with the 2020 season).}
#' \item{headshot_url}{URL to a player image (starting in the 2020 season on ESPN servers).}
#' }
#' @examples
#' \donttest{
#' # Roster of the 2019 and 2020 seasons
#' fast_scraper_roster(2019:2020)
#' }
#' @export
fast_scraper_roster <- function(seasons, pp = FALSE) {

  # No parallel processing demanded -> use purrr
  if (pp == FALSE) {
    suppressWarnings(
      progressr::with_progress({
        p <- progressr::progressor(along = seasons)
        ret <- purrr::map_dfr(seasons, function(x){
          out <- get_scheds_and_rosters(x, "roster")
          p(sprintf("x=%s", as.integer(x)))
          return(out)
        })
      })
    )
  }

  # User wants parallel processing: check if the required package is installed.
  # Stop and Error when missing
  else if (pp == TRUE & !requireNamespace("furrr", quietly = TRUE)) {
    usethis::ui_stop("Package {usethis::ui_value('furrr')} needed for parallel processing. Please install it with {usethis::ui_code('install.packages(\"furrr\")')}.")
  }
  else {
    if (length(seasons)<=10){
      usethis::ui_info("You have passed only {length(seasons)} season(s) to parallel processing.\nPlease note that the initiating process takes a few seconds\nand consider using {usethis::ui_code('pp = FALSE')} for a small number of seasons.")
    }
    suppressWarnings({
      progressr::with_progress({
        p <- progressr::progressor(along = seasons)
        future::plan("multiprocess")
        ret <- furrr::future_map_dfr(seasons, function(x){
          out <- get_scheds_and_rosters(x, "roster")
          p(sprintf("x=%s", as.integer(x)))
          return(out)
        })
      })
    })
  }
  return(ret)
}

#' Get NFL Season Schedules
#'
#' @param seasons Vector of numeric or character 4 digit seasons
#' @param pp Logical - either \code{TRUE} or \code{FALSE} (see details for further information)
#' @details This functions now incorporates the games file provided and maintained
#' by Lee Sharpe.
#'
#' The \code{pp} parameter controls if the scraper should use parallel processing.
#' Please note that the initiating process takes a few seconds which means it
#' may be better to set \code{pp = FALSE} if you are scraping less than 10 seasons.
#' @return Data frame containing the following detailed game information:
#' \describe{
#' \item{game_id}{Character identifier including season, week, away team and home team}
#' \item{season}{4 digit season year.}
#' \item{game_type}{One of 'REG', 'WC', 'DIV', 'CON', 'SB' indicating if a game was a regular season game or one of the playoff rounds.}
#' \item{week}{Numeric week number.}
#' \item{gameday}{Game date in format yyyy/mm/dd.}
#' \item{weekday}{The day of the week on which the game occcured.}
#' \item{gametime}{The kickoff time of the game. This is represented in 24-hour time and the Eastern time zone, regardless of what time zone the game was being played in.}
#' \item{away_team}{Away team abbreviation.}
#' \item{home_team}{Home team abbreviation.}
#' \item{away_score}{The number of points the away team scored. Is 'NA' for games which haven't yet been played.}
#' \item{home_score}{The number of points the home team scored. Is 'NA' for games which haven't yet been played.}
#' \item{home_result}{Equals home_score - away_score and means the game outcome from the perspective of the home team.}
#' \item{stadium}{Name of the stadium the game was or will be played in. (Source: Pro-Football-Reference)}
#' \item{location}{Either 'Home' o 'Neutral' indicating if the home team played at home or at a neutral site. }
#' \item{roof}{One of 'dome', 'outdoors', 'closed', 'open' indicating indicating the roof status of the stadium the game was played in. (Source: Pro-Football-Reference)}
#' \item{surface}{What type of ground the game was played on. (Source: Pro-Football-Reference)}
#' \item{old_game_id}{Unique game identifier of the old NFL API.}
#' }
#' @export
#' @examples
#'\donttest{
#' # Get schedules for the whole 2015 - 2018 seasons
#' fast_scraper_schedules(2015:2018)
#' }
fast_scraper_schedules <- function(seasons, pp = FALSE) {

  # No parallel processing demanded -> use purrr
  if (pp == FALSE) {
    suppressWarnings(
      progressr::with_progress({
        p <- progressr::progressor(along = seasons)
        ret <- purrr::map_dfr(seasons, function(x){
          out <- get_scheds_and_rosters(x, "schedule")
          p(sprintf("x=%s", as.integer(x)))
          return(out)
        })
      })
    )
  }

  # User wants parallel processing: check if the required package is installed.
  # Stop and Error when missing
  else if (pp == TRUE & !requireNamespace("furrr", quietly = TRUE)) {
    usethis::ui_stop("Package {usethis::ui_value('furrr')} needed for parallel processing. Please install it with {usethis::ui_code('install.packages(\"furrr\")')}.")
  }
  else {
    if (length(seasons)<=10){
      usethis::ui_info("You have passed only {length(seasons)} season(s) to parallel processing.\nPlease note that the initiating process takes a few seconds\nand consider using {usethis::ui_code('pp = FALSE')} for a small number of seasons.")
    }
    suppressWarnings({
      progressr::with_progress({
        p <- progressr::progressor(along = seasons)
        future::plan("multiprocess")
        ret <- furrr::future_map_dfr(seasons, function(x){
          out <- get_scheds_and_rosters(x, "schedule")
          p(sprintf("x=%s", as.integer(x)))
          return(out)
        })
      })
    })
  }
  return(ret)
}
