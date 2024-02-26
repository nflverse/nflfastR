#' Download Raw PBP Data to Local Filesystem
#'
#' The functions [build_nflfastR_pbp()] and [fast_scraper()] support loading
#' raw pbp data from local file systems instead of Github servers.
#' This function is intended to help setting this up. It loads raw pbp data
#' and saves it in the given directory split by season in subdirectories.
#'
#' @param game_ids A vector of nflverse game IDs.
#' @param dir Path to local directory (defaults to option "nflfastR.raw_directory").
#'   nflfastR will download the raw game files split by season into one sub
#'   directory per season.
#'
#' @returns The function returns a data frame with one row for each downloaded file and
#' the following columns:
#'  - `success` if the HTTP request was successfully performed, regardless of the
#'  response status code. This is `FALSE` in case of a network error, or in case
#'  you tried to resume from a server that did not support this. A value of `NA`
#'  means the download was interrupted while in progress.
#'  - `status_code` the HTTP status code from the request. A successful download is
#'  usually `200` for full requests or `206` for resumed requests. Anything else
#'  could indicate that the downloaded file contains an error page instead of the
#'  requested content.
#'  - `resumefrom` the file size before the request, in case a download was resumed.
#'  - `url` final url (after redirects) of the request.
#'  - `destfile` downloaded file on disk.
#'  - `error` if `success == FALSE` this column contains an error message.
#'  - `type` the `Content-Type` response header value.
#'  - `modified` the `Last-Modified` response header value.
#'  - `time` total elapsed download time for this file in seconds.
#'  - `headers` vector with http response headers for the request.
#' @export
#'
#' @seealso [build_nflfastR_pbp()], [missing_raw_pbp()]
#'
#' @examples
#' \donttest{
#' # CREATE LOCAL TEMP DIRECTORY
#' local_dir <- tempdir()
#'
#' # LOAD AND SAVE A GAME TO TEMP DIRECTORY
#' save_raw_pbp("2021_20_BUF_KC", dir = local_dir)
#'
#' # REMOVE THE DIRECTORY
#' unlink(file.path(local_dir, 2021))
#' }
save_raw_pbp <- function(game_ids,
                         dir = getOption("nflfastR.raw_directory", default = NULL)){
  verify_game_ids(game_ids = game_ids)
  if(is.null(dir)){
    cli::cli_abort("Invalid argument {.arg dir}. Do you need to set \\
                   {.code options(nflfastR.raw_directory)}?")
  } else if (!dir.exists(dir)){
    cli::cli_abort("You've asked to save raw pbp to {.path {dir}} which \\
                   doesn't exist. Please create it.")
  }
  seasons <- substr(game_ids, 1, 4)
  season_folders <- file.path(dir, unique(seasons)) %>% sort()
  missing_season_folders <- season_folders[!dir.exists(season_folders)]
  created_folders <- vapply(missing_season_folders, dir.create, FUN.VALUE = logical(1L))
  to_load <- file.path(
    "https://raw.githubusercontent.com/nflverse/nflfastR-raw/master/raw",
    seasons,
    paste0(game_ids, ".rds"),
    fsep = "/"
  )
  save_to <- file.path(
    dir, seasons, paste0(game_ids, ".rds")
  )
  curl::multi_download(to_load, save_to)
}

#' Compute Missing Raw PBP Data on Local Filesystem
#'
#' Uses [nflreadr::load_schedules()] to load game IDs of finished games and
#' compares these IDs to all files saved under `dir`.
#' This function is intended to serve as input for [save_raw_pbp()].
#'
#' @inheritParams save_raw_pbp
#' @inheritParams nflreadr::load_schedules
#' @param verbose If `TRUE`, will print number of missing game files as well as
#'   oldest and most recent missing ID to console.
#'
#' @return A character vector of missing game IDs. If no files are missing,
#'  returns `NULL` invisibly.
#' @export
#'
#' @seealso [save_raw_pbp()]
#'
#' @examples
#' \donttest{
#' try(
#' missing <- missing_raw_pbp(tempdir())
#' )
#' }
missing_raw_pbp <- function(dir = getOption("nflfastR.raw_directory", default = NULL),
                            seasons = TRUE,
                            verbose = TRUE){
  if(is.null(dir)){
    cli::cli_abort("Invalid argument {.arg dir}. Do you need to set \\
                   {.code options(nflfastR.raw_directory)}?")
  } else if (!dir.exists(dir)){
    cli::cli_abort("You've asked to check raw pbp in {.path {dir}} which \\
                   doesn't exist. Please create it.")
  }
  local_games <- sapply(list.files(dir, full.names = TRUE), list.files) %>%
    unlist(use.names = FALSE) %>%
    tools::file_path_sans_ext()

  finished_games <- nflreadr::load_schedules(seasons = seasons) %>%
    dplyr::filter(!is.na(.data$result)) %>%
    dplyr::pull(.data$game_id)

  local_missing_games <- finished_games[!finished_games %in% local_games]

  if (length(local_missing_games) == 0){
    cli::cli_alert_success("No missing games!")
    return(invisible(NULL))
  }

  if (isTRUE(verbose)){
    cli::cli_alert_info(
      "You are missing {length(local_missing_games)} game file{?s}. \\
       The oldest missing game is {.val {local_missing_games[[1]]}}. \\
       The most recent missing game is \\
       {.val {local_missing_games[length(local_missing_games)]}}."
    )
  }

  local_missing_games
}


verify_game_ids <- function(game_ids){
  # game_ids <- c(
  #   "2021_02_LAC_KC",
  #   "Hello World",
  #   "2028_01_LAC_JAX",
  #   "2022_27_LAC_BUF",
  #   "2021_02_LAC_KAC"
  # )
  season_check <- substr(game_ids, 1, 4) %in% seq.int(1999, as.integer(format(Sys.Date(), "%Y")) + 1, 1)
  week_check <- as.integer(substr(game_ids, 6, 7)) %in% seq_len(22)
  team_name_check <-
    vapply(
      str_extract_all(game_ids, "(?<=_)[:upper:]{2,3}"),
      function(t) all(t %in% nflfastR::teams_colors_logos$team_abbr),
      FUN.VALUE = logical(1L)
    )
  combined_check <- season_check & week_check & team_name_check

  if (any(combined_check == FALSE)){
    cli::cli_abort("The game IDs {.val {game_ids[!combined_check]}} seem to be invalid!")
  }

  invisible(NULL)
}
