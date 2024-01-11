# The function `message_completed` to create the green "...completed" message
# only exists to hide the option `in_builder` in dots
message_completed <- function(x, in_builder = FALSE) {
  if (isFALSE(in_builder)) {
    str <- paste0(my_time(), " | ", x)
    cli::cli_alert_success("{.field {str}}")
  } else if (in_builder) {
    cli::cli_alert_success("{my_time()} | {x}")
  }
}

user_message <- function(x, type) {
  if (type == "done") {
    cli::cli_alert_success("{my_time()} | {x}")
  } else if (type == "todo") {
    cli::cli_ul("{my_time()} | {x}")
  } else if (type == "info") {
    cli::cli_alert_info("{my_time()} | {x}")
  } else if (type == "oops") {
    cli::cli_alert_danger("{my_time()} | {x}")
  }
}

cli_message <- function(msg,
                        ...,
                        .cli_fct = cli::cli_alert_info,
                        .envir = parent.frame()) {
  .cli_fct(c(my_time(), " | ", msg), ..., .envir = .envir)
}

my_time <- function() strftime(Sys.time(), format = "%H:%M:%S")

# custom mode function from https://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode/8189441
custom_mode <- function(x, na.rm = TRUE) {
  if(na.rm){x <- x[!is.na(x)]}
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

rule_header <- function(x) {
  print(cli::rule(
    left = cli::style_bold(x),
    right = paste("nflfastR version", utils::packageVersion("nflfastR")),
  ))
}

rule_footer <- function(x) {
  print(cli::rule(
    left = cli::style_bold(x)
  ))
}

# read rds that has been pre-fetched
read_raw_rds <- function(raw) {
  con <- gzcon(rawConnection(raw))
  ret <- readRDS(con)
  on.exit(close(con))
  ret
}

# helper to make sure the output of the
# schedule scraper is not named 'invalid' if the source file not yet exists
maybe_valid <- function(id) {
  all(
    length(id) == 1,
    is.character(id),
    substr(id, 1, 4) %in% seq.int(1999, as.integer(format(Sys.Date(), "%Y")) + 1, 1),
    as.integer(substr(id, 6, 7)) %in% seq_len(22),
    str_extract_all(id, "(?<=_)[:upper:]{2,3}")[[1]] %in% nflfastR::teams_colors_logos$team_abbr
  )
}

# check if a package is installed
is_installed <- function(pkg) requireNamespace(pkg, quietly = TRUE)

# load raw game files esp. for debugging
load_raw_game <- function(game_id,
                          dir = getOption("nflfastR.raw_directory", default = NULL),
                          skip_local = FALSE){

  # game_id <- "2022_19_LAC_JAX"

  season <- substr(game_id, 1, 4)

  local_file <- file.path(
    dir,
    season,
    paste0(game_id, ".rds")
  )

  if (length(local_file) == 1 && file.exists(local_file) && isFALSE(skip_local)) {
    # cli::cli_progress_step("Load locally from {.path {local_file}}")
    raw <- readRDS(local_file)
  } else {
    to_load <- file.path(
      "https://raw.githubusercontent.com/nflverse/nflfastR-raw/master/raw",
      season,
      paste0(game_id, ".rds"),
      fsep = "/"
    )
    raw <- nflreadr::rds_from_url(to_load)
  }

  raw

}

# Identify sessions with sequential future resolving
is_sequential <- function() inherits(future::plan(), "sequential")

# take a time string of the format "MM:SS" and convert it to seconds
time_to_seconds <- function(time){
  as.numeric(strptime(time, format = "%M:%S")) -
    as.numeric(strptime("0", format = "%S"))
}

# write season pbp to a connected db
write_pbp <- function(seasons, dbConnection, tablename){
  p <- progressr::progressor(along = seasons)
  purrr::walk(seasons, function(x, p){
    pbp <- nflreadr::load_pbp(x)
    if (!DBI::dbExistsTable(dbConnection, tablename)){
      pbp <- dplyr::bind_rows(default_play, pbp)
    }
    DBI::dbWriteTable(dbConnection, tablename, pbp, append = TRUE)
    p("loading...")
  }, p)
}

make_nflverse_data <- function(data, type = c("play by play")){
  attr(data, "nflverse_timestamp") <- Sys.time()
  attr(data, "nflverse_type") <- type
  attr(data, "nflfastR_version") <- packageVersion("nflfastR")
  class(data) <- c("nflverse_data", "tbl_df", "tbl", "data.table", "data.frame")
  data
}

str_split_and_extract <- function(string, pattern, i){
  split_list <- stringr::str_split(string, pattern, simplify = TRUE, n = i + 1)
  split_list[, i]
}

# slightly modified version of purrr::possibly
please_work <- function(.f, otherwise = data.frame(), quiet = FALSE){
  function(...){
    tryCatch(
      expr = .f(...),
      error = function(e){
        if(isFALSE(quiet)) cli::cli_alert_warning(conditionMessage(e))
        otherwise
      }
    )
  }
}

# THIS IS CALLED FROM INSIDE get_pbp_gc AND get_pbp_nfl
# MODIFY WITH CAUTION
fetch_raw <- function(game_id,
                      dir = getOption("nflfastR.raw_directory", default = NULL)){

  season <- substr(game_id, 1, 4)

  if (is.null(dir)) {

    to_load <- file.path(
      "https://raw.githubusercontent.com/nflverse/nflfastR-raw/master/raw",
      season,
      paste0(game_id, ".rds"),
      fsep = "/"
    )

    fetched <- curl::curl_fetch_memory(to_load)

    if (fetched$status_code == 404 & maybe_valid(game_id)) {
      cli::cli_abort("The requested GameID {.val {game_id}} is not loaded yet, please try again later!")
    } else if (fetched$status_code == 500) {
      cli::cli_abort("The data hosting servers are down, please try again later!")
    } else if (fetched$status_code == 404) {
      cli::cli_abort("The requested GameID {.val {game_id}} is invalid!")
    }

    out <- read_raw_rds(fetched$content)

  } else {
    # build path to locally stored game files
    local_file <- file.path(
      dir,
      season,
      paste0(game_id, ".rds")
    )

    if (!file.exists(local_file)) {
      cli::cli_abort("File {.path {local_file}} doesn't exist!")
    }

    out <- readRDS(local_file)
  }

  out
}

release_bullets <- function() {
  c(
    '`devtools::check_mac_release()`',
    '`rhub::check_for_cran(email = "mrcaseb@gmail.com", show_status = FALSE)`',
    '`pkgdown::check_pkgdown()`',
    '`usethis::use_tidy_thanks()`',
    NULL
  )
}
