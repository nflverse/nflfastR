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
    stringr::str_extract_all(id, "(?<=_)[:upper:]{2,3}")[[1]] %in% nflfastR::teams_colors_logos$team_abbr
  )
}

# some 2000 games have play_ids like 2767.375 and 2767.703 which results in
# duplicates that can be fixed. We save play IDs as numeric first and then
# check whether or not there are duplicates when we convert them to integer
# If there are duplicates, we multiply all play IDs by 10 and check again
# If there are still duplicates, we multiply all play IDs by 100 and so on
# As soon as play IDs are unique, we save them as integer and go on
uniquify_ids <- function(ids){
  ids <- as.numeric(ids)
  int_ids <- as.integer(ids)
  mult <- 10
  while (anyDuplicated(int_ids) > 0) {
    int_ids <- as.integer(ids * mult)
    mult <- mult * 10
  }
  int_ids
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
  attr(data, "nflfastR_version") <- utils::packageVersion("nflfastR")
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
    '`nflfastR:::my_rhub_check()`',
    '`pkgdown::check_pkgdown()`',
    '`nflfastR:::nflverse_thanks()`',
    NULL
  )
}

load_model <- function(name){
  model <- switch(name,
    "ep" = fastrmodels::ep_model,
    "cp" = fastrmodels::cp_model,
    "wp" = fastrmodels::wp_model,
    "wp_spread" = fastrmodels::wp_model_spread,
    "fg" = fastrmodels::fg_model,
    "xpass" = fastrmodels::xpass_model,
    "xyac" = fastrmodels::xyac_model
  )

  # fastrmodels v2 introduced raw model vectors to make sure the models
  # are compatible with future xgboost versions
  out <- if (is.raw(model)) {
    xgboost::xgb.load.raw(model)
  } else {
    model
  }
  out
}

my_rhub_check <- function() {
  cli::cli_text("Please run the following code")
  cli::cli_text(
    "{.run rhub::rhub_check(platforms = nflfastR:::rhub_check_platforms())}"
  )
}

rhub_check_platforms <- function(){
  # plts created with
  # out <- paste0('"', rhub::rhub_platforms()$name, '"', collapse = ",\n")
  # cli::cli_code(paste0(
  #   "plts <- c(\n", out, "\n)"
  # ))

  plts <- c(
    "linux",
    "m1-san",
    "macos",
    "macos-arm64",
    "windows",
    "atlas",
    "c23",
    "clang-asan",
    "clang-ubsan",
    "clang16",
    "clang17",
    "clang18",
    "clang19",
    "clang20",
    "donttest",
    "gcc-asan",
    "gcc13",
    "gcc14",
    "gcc15",
    "intel",
    "mkl",
    "nold",
    "noremap",
    "nosuggests",
    "rchk",
    "ubuntu-clang",
    "ubuntu-gcc12",
    "ubuntu-next",
    "ubuntu-release",
    "valgrind"
  )
  exclude <- c("rchk", "nosuggests", "valgrind")
  plts[!plts %in% exclude]
}

nflverse_thanks <- function(){
  cli::cli_text("Run the following code and copy/paste its output to NEWS.md")

  cli::cli_code(
    '
    contributors <- usethis::use_tidy_thanks()
    paste(
      "Thank you to",
      glue::glue_collapse(
        paste0("&#x0040;", contributors), sep = ", ", last = ", and "
      ),
      "for their questions, feedback, and contributions towards this release."
    )'
  )
}
