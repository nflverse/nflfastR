# The function `message_completed` to create the green "...completed" message
# only exists to hide the option `in_builder` in dots

message_completed <- function(x, in_builder = FALSE) {
  if (!in_builder) {
    usethis::ui_done("{usethis::ui_field(x)}")
  } else if (in_builder) {
    usethis::ui_done(x)
  }
}

# custom mode function from https://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode/8189441
custom_mode <- function(x, na.rm = TRUE) {
  if(na.rm){x <- x[!is.na(x)]}
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

rule_header <- function(x) {
  rlang::inform(
    cli::rule(
      left = glue::glue("\033[1m{x}\033[22m"),#crayon::bold(x),
      right = paste0("nflfastR version ", utils::packageVersion("nflfastR")),
      width = getOption("width")
    )
  )
}

rule_footer <- function(x) {
  rlang::inform(
    cli::rule(
      left = glue::glue("\033[1m{x}\033[22m"),#crayon::bold(x),
      width = getOption("width")
    )
  )
}


# Load cleaned pbp from the data repo -------------------------------------

# helper that loads multiple seasons from the datarepo either into memory
# or writes it into a db using some forwarded arguments in the dots
load_pbp <- function(seasons, in_db = FALSE, ..., qs = FALSE) {

  if (isTRUE(qs) && !is_installed("qs")) {
    usethis::ui_stop("Package {usethis::ui_value('qs')} required for argument {usethis::ui_value('qs = TRUE')}. Please install it.")
  }

  most_recent <- dplyr::if_else(
    lubridate::month(lubridate::today("America/New_York")) >= 9,
    lubridate::year(lubridate::today("America/New_York")),
    lubridate::year(lubridate::today("America/New_York")) - 1
  )

  if (!all(seasons %in% 1999:most_recent)) {
    usethis::ui_stop("Please pass valid seasons between 1999 and {most_recent}")
  }

  if (length(seasons) > 1 && inherits(future::plan(), "sequential") && isFALSE(in_db)) {
    usethis::ui_info(c(
      "It is recommended to use parallel processing when trying to load multiple seasons.",
      "Please consider running {usethis::ui_code('future::plan(\"multisession\")')}!",
      "Will go on sequentially..."
    ))
  }

  p <- progressr::progressor(along = seasons)

  if (isFALSE(in_db)) {
    out <- furrr::future_map_dfr(seasons, single_season, p, ..., qs = qs)
  }

  if (isTRUE(in_db)) {
    purrr::walk(seasons, single_season, p, ..., qs = qs)
    out <- NULL
  }

  return(out)
}

single_season <- function(season, p, dbConnection = NULL, tablename = NULL, qs = FALSE) {
  if (isTRUE(qs)){
    .url <- glue::glue("https://github.com/guga31bb/nflfastR-data/blob/master/data/play_by_play_{season}.qs?raw=true")
    pbp <- qs_from_url(.url)
  }
  if (isFALSE(qs)) {
    .url <- glue::glue("https://github.com/guga31bb/nflfastR-data/blob/master/data/play_by_play_{season}.rds?raw=true")
    pbp <- readRDS(url(.url))
  }
  if (!is.null(dbConnection) && !is.null(tablename)) {
    DBI::dbWriteTable(dbConnection, tablename, pbp, append = TRUE)
    out <- NULL
  } else {
    out <- pbp
  }
  p(sprintf("season=%g", season))
  return(out)
}


# Load Next Gen Stats from Github -----------------------------------------

load_ngs <- function(seasons, type) {
  if (!type %in% c("passing", "rushing", "receiving")) usethis::ui_stop('Please pass valid type ("passing", "rushing" or "receiving")!')

  most_recent <- dplyr::if_else(
    lubridate::month(lubridate::today("America/New_York")) >= 9,
    lubridate::year(lubridate::today("America/New_York")),
    lubridate::year(lubridate::today("America/New_York")) - 1
  )

  if (!all(seasons %in% 2016:most_recent)) {
    usethis::ui_stop("Please pass valid seasons between 2016 and {most_recent}")
  }

  if (length(seasons) > 1 && inherits(future::plan(), "sequential")) {
    usethis::ui_info(c(
      "It is recommended to use parallel processing when trying to load multiple seasons.",
      "Please consider running {usethis::ui_code('future::plan(\"multisession\")')}!",
      "Will go on sequentially..."
    ))  }

  p <- progressr::progressor(along = seasons)

  out <- furrr::future_map_dfr(seasons, single_season_ngs, type, p)

  return(out)
}

single_season_ngs <- function(season, type, p, qs = FALSE) {

  if (isTRUE(qs)){
    .url <- glue::glue("https://github.com/mrcaseb/nfl-data/blob/master/data/ngs/ngs_{season}_{type}.qs?raw=true")
    ret <- qs_from_url(.url)
  }
  if (isFALSE(qs)) {
    .url <- glue::glue("https://github.com/mrcaseb/nfl-data/blob/master/data/ngs/ngs_{season}_{type}.rds?raw=true")
    ret <- readRDS(url(.url))
  }

  p(sprintf("season=%g", season))
  return(ret)
}

qs_from_url <- function(url) qs::qdeserialize(curl::curl_fetch_memory(url)$content)

read_raw_rds <- function(raw) {
  con <- gzcon(rawConnection(raw))
  ret <- readRDS(con)
  close(con)
  return(ret)
}

# helper to make sure the output of the
# schedule scraper is not named 'invalid' if the source file not yet exists
maybe_valid <- function(id) {
  all(
    length(id) == 1,
    is.character(id),
    substr(id, 1, 4) %in% 1999:format(Sys.Date(), "%Y"),
    as.integer(substr(id, 6, 7)) %in% seq_len(22),
    str_extract_all(id, "(?<=_)[:upper:]{2,3}")[[1]] %in% nflfastR::teams_colors_logos$team_abbr
  )
}

is_installed <- function(pkg) requireNamespace(pkg, quietly = TRUE)

load_lees_games <- function() readRDS(url("https://github.com/leesharpe/nfldata/blob/master/data/games.rds?raw=true"))

load_raw_game <- function(game_id, qs = FALSE){

  if (isTRUE(qs) && !is_installed("qs")) {
    usethis::ui_stop("Package {usethis::ui_value('qs')} required for argument {usethis::ui_value('qs = TRUE')}. Please install it.")
  }

  season <- substr(game_id, 1, 4)
  path <- "https://raw.githubusercontent.com/guga31bb/nflfastR-raw/master/raw"

  if(isFALSE(qs)) fetched <- curl::curl_fetch_memory(glue::glue("{path}/{season}/{game_id}.rds"))

  if(isTRUE(qs)) fetched <- curl::curl_fetch_memory(glue::glue("{path}/{season}/{game_id}.qs"))

  if (fetched$status_code == 404 & maybe_valid(game_id)) {
    usethis::ui_stop("The requested GameID {game_id} is not loaded yet, please try again later!")
  } else if (fetched$status_code == 500) {
    usethis::ui_stop("The data hosting servers are down, please try again later!")
  } else if (fetched$status_code == 404) {
    usethis::ui_stop("The requested GameID {game_id} is invalid!")
  }

  if(isFALSE(qs)) raw_data <- read_raw_rds(fetched$content)

  if(isTRUE(qs)) raw_data <- qs::qdeserialize(fetched$content)

  return(raw_data)

}
