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
      left = crayon::bold(x),
      right = paste0("nflfastR version ", utils::packageVersion("nflfastR")),
      width = getOption("width")
    )
  )
}

rule_footer <- function(x) {
  rlang::inform(
    cli::rule(
      left = crayon::bold(x),
      width = getOption("width")
    )
  )
}

get_seasons <- function(seasons) {

  most_recent <- dplyr::if_else(
    lubridate::month(lubridate::today("America/New_York")) >= 9,
    lubridate::year(lubridate::today("America/New_York")) ,
    lubridate::year(lubridate::today("America/New_York")) - 1
  )

  if (!all(seasons %in% 1999:most_recent)) {
    usethis::ui_stop("Please pass valid seasons between 1999 and {most_recent}")
  }

  season_count <- length(seasons)

  if (season_count >= 10 & !requireNamespace("furrr", quietly = TRUE)) {
    pp <- FALSE
    usethis::ui_info("It is recommended to use parallel processing when trying to load {season_count} seasons but the package {usethis::ui_value('furrr')} is not installed.\nPlease consider installing it with {usethis::ui_code('install.packages(\"furrr\")')}. Will go on sequentially...")
  } else if (season_count >= 10 & requireNamespace("furrr", quietly = TRUE)) {
    pp <- TRUE
  } else {
    pp <- FALSE
  }

  progressr::with_progress({
    p <- progressr::progressor(along = seasons)

    if (pp == TRUE) {
      future::plan("multiprocess")
      out <- furrr::future_map_dfr(seasons, single_season, p)
    } else {
      out <- purrr::map_dfr(seasons, single_season, p)
    }

  })

  return(out)
}

single_season <- function(season, p) {
  ret <- readRDS(url(
    glue::glue("https://github.com/guga31bb/nflfastR-data/blob/master/data/play_by_play_{season}.rds?raw=true")
  ))
  p(sprintf("season=%g", season))
  return(ret)
}
