# sample games we'll use to check with
game_ids <- c("1999_01_MIN_ATL", "2019_01_GB_CHI")

test_dir <- getwd()

pbp_cache <- tempfile("pbp_cache", fileext = ".rds")

load_test_pbp <- function(pbp = pbp_cache, dir = test_dir){
  if (file.exists(pbp) && !is.null(dir)){
    if(interactive()) cli::cli_alert_info("Will return pbp from cache")
    return(readRDS(pbp))
  }

  g <- readRDS(file.path(test_dir, paste0("games.rds")))

  # model output differs across machines so we round to 4 significant digits
  # to prevent failing tests
  pbp_data <- build_nflfastR_pbp(game_ids, dir = dir, games = g)
  if(!is.null(dir)) saveRDS(pbp_data, pbp)
  pbp_data
}

save_test_object <- function(object){
  obj_name <- deparse(substitute(object))
  tmp_file <- tempfile(obj_name, fileext = ".csv")
  modify_digits <- dplyr::mutate_if(object, is.numeric, signif, digits = 3)
  data.table::fwrite(modify_digits, tmp_file, na = "NA")
  invisible(tmp_file)
}

load_expectation <- function(type = c("pbp", "sc", "sc_weekly", "ep", "wp"),
                             dir = test_dir){
  type <- match.arg(type)
  file_name <- switch (
    type,
    "pbp" = "expected_pbp.rds",
    "sc" = "expected_sc.rds",
    "sc_weekly" = "expected_sc_weekly.rds",
    "ep" = "expected_ep.rds",
    "wp" = "expected_wp.rds",
  )
  strip_nflverse_attributes(readRDS(file.path(dir, file_name))) %>%
    # we gotta round floating point numbers because of different model output
    # across platforms
    round_double_to_digits()
}

# strip nflverse attributes for tests because timestamp and version cause failures
# .internal.selfref is a data.table attribute that is not necessary in this case
strip_nflverse_attributes <- function(df){
  input_attrs <- names(attributes(df))
  input_remove <- input_attrs[grepl("nflverse|.internal.selfref|nflfastR", input_attrs)]
  attributes(df)[input_remove] <- NULL
  df
}

round_double_to_digits <- function(df, digits = 3){
  dplyr::mutate(df, dplyr::across(
    .cols = relevant_variables(),
    .fns = function(vec){
      formatC(vec, digits = digits, format = "fg") %>%
        as.numeric() %>%
        suppressWarnings()
    }
  ))
}

relevant_variables <- function(){
  c(
    tidyselect::any_of(c(
      "no_score_prob", "opp_fg_prob", "opp_safety_prob", "opp_td_prob", "fg_prob",
      "safety_prob", "td_prob", "ep", "cp", "cpoe", "pass_oe", "xpass"
    )),
    tidyselect::ends_with("epa"),
    tidyselect::ends_with("wp"),
    tidyselect::ends_with("wp_post"),
    tidyselect::ends_with("wpa"),
    tidyselect::starts_with("xyac")
  )
}
