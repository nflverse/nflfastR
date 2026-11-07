# sample games we'll use to check with
game_ids <- c("2025_01_KC_LAC", "2019_01_GB_CHI")

test_dir <- getwd()

pbp_cache <- tempfile("pbp_cache", fileext = ".rds")

# Builds pbp of the two sample games, from the local raw files (cached for the
# test session) or, with `dir = NULL`, from downloaded raw files
load_test_pbp <- function(dir = test_dir) {
  if (!is.null(dir) && file.exists(pbp_cache)) {
    return(readRDS(pbp_cache))
  }
  g <- readRDS(file.path(test_dir, "games.rds"))
  pbp_data <- build_nflfastR_pbp(game_ids, dir = dir, games = g) |>
    suppressMessages()
  if (!is.null(dir)) {
    saveRDS(pbp_data, pbp_cache)
  }
  pbp_data
}

# Expected output of load_test_pbp(). Regenerate after an intentional output
# change with
# saveRDS(strip_nflverse_attributes(load_test_pbp()), file.path(test_dir, "expected_pbp.rds"), compress = "xz")
load_expected_pbp <- function() {
  readRDS(file.path(test_dir, "expected_pbp.rds"))
}

load_expectation <- function(
  type = c("sc", "sc_weekly", "ep", "wp"),
  dir = test_dir
) {
  type <- match.arg(type)
  file_name <- switch(
    type,
    "sc" = "expected_sc.rds",
    "sc_weekly" = "expected_sc_weekly.rds",
    "ep" = "expected_ep.rds",
    "wp" = "expected_wp.rds",
  )
  strip_nflverse_attributes(readRDS(file.path(dir, file_name))) |>
    # we gotta round floating point numbers because of different model output
    # across platforms
    round_double_to_digits()
}

# strip nflverse attributes for tests because timestamp and version cause failures
# .internal.selfref is a data.table attribute that is not necessary in this case
strip_nflverse_attributes <- function(df) {
  input_attrs <- names(attributes(df))
  input_remove <- input_attrs[grepl(
    "nflverse|.internal.selfref|nflfastR",
    input_attrs
  )]
  attributes(df)[input_remove] <- NULL
  df
}

round_double_to_digits <- function(df, digits = 3) {
  dplyr::mutate(
    df,
    dplyr::across(
      .cols = relevant_variables(),
      .fns = function(vec) {
        formatC(vec, digits = digits, format = "fg") |>
          as.numeric() |>
          suppressWarnings()
      }
    )
  )
}

relevant_variables <- function() {
  c(
    dplyr::any_of(c(
      "no_score_prob",
      "opp_fg_prob",
      "opp_safety_prob",
      "opp_td_prob",
      "fg_prob",
      "safety_prob",
      "td_prob",
      "ep",
      "cp",
      "cpoe",
      "pass_oe",
      "xpass"
    )),
    dplyr::ends_with("epa"),
    dplyr::ends_with("wp"),
    dplyr::ends_with("wp_post"),
    dplyr::ends_with("wpa"),
    dplyr::starts_with("xyac")
  )
}

# Compare pbp against expected pbp and report differences per column.
# Plays are matched by game_id and play_id. Every value is compared on its own:
# non-doubles exactly, doubles with a relative tolerance and model output with
# a looser relative tolerance, because model output differs slightly across
# platforms. The model tolerance of 1e-3 matches the 3 significant digits the
# old tests rounded to (6 and 4 digits failed on some OSes). `abs_tolerance`
# only matters for values close to zero, where relative noise is meaningless.
# Outside CRAN, all value differences are written to `<diff_name>_diff.csv`,
# see write_pbp_diff().
expect_pbp_unchanged <- function(pbp, exp, ..., diff_name = "pbp") {
  res <- pbp_differences(pbp, exp, ...)
  msg <- res$messages
  path <- write_pbp_diff(res$diffs, diff_name)
  if (!is.null(path)) {
    msg <- c(msg, paste("All value differences written to", path))
  }

  testthat::expect(
    length(msg) == 0,
    paste(
      c("pbp differs from expected output:", paste("-", msg)),
      collapse = "\n"
    )
  )
  invisible(pbp)
}

# The comparison of expect_pbp_unchanged(), also used by the benchmark in
# data-raw/parser_performance/compare_benchmark.R. Returns a list with a
# message for every difference and a data frame per changed column with the
# value differences.
pbp_differences <- function(
  pbp,
  exp,
  tolerance = 1e-6,
  model_tolerance = 1e-3,
  abs_tolerance = 1e-6,
  n_examples = 3
) {
  act <- tibble::as_tibble(strip_nflverse_attributes(pbp))
  exp <- tibble::as_tibble(strip_nflverse_attributes(exp))
  model_cols <- regression_model_columns(exp)
  msg <- character()

  # columns
  missing_cols <- setdiff(names(exp), names(act))
  new_cols <- setdiff(names(act), names(exp))
  if (length(missing_cols)) {
    msg <- c(msg, paste("Missing columns:", toString(missing_cols)))
  }
  if (length(new_cols)) {
    msg <- c(msg, paste("New columns:", toString(new_cols)))
  }
  common_cols <- intersect(names(exp), names(act))
  if (!identical(common_cols, intersect(names(act), names(exp)))) {
    msg <- c(msg, "Column order changed")
  }

  # plays
  key <- function(df) paste(df$game_id, df$play_id)
  k_act <- key(act)
  k_exp <- key(exp)
  if (anyDuplicated(k_act)) {
    msg <- c(
      msg,
      paste("Duplicated plays:", toString(head(k_act[duplicated(k_act)], 10)))
    )
  }
  missing_plays <- setdiff(k_exp, k_act)
  new_plays <- setdiff(k_act, k_exp)
  if (length(missing_plays)) {
    msg <- c(
      msg,
      sprintf(
        "%d missing plays, e.g. %s",
        length(missing_plays),
        toString(head(missing_plays, n_examples))
      )
    )
  }
  if (length(new_plays)) {
    msg <- c(
      msg,
      sprintf(
        "%d new plays, e.g. %s",
        length(new_plays),
        toString(head(new_plays, n_examples))
      )
    )
  }
  common <- intersect(k_exp, k_act)

  # values of common plays and columns
  a <- act[match(common, k_act), ]
  e <- exp[match(common, k_exp), ]
  diffs <- list()
  for (col in common_cols) {
    x <- a[[col]]
    y <- e[[col]]
    if (!identical(class(x), class(y))) {
      msg <- c(
        msg,
        sprintf(
          "%s: type changed from %s to %s",
          col,
          toString(class(y)),
          toString(class(x))
        )
      )
      next
    }
    tol <- if (col %in% model_cols) model_tolerance else tolerance
    diff <- values_differ(x, y, tol, abs_tolerance)
    if (!any(diff)) {
      next
    }
    i <- which(diff)
    diffs[[col]] <- data.frame(
      column = col,
      game_id = a$game_id[i],
      play_id = a$play_id[i],
      expected = as.character(y[i]),
      actual = as.character(x[i])
    )
    ex <- head(i, n_examples)
    msg <- c(
      msg,
      sprintf(
        "%s: %d values changed in %d games, e.g.\n    %s",
        col,
        length(i),
        length(unique(a$game_id[i])),
        paste(
          sprintf(
            "%s (expected %s, got %s)",
            common[ex],
            format(y[ex]),
            format(x[ex])
          ),
          collapse = "\n    "
        )
      )
    )
  }

  list(messages = msg, diffs = diffs)
}

regression_model_columns <- function(pbp) {
  c(
    names(dplyr::select(pbp, relevant_variables())),
    intersect(c("extra_point_prob", "two_point_conversion_prob"), names(pbp))
  )
}

values_differ <- function(x, y, tolerance, abs_tolerance) {
  both <- !is.na(x) & !is.na(y)
  diff <- xor(is.na(x), is.na(y))
  if (is.double(x)) {
    diff[both] <- abs(x[both] - y[both]) >
      tolerance * abs(y[both]) + abs_tolerance
  } else {
    diff[both] <- x[both] != y[both]
  }
  diff
}

# Writes the value differences to the nflfastR user cache, where CI picks
# them up to upload them as artifacts, and returns the path. A comparison
# without differences removes an old file. Does nothing on CRAN, where nobody
# could read the file.
write_pbp_diff <- function(diffs, name) {
  if (is_on_cran()) {
    return(NULL)
  }
  dir <- tools::R_user_dir("nflfastR", which = "cache")
  path <- file.path(dir, paste0(name, "_diff.csv"))
  unlink(path)
  if (length(diffs) == 0) {
    return(NULL)
  }
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(do.call(rbind, diffs), path, row.names = FALSE)
  path
}

# Same logic as the internal testthat:::on_cran()
is_on_cran <- function() {
  env <- Sys.getenv("NOT_CRAN")
  if (identical(env, "")) {
    !interactive()
  } else {
    !isTRUE(as.logical(env))
  }
}
