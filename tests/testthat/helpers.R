# sample games we'll use to check with
game_ids <- c("2025_01_KC_LAC", "2019_01_GB_CHI")

test_dir <- getwd()

pbp_cache <- tempfile("pbp_cache", fileext = ".rds")

load_test_pbp <- function(pbp = pbp_cache, dir = test_dir) {
  if (file.exists(pbp) && !is.null(dir)) {
    if (interactive()) {
      cli::cli_alert_info("Will return pbp from cache")
    }
    return(readRDS(pbp))
  }

  g <- readRDS(file.path(test_dir, paste0("games.rds")))

  # model output differs across machines so we round to 4 significant digits
  # to prevent failing tests
  pbp_data <- build_nflfastR_pbp(game_ids, dir = dir, games = g)
  if (!is.null(dir)) {
    saveRDS(pbp_data, pbp)
  }
  pbp_data
}

save_test_object <- function(object) {
  obj_name <- deparse(substitute(object))
  tmp_file <- tempfile(obj_name, fileext = ".csv")
  modify_digits <- dplyr::mutate_if(object, is.numeric, signif, digits = 3)
  data.table::fwrite(modify_digits, tmp_file, na = "NA")
  invisible(tmp_file)
}

load_expectation <- function(
  type = c("pbp", "sc", "sc_weekly", "ep", "wp"),
  dir = test_dir
) {
  type <- match.arg(type)
  file_name <- switch(
    type,
    "pbp" = "expected_pbp.rds",
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
# Plays are matched by game_id and play_id. Doubles are compared with a small
# relative tolerance, model outputs (see relevant_variables()) after rounding,
# because model output differs slightly across platforms.
expect_pbp_unchanged <- function(pbp, exp, tolerance = 1e-6, n_examples = 3) {
  act <- strip_nflverse_attributes(pbp) |> round_double_to_digits()
  exp <- strip_nflverse_attributes(exp) |> round_double_to_digits()
  act <- tibble::as_tibble(act)
  exp <- tibble::as_tibble(exp)
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
  if (!identical(intersect(names(exp), names(act)), intersect(names(act), names(exp)))) {
    msg <- c(msg, "Column order changed")
  }

  # plays
  key <- function(df) paste(df$game_id, df$play_id)
  k_act <- key(act)
  k_exp <- key(exp)
  if (anyDuplicated(k_act)) {
    msg <- c(msg, paste("Duplicated plays:", toString(head(k_act[duplicated(k_act)], 10))))
  }
  missing_plays <- setdiff(k_exp, k_act)
  new_plays <- setdiff(k_act, k_exp)
  if (length(missing_plays)) {
    msg <- c(msg, sprintf("%d missing plays, e.g. %s", length(missing_plays),
                          toString(head(missing_plays, n_examples))))
  }
  if (length(new_plays)) {
    msg <- c(msg, sprintf("%d new plays, e.g. %s", length(new_plays),
                          toString(head(new_plays, n_examples))))
  }

  # values of common plays and columns
  common <- intersect(k_exp, k_act)
  a <- act[match(common, k_act), ]
  e <- exp[match(common, k_exp), ]
  for (col in intersect(names(exp), names(act))) {
    x <- a[[col]]
    y <- e[[col]]
    if (!identical(class(x), class(y))) {
      msg <- c(msg, sprintf("%s: type changed from %s to %s", col,
                            toString(class(y)), toString(class(x))))
      next
    }
    if (is.double(x)) {
      diff <- xor(is.na(x), is.na(y)) |
        (!is.na(x) & !is.na(y) & abs(x - y) > tolerance * pmax(1, abs(y)))
    } else {
      diff <- xor(is.na(x), is.na(y)) | (!is.na(x) & !is.na(y) & x != y)
    }
    diff[is.na(diff)] <- FALSE
    if (any(diff)) {
      i <- head(which(diff), n_examples)
      examples <- sprintf("%s (expected %s, got %s)", common[i],
                          format(y[i]), format(x[i]))
      msg <- c(msg, sprintf(
        "%s: %d values changed in %d games, e.g.\n    %s",
        col, sum(diff), length(unique(a$game_id[diff])),
        paste(examples, collapse = "\n    ")
      ))
    }
  }

  testthat::expect(
    length(msg) == 0,
    paste(c("pbp differs from expected output:", paste("-", msg)), collapse = "\n")
  )
  invisible(pbp)
}
