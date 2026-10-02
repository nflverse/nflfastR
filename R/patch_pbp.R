patch_pbp <- function(pbp) {
  patch_data <- .patch_read_data()
  if (is.null(patch_data)) {
    cli_message(
      "can't apply manual pbp patches as the patch file misses locally",
      .cli_fct = cli::cli_alert_warning
    )
    return(pbp)
  }
  # these are all game_ids included in the manual patch file
  patch_ids <- unique(patch_data$game_id)
  # no need to apply patches when pbp doesn't contain games to patch
  if (!any(patch_ids %in% pbp$game_id)) {
    cli_message("no games to patch")
    return(pbp)
  }

  # at the stage of parsing pbp, data isn't necessarily named the way it is named
  # after parsing is completed. That's why we have to rename some columns.
  # Newer dplyr has replace_values() for this but we don't want to
  # force users into dplyr updates
  patch_data <- patch_data |>
    dplyr::mutate(
      column = dplyr::case_when(
        column == "qtr" ~ "quarter",
        TRUE ~ column
      )
    )

  for (colname in unique(patch_data$column)) {
    new <- patch_data |>
      dplyr::filter(column == colname) |>
      dplyr::mutate(
        value = unlist(value, use.names = FALSE)
      ) |>
      dplyr::rename_with(.fn = ~colname, .cols = "value") |>
      dplyr::select(-column)

    pbp <- dplyr::rows_update(
      pbp,
      new,
      by = c("game_id", "play_id"),
      unmatched = "ignore"
    )
  }
  cli_message(
    "applied manual pbp patches",
    .cli_fct = cli::cli_alert_success
  )
  pbp
}


.patch_read_data <- function() {
  local_file <- system.file("patch_pbp.json", package = "nflfastR")
  if (!file.exists(local_file)) {
    return(NULL)
  }
  jsonlite::read_json(local_file, simplifyVector = TRUE)
}

.patch_clean_data <- function() {
  patch_data <- .patch_read_data()
  cleaned <- patch_data |>
    dplyr::distinct() |>
    dplyr::arrange(dplyr::desc(game_id), play_id, column)

  if (!identical(patch_data, cleaned) && interactive()) {
    update <- utils::menu(
      title = "It is possible to clean up (sort and/or remove duplicates) the manual patch json file.\nDo you wish to overwrite the file?",
      choices = c("Yes", "No")
    ) ==
      1
    if (isTRUE(update)) {
      # NOTE the pretty arg for better readability
      jsonlite::write_json(
        cleaned,
        "inst/patch_pbp.json",
        pretty = TRUE
      )
    }
  } else if (!identical(patch_data, cleaned)) {
    cli::cli_alert_warning(
      "Detected entries that should be cleaned.\\
      Please run {.fun .patch_clean_data} interactively."
    )
  }
  invisible(cleaned)
}
