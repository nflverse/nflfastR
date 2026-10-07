# This function applies manual data patches to pbp data during the nflfastR
# parsing process. It runs before any columns are added or modified.
# The manual patch file is saved as JSON in "inst/" which makes it available
# on every local machine

# at the time of writing, the manual patch file has the following structure
# when read into memory (note the list class of "value" which allows us to
# store "value"s of different classes):

# game_id         | play_id |     column |              value
# character       | integer |  character |               list
# -----------------------------------------------------------
# 2020_01_LV_CAR  |     894 |    quarter |                  2 <- integer
# 2020_01_LV_CAR  |     911 |    quarter |                  2
# 2020_01_LV_CAR  |     935 |    quarter |                  2
# 2020_01_LV_CAR  |     978 |    quarter |                  2
# 2020_01_LV_CAR  |     999 |    quarter |                  2
# 2018_08_IND_OAK |     874 |    quarter |                  2
# 2016_15_MIA_NYJ |     901 |    quarter |                  1
# 2016_05_TEN_MIA |    2919 |    quarter |                  4
# 2016_05_TEN_MIA |    2943 |    quarter |                  4
# 2015_01_IND_BUF |    1185 |    quarter |                  2
# 2015_01_IND_BUF |    1207 |    quarter |                  2
# 2014_07_ATL_BAL |   99999 | start_time | 10/19/14, 13:02:00 <- character
# 2014_04_CAR_BAL |   99999 | start_time |  9/28/14, 13:02:00
# 2014_01_CIN_BAL |   99999 | start_time |   9/7/14, 13:02:00
# 2009_15_CHI_BAL |   99999 | start_time | 12/20/09, 16:15:00
# 2009_14_DET_BAL |   99999 | start_time | 12/13/09, 13:02:00
# 2009_12_PIT_BAL |   99999 | start_time | 11/29/09, 20:30:00
# 2009_08_DEN_BAL |   99999 | start_time |  11/1/09, 13:02:00
# 2009_05_CIN_BAL |   99999 | start_time | 10/11/09, 13:02:00
# 2009_03_CLE_BAL |   99999 | start_time |  9/27/09, 13:02:00
# 2009_01_KC_BAL  |   99999 | start_time |  9/13/09, 13:02:00

# The function calls dplyr::rows_update and updates by game_id and play_id, unless
# the play_id is 99999. In the case of 99999, dplyr::rows_update updates by game_id only.
# since we can't update all columns in one step - this would introduce unintended -
# NA values, we have to loop over the values of the column variable.

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
        .data$column == "qtr" ~ "quarter",
        .data$column == "yrdln" ~ "yardline",
        .data$column == "ydstogo" ~ "yards_to_go",
        .data$column == "side_of_field" ~ "yardline_side",
        .data$column == "desc" ~ "play_description",
        TRUE ~ .data$column
      )
    )

  for (colname in unique(patch_data$column)) {
    # PART 1: apply play level patches
    new <- patch_data |>
      dplyr::filter(.data$column == colname, .data$play_id != 99999L)

    # nrow can be 0 if a specific colname values has 99999 play_ids only
    if (nrow(new) > 0L) {
      new <- new |>
        dplyr::mutate(
          # values are store in a list to preserve classes
          value = unlist(.data$value, use.names = FALSE)
        ) |>
        # the value column gets renamed to colname so rows_update knows
        # which column to update
        dplyr::rename_with(.fn = ~colname, .cols = "value") |>
        dplyr::select(-"column")

      pbp <- dplyr::rows_update(
        pbp,
        new,
        by = c("game_id", "play_id"),
        unmatched = "ignore"
      )
    }

    # PART 2: apply game level patches
    new <- patch_data |>
      dplyr::filter(.data$column == colname, .data$play_id == 99999L)

    # nrow can be 0 if a specific colname values has non 99999 play_ids only
    if (nrow(new) > 0L) {
      new <- new |>
        dplyr::mutate(
          # values are store in a list to preserve classes
          value = unlist(.data$value, use.names = FALSE)
        ) |>
        # the value column gets renamed to colname so rows_update knows
        # which column to update
        dplyr::rename_with(.fn = ~colname, .cols = "value") |>
        dplyr::select(-"column", -"play_id")

      pbp <- dplyr::rows_update(
        pbp,
        new,
        by = "game_id",
        unmatched = "ignore"
      )
    }
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
    dplyr::arrange(dplyr::desc(.data$game_id), .data$play_id, .data$column)

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
