# Applies every patch of inst/pbp_patches.dcf to the raw data and fails if a
# patch doesn't fit anymore (e.g. because the NFL fixed the raw data or a
# play ID changed). Run it after adding patches and after the raw data was
# updated:
#
#   Rscript data-raw/pbp_patches/check_patches.R
#   Rscript data-raw/pbp_patches/check_patches.R --prune
#
# With --prune, the patches that are obsolete because the raw data was fixed
# are removed from the file:
#
# - set patches where the raw data has the value `to` now,
# - drop_stat patches where the play still has stats, but not this stat ID.
#
# Patches that don't fit for other reasons (a value other than `from` or `to`,
# a missing play) are only listed, because the fix may still be needed with
# other play IDs or values.
#
# Uses the raw data of option nflfastR.raw_directory if it is set, otherwise
# downloads it.
pkgload::load_all(quiet = TRUE)

file <- "inst/pbp_patches.dcf"
prune <- "--prune" %chin% commandArgs(trailingOnly = TRUE)

cli::cli_inform("Checking the patches of {length(pbp_patches())} game{?s}")
not_applied <- check_pbp_patches()

if (nrow(not_applied) == 0) {
  cli::cli_alert_success("All patches applied")
  quit(save = "no")
}

obsolete <- not_applied[obsolete == TRUE]
review <- not_applied[obsolete == FALSE]

if (prune && nrow(obsolete) > 0) {
  # The records of the file as text, in the order of read_pbp_patches(), so
  # the records we keep stay as they are
  lines <- readLines(file)
  record <- cumsum(lines == "" & c(TRUE, utils::head(lines, -1) != ""))
  records <- split(lines[lines != ""], record[lines != ""])
  patches <- read_pbp_patches(file)
  stopifnot(length(records) == nrow(patches))
  key <- c("game_id", "play_id", "action", "stat_id", "column")
  drop <- patches[obsolete, on = key, which = TRUE]
  writeLines(
    paste(
      vapply(records[-drop], paste, "", collapse = "\n"),
      collapse = "\n\n"
    ),
    file
  )
  cli::cli_alert_success(
    "Removed {length(drop)} obsolete patch{?es} from {.path {file}}"
  )
  obsolete <- obsolete[0]
}

if (nrow(obsolete) > 0) {
  cli::cli_alert_info(
    "Obsolete because the raw data was fixed (remove them with {.arg --prune}):"
  )
  cli::cli_bullets(stats::setNames(obsolete$message, rep("*", nrow(obsolete))))
}
if (nrow(review) > 0) {
  cli::cli_abort(c(
    "Patches that don't fit the raw data:",
    stats::setNames(review$message, rep("x", nrow(review)))
  ))
}
if (nrow(obsolete) > 0) {
  quit(save = "no", status = 1)
}
