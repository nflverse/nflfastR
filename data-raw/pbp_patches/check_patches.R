# Applies every patch of inst/pbp_patches.dcf to the raw data and fails if a
# patch doesn't fit anymore (e.g. because the NFL fixed the raw data or a
# play ID changed). Run it after adding patches:
#
#   Rscript data-raw/pbp_patches/check_patches.R
#
# Uses the raw data of option nflfastR.raw_directory if it is set, otherwise
# downloads it.
pkgload::load_all(quiet = TRUE)

ids <- names(pbp_patches())
cli::cli_inform("Checking the patches of {length(ids)} game{?s}")
not_applied <- character()
for (id in ids) {
  withCallingHandlers(
    get_pbp_nfl(id),
    nflfastR_patch_not_applied = function(w) {
      not_applied <<- c(not_applied, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
}
if (length(not_applied) > 0) {
  cli::cli_abort(c("Patches that weren't applied:", not_applied))
}
cli::cli_alert_success("All patches applied")
