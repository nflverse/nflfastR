# Replaces the patches of the package until the end of the calling test. Each
# argument is one record of the patch file, with the fields separated by "; ".
# Returns the path of the patch file.
local_pbp_patches <- function(..., env = parent.frame()) {
  records <- gsub("; ", "\n", c(...), fixed = TRUE)
  file <- withr::local_tempfile(
    lines = paste(records, collapse = "\n\n"),
    .local_envir = env
  )
  patches <- read_pbp_patches(file)
  old <- patch_env$override
  patch_env$override <- split(patches, by = "game_id", keep.by = FALSE)
  withr::defer(patch_env$override <- old, envir = env)
  invisible(file)
}
