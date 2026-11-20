# Patches like pbp_patches() returns them. Each argument is one record of the
# patch file, with the fields separated by "; ".
local_pbp_patches <- function(..., env = parent.frame()) {
  records <- gsub("; ", "\n", c(...), fixed = TRUE)
  file <- withr::local_tempfile(
    lines = paste(records, collapse = "\n\n"),
    .local_envir = env
  )
  patches <- read_pbp_patches(file)
  local_mocked_bindings(
    pbp_patches = \() split(patches, by = "game_id", keep.by = FALSE),
    .env = env
  )
}
