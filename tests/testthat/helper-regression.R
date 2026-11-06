# Regression test data
#
# The raw files, schedule and expected output of the regression games live in
# a release of the nflfastR GitHub repo (tag in regression_data.json), not in
# git. regression_data.json lists the asset names and their md5 checksums.
# Asset names contain a checksum, so updating the data never changes the
# files an older commit points to.
#
# Create or update the data with
# data-raw/test_game_selection/update_regression_test.R

# Download (or reuse cached) regression data and return a list with paths:
# raw_dir, games, expected. Returns NULL if a download fails.
# Files are cached in the directory given by the env var
# NFLFASTR_REGRESSION_CACHE, or in a temporary directory of the R session.
regression_data <- function(
  manifest = "regression_data.json",
  cache_dir = Sys.getenv(
    "NFLFASTR_REGRESSION_CACHE",
    file.path(tempdir(), "nflfastR_regression")
  )
) {
  m <- jsonlite::read_json(manifest)
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  md5 <- function(path) unname(tools::md5sum(path))
  paths <- list()
  for (kind in names(m$files)) {
    f <- m$files[[kind]]
    dest <- file.path(cache_dir, f$name)
    if (!file.exists(dest) || md5(dest) != f$md5) {
      url <- sprintf(
        "https://github.com/%s/releases/download/%s/%s",
        m$repo, m$tag, f$name
      )
      ok <- tryCatch(
        {
          curl::curl_download(url, dest, quiet = TRUE)
          TRUE
        },
        error = function(e) FALSE
      )
      if (!ok) {
        unlink(dest)
        return(NULL)
      }
      if (md5(dest) != f$md5) {
        unlink(dest)
        stop("Checksum of downloaded ", f$name, " doesn't match regression_data.json")
      }
    }
    paths[[kind]] <- dest
  }

  raw_dir <- file.path(cache_dir, sub("\\.tar$", "", m$files$raw$name))
  if (!dir.exists(raw_dir)) {
    utils::untar(paths$raw, exdir = raw_dir)
  }
  list(raw_dir = raw_dir, games = paths$games, expected = paths$expected)
}
