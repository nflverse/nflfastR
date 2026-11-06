# Regression test data
#
# The raw files, schedule and expected output of the regression games live in
# a release of the nflfastR GitHub repo, not in git. The manifest
# regression_data.dcf names the release tag, the asset names and their md5
# checksums. Asset names contain a checksum, so updating the data never
# changes the files an older commit points to.
#
# Create or update the data with
# data-raw/test_game_selection/update_regression_test.R

regression_kinds <- c("raw", "games", "expected")

# The manifest is a DCF file (see ?read.dcf) with the fields repo, tag and,
# for each kind, the asset name and md5 checksum (e.g. raw and raw_md5).
read_regression_manifest <- function(path) {
  as.list(read.dcf(path)[1, ])
}

write_regression_manifest <- function(manifest, path) {
  fields <- c(
    "repo",
    "tag",
    as.vector(rbind(regression_kinds, paste0(regression_kinds, "_md5")))
  )
  write.dcf(as.data.frame(manifest[fields]), path)
}

# Download (or reuse cached) regression data and return a list with paths:
# raw_dir, games, expected. Returns NULL if a download fails.
regression_data <- function(
  manifest = "regression_data.dcf",
  cache_dir = tools::R_user_dir("nflfastR", which = "cache")
) {
  m <- read_regression_manifest(manifest)
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  md5 <- function(path) unname(tools::md5sum(path))
  paths <- list()
  for (kind in regression_kinds) {
    name <- m[[kind]]
    expected_md5 <- m[[paste0(kind, "_md5")]]
    dest <- file.path(cache_dir, name)
    if (!file.exists(dest) || md5(dest) != expected_md5) {
      url <- sprintf(
        "https://github.com/%s/releases/download/%s/%s",
        m$repo,
        m$tag,
        name
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
      if (md5(dest) != expected_md5) {
        unlink(dest)
        stop("Checksum of downloaded ", name, " doesn't match the manifest")
      }
    }
    paths[[kind]] <- dest
  }

  raw_dir <- file.path(cache_dir, sub("\\.tar$", "", m$raw))
  if (!dir.exists(raw_dir)) {
    utils::untar(paths$raw, exdir = raw_dir)
  }
  list(raw_dir = raw_dir, games = paths$games, expected = paths$expected)
}
