# Regression test data
#
# The test data of the regression tests lives in a release of the nflfastR
# GitHub repo, not in git. The manifest regression_data.dcf names the release
# tag, the asset names and their md5 checksums. Asset names contain a
# checksum, so updating the data never changes the files an older commit
# points to.
#
# Two sets of data share the manifest:
#   pbp    raw files, schedule and expected output of build_nflfastR_pbp()
#          (data-raw/test_game_selection/update_regression_test.R)
#   stats  inputs and expected output of calculate_stats()
#          (data-raw/calculate_stats_performance/update_stats_regression.R)

regression_kinds <- list(
  pbp = c("raw", "games", "expected"),
  stats = c("stats_inputs", "stats_expected")
)

# The manifest is a DCF file (see ?read.dcf) with the fields repo, tag and,
# for each kind, the asset name and md5 checksum (e.g. raw and raw_md5).
read_regression_manifest <- function(path) {
  as.list(read.dcf(path)[1, ])
}

# Writes the fields of all sets that are in `manifest`, so updating one set
# keeps the entries of the other
write_regression_manifest <- function(manifest, path) {
  kinds <- unlist(regression_kinds, use.names = FALSE)
  fields <- c("repo", "tag", as.vector(rbind(kinds, paste0(kinds, "_md5"))))
  fields <- fields[fields %in% names(manifest)]
  write.dcf(as.data.frame(manifest[fields]), path)
}

# Download (or reuse cached) regression data of one set and return a list
# with paths. For set "pbp": raw_dir, games, expected. For set "stats":
# stats_inputs, stats_expected. Returns NULL if a download fails.
regression_data <- function(
  manifest = "regression_data.dcf",
  cache_dir = tools::R_user_dir("nflfastR", which = "cache"),
  set = c("pbp", "stats")
) {
  set <- match.arg(set)
  m <- read_regression_manifest(manifest)
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  md5 <- function(path) unname(tools::md5sum(path))
  paths <- list()
  for (kind in regression_kinds[[set]]) {
    name <- m[[kind]]
    expected_md5 <- m[[paste0(kind, "_md5")]]
    if (is.null(name) || is.null(expected_md5)) {
      stop("The manifest has no entry for ", kind)
    }
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

  if (set == "stats") {
    return(paths)
  }
  raw_dir <- file.path(cache_dir, sub("\\.tar$", "", m$raw))
  if (!dir.exists(raw_dir)) {
    utils::untar(paths$raw, exdir = raw_dir)
  }
  list(raw_dir = raw_dir, games = paths$games, expected = paths$expected)
}

# calculate_stats() regression data -------------------------------------------

# Downloads (or reuses) the stats regression data, mocks the loaders of
# calculate_stats() with the pinned inputs for the calling test and returns a
# list with `inputs` (pbp, playstats, players) and `expected` (named list of
# outputs, see stats_regression_run()). Skips if the download fails.
local_stats_regression <- function(env = parent.frame()) {
  reg <- regression_data(
    testthat::test_path("regression_data.dcf"),
    set = "stats"
  )
  testthat::skip_if(is.null(reg), "download of regression test data failed")
  inputs <- readRDS(reg$stats_inputs)
  testthat::local_mocked_bindings(
    stats_load_from_url = function(urls) inputs$playstats,
    stats_load_players = function() inputs$players,
    stats_load_pbp = function(seasons) inputs$pbp,
    .env = env
  )
  list(inputs = inputs, expected = readRDS(reg$stats_expected))
}

# Runs calculate_stats() for the outputs named like the expected output:
# "<summary_level>/<stat_type>/<season_type>", with the prefix "pbp_null/"
# for calls without custom pbp
stats_regression_run <- function(names, pbp) {
  out <- lapply(names, function(name) {
    parts <- strsplit(name, "/", fixed = TRUE)[[1]]
    pbp_null <- parts[[1]] == "pbp_null"
    if (pbp_null) {
      parts <- parts[-1]
    }
    suppressMessages(calculate_stats(
      seasons = sort(unique(pbp$season)),
      summary_level = parts[[1]],
      stat_type = parts[[2]],
      season_type = parts[[3]],
      pbp = if (!pbp_null) pbp
    ))
  })
  names(out) <- names
  out
}
