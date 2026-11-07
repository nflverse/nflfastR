# Create or update the data of the regression test
# ("build_nflfastR_pbp output is unchanged for the regression games" in
# tests/testthat/test-build_nflfastR_pbp.R).
#
# The data lives in a release of the nflfastR GitHub repo. This script builds
# new files, uploads them as release assets and updates
# tests/testthat/regression_data.dcf, which is the only file to commit.
# Asset names contain an md5 checksum, so existing assets are never replaced
# and older commits keep working.
#
# Usage (from the package root, needs the gh CLI with write access):
#
#   Rscript data-raw/test_game_selection/update_regression_test.R
#     Rebuild the expected output with the current package code, using the
#     raw files and schedule of the current release data. Run this after an
#     intentional change of the pbp output and check the test output first.
#
#   Rscript data-raw/test_game_selection/update_regression_test.R --new-games <raw_dir>
#     Use the games in data-raw/test_game_selection/selected_games.csv with
#     raw files from <raw_dir>/<season>/<game_id>.rds and rebuild everything.
#
#   Add --dry-run to build the files without uploading or touching the
#   manifest.

args <- commandArgs(trailingOnly = TRUE)
dry_run <- "--dry-run" %in% args
new_games <- "--new-games" %in% args
manifest_path <- "tests/testthat/regression_data.dcf"
repo <- "nflverse/nflfastR"
tag <- "regression_tests"

source("tests/testthat/helper-regression.R")
md5 <- function(path) unname(tools::md5sum(path))

staging <- file.path(tempdir(), "regression_staging")
unlink(staging, recursive = TRUE)
dir.create(staging)

old <- if (file.exists(manifest_path)) read_regression_manifest(manifest_path)
files <- list()

if (new_games) {
  raw_src <- args[[which(args == "--new-games") + 1]]
  game_ids <- read.csv(
    "data-raw/test_game_selection/selected_games.csv"
  )$game_id

  raw_dir <- file.path(staging, "raw")
  for (id in game_ids) {
    season <- substr(id, 1, 4)
    dir.create(
      file.path(raw_dir, season),
      recursive = TRUE,
      showWarnings = FALSE
    )
    ok <- file.copy(
      file.path(raw_src, season, paste0(id, ".rds")),
      file.path(raw_dir, season, paste0(id, ".rds"))
    )
    if (!ok) stop("Could not copy raw file of ", id)
  }
  raw_tar <- file.path(staging, "raw.tar")
  # raw files are already compressed. R's own tar writes no extended file
  # attributes (macOS tar adds them and GNU tar warns about them on Linux)
  withr::with_dir(
    raw_dir,
    utils::tar(raw_tar, files = ".", compression = "none", tar = "internal")
  )
  files$raw <- raw_tar

  games <- nflreadr::load_schedules(2001:2030)
  games <- games[games$game_id %in% game_ids, ]
  stopifnot(nrow(games) == length(game_ids))
  games_file <- file.path(staging, "games.rds")
  saveRDS(games, games_file, compress = "xz")
  files$games <- games_file
} else {
  if (is.null(old)) {
    stop("No manifest yet, run with --new-games <raw_dir> first")
  }
  reg <- regression_data(manifest_path)
  if (is.null(reg)) {
    stop("Download of current regression data failed")
  }
  raw_dir <- reg$raw_dir
  games_file <- reg$games
  game_ids <- unique(readRDS(reg$expected)$game_id)
}

pkgload::load_all(".", quiet = TRUE)
future::plan("multisession", workers = max(1, parallel::detectCores() - 1))
pbp <- build_nflfastR_pbp(
  game_ids,
  dir = raw_dir,
  games = readRDS(games_file),
  rules = FALSE
)
future::plan("sequential")

# drop attributes that change with every build (timestamp, version)
attrs <- names(attributes(pbp))
attributes(pbp)[attrs[grepl(
  "nflverse|nflfastR|.internal.selfref",
  attrs
)]] <- NULL
pbp <- tibble::as_tibble(pbp)
expected_file <- file.path(staging, "expected.rds")
saveRDS(pbp, expected_file, compress = "xz")
files$expected <- expected_file
message(
  "Built expected output: ",
  nrow(pbp),
  " plays of ",
  length(game_ids),
  " games"
)

# name new assets by checksum, keep unchanged entries of the old manifest
manifest <- c(
  list(repo = repo, tag = tag),
  old[setdiff(names(old), c("repo", "tag"))]
)
ext <- c(raw = "tar", games = "rds", expected = "rds")
to_upload <- character()
for (kind in names(files)) {
  hash <- md5(files[[kind]])
  name <- sprintf("regression_%s_%s.%s", kind, substr(hash, 1, 10), ext[[kind]])
  renamed <- file.path(staging, name)
  file.copy(files[[kind]], renamed, overwrite = TRUE)
  manifest[[kind]] <- name
  manifest[[paste0(kind, "_md5")]] <- hash
  if (!identical(old[[paste0(kind, "_md5")]], hash)) {
    to_upload <- c(to_upload, renamed)
  }
}

if (length(to_upload) == 0) {
  message("Nothing changed, no upload needed.")
  quit(save = "no")
}
message("New assets:\n", paste(" ", basename(to_upload), collapse = "\n"))
if (dry_run) {
  message("Dry run: files are in ", staging)
  quit(save = "no")
}

gh <- function(...) {
  status <- system2("gh", c(...))
  if (status != 0) stop("gh command failed: gh ", paste(c(...), collapse = " "))
}
release_exists <- system2(
  "gh",
  c("release", "view", tag, "-R", repo),
  stdout = FALSE,
  stderr = FALSE
) ==
  0
if (!release_exists) {
  gh(
    "release",
    "create",
    tag,
    "-R",
    repo,
    "--title",
    shQuote("Regression test data"),
    "--notes",
    shQuote(paste(
      "Raw files, schedule and expected output of the games used in",
      "tests/testthat/test-build_nflfastR_pbp.R. Do not delete assets:",
      "older commits refer to them by name (see tests/testthat/regression_data.dcf)."
    )),
    "--prerelease",
    "--latest=false"
  )
}
gh(c("release", "upload", tag, "-R", repo, shQuote(to_upload)))

write_regression_manifest(manifest, manifest_path)
message("Updated ", manifest_path, ". Commit it together with the code change.")
