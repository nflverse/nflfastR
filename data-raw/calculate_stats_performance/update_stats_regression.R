# Create or update the data of the calculate_stats() regression test
# (see README.md)
#
# Pins the inputs of calculate_stats() for the games in selected_games.csv
# (pbp, play stats and players) and stores the output of the current
# calculate_stats() on them as expected output. Both go to the
# regression_tests release of the nflfastR GitHub repo, next to the pbp
# regression data, and get entries in tests/testthat/regression_data.dcf,
# the only file to commit. Asset names contain an md5 checksum, so existing
# assets are never replaced and older commits keep working.
#
# Usage (from the package root, needs the gh CLI with write access):
#
#   Rscript data-raw/calculate_stats_performance/update_stats_regression.R
#     Rebuild the expected output with the current package code from the
#     pinned inputs of the current manifest. Run this after an intentional
#     change of the calculate_stats() output and check the test output first.
#
#   Rscript data-raw/calculate_stats_performance/update_stats_regression.R --new-inputs
#     Download pbp, play stats and players of the games in selected_games.csv
#     and rebuild inputs and expected output. Downloads are kept in
#     hits/pin_download (git-ignored); delete it to download again.
#
#   Add --dry-run to build the files without uploading or touching the
#   manifest.
#
# Never run this without --dry-run unless the user explicitly asks: the
# upload goes to a public release.

args <- commandArgs(trailingOnly = TRUE)
dry_run <- "--dry-run" %in% args
new_inputs <- "--new-inputs" %in% args
manifest_path <- "tests/testthat/regression_data.dcf"
repo <- "nflverse/nflfastR"
tag <- "regression_tests"
here <- "data-raw/calculate_stats_performance"

suppressMessages(pkgload::load_all(".", quiet = TRUE))
source("tests/testthat/helper-regression.R")
source(file.path(here, "checklist.R"))
md5 <- function(path) unname(tools::md5sum(path))

staging <- file.path(tempdir(), "stats_regression_staging")
unlink(staging, recursive = TRUE)
dir.create(staging)

old <- if (file.exists(manifest_path)) read_regression_manifest(manifest_path)
files <- list()

# Plain tibble without the attributes that change with every download
# (timestamp, version) and without data.table's self reference
as_plain_tibble <- function(x) {
  x <- as.data.frame(x)
  attrs <- names(attributes(x))
  attributes(x)[attrs[grepl("nflverse|nflfastR|.internal.selfref", attrs)]] <-
    NULL
  tibble::as_tibble(x)
}

# Inputs -----------------------------------------------------------------------

build_inputs <- function() {
  selected <- data.table::fread(file.path(here, "selected_games.csv"))
  game_ids <- selected$game_id
  seasons <- sort(unique(selected$season))
  stopifnot(all(seasons >= 2001L))

  download_dir <- file.path(here, "hits", "pin_download")
  dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)
  release <- "https://github.com/nflverse/%s/releases/download/%s/%s.rds"
  # readRDS() of a URL times out after 60 s on slow connections, so download
  # to a file first
  download_rds <- function(repo, tag, name) {
    file <- file.path(download_dir, paste0(name, ".rds"))
    if (!file.exists(file)) {
      cli::cli_inform("Downloading {name}")
      curl::curl_download(sprintf(release, repo, tag, name), file)
    }
    readRDS(file)
  }

  pbp <- data.table::rbindlist(lapply(seasons, function(s) {
    p <- data.table::as.data.table(
      download_rds("nflverse-data", "pbp", paste0("play_by_play_", s))
    )
    p[game_id %chin% game_ids, ..checklist_pbp_columns]
  }))
  playstats <- data.table::rbindlist(lapply(seasons, function(s) {
    p <- data.table::as.data.table(
      download_rds("nflverse-pbp", "playstats", paste0("play_stats_", s))
    )
    p[game_id %chin% game_ids]
  }))
  missing_games <- setdiff(game_ids, pbp$game_id)
  if (length(missing_games) > 0L) {
    stop("Games missing from pbp: ", paste(missing_games, collapse = ", "))
  }
  missing_games <- setdiff(game_ids, playstats$game_id)
  if (length(missing_games) > 0L) {
    stop(
      "Games missing from playstats: ",
      paste(missing_games, collapse = ", ")
    )
  }
  data.table::setorderv(pbp, c("game_id", "play_id"))

  players <- data.table::as.data.table(
    download_rds("nflverse-data", "players", "players")
  )
  players <- players[
    gsis_id %chin% playstats$gsis_player_id,
    c("gsis_id", "display_name", "position", "position_group", "headshot")
  ]
  data.table::setorderv(players, "gsis_id")

  # "player id missing from load_players()": remove two players with negative
  # receiving air yards in a game, chosen by the smallest gsis_id. One isn't an
  # RB/FB/HB (racr is 0 with and without the player info), one is (racr
  # changes from negative to 0 because the player is no longer in racr_ids).
  out <- checklist_outputs(pbp, playstats, players)
  wp <- data.table::as.data.table(out[["week/player/REG+POST"]])
  neg <- sort(unique(wp[receiving_air_yards < 0, player_id]))
  rb_ids <- players[position %chin% c("RB", "FB", "HB"), gsis_id]
  removed <- c(
    neg[!neg %chin% rb_ids & neg %chin% players$gsis_id][1L],
    neg[neg %chin% rb_ids][1L]
  )
  stopifnot(!anyNA(removed))
  cli::cli_inform("Removed from players: {removed}")
  players <- players[!gsis_id %chin% removed]

  list(
    pbp = as_plain_tibble(pbp),
    playstats = as_plain_tibble(playstats),
    players = as_plain_tibble(players)
  )
}

if (new_inputs) {
  inputs <- build_inputs()
  inputs_file <- file.path(staging, "stats_inputs.rds")
  saveRDS(inputs, inputs_file, compress = "xz")
  files$stats_inputs <- inputs_file
} else {
  if (is.null(old$stats_inputs)) {
    stop("No stats inputs in the manifest yet, run with --new-inputs first")
  }
  reg <- regression_data(manifest_path, set = "stats")
  if (is.null(reg)) {
    stop("Download of current stats regression data failed")
  }
  inputs <- readRDS(reg$stats_inputs)
}

# The checklist on the pinned inputs. Upstream data can change between the
# game selection and the pinning, so stop if anything is uncovered other than
# the unreachable fg_missed_0_19 bucket (see README.md).
out <- checklist_outputs(inputs$pbp, inputs$playstats, inputs$players)
items <- checklist_items(inputs$pbp, inputs$playstats, inputs$players, out)
gaps <- checklist_report(items)
unexpected <- gaps[item != "fg_missed_0_19 != 0"]
if (nrow(unexpected) > 0L) {
  stop("The pinned inputs don't cover the checklist, see above")
}

# Expected output --------------------------------------------------------------

expected_outputs <- function(inputs) {
  testthat::local_mocked_bindings(
    load_playstats = function(...) inputs$playstats,
    stats_load_players = function() inputs$players,
    stats_load_pbp = function(seasons) inputs$pbp,
    .package = "nflfastR"
  )
  combos <- data.table::CJ(
    summary_level = c("season", "week"),
    stat_type = c("player", "team"),
    season_type = c("REG", "POST", "REG+POST"),
    sorted = FALSE
  )
  run <- function(summary_level, stat_type, season_type, pbp) {
    suppressMessages(calculate_stats(
      seasons = sort(unique(inputs$pbp$season)),
      summary_level = summary_level,
      stat_type = stat_type,
      season_type = season_type,
      pbp = pbp
    ))
  }
  out <- lapply(seq_len(nrow(combos)), function(i) {
    run(
      combos$summary_level[i],
      combos$stat_type[i],
      combos$season_type[i],
      inputs$pbp
    )
  })
  names(out) <- combo_name(
    combos$summary_level,
    combos$stat_type,
    combos$season_type
  )
  # pbp = NULL loads pbp with the (mocked) stats_load_pbp()
  out[["pbp_null/season/player/REG"]] <- run("season", "player", "REG", NULL)
  out
}

expected <- expected_outputs(inputs)
expected_file <- file.path(staging, "stats_expected.rds")
saveRDS(expected, expected_file, compress = "xz")
files$stats_expected <- expected_file
cli::cli_inform(c(
  "Built expected output of {length(expected)} calls from \\
  {length(unique(inputs$pbp$game_id))} games:",
  paste0(names(expected), ": ", vapply(expected, nrow, 0L), " rows")
))

# Manifest and upload ----------------------------------------------------------

# name new assets by checksum, keep the other entries of the old manifest
manifest <- c(
  list(repo = repo, tag = tag),
  old[setdiff(names(old), c("repo", "tag"))]
)
to_upload <- character()
for (kind in names(files)) {
  hash <- md5(files[[kind]])
  name <- sprintf("regression_%s_%s.rds", kind, substr(hash, 1, 10))
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
sizes <- sprintf(
  "  %s (%.0f kB)",
  basename(to_upload),
  file.size(to_upload) / 1024
)
message("New assets:\n", paste(sizes, collapse = "\n"))
if (dry_run) {
  message("Dry run: files are in ", staging)
  quit(save = "no")
}

# the release already exists (see update_regression_test.R)
status <- system2(
  "gh",
  c("release", "upload", tag, "-R", repo, shQuote(to_upload))
)
if (status != 0) {
  stop("gh release upload failed")
}

write_regression_manifest(manifest, manifest_path)
message("Updated ", manifest_path, ". Commit it together with the code change.")
