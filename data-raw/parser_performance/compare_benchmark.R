# Compare the speed of build_nflfastR_pbp() of two versions of nflfastR.
#
# Usage (paths to the source of both versions, e.g. two git checkouts):
#   Rscript compare_benchmark.R <base_dir> <head_dir> [iterations] [out_dir]
#
# Both versions parse the pinned raw data of the regression games (see
# data-raw/test_game_selection/README.md) with a sequential future plan:
# - all regression games in one build_nflfastR_pbp() call
# - one game 10 times (median)
# Every measurement runs in a fresh R process that loads the package with
# pkgload::load_all(), alternating between the versions, so changing load on
# the machine affects both. The script also checks that both versions return
# the same output. The results are written to out_dir as results.csv and
# summary.md, and appended to the GitHub job summary if there is one.

args <- commandArgs(TRUE)

# ---- worker: one measurement of one version ----------------------------------
if (identical(args[1], "worker")) {
  pkg_dir <- args[2]
  raw_dir <- args[3]
  games_file <- args[4]
  out_file <- args[5]
  save_output <- identical(args[6], "TRUE")

  suppressMessages(pkgload::load_all(pkg_dir, quiet = TRUE))
  future::plan("sequential")
  games <- readRDS(games_file)
  ids <- sub(
    "\\.rds$",
    "",
    basename(list.files(
      raw_dir,
      pattern = "\\.rds$",
      recursive = TRUE
    ))
  )
  single_id <- max(ids)
  build <- function(game_ids) {
    suppressWarnings(suppressMessages(build_nflfastR_pbp(
      game_ids,
      dir = raw_dir,
      games = games,
      rules = FALSE
    )))
  }

  # warm up: lazy loading, models, compiled functions
  invisible(build(single_id))
  single <- vapply(
    1:10,
    function(i) system.time(build(single_id))[["elapsed"]],
    numeric(1)
  )
  all_games <- system.time(out <- build(ids))[["elapsed"]]

  if (save_output) {
    for (a in c("nflverse_timestamp", "nflfastR_version")) {
      attr(out, a) <- NULL
    }
    saveRDS(out, sub("\\.rds$", "_output.rds", out_file))
  }
  saveRDS(
    list(
      single_game = stats::median(single),
      all_games = all_games,
      n_games = length(ids),
      n_plays = nrow(out),
      single_id = single_id
    ),
    out_file
  )
  quit(save = "no")
}

# ---- controller ------------------------------------------------------------------
base_dir <- normalizePath(args[1])
head_dir <- normalizePath(args[2])
iterations <- if (length(args) >= 3) as.integer(args[3]) else 3L
out_dir <- if (length(args) >= 4) args[4] else "benchmark_results"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
out_dir <- normalizePath(out_dir)

# the pinned regression data of the head version
local({
  helper <- new.env()
  sys.source(
    file.path(head_dir, "tests", "testthat", "helper-regression.R"),
    envir = helper
  )
  reg <- helper$regression_data(
    file.path(head_dir, "tests", "testthat", "regression_data.dcf")
  )
  if (is.null(reg)) {
    stop("Download of the regression data failed")
  }
  assign("reg", reg, envir = globalenv())
})

# this script, to start the workers with it
script <- normalizePath(sub(
  "^--file=",
  "",
  grep("^--file=", commandArgs(FALSE), value = TRUE)
))
rscript <- file.path(R.home("bin"), "Rscript")

results <- list()
for (i in seq_len(iterations)) {
  for (version in c("base", "head")) {
    cat(sprintf("iteration %d/%d: %s\n", i, iterations, version))
    out_file <- file.path(out_dir, sprintf("%s_%d.rds", version, i))
    status <- system2(
      rscript,
      c(
        script,
        "worker",
        if (version == "base") base_dir else head_dir,
        reg$raw_dir,
        reg$games,
        out_file,
        i == 1
      )
    )
    if (status != 0) {
      stop("Benchmark of ", version, " failed")
    }
    r <- readRDS(out_file)
    results[[length(results) + 1]] <- data.frame(
      version = version,
      iteration = i,
      single_game_s = r$single_game,
      all_games_s = r$all_games,
      n_games = r$n_games,
      n_plays = r$n_plays,
      single_id = r$single_id
    )
  }
}
results <- do.call(rbind, results)
utils::write.csv(results, file.path(out_dir, "results.csv"), row.names = FALSE)

base_out <- readRDS(file.path(out_dir, "base_1_output.rds"))
head_out <- readRDS(file.path(out_dir, "head_1_output.rds"))
same_output <- identical(base_out, head_out)
differences <- if (!same_output) {
  cols <- intersect(names(base_out), names(head_out))
  changed <- cols[
    !vapply(cols, function(n) identical(base_out[[n]], head_out[[n]]), TRUE)
  ]
  describe <- function(n) {
    b <- base_out[[n]]
    h <- head_out[[n]]
    if (!identical(typeof(b), typeof(h))) {
      sprintf("`%s`: type %s -> %s", n, typeof(b), typeof(h))
    } else if (length(b) == length(h)) {
      sprintf(
        "`%s`: %d values differ",
        n,
        sum(!(b == h | (is.na(b) & is.na(h))) | xor(is.na(b), is.na(h)))
      )
    } else {
      sprintf("`%s`", n)
    }
  }
  c(
    if (!identical(names(base_out), names(head_out))) "column names or order",
    if (!identical(class(base_out), class(head_out))) "class",
    if (nrow(base_out) != nrow(head_out)) "number of plays",
    vapply(changed, describe, character(1), USE.NAMES = FALSE)
  )
}

med <- function(version, col) {
  stats::median(results[[col]][results$version == version])
}
row <- function(label, col, digits, unit, scale = 1) {
  b <- med("base", col) * scale
  h <- med("head", col) * scale
  sprintf(
    "| %s | %s %s | %s %s | %.1fx |",
    label,
    format(round(b, digits), nsmall = digits),
    unit,
    format(round(h, digits), nsmall = digits),
    unit,
    b / h
  )
}
summary_md <- c(
  "## Parser benchmark",
  "",
  sprintf(
    "`build_nflfastR_pbp()` of head (`%s`) vs base (`%s`), median of %d runs per version on %s with %d cores.",
    Sys.getenv("HEAD_REF", basename(head_dir)),
    Sys.getenv("BASE_REF", basename(base_dir)),
    iterations,
    R.version$platform,
    parallel::detectCores()
  ),
  "",
  "| | base | head | speedup |",
  "|---|---|---|---|",
  row(
    sprintf(
      "All %d regression games (%d plays)",
      results$n_games[1],
      results$n_plays[1]
    ),
    "all_games_s",
    1,
    "s"
  ),
  row(
    sprintf("Single game (`%s`, median of 10)", results$single_id[1]),
    "single_game_s",
    0,
    "ms",
    scale = 1000
  ),
  "",
  if (same_output) {
    "Both versions return identical output."
  } else {
    c(
      "**The output differs** between the versions:",
      "",
      paste0("- ", differences)
    )
  }
)
writeLines(summary_md, file.path(out_dir, "summary.md"))
if (nzchar(Sys.getenv("GITHUB_STEP_SUMMARY"))) {
  cat(
    summary_md,
    file = Sys.getenv("GITHUB_STEP_SUMMARY"),
    sep = "\n",
    append = TRUE
  )
}
cat(summary_md, sep = "\n")
