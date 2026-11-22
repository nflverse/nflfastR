# Tools for the maintainers of inst/pbp_patches.dcf, see .github/CONTRIBUTING.md
#
# Check all patches against the current raw data and parser, from the root of
# the package:
#
#   devtools::load_all()
#   check_pbp_patches()
#   check_pbp_patches(prune = TRUE) # removes obsolete and unused patches
#
# The workflow .github/workflows/check-patches.yaml runs check_pbp_patches()
# every week, because the raw data can change without a change of nflfastR.

# Checks every patch and fails if one has a problem (see pbp_patch_problems()).
# Checks unused patches by default, unlike pbp_patch_problems().
# With prune = TRUE, the obsolete and unused patches are removed from file
# first, without changing the other records. The other problems need a look,
# because the error of the raw data may still be there with other play IDs or
# values.
check_pbp_patches <- function(
  dir = getOption("nflfastR.raw_directory", default = NULL),
  unused = TRUE,
  prune = FALSE,
  file = "inst/pbp_patches.dcf",
  ...
) {
  if (prune) {
    in_file <- split(read_pbp_patches(file), by = "game_id", keep.by = FALSE)
    if (!identical(in_file, pbp_patches())) {
      cli::cli_abort(
        "{.path {file}} doesn't have the patches that are checked. Run \\
        {.code devtools::load_all()} from the root of the package first."
      )
    }
  }
  cli::cli_inform("Checking the patches of {length(pbp_patches())} game{?s}")
  problems <- pbp_patch_problems(dir = dir, unused = unused, ...)

  removable <- problems$status %chin% c("obsolete", "unused")
  if (prune && any(removable)) {
    remove_pbp_patches(problems[removable], file)
    removed <- problems$message[removable]
    cli::cli_alert_success(
      "Removed {length(removed)} obsolete or unused patch{?es}:"
    )
    cli::cli_bullets(stats::setNames(removed, rep("*", length(removed))))
    problems <- problems[!removable]
  }
  if (nrow(problems) > 0) {
    bullets <- paste0(problems$message, " (", problems$todo, ")")
    cli::cli_abort(c(
      "{nrow(problems)} patch{?es} need{?s/} a look:",
      stats::setNames(bullets, rep("x", length(bullets)))
    ))
  }
  cli::cli_alert_success("All patches are applied and change the data")
  invisible(problems)
}

# The patches that have a problem with the current raw data and parser, with
# the key columns of the patch file plus status, message and todo (what to do
# about it). status is
#
# - "obsolete": a set patch that isn't applied because the raw data was fixed:
#   the column has the value `to` now. Safe to remove.
# - "not_applied": not applied for another reason (a missing play or stat, a
#   value other than `from` and `to`).
# - "unused": applied, but the pbp (and for drop_stat the play stats) is the
#   same without it, e.g. because the parser handles the error now. Safe to
#   remove, also all unused patches together: each patch is tested without the
#   unused patches found before it. Only with unused = TRUE, which builds every
#   patched game once more for each of its patches.
#
# unused is FALSE by default because the online test only needs to know
# whether the patches fit the raw data, and the builds take a while.
# check_pbp_patches() checks everything by default.
#
# Without dir, the raw data of the patched games is downloaded once into a
# temporary directory. ... is passed to build_nflfastR_pbp() (e.g. games).
pbp_patch_problems <- function(
  dir = getOption("nflfastR.raw_directory", default = NULL),
  unused = FALSE,
  ...
) {
  patches <- data.table::rbindlist(pbp_patches(), idcol = "game_id")
  ids <- unique(patches$game_id)
  if (is.null(dir)) {
    dir <- tempfile("raw_pbp")
    dir.create(dir)
    on.exit(unlink(dir, recursive = TRUE), add = TRUE)
    downloads <- save_raw_pbp(ids, dir = dir)
    if (any(downloads$status_code != 200)) {
      cli::cli_abort("Can't download the raw data of all patched games.")
    }
  }

  key <- c("game_id", "play_id", "action", "stat_id", "column")
  problems <- patches[0, key, with = FALSE]
  problems[,
    c("status", "message", "todo") := list(
      character(),
      character(),
      character()
    )
  ]
  acc <- new.env(parent = emptyenv())
  acc$rows <- list(problems)

  for (id in ids) {
    withCallingHandlers(
      suppressMessages(get_pbp_nfl(id, dir = dir)),
      nflfastR_patch_not_applied = function(w) {
        status <- if (w$obsolete) "obsolete" else "not_applied"
        patch <- data.table::data.table(game_id = w$game_id, w$patch)
        acc$rows[[length(acc$rows) + 1]] <- patch_problem(
          patch[, key, with = FALSE],
          status,
          w$detail
        )
        invokeRestart("muffleWarning")
      }
    )
  }
  problems <- data.table::rbindlist(acc$rows, use.names = TRUE)
  if (!unused) {
    return(problems)
  }

  # with_pbp_patches() only reaches this process. In future workers, the
  # builds without a patch would use all patches, and every patch would look
  # unused.
  old_plan <- future::plan("sequential")
  on.exit(future::plan(old_plan), add = TRUE)

  # A patch is unused if the output is the same without it and without the
  # unused patches found before it
  applied <- patches[!problems, on = key]
  kept <- patches
  out <- list(problems)
  for (id in unique(applied$game_id)) {
    with_all <- patched_game_output(id, dir, ...)
    for (i in which(applied$game_id == id)) {
      patch <- applied[i]
      others <- kept[!patch, on = key]
      without <- with_pbp_patches(others, patched_game_output(id, dir, ...))
      same <- identical(with_all$pbp, without$pbp) &&
        (patch$action != "drop_stat" ||
          identical(with_all$playstats, without$playstats))
      if (same) {
        kept <- others
        out[[length(out) + 1]] <- patch_problem(
          patch[, key, with = FALSE],
          "unused",
          "the pbp is the same without it"
        )
      }
    }
  }
  data.table::rbindlist(out, use.names = TRUE)
}

# One row of pbp_patch_problems(): the patch (its key columns), status, what
# is wrong and what to do about it
patch_problem <- function(patch, status, detail) {
  todo <- switch(
    status,
    obsolete = "the raw data was fixed, {.code check_pbp_patches(prune = TRUE)} removes it",
    unused = "{.code check_pbp_patches(prune = TRUE)} removes it",
    not_applied = "check the raw data and fix or remove the patch"
  )
  what <- paste0(
    patch$game_id,
    " ",
    patch$action,
    if (!is.na(patch$play_id)) paste0(" play ", patch$play_id),
    if (!is.na(patch$stat_id)) paste0(" stat ", patch$stat_id),
    if (!is.na(patch$column)) paste0(" ", patch$column)
  )
  patch[,
    c("status", "message", "todo") := list(
      status,
      paste0(status, ": ", what, ": ", cli::ansi_strip(detail)),
      cli::format_inline(todo)
    )
  ]
}

# The pbp and play stats of a game with the current patches, as lists of
# columns without other attributes (like the time it was built). Built
# sequentially, because future workers wouldn't see with_pbp_patches(). The
# patch warnings are muffled because pbp_patch_problems() collected them.
patched_game_output <- function(id, dir, ...) {
  withCallingHandlers(
    {
      pbp <- suppressMessages(
        build_nflfastR_pbp(id, dir = dir, ..., rules = FALSE)
      )
      playstats <- playstats_of_game(
        id,
        p = \(...) NULL,
        stat_ids = 1:1000,
        dir = dir,
        skip_local = FALSE
      )
    },
    nflfastR_patch_not_applied = \(w) invokeRestart("muffleWarning")
  )
  columns <- function(x) {
    x <- as.list(x)
    attributes(x) <- list(names = names(x))
    x
  }
  list(pbp = columns(pbp), playstats = columns(playstats))
}

# Removes patches (rows with the key columns of the patch file) from file. The
# other records stay as they are, with their line breaks.
remove_pbp_patches <- function(remove, file) {
  lines <- readLines(file)
  # a record starts at its first line after an empty line
  record <- cumsum(lines != "" & c(TRUE, utils::head(lines, -1) == ""))
  records <- split(lines[lines != ""], record[lines != ""])
  patches <- read_pbp_patches(file)
  if (length(records) != nrow(patches)) {
    cli::cli_abort("Can't split {.path {file}} into its records.")
  }
  key <- c("game_id", "play_id", "action", "stat_id", "column")
  drop <- patches[remove, on = key, which = TRUE, nomatch = NULL]
  keep <- if (length(drop) > 0) records[-drop] else records
  writeLines(
    paste(vapply(keep, paste, "", collapse = "\n"), collapse = "\n\n"),
    file
  )
  invisible(file)
}
