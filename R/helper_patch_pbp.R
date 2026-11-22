# Patches of single values in the raw data, see .github/CONTRIBUTING.md
#
# inst/pbp_patches.dcf lists errors of the raw data that are easier to fix one
# by one than with a rule in the parser. get_pbp_nfl() applies the patches of a
# game in three places:
#
# - "drop_play" removes a play from the raw plays (before anything is derived
#   from it).
# - "drop_stat" removes all rows of a stat ID on a play from the raw play stats
#   (before tidy_play_stats() derives the stat columns from them).
# - "set" changes a column of the parsed game (after fix_posteams() and
#   fix_bad_games(), before duplicated plays are removed). It only changes the
#   value if the current value is `from`, so a patch that doesn't fit the raw
#   data anymore (e.g. the NFL fixed it) warns instead of silently doing
#   something else. A `set` patch without `play_id` changes all plays of the
#   game.
#
# A patch that doesn't fit warns, and the warning says whether the patch is
# obsolete (the raw data was fixed). check_pbp_patches() collects these
# warnings for all patched games.
#
# build_playstats() applies the drop_play and drop_stat patches too, so the
# play stats of calculate_stats() agree with the pbp.

# All patches of the package, split by game ID. Read once per session.
pbp_patches <- function() {
  if (is.null(patch_env$patches)) {
    patches <- read_pbp_patches()
    patch_env$patches <- split(patches, by = "game_id", keep.by = FALSE)
  }
  patch_env$patches
}

patch_env <- new.env(parent = emptyenv())

read_pbp_patches <- function(
  file = system.file("pbp_patches.dcf", package = "nflfastR")
) {
  # One record per patch. A field that a record leaves out is NA. All values
  # are character, so from and to keep their text until they are converted to
  # the class of their column.
  fields <- c(
    "game_id",
    "play_id",
    "action",
    "stat_id",
    "column",
    "from",
    "to",
    "reason"
  )
  records <- read.dcf(file)
  unknown <- setdiff(colnames(records), fields)
  if (length(unknown) > 0) {
    cli::cli_abort("Unknown field{?s} {.field {unknown}} in the patch file.")
  }
  patches <- data.table::as.data.table(
    lapply(stats::setNames(nm = fields), function(f) {
      x <- if (f %chin% colnames(records)) {
        unname(records[, f])
      } else {
        rep(NA_character_, nrow(records))
      }
      x[x %chin% ""] <- NA_character_
      x
    })
  )
  patches[,
    c("play_id", "stat_id") := lapply(.SD, as.numeric),
    .SDcols = c("play_id", "stat_id")
  ]
  patches[]
}

# The patches of one game with the given action, NULL if there are none.
game_patches <- function(id, action) {
  patches <- pbp_patches()[[id]]
  if (is.null(patches)) {
    return(NULL)
  }
  # outside of [ because action would be the column there
  keep <- patches$action == action
  if (any(keep)) patches[keep] else NULL
}

# Removes plays from the raw plays of a game (a data.table)
patch_drop_plays <- function(plays, id) {
  patches <- game_patches(id, "drop_play")
  if (is.null(patches)) {
    return(plays)
  }
  # A missing play isn't obsolete for sure: the play IDs may have changed
  for (i in which(!patches$play_id %in% plays$playId)) {
    patch_warn(
      id,
      patches[i],
      "{.field play_id} {patches$play_id[i]} doesn't exist"
    )
  }
  plays[!plays$playId %in% patches$play_id]
}

# Removes stat IDs from the raw play stats of a game
patch_drop_stats <- function(stats, id) {
  patches <- game_patches(id, "drop_stat")
  if (is.null(patches)) {
    return(stats)
  }
  drop <- logical(nrow(stats))
  for (i in seq_len(nrow(patches))) {
    play <- stats$playId == patches$play_id[i]
    hit <- play & stats$statId == patches$stat_id[i]
    if (!any(hit)) {
      # The play still has stats, just not this one: the raw data was fixed
      patch_warn(
        id,
        patches[i],
        "{.field play_id} {patches$play_id[i]} has no {.field stat_id} \\
        {patches$stat_id[i]}",
        obsolete = any(play)
      )
    }
    drop <- drop | hit
  }
  if (any(drop)) stats[!drop] else stats
}

# Changes values of the parsed game by reference
patch_set_values <- function(pbp, id) {
  patches <- game_patches(id, "set")
  if (is.null(patches)) {
    return(invisible(pbp))
  }
  for (i in seq_len(nrow(patches))) {
    column <- patches$column[i]
    if (!column %chin% names(pbp)) {
      cli::cli_abort(
        "Patch of {.val {id}}: column {.field {column}} doesn't exist."
      )
    }
    play_id <- patches$play_id[i]
    rows <- if (is.na(play_id)) {
      seq_len(nrow(pbp))
    } else {
      which(pbp$play_id == play_id)
    }
    if (length(rows) == 0) {
      patch_warn(id, patches[i], "{.field play_id} {play_id} doesn't exist")
      next
    }
    current <- pbp[[column]][rows]
    from <- patch_value(patches$from[i], current, id, column)
    to <- patch_value(patches$to[i], current, id, column)
    if (!all(is_value(current, from))) {
      # The raw data has the correct value now
      patch_warn(
        id,
        patches[i],
        "{.field {column}} of {.field play_id} {play_id} is \\
        {.val {unique(current)}}, not {.val {from}} as the patch expects",
        obsolete = all(is_value(current, to))
      )
      next
    }
    data.table::set(pbp, i = rows, j = column, value = to)
  }
  invisible(pbp)
}

# Whether x is value, where NA is a value too
is_value <- function(x, value) {
  if (is.na(value)) is.na(x) else !is.na(x) & x == value
}

# Converts the text of a patch value to the class of its column
patch_value <- function(x, column_values, id, column) {
  value <- switch(
    typeof(column_values),
    character = x,
    integer = suppressWarnings(as.integer(x)),
    double = suppressWarnings(as.numeric(x)),
    logical = as.logical(x),
    cli::cli_abort(
      "Patch of {.val {id}}: can't patch column {.field {column}} of type \\
      {.cls {typeof(column_values)}}."
    )
  )
  if (is.na(value) && !is.na(x)) {
    cli::cli_abort(
      "Patch of {.val {id}}: {.val {x}} isn't a valid value of column \\
      {.field {column}} ({.cls {typeof(column_values)}})."
    )
  }
  value
}

# Warns that a patch (one row of game_patches()) wasn't applied. obsolete means
# that the raw data was fixed, so the patch can be removed from the file.
patch_warn <- function(
  id,
  patch,
  msg,
  obsolete = FALSE,
  .envir = parent.frame()
) {
  cli::cli_warn(
    c("Patch of {.val {id}} not applied:", "x" = msg),
    class = "nflfastR_patch_not_applied",
    game_id = id,
    patch = patch,
    obsolete = obsolete,
    .envir = .envir
  )
}

# Applies the patches of all patched games to their raw data. Returns the
# patches that weren't applied with the columns of the patch file, obsolete and
# message. Used by the tests and data-raw/pbp_patches/check_patches.R.
check_pbp_patches <- function(
  dir = getOption("nflfastR.raw_directory", default = NULL)
) {
  # an empty table with the columns of the result
  template <- read_pbp_patches()[0]
  template[, c("obsolete", "message") := list(logical(), character())]
  acc <- new.env(parent = emptyenv())
  acc$rows <- list(template)
  for (id in names(pbp_patches())) {
    withCallingHandlers(
      suppressMessages(get_pbp_nfl(id, dir = dir)),
      nflfastR_patch_not_applied = function(w) {
        row <- data.table::data.table(game_id = w$game_id, w$patch)
        row[,
          c("obsolete", "message") := list(
            w$obsolete,
            cli::ansi_strip(conditionMessage(w))
          )
        ]
        acc$rows[[length(acc$rows) + 1]] <- row
        invokeRestart("muffleWarning")
      }
    )
  }
  data.table::rbindlist(acc$rows, use.names = TRUE)
}
