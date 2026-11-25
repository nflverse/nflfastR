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
# - "set" changes a column of the parsed game, before anything is derived from
#   it (e.g. yardline_100 from yardline). Patches of the columns that
#   get_pbp_nfl() overwrites later (late_patch_columns) are applied after
#   fix_posteams() and fix_bad_games(). Both before duplicated plays are
#   removed. A set patch only changes the value if the current value is
#   `from`, so a patch that doesn't fit the raw data anymore (e.g. the NFL
#   fixed it) warns instead of silently doing something else. A `set` patch
#   without `play_id` changes all plays of the game. Columns that don't exist
#   yet (e.g. yardline_100) can't be patched; patch_source_columns names the
#   column to patch instead.
#
# A patch that doesn't fit warns, and the warning says whether the patch is
# obsolete (a set patch whose column has the value `to` now).
# check_pbp_patches() in R/check_pbp_patches.R collects these warnings for all
# patched games.
#
# build_playstats() applies the drop_play and drop_stat patches too, so the
# play stats of calculate_stats() agree with the pbp.

# All patches of the package, split by game ID. Read once per session.
# with_pbp_patches() replaces them temporarily.
pbp_patches <- function() {
  if (!is.null(patch_env$override)) {
    return(patch_env$override)
  }
  if (is.null(patch_env$patches)) {
    patches <- read_pbp_patches()
    patch_env$patches <- split(patches, by = "game_id", keep.by = FALSE)
  }
  patch_env$patches
}

patch_env <- new.env(parent = emptyenv())

# Evaluates code with other patches (a data.table like read_pbp_patches()
# returns) instead of those of the package. Only works sequentially: future
# workers don't see the replacement.
with_pbp_patches <- function(patches, code) {
  old <- patch_env$override
  patch_env$override <- split(patches, by = "game_id", keep.by = FALSE)
  on.exit(assign("override", old, envir = patch_env))
  force(code)
}

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
  # A play_id that isn't a number would become NA, and a set patch without
  # play_id changes all plays of the game
  for (f in c("play_id", "stat_id")) {
    text <- patches[[f]]
    number <- suppressWarnings(as.numeric(text))
    bad <- !is.na(text) & is.na(number)
    if (any(bad)) {
      cli::cli_abort(
        "{.field {f}} must be a number, not {.val {text[bad]}}, in the \
        patch file."
      )
    }
    data.table::set(patches, j = f, value = number)
  }
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
    # %in% because playId or statId can be NA
    on_play <- stats$playId %in% patches$play_id[i]
    hit <- on_play & stats$statId %in% patches$stat_id[i]
    # Not obsolete for sure even if the play is there: the NFL may have changed
    # the stat ID and kept the error
    if (!any(hit)) {
      patch_warn(
        id,
        patches[i],
        "{.field play_id} {patches$play_id[i]} has no {.field stat_id} \\
        {patches$stat_id[i]}"
      )
    }
    drop <- drop | hit
  }
  if (any(drop)) stats[!drop] else stats
}

# Columns that don't exist when set patches are applied, with the column to
# patch instead: those of the final data (keep in sync with the table in
# .github/CONTRIBUTING.md) and yardline_number and yardline_side, which
# get_pbp_nfl() derives right after the early pass
patch_source_columns <- c(
  desc = "play_description",
  qtr = "quarter",
  quarter_end = "play_description",
  side_of_field = "yardline",
  yardline_100 = "yardline",
  yardline_number = "yardline",
  yardline_side = "yardline",
  ydstogo = "yards_to_go",
  yrdln = "yardline"
)

# Columns that get_pbp_nfl() overwrites after it applies the first set
# patches: posteam (fix_posteams()), goal_to_go and the team columns of
# fix_bad_games(). Their patches are applied after these steps.
late_patch_columns <- c(
  "posteam",
  "goal_to_go",
  "td_team",
  "penalty_team",
  "safety_team",
  "return_team",
  "fumble_recovery_1_team",
  "timeout_team"
)

# Changes values of the parsed game by reference. With late = TRUE, only the
# patches of late_patch_columns, otherwise all others.
patch_set_values <- function(pbp, id, late = FALSE) {
  patches <- game_patches(id, "set")
  if (!is.null(patches)) {
    patches <- patches[(patches$column %chin% late_patch_columns) == late]
  }
  if (is.null(patches) || nrow(patches) == 0) {
    return(invisible(pbp))
  }
  for (i in seq_len(nrow(patches))) {
    column <- patches$column[i]
    if (!column %chin% names(pbp)) {
      source <- patch_source_columns[column]
      cli::cli_abort(c(
        "Patch of {.val {id}}: column {.field {column}} doesn't exist when \\
        {.code set} patches are applied.",
        i = if (is.na(source)) {
          "See the section on patches in {.file .github/CONTRIBUTING.md}."
        } else {
          "Patch {.field {source}} instead."
        }
      ))
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
    # empty_strings_to_na() hasn't run before the early pass, so a missing
    # value can be ""
    if (is.character(current)) {
      current[current %chin% ""] <- NA_character_
    }
    from <- patch_value(patches$from[i], current, id, column)
    to <- patch_value(patches$to[i], current, id, column)
    fits <- is_value(current, from)
    if (!all(fits)) {
      # obsolete if the raw data has the correct value now
      other <- cli::cli_vec(unique(current[!fits]), list("vec-trunc" = 3))
      msg <- if (is.na(play_id)) {
        "{.field {column}} is {.val {other}} on some plays, not \\
        {.val {from}} as the patch expects"
      } else {
        "{.field {column}} of {.field play_id} {play_id} is {.val {other}}, \\
        not {.val {from}} as the patch expects"
      }
      patch_warn(id, patches[i], msg, obsolete = all(is_value(current, to)))
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
# that the raw data was fixed (a set patch whose column has the value `to`
# now), so the patch can be removed from the file.
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
    detail = cli::format_inline(msg, .envir = .envir),
    game_id = id,
    patch = patch,
    obsolete = obsolete,
    .envir = .envir
  )
}
