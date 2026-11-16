# Condition coverage for the nflfastR parser
#
# Line coverage is useless for picking test games because most of the parser
# is vectorized: a `case_when()` line runs for every game, whether or not any
# play matches. So instead we rewrite the parser source and wrap every
# condition in `.cov_hit(<id>, <condition>)`. That call records, per game,
# whether the condition was evaluated at all, TRUE for at least one value and
# FALSE for at least one value, and returns the condition unchanged.
#
# Instrumented conditions:
#   - `if (cond)`
#   - left hand sides of `case_when()` formulas
#   - first argument of `if_else()`, `ifelse()`, `fifelse()`
#   - condition arguments of `fcase()`
#   - condition arguments of piped `filter()` calls
#   - function bodies (reached or not)
#
# Insertions never add newlines, so line numbers in the instrumented code
# match the original files.

parser_files <- c(
  "R/build_nflfastR_pbp.R",
  "R/top-level_scraper.R",
  "R/helper_scrape_nfl.R",
  "R/helper_tidy_play_stats.R",
  "R/helper_add_game_data.R",
  "R/helper_add_nflscrapr_mutations.R",
  "R/helper_add_ep_wp.R",
  "R/helper_add_cp_cpoe.R",
  "R/helper_add_xyac.R",
  "R/helper_add_xpass.R",
  "R/helper_add_fixed_drives.R",
  "R/helper_add_series_data.R",
  "R/helper_additional_functions.R",
  "R/helper_variable_selector.R",
  "R/helper_decode_player_ids.R",
  "R/utils.R"
)

# Find all code locations to instrument in one file.
# Returns a data frame with one row per location.
find_targets <- function(file) {
  pd <- utils::getParseData(parse(file, keep.source = TRUE), includeText = TRUE)
  pd <- pd[order(pd$line1, pd$col1, -pd$line2, -pd$col2), ]
  children <- split(pd, pd$parent)
  kids <- function(id) {
    k <- children[[as.character(id)]]
    if (is.null(k)) pd[0, ] else k
  }

  out <- list()
  add <- function(row, kind) {
    if (row$token != "expr" || identical(trimws(row$text), "TRUE")) {
      return()
    }
    out[[length(out) + 1]] <<- data.frame(
      file = file,
      line1 = row$line1,
      col1 = row$col1,
      line2 = row$line2,
      col2 = row$col2,
      kind = kind,
      text = row$text
    )
  }

  # positional (unnamed) arguments of a call node
  positional_args <- function(call_id) {
    k <- kids(call_id)
    k <- k[k$token %in% c("expr", "SYMBOL_SUB", "','", "')'"), ]
    args <- list()
    named <- FALSE
    for (i in seq_len(nrow(k))[-1]) {
      tok <- k$token[i]
      if (tok == "SYMBOL_SUB") {
        named <- TRUE
      } else if (tok %in% c("','", "')'")) {
        named <- FALSE
      } else if (tok == "expr" && !named) {
        args[[length(args) + 1]] <- k[i, ]
      }
    }
    args
  }

  is_piped <- function(call_id) {
    parent <- pd$parent[pd$id == call_id]
    sib <- kids(parent)
    any(sib$token %in% c("PIPE", "SPECIAL")) &&
      which(sib$id == call_id) > which(sib$token %in% c("PIPE", "SPECIAL"))[1]
  }

  # if (cond)
  for (if_id in pd$id[pd$token == "IF"]) {
    k <- kids(pd$parent[pd$id == if_id])
    open <- which(k$token == "'('")[1]
    add(k[open + 1, ], "if")
  }

  # function bodies: last child of the FUNCTION node's parent
  for (fn_id in pd$id[pd$token == "FUNCTION"]) {
    k <- kids(pd$parent[pd$id == fn_id])
    body <- k[nrow(k), ]
    if (body$token == "expr") {
      out[[length(out) + 1]] <- data.frame(
        file = file,
        line1 = body$line1,
        col1 = body$col1,
        line2 = body$line2,
        col2 = body$col2,
        kind = "function",
        text = substr(pd$text[pd$id == pd$parent[pd$id == fn_id]], 1, 80)
      )
    }
  }

  calls <- pd[
    pd$token == "SYMBOL_FUNCTION_CALL" &
      pd$text %in%
        c("case_when", "if_else", "ifelse", "fifelse", "fcase", "filter"),
  ]
  for (i in seq_len(nrow(calls))) {
    fn <- calls$text[i]
    fn_expr <- calls$parent[i]
    call_id <- pd$parent[pd$id == fn_expr]
    args <- positional_args(call_id)
    if (length(args) == 0) {
      next
    }
    piped <- is_piped(call_id)

    if (fn == "case_when") {
      for (a in args) {
        ak <- kids(a$id)
        if ("'~'" %in% ak$token) add(ak[ak$token == "expr", ][1, ], fn)
      }
    } else if (fn %in% c("if_else", "ifelse", "fifelse")) {
      if (!piped) add(args[[1]], fn)
    } else if (fn == "fcase") {
      for (j in seq(1, length(args), by = 2)) {
        add(args[[j]], fn)
      }
    } else if (fn == "filter") {
      if (!piped) {
        args <- args[-1]
      }
      for (a in args) {
        add(a, fn)
      }
    }
  }

  do.call(rbind, out)
}

# Insert `.cov_hit(id, ` / `)` (or `{.cov_reach(id); ` / `}` for function
# bodies) around every target and return the modified source lines.
instrument_lines <- function(lines, targets) {
  open_txt <- ifelse(
    targets$kind == "function",
    sprintf("{.cov_reach(%dL); ", targets$id),
    sprintf(".cov_hit(%dL, ", targets$id)
  )
  close_txt <- ifelse(targets$kind == "function", "}", ")")
  span <- (targets$line2 - targets$line1) * 1e4 + (targets$col2 - targets$col1)

  # gap g on a line means "after character g"
  ins <- rbind(
    data.frame(
      line = targets$line1,
      gap = targets$col1 - 1,
      txt = open_txt,
      type = 0,
      span = span
    ),
    data.frame(
      line = targets$line2,
      gap = targets$col2,
      txt = close_txt,
      type = 1,
      span = span
    )
  )
  # Insertions are applied back to front. At the same position, whatever is
  # applied last ends up first in the text. We want: closings before openings,
  # inner closings before outer closings, outer openings before inner openings.
  ins <- ins[
    order(
      -ins$line,
      -ins$gap,
      ins$type,
      ifelse(ins$type == 0, ins$span, -ins$span)
    ),
  ]
  for (i in seq_len(nrow(ins))) {
    l <- lines[ins$line[i]]
    g <- ins$gap[i]
    lines[ins$line[i]] <- paste0(
      substr(l, 1, g),
      ins$txt[i],
      substr(l, g + 1, nchar(l))
    )
  }
  lines
}

# Load the package with pkgload, instrument the parser files in its namespace
# and return the namespace. The namespace is locked, so the coverage helpers
# live in an environment attached to the search path (which namespace code
# reaches through the global env) and existing bindings are unlocked before
# they are replaced.
load_instrumented <- function(pkg_dir = ".") {
  pkgload::load_all(pkg_dir, quiet = TRUE, export_all = FALSE)
  ns <- asNamespace("nflfastR")

  targets <- do.call(
    rbind,
    lapply(file.path(pkg_dir, parser_files), find_targets)
  )
  targets$file <- sub(
    paste0("^", normalizePath(pkg_dir), "/"),
    "",
    normalizePath(targets$file)
  )
  targets$id <- seq_len(nrow(targets))

  cov <- new.env()
  n <- nrow(targets)
  cov$targets <- targets
  cov$reset <- function() {
    cov$reach <- logical(n)
    cov$t <- logical(n)
    cov$f <- logical(n)
  }
  cov$reset()
  cov$.cov_reach <- function(id) {
    cov$reach[id] <- TRUE
    invisible()
  }
  cov$.cov_hit <- function(id, x) {
    cov$reach[id] <- TRUE
    if (is.logical(x) && length(x)) {
      if (!cov$t[id] && any(x, na.rm = TRUE)) {
        cov$t[id] <- TRUE
      }
      if (!cov$f[id] && any(!x, na.rm = TRUE)) cov$f[id] <- TRUE
    }
    x
  }
  # attach() copies the environment, so attach only the helpers plus a
  # reference to the state env
  if ("nflfastR_cov" %in% search()) {
    detach("nflfastR_cov", character.only = TRUE)
  }
  attach(
    list(.cov_hit = cov$.cov_hit, .cov_reach = cov$.cov_reach, cov = cov),
    name = "nflfastR_cov",
    warn.conflicts = FALSE
  )

  replaced <- character()
  for (f in unique(targets$file)) {
    lines <- readLines(file.path(pkg_dir, f), warn = FALSE)
    new_lines <- instrument_lines(lines, targets[targets$file == f, ])
    exprs <- parse(text = new_lines, keep.source = FALSE)
    for (e in exprs) {
      # only re-evaluate top level assignments of existing objects
      if (!is.call(e) || !as.character(e[[1]]) %in% c("<-", "=")) {
        next
      }
      name <- as.character(e[[2]])
      if (!exists(name, envir = ns, inherits = FALSE)) {
        next
      }
      unlockBinding(name, ns)
      eval(e, ns)
      lockBinding(name, ns)
      replaced <- c(replaced, name)
    }
  }
  cov$replaced <- replaced
  ns
}

# Hits of the current game as a vector of item names like "123:t".
# Items: "<id>:r" (reached), "<id>:t" (TRUE at least once),
# "<id>:f" (FALSE at least once).
current_hits <- function() {
  cov <- cov_state()
  c(
    paste0(which(cov$reach), ":r"),
    paste0(which(cov$t), ":t"),
    paste0(which(cov$f), ":f")
  )
}

# The coverage state environment (targets, hits, reset function)
cov_state <- function() as.environment("nflfastR_cov")$cov
