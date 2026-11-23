# The expected points model
#
# The model gives the probabilities of the seven ways the next score of the
# half can go (a touchdown, field goal or safety for either team, or no score)
# for a game state. It is a multinomial logit model whose terms are smooth
# curves and surfaces (cubic splines) of the yard line, the distance and the
# clock, by down, era and timeouts. Its coefficients are `ep_model` in
# R/sysdata.rda, built in data-raw/ep_model.R.
#
# A spline is a smooth function of the game state: EP doesn't move in steps
# when the ball moves a yard, the distance changes by a yard or the clock runs,
# as it did with the trees of the previous model. And the fit has one solution,
# so a refit with the same data gives the same model and one with new data
# only changes it as much as the data do.
#
# EP is the value of a state for an average team. Good teams reach the red
# zone more often, and bad teams face more 3rd and longs and are backed up more
# often, so the next score of the plays in a state also reflects who gets
# there. The model is fit to plays that are weighted so that in every state the
# strength of the teams (from the pregame lines) is that of all games. It
# doesn't need team strength to predict.

# The columns of the game state that the EP model needs. Its eras are the
# seasons through 2005 (`era0` and `era1` together), 2006 to 2013 (`era2`),
# 2014 to 2017 (`era3`), 2018 to 2021 and the seasons since 2022 (`era5`). 2018
# to 2021 and a retractable roof are the reference, so `era4` (which includes
# the seasons since 2022) and `retractable` aren't needed.
ep_model_features <- c(
  "half_seconds_remaining",
  "yardline_100",
  "home",
  "dome",
  "outdoors",
  "ydstogo",
  "era0",
  "era1",
  "era2",
  "era3",
  "era5",
  "down1",
  "down2",
  "down3",
  "down4",
  "posteam_timeouts_remaining",
  "defteam_timeouts_remaining"
)

# A cubic B-spline basis with `n` equal intervals on [lo, hi] (n + 3
# functions). For every x: the index of its first nonzero function (`first`)
# and the values of its 4 nonzero functions (`val`, one column each). x outside
# of [lo, hi] gets the value at the nearest end. With `fade`, the functions
# that aren't 0 at hi are left out, so the curve goes smoothly to 0 at hi and
# is 0 above it.
ep_spline_basis <- function(x, lo, hi, n, fade = FALSE) {
  s <- (pmin(pmax(x, lo), hi) - lo) / (hi - lo) * n
  j <- pmin(floor(s), n - 1)
  u <- s - j
  u2 <- u * u
  u3 <- u2 * u
  v <- 1 - u
  val <- cbind(
    v * v * v,
    3 * u3 - 6 * u2 + 4,
    -3 * u3 + 3 * u2 + 3 * u + 1,
    u3
  ) /
    6
  if (fade) {
    val[j + col(val) > n] <- 0
  }
  list(first = j + 1, val = val, size = n + 3)
}

# The scales of the curves. EP changes fastest near the goal lines, on short
# distances and at the end of a half, so curves on a log scale have more detail
# there. States outside of the range of the plays get the value of the nearest
# state inside of it: a yard line of 0 (a touchdown in the xYAC variables), 0
# yards to go, a clock below 0 (time taken off for a play), and distances over
# 40 yards.
ep_spline_scales <- list(
  yardline_log = list(
    var = "yardline_100",
    f = function(x) log((x + 2) / (102 - x)),
    lo = 1,
    hi = 99
  ),
  yardline = list(var = "yardline_100", f = identity, lo = 1, hi = 99),
  ydstogo_log = list(var = "ydstogo", f = log, lo = 1, hi = 40),
  clock_log = list(
    var = "half_seconds_remaining",
    f = function(x) log1p(x / 10),
    lo = 0,
    hi = 1800
  ),
  # the last 5 minutes of a half
  clock_late = list(
    var = "half_seconds_remaining",
    f = identity,
    lo = 0,
    hi = 300
  )
)

# The curves the terms are made of: a scale and the number of intervals
ep_spline_margins <- list(
  yl_22 = list(scale = "yardline_log", n = 22),
  yl_10 = list(scale = "yardline_log", n = 10),
  yl_8 = list(scale = "yardline_log", n = 8),
  yl_5 = list(scale = "yardline_log", n = 5),
  ytg_12 = list(scale = "ydstogo_log", n = 12),
  ytg_5 = list(scale = "ydstogo_log", n = 5),
  clock_16 = list(scale = "clock_log", n = 16),
  clock_10 = list(scale = "clock_log", n = 10),
  clock_5 = list(scale = "clock_log", n = 5),
  yards_20 = list(scale = "yardline", n = 20),
  yards_12 = list(scale = "yardline", n = 12),
  yards_10 = list(scale = "yardline", n = 10),
  yards_8 = list(scale = "yardline", n = 8),
  yards_6 = list(scale = "yardline", n = 6),
  late_10 = list(scale = "clock_late", n = 10, fade = TRUE),
  late_6 = list(scale = "clock_late", n = 6, fade = TRUE)
)

# The levels a term can have its own copy of: one copy per down; per era or
# per era on 2nd to 4th down, relative to a reference that has none (level 0:
# 2018 to 2021); or only on 4th down. The timeouts are steps: a team with 2
# timeouts left has the term of the first used timeout (`posteam_used_1`), one
# with 1 left also that of the second, one with none all three. `both_used_3`
# is the state where neither team has a timeout left.
ep_spline_levels <- function(features) {
  down <- as.vector(features[, c("down1", "down2", "down3", "down4")] %*% 1:4)
  era <- as.vector(
    cbind(
      features[, "era0"] + features[, "era1"],
      features[, "era2"],
      features[, "era3"],
      features[, "era5"]
    ) %*%
      1:4
  )
  used <- function(x, k) as.numeric(pmin(pmax(x, 0), 3) <= 3 - k)
  pto <- features[, "posteam_timeouts_remaining"]
  dto <- features[, "defteam_timeouts_remaining"]
  list(
    all = rep(1, nrow(features)),
    down = down,
    era = era,
    down4 = as.numeric(down == 4),
    era_down4 = era * (down == 4),
    era_down = ifelse(era > 0 & down > 1, (era - 1) * 3 + down - 1, 0),
    posteam_used_1 = used(pto, 1),
    posteam_used_2 = used(pto, 2),
    posteam_used_3 = used(pto, 3),
    defteam_used_1 = used(dto, 1),
    defteam_used_2 = used(dto, 2),
    defteam_used_3 = used(dto, 3),
    both_used_3 = used(pto, 3) * used(dto, 3)
  )
}
ep_spline_level_count <- c(
  all = 1,
  down = 4,
  era = 4,
  down4 = 1,
  era_down4 = 4,
  era_down = 12,
  posteam_used_1 = 1,
  posteam_used_2 = 1,
  posteam_used_3 = 1,
  defteam_used_1 = 1,
  defteam_used_2 = 1,
  defteam_used_3 = 1,
  both_used_3 = 1
)

# The terms of the model, in the order of their coefficients. A term is a
# curve (one margin) or a surface (the product of two margins, the first one
# varying slowest), with a copy per level, times a column of the state
# (`times`). The terms with the clock are the ones that change in a game, the
# others make the table of ep_model_tables(). The fit shrinks the terms with
# `ridge` towards 0 (see data-raw/ep_model.R): a timeout step with little data
# (3 timeouts left late in the half are rare) is then small instead of an
# extrapolation of the curves.
ep_spline_terms <- list(
  int = list(margins = character(), level = "all"),
  yl_by_down = list(margins = "yl_22", level = "down"),
  ytg_by_down = list(margins = "ytg_12", level = "down"),
  yl_ytg_by_down = list(margins = c("yl_8", "ytg_5"), level = "down"),
  clock_by_down = list(margins = "clock_16", level = "down"),
  clock_yl_by_down = list(margins = c("clock_10", "yl_10"), level = "down"),
  era_yards = list(margins = "yards_12", level = "era"),
  era_clock = list(margins = "clock_5", level = "era"),
  home_clock = list(margins = "clock_5", level = "all", times = "home"),
  dome = list(margins = character(), level = "all", times = "dome"),
  outdoors = list(margins = character(), level = "all", times = "outdoors"),
  posteam_used_1 = list(
    margins = c("clock_5", "yl_5"),
    level = "posteam_used_1",
    ridge = TRUE
  ),
  posteam_used_2 = list(
    margins = c("clock_5", "yl_5"),
    level = "posteam_used_2",
    ridge = TRUE
  ),
  posteam_used_3 = list(
    margins = c("clock_5", "yl_5"),
    level = "posteam_used_3",
    ridge = TRUE
  ),
  defteam_used_1 = list(
    margins = c("clock_5", "yl_5"),
    level = "defteam_used_1",
    ridge = TRUE
  ),
  defteam_used_2 = list(
    margins = c("clock_5", "yl_5"),
    level = "defteam_used_2",
    ridge = TRUE
  ),
  defteam_used_3 = list(
    margins = c("clock_5", "yl_5"),
    level = "defteam_used_3",
    ridge = TRUE
  ),
  clock_ytg_by_down = list(margins = c("clock_5", "ytg_5"), level = "down"),
  # 4th down
  yards_4th = list(margins = "yards_20", level = "down4"),
  era_yards_4th = list(margins = "yards_8", level = "era_down4"),
  yards_ytg_4th = list(margins = c("yards_10", "ytg_5"), level = "down4"),
  # the last 5 minutes of a half
  late_yards_by_down = list(margins = c("late_10", "yards_12"), level = "down"),
  late_posteam_used_1 = list(
    margins = c("late_6", "yards_6"),
    level = "posteam_used_1",
    ridge = TRUE
  ),
  late_posteam_used_2 = list(
    margins = c("late_6", "yards_6"),
    level = "posteam_used_2",
    ridge = TRUE
  ),
  late_posteam_used_3 = list(
    margins = c("late_6", "yards_6"),
    level = "posteam_used_3",
    ridge = TRUE
  ),
  late_defteam_used_1 = list(
    margins = c("late_6", "yards_6"),
    level = "defteam_used_1",
    ridge = TRUE
  ),
  late_defteam_used_2 = list(
    margins = c("late_6", "yards_6"),
    level = "defteam_used_2",
    ridge = TRUE
  ),
  late_defteam_used_3 = list(
    margins = c("late_6", "yards_6"),
    level = "defteam_used_3",
    ridge = TRUE
  ),
  # neither team can stop the clock. Not shrunk: the timeout steps add up on
  # the scale of the linear predictor, which gives too little EP when both
  # teams are out of timeouts, and there are enough plays to fit it.
  late_both_used_3 = list(
    margins = c("late_6", "yards_6"),
    level = "both_used_3"
  ),
  late_ytg_by_down = list(margins = c("late_6", "ytg_5"), level = "down"),
  era_down = list(margins = character(), level = "era_down")
)

# What the coefficients of a model mean: its terms, their curves and levels,
# and the ranges of the scales (without their functions). data-raw/ep_model.R
# stores it with the model, and ep_model_predict() checks that the code still
# has the layout the model was fit with.
ep_spline_layout <- function() {
  list(
    scales = lapply(ep_spline_scales, function(s) s[c("var", "lo", "hi")]),
    margins = ep_spline_margins,
    terms = ep_spline_terms,
    level_count = ep_spline_level_count
  )
}

# The basis of a margin for the states `features`. The state is moved into
# the range of the scale first, because the functions of the scales aren't
# defined everywhere (e.g. a clock below -10 s on the log scale).
ep_spline_margin <- function(features, margin) {
  m <- ep_spline_margins[[margin]]
  s <- ep_spline_scales[[m$scale]]
  ep_spline_basis(
    s$f(pmin(pmax(features[, s$var], s$lo), s$hi)),
    s$f(s$lo),
    s$f(s$hi),
    m$n,
    fade = isTRUE(m$fade)
  )
}

# The number of coefficients of a term (per outcome)
ep_spline_term_size <- function(term) {
  sizes <- vapply(term$margins, function(m) ep_spline_margins[[m]]$n + 3, 1)
  prod(sizes) * ep_spline_level_count[[term$level]]
}

# The nonzero entries of a term in the rows of the design for the states
# `features`: for every row, the indices of the term's coefficients (`idx`)
# and their factors (`val`), one column per entry. `levels` is
# ep_spline_levels(features).
ep_spline_entries <- function(features, term, levels) {
  level <- levels[[term$level]]
  idx <- matrix(1, nrow(features), 1)
  val <- matrix(1, nrow(features), 1)
  size <- 1
  for (m in term$margins) {
    b <- ep_spline_margin(features, m)
    # the margins after this one vary fastest
    pairs <- expand.grid(old = seq_len(ncol(idx)), new = 1:4)
    idx <- (idx[, pairs$old, drop = FALSE] - 1) *
      b$size +
      outer(b$first, 0:3, `+`)[, pairs$new, drop = FALSE]
    val <- val[, pairs$old, drop = FALSE] * b$val[, pairs$new, drop = FALSE]
    size <- size * b$size
  }
  if (!is.null(term$times)) {
    val <- val * features[, term$times]
  }
  # level 0 (the reference) has no copy
  val[level == 0, ] <- 0
  list(idx = idx + (pmax(level, 1) - 1) * size, val = val)
}

# The linear predictors (one column per outcome but the last, No_Score) of the
# states `features`, term by term. This is the reference; ep_model_predict()
# gets the same numbers faster from the tables of ep_model_tables().
ep_model_eta_exact <- function(features, coefficients) {
  levels <- ep_spline_levels(features)
  eta <- matrix(0, nrow(features), ncol(coefficients))
  offset <- 0
  for (term in ep_spline_terms) {
    e <- ep_spline_entries(features, term, levels)
    for (k in seq_len(ncol(e$idx))) {
      eta <- eta +
        e$val[, k] * coefficients[offset + e$idx[, k], , drop = FALSE]
    }
    offset <- offset + ep_spline_term_size(term)
  }
  eta
}

# A term has the clock if its first margin is on a clock scale
ep_spline_has_clock <- function(term) {
  length(term$margins) > 0 &&
    startsWith(ep_spline_margins[[term$margins[1]]]$scale, "clock")
}

# The tables that ep_model_predict() needs, from the coefficients of the model
# (one row per coefficient, one column per outcome but No_Score):
# - `static`: the sum of the terms without the clock for every era, down, yard
#   line (1 to 99) and distance (1 to 40), one row per state.
# - `times`: the coefficients of the columns that enter as they are (dome,
#   outdoors).
# - `clock`: for every term with the clock, its other factor multiplied out:
#   the value for level l, yard line or distance x and clock function a is in
#   row ((l - 1) * nx + x - 1) * na + a.
# The columns are separate vectors, which are faster to index than the rows of
# a matrix.
ep_model_tables <- function(coefficients) {
  sizes <- vapply(ep_spline_terms, ep_spline_term_size, 1)
  stopifnot(nrow(coefficients) == sum(sizes))
  offset <- stats::setNames(c(0, cumsum(sizes))[seq_along(sizes)], names(sizes))
  outcomes <- seq_len(ncol(coefficients))
  coefs <- function(term, rows) {
    coefficients[offset[[term]] + rows, , drop = FALSE]
  }

  # No term without the clock depends on both the era and the distance, so the
  # table is the sum of one without the era terms (on down x yard line x
  # distance) and one of the era terms (on era x down x yard line).
  grid_states <- function(grid) {
    states <- matrix(
      0,
      nrow(grid),
      length(ep_model_features),
      dimnames = list(NULL, ep_model_features)
    )
    states[, "yardline_100"] <- grid$yardline_100
    states[, "ydstogo"] <- grid$ydstogo
    states[cbind(
      seq_len(nrow(grid)),
      match(paste0("down", grid$down), ep_model_features)
    )] <- 1
    era <- which(grid$era > 0)
    era_columns <- c("era1", "era2", "era3", "era5")
    states[cbind(
      era,
      match(era_columns[grid$era[era]], ep_model_features)
    )] <- 1
    states
  }
  static_terms <- names(ep_spline_terms)[vapply(
    ep_spline_terms,
    function(term) !ep_spline_has_clock(term) && is.null(term$times),
    TRUE
  )]
  with_era <- vapply(
    ep_spline_terms[static_terms],
    function(term) grepl("era", term$level),
    TRUE
  )
  with_ytg <- vapply(
    ep_spline_terms[static_terms],
    function(term) any(startsWith(term$margins, "ytg")),
    TRUE
  )
  stopifnot(!any(with_era & with_ytg))
  sum_terms <- function(grid, terms) {
    states <- grid_states(grid)
    levels <- ep_spline_levels(states)
    out <- lapply(outcomes, function(j) numeric(nrow(grid)))
    for (name in terms) {
      e <- ep_spline_entries(states, ep_spline_terms[[name]], levels)
      for (j in outcomes) {
        cj <- coefficients[offset[[name]] + seq_len(sizes[[name]]), j]
        for (k in seq_len(ncol(e$idx))) {
          out[[j]] <- out[[j]] + e$val[, k] * cj[e$idx[, k]]
        }
      }
    }
    out
  }
  no_era <- sum_terms(
    expand.grid(ydstogo = 1:40, yardline_100 = 1:99, down = 1:4, era = 0),
    static_terms[!with_era]
  )
  era <- sum_terms(
    expand.grid(yardline_100 = 1:99, down = 1:4, era = 0:4, ydstogo = 1),
    static_terms[with_era]
  )
  grid <- expand.grid(
    ydstogo = 1:40,
    yardline_100 = 1:99,
    down = 1:4,
    era = 0:4
  )
  i_no_era <- grid$ydstogo + 40 * (grid$yardline_100 - 1 + 99 * (grid$down - 1))
  i_era <- grid$yardline_100 + 99 * (grid$down - 1 + 4 * grid$era)
  static <- lapply(outcomes, function(j) {
    no_era[[j]][i_no_era] + era[[j]][i_era]
  })

  times <- list()
  clock <- list()
  for (name in names(ep_spline_terms)) {
    term <- ep_spline_terms[[name]]
    if (!ep_spline_has_clock(term)) {
      if (!is.null(term$times)) {
        times[[term$times]] <- coefs(name, 1)
      }
      next
    }
    na <- ep_spline_margins[[term$margins[1]]]$n + 3
    # the other factor on its grid: one row per yard line or distance
    other <- if (length(term$margins) == 2) {
      m <- term$margins[2]
      var <- ep_spline_scales[[ep_spline_margins[[m]]$scale]]$var
      x <- if (var == "ydstogo") 1:40 else 1:99
      b <- ep_spline_margin(matrix(x, dimnames = list(NULL, var)), m)
      o <- matrix(0, length(x), b$size)
      for (k in 1:4) {
        o[cbind(seq_along(x), b$first + k - 1)] <- b$val[, k]
      }
      list(var = var, basis = o)
    } else {
      list(var = NULL, basis = matrix(1, 1, 1))
    }
    nb <- ncol(other$basis)
    levels_n <- ep_spline_level_count[[term$level]]
    tables <- lapply(outcomes, function(j) {
      unlist(lapply(seq_len(levels_n), function(l) {
        block <- coefs(name, (l - 1) * na * nb + seq_len(na * nb))[, j]
        # rows x, columns a, stored with a varying fastest
        as.vector(t(other$basis %*% matrix(block, nb, na)))
      }))
    })
    clock[[name]] <- list(
      term = term,
      var = other$var,
      nx = nrow(other$basis),
      na = na,
      tables = tables
    )
  }
  list(
    static = static,
    times = times,
    clock = clock
  )
}

# The tables of the coefficients of `model`, made once per session (they take
# a few tenths of a second)
ep_model_tables_cached <- function(model) {
  stopifnot(
    "The EP model was fit with other terms than `ep_spline_terms`" = identical(
      model$layout,
      ep_spline_layout()
    )
  )
  coefficients <- model$coefficients
  cached <- nflfastR_cache[["ep_model_tables"]]
  if (is.null(cached) || !identical(cached$coefficients, coefficients)) {
    cached <- list(
      coefficients = coefficients,
      tables = ep_model_tables(coefficients)
    )
    assign("ep_model_tables", cached, envir = nflfastR_cache)
  }
  cached$tables
}

# The probabilities of the next score (one row per play, one column per
# outcome) for a matrix with the columns `ep_model_features`. They are NA for
# plays with a missing feature or a down other than 1 to 4. Plays with a yard
# line or distance that isn't a whole number are computed term by term with
# ep_model_eta_exact().
ep_model_predict <- function(features, model = ep_model) {
  tables <- ep_model_tables_cached(model)
  probs <- matrix(
    NA_real_,
    nrow(features),
    length(model$classes),
    dimnames = list(NULL, model$classes)
  )
  ok <- stats::complete.cases(features) &
    rowSums(features[, c("down1", "down2", "down3", "down4"), drop = FALSE]) ==
      1
  yardline <- pmin(pmax(features[, "yardline_100"], 1), 99)
  ydstogo <- pmin(pmax(features[, "ydstogo"], 1), 40)
  whole <- ok & yardline == round(yardline) & ydstogo == round(ydstogo)
  if (any(ok & !whole)) {
    eta <- ep_model_eta_exact(
      features[ok & !whole, , drop = FALSE],
      model$coefficients
    )
    probs[ok & !whole, ] <- ep_model_softmax(lapply(
      seq_len(ncol(eta)),
      function(j) eta[, j]
    ))
  }
  if (!any(whole)) {
    return(probs)
  }
  f <- features[whole, , drop = FALSE]
  x <- list(yardline_100 = yardline[whole], ydstogo = ydstogo[whole])
  levels <- ep_spline_levels(f)
  key <- x$ydstogo +
    40 * (x$yardline_100 - 1 + 99 * (levels$down - 1 + 4 * levels$era))
  eta <- lapply(seq_along(tables$static), function(j) {
    e <- tables$static[[j]][key]
    for (column in names(tables$times)) {
      e <- e + f[, column] * tables$times[[column]][j]
    }
    e
  })
  bases <- list()
  for (ct in tables$clock) {
    margin <- ct$term$margins[1]
    if (is.null(bases[[margin]])) {
      bases[[margin]] <- ep_spline_margin(f, margin)
    }
    b <- bases[[margin]]
    level <- levels[[ct$term$level]]
    # only the rows where the term isn't 0
    rows <- level > 0
    if (isTRUE(ep_spline_margins[[margin]]$fade)) {
      rows <- rows & rowSums(b$val) > 0
    }
    rows <- if (all(rows)) NULL else which(rows)
    if (!is.null(rows) && !length(rows)) {
      next
    }
    pick <- function(v) if (is.null(rows)) v else v[rows]
    xv <- if (is.null(ct$var)) 1 else pick(x[[ct$var]])
    start <- ((pick(level) - 1) * ct$nx + xv - 1) * ct$na + pick(b$first)
    w <- if (is.null(ct$term$times)) 1 else pick(f[, ct$term$times])
    v <- lapply(1:4, function(k) pick(b$val[, k]) * w)
    for (j in seq_along(eta)) {
      tj <- ct$tables[[j]]
      add <- v[[1]] *
        tj[start] +
        v[[2]] * tj[start + 1] +
        v[[3]] * tj[start + 2] +
        v[[4]] * tj[start + 3]
      if (is.null(rows)) {
        eta[[j]] <- eta[[j]] + add
      } else {
        eta[[j]][rows] <- eta[[j]][rows] + add
      }
    }
  }
  probs[whole, ] <- ep_model_softmax(eta)
  probs
}

# The probabilities of the outcomes from the linear predictors (a list with a
# vector per outcome but the last, whose predictor is 0)
ep_model_softmax <- function(eta) {
  m <- pmax(do.call(pmax, eta), 0)
  e <- lapply(eta, function(x) exp(x - m))
  last <- exp(-m)
  total <- Reduce(`+`, e) + last
  unname(cbind(do.call(cbind, e), last) / total)
}
