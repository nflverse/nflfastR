# Builds the expected points model (ep) that lives in R/sysdata.rda (object
# `ep_model`). See R/helper_ep_model.R for what the model is and how it
# predicts.
#
# It is built in two steps:
#
# 1. The teacher: boosted trees (xgboost, multiclass) on the plays with a down
#    since 2001. This is the recipe of the previous model
#    (`fastrmodels::ep_model`) with these changes. The trees also get the yard
#    line and the distance separately for 2nd, 3rd and 4th down, and there are
#    800 of them instead of 525 (see teacher_matrix()). A play is only weighted
#    by how close the score is and no longer by the number of drives until the
#    next score. That weight depends on the outcome (a score on the same drive
#    got the highest weight), which made EP 0.08 points too high on average.
#    And the eras are different: 2001 is in the era of 2002 to 2005 instead of
#    an era with 1999 and 2000, which the model is no longer built with, and
#    the seasons since 2022 are an era of their own instead of being in one
#    era with 2018 to 2021 (fewer touchdowns and more field goals are the next
#    score since 2022). And the plays are reweighted so that EP is the value
#    of a state for an average team, see "team strength" below.
# 2. The model: a multinomial logit model whose terms are cubic splines of the
#    yard line, the distance and the clock, by down, era and timeouts (see
#    R/helper_ep_model.R), fit to the probabilities of the teacher instead of
#    the outcomes. The trees are accurate but jagged: EP changes in steps of
#    uneven size and often in the wrong direction when the ball moves a yard,
#    the distance changes by a yard or the clock runs. The splines are smooth
#    and learn the same surface without the steps. The fit minimizes the
#    cross entropy to the teacher's probabilities plus a penalty on the
#    curvature of every curve (see ep_fit()). It is convex, so it has one
#    solution: a refit with the same data gives the same model, and a refit
#    with other random draws of the teacher moves EP by about 0.01 per play.
#
# Only the coefficients are stored (3,825 per outcome, 176 KB). Predictions
# are table lookups (see ep_model_predict()), so they don't need xgboost.
#
# Model choices were made with season grouped cross validation (7 folds), see
# the PR that introduced this file for the evaluation. What was tried and
# dropped: monotone constraints (the class probabilities are not monotone in
# field position, only EP is), a goal to go feature, an era for the seasons
# since 2024, kneel downs as training rows, and team strength as features of the
# teacher, with the model fit to the teacher averaged over the matchups of real
# games. That gives the same average team, but the trees then fit the game state
# less well: they overrated 3rd and long and backed up states by another 0.04
# and 0.10 points out of sample. An average of three neural networks fit to the
# same target fits about as well (log loss 1.30283 out of fold, this model
# 1.30295), but it is less smooth in the clock, moves EP by 0.026 per play when
# refit with other seeds and predicts three times slower. A lighter penalty
# (lambda 1e-6) fits no better and has more steps in the wrong direction, a
# heavier one (1e-4) fits worse, and so does a fit to the outcomes instead of
# the teacher (1.30351). Finer curves of the distance fit slightly better but
# put bumps into EP on 2nd and 10. Terms for the era by home field, by distance
# and by the end of the half didn't help, and neither did the average of three
# teachers as the target.
#
# Run from the package root. Takes about 10 minutes and 6 GB of memory.

library(data.table)
library(xgboost)
source("R/helper_add_nflscrapr_mutations.R")
source("R/helper_ep_model.R")
# ep_model_predict() keeps its tables there (R/utils.R in the package)
nflfastR_cache <- new.env(parent = emptyenv())
source("data-raw/save_sysdata.R")
set.seed(2013)

seasons <- 2001:nflreadr::most_recent_season()

# the outcomes in the order the package expects (see get_preds_stacked())
ep_classes <- c(
  "Touchdown",
  "Opp_Touchdown",
  "Field_Goal",
  "Opp_Field_Goal",
  "Safety",
  "Opp_Safety",
  "No_Score"
)

# the teacher is trained on these plays (all plays with a down except kneel
# downs, as the previous model). A team that kneels doesn't try to score, so
# with kneel downs EP is too low for the teams that run a play in that spot
teacher_play_types <- c(
  "field_goal",
  "no_play",
  "pass",
  "punt",
  "run",
  "qb_spike"
)
teacher_rounds <- 800
teacher_params <- list(
  booster = "gbtree",
  objective = "multi:softprob",
  eval_metric = "mlogloss",
  num_class = length(ep_classes),
  eta = 0.025,
  gamma = 1,
  subsample = 0.8,
  colsample_bytree = 0.8,
  max_depth = 5,
  min_child_weight = 1,
  tree_method = "hist",
  nthread = parallel::detectCores()
)

model_params <- list(
  # the weights of the curvature penalty and of the ridge penalty of the
  # timeout steps (relative to lambda), see ep_penalty()
  lambda = 1e-5,
  ridge = 1,
  # synthetic states per play, see synthetic_states()
  extra = 0.25,
  seed = 2013L,
  # forked worker processes of the fit, see ep_fit()
  cores = 4L
)

# ---- team strength ----------------------------------------------------------
# Good teams reach the red zone more often than bad teams, and bad teams face
# more 3rd and longs (they didn't convert on 1st and 2nd down) and are backed
# up more often. The next score of the plays in a state depends on the state
# and on how good the teams are that got there, so a model of the state alone
# credits the state with the quality of its teams: EP in the red zone is too
# high and EP on 3rd and long and backed up is too low for an average team.
#
# The strength of the teams comes from the pregame lines: `quality`, the
# spread of the team with the ball net of the home field advantage of the
# season (how many points it is better than its opponent), and `total`, the
# total line of the game minus the season average (how many points both teams
# were expected to score). The plays are weighted so that in every state the
# strength of the teams is distributed as in all games: quality as that of
# both teams of all games, and total as that of the games with the same roof
# (dome games are expected to score more, and the roof is a feature of the
# model). The weight is the density of the strength in all games divided by
# its density in the state, both normal, with the mean in the state from a
# small regression on the state. Weights are close to 1, because the mean
# quality of a state is at most about a point from 0. The package needs no
# team strength to predict.

# The lines of the games: the home field advantage of the season (the average
# spread of the home teams), the quality of the home team and the total
# relative to the season average. `games` has the columns season, location,
# spread_line (the expected margin of the home team) and total_line.
game_lines <- function(games) {
  games <- copy(games)
  games[, home_edge := mean(spread_line[location == "Home"]), by = season]
  games[,
    home_quality := spread_line - fifelse(location == "Neutral", 0, home_edge)
  ]
  games[, total := total_line - mean(total_line), by = season]
  games
}

# Weights of the plays (game states `features`, team strength `strength` with
# the columns quality and total, and the roof of the stadium `roof`) that give
# every state the strength of the teams of the games in `games` (from
# game_lines(), with the column model_roof). Trimmed to 0.2 to 5.
strength_weights <- function(features, strength, roof, games) {
  x <- teacher_matrix(features)
  state_mean <- function(y) {
    params <- list(
      objective = "reg:squarederror",
      eta = 0.1,
      max_depth = 4,
      min_child_weight = 200,
      subsample = 0.8,
      nthread = teacher_params$nthread
    )
    m <- xgb.train(
      params,
      xgb.DMatrix(x, label = y),
      nrounds = 150,
      verbose = 0
    )
    predict(m, x)
  }
  log_ratio <- function(y, target_mean, target_sd) {
    state <- state_mean(y)
    stats::dnorm(y, target_mean, target_sd, log = TRUE) -
      stats::dnorm(y, state, stats::sd(y - state), log = TRUE)
  }
  game_quality <- c(games$home_quality, -games$home_quality)
  roof_total <- games[, .(total = mean(total)), by = model_roof]
  w <- exp(
    log_ratio(strength[, "quality"], 0, stats::sd(game_quality)) +
      log_ratio(
        strength[, "total"],
        roof_total$total[match(roof, roof_total$model_roof)],
        stats::sd(games$total)
      )
  )
  pmin(pmax(w, 0.2), 5)
}

# ---- teacher ----------------------------------------------------------------
# All functions take the game states as a matrix with the columns
# `ep_model_features`.

# The columns of the teacher: 2001 is in the era of 2002 to 2005 (its other
# eras are those of `ep_model_features`), and the yard line and the distance
# for each of 2nd, 3rd and 4th down. States far from the average play, like
# 3rd and long or 3rd and 4th down deep in the own territory, are reached by a
# small share of the plays. Small trees rarely give them leaves of their own,
# so the trees approach their outcomes slowly: with only the state columns and
# 525 trees, EP of 3rd and 4th down in the own 1 to 10 was 0.18 and 0.24
# points too high even on the training plays. With the yard line and the
# distance by down one split is enough to separate them. In season grouped
# cross validation this halves the error of these states and improves the log
# loss. Deeper trees (depth 7) fix them about as well but fit the rest worse,
# and so do more trees without these columns. `gamma = 0` changes nothing.
teacher_matrix <- function(features) {
  by_down <- function(column) {
    out <- features[, column] * features[, c("down2", "down3", "down4")]
    colnames(out) <- paste0(column, "_", c("down2", "down3", "down4"))
    out
  }
  cbind(
    features[, setdiff(ep_model_features, c("era0", "era1"))],
    era1 = features[, "era0"] + features[, "era1"],
    by_down("yardline_100"),
    by_down("ydstogo")
  )
}

# `label` is the index of the outcome in `ep_classes`, starting at 0
fit_teacher <- function(features, label, weight) {
  xgb.train(
    params = teacher_params,
    data = xgb.DMatrix(
      teacher_matrix(features),
      label = label,
      weight = weight
    ),
    nrounds = teacher_rounds,
    verbose = 0
  )
}

teacher_probs <- function(teacher, features) {
  p <- predict(teacher, teacher_matrix(features))
  # xgboost before version 2 returns a vector
  if (is.null(dim(p))) {
    p <- matrix(p, ncol = length(ep_classes), byrow = TRUE)
  }
  p
}

# ---- the model --------------------------------------------------------------
# The model is fit to every play with a down (kneel downs too, their states are
# valid) and synthetic states: plays whose down and distance and whose yard
# line are swapped with those of other plays, while clock, timeouts, home
# field, roof and era stay as they were. They give the model the teacher's
# values for unusual downs, distances and field positions, which the package
# also asks for (e.g. the states after every possible gain of a pass in the
# xYAC variables). Without them EP sometimes rises with the distance to go
# where there are few plays. Clock and timeouts are not swapped, because the
# teacher is not reliable for combinations that don't occur (e.g. no timeouts
# left early in a half).
synthetic_states <- function(states) {
  n <- nrow(states)
  states[, "yardline_100"] <- sample(states[, "yardline_100"])
  down_cols <- c("ydstogo", "down1", "down2", "down3", "down4")
  states[, down_cols] <- states[sample.int(n), down_cols]
  states[, "ydstogo"] <- pmin(states[, "ydstogo"], states[, "yardline_100"])
  states
}

# The design matrix (sparse, one row per state, one column per coefficient)
# in chunks of rows, so the entries of all rows never exist at once
ep_design <- function(features, chunk = 50000L) {
  sizes <- vapply(ep_spline_terms, ep_spline_term_size, 1)
  offset <- c(0, cumsum(sizes))[seq_along(sizes)]
  lapply(seq.int(1L, nrow(features), by = chunk), function(s) {
    f <- features[s:min(s + chunk - 1L, nrow(features)), , drop = FALSE]
    levels <- ep_spline_levels(f)
    parts <- Map(
      function(term, o) {
        e <- ep_spline_entries(f, term, levels)
        keep <- e$val != 0
        list(i = row(e$val)[keep], j = e$idx[keep] + o, x = e$val[keep])
      },
      ep_spline_terms,
      offset
    )
    Matrix::sparseMatrix(
      i = unlist(lapply(parts, `[[`, "i")),
      j = unlist(lapply(parts, `[[`, "j")),
      x = unlist(lapply(parts, `[[`, "x")),
      dims = c(nrow(f), sum(sizes))
    )
  })
}

# The penalty of the fit: lambda times the sum of the squared second
# differences of the coefficients of every curve (along each margin of a
# surface), per level. The terms with `ridge` (the timeout steps) also get
# lambda * ridge times the sum of their squared coefficients. Terms without a
# margin aren't penalized.
ep_penalty <- function(lambda, ridge) {
  blocks <- lapply(ep_spline_terms, function(term) {
    k <- vapply(term$margins, function(m) ep_spline_margins[[m]]$n + 3, 1)
    second <- function(n) crossprod(diff(diag(n), differences = 2))
    s <- switch(
      as.character(length(k)),
      "0" = matrix(0, 1, 1),
      "1" = second(k[1]),
      "2" = kronecker(second(k[1]), diag(k[2])) +
        kronecker(diag(k[1]), second(k[2]))
    )
    if (isTRUE(term$ridge)) {
      s <- s + ridge * diag(nrow(s))
    }
    Matrix::Matrix(
      lambda * kronecker(diag(ep_spline_level_count[[term$level]]), s),
      sparse = TRUE
    )
  })
  Matrix::bdiag(blocks)
}

# Fits the coefficients: minimizes the cross entropy between the targets and
# the model's probabilities (summed over the states and divided by their
# number) plus half the penalty b'(S + eps I)b of every outcome's
# coefficients b. `target` has a row per state and a column per outcome
# (No_Score last); a row can be probabilities or a weighted outcome (a single
# nonzero entry). `X` is the design in chunks of rows (ep_design()).
#
# The problem is convex but badly conditioned: neighboring functions of a
# curve overlap, the levels of the terms overlap (e.g. a curve by down and a
# surface by down), and the penalty is light. Plain L-BFGS takes thousands of
# iterations. Here it works on z = R b, where R'R is the outcome's block of the
# Hessian at the probabilities `hess_probs` (from every 4th chunk of rows),
# and converges in about 100. Forked workers compute the loss and gradient of
# the chunks.
ep_fit <- function(X, target, S, hess_probs = target, cores = 4L, eps = 1e-7) {
  rows <- vapply(X, nrow, integer(1))
  n <- sum(rows)
  p <- ncol(X[[1]])
  C <- ncol(target) - 1L
  ends <- cumsum(rows)
  chunk_rows <- lapply(seq_along(X), function(k) {
    (ends[k] - rows[k] + 1):ends[k]
  })
  Y <- lapply(chunk_rows, function(r) target[r, , drop = FALSE])
  b0 <- matrix(0, p, C)
  b0[1, ] <- log(colMeans(target[, 1:C]) / mean(target[, C + 1]))
  pen <- S + eps * Matrix::Diagonal(p)

  # the preconditioner
  hess_chunks <- seq(1L, length(X), by = max(1L, round(n / 3e5)))
  n_hess <- sum(rows[hess_chunks])
  R <- lapply(seq_len(C), function(cl) {
    h <- as.matrix(pen) * n_hess
    for (k in hess_chunks) {
      q <- hess_probs[chunk_rows[[k]], cl]
      w <- rowSums(Y[[k]]) * q * (1 - q)
      h <- h +
        as.matrix(Matrix::crossprod(X[[k]], Matrix::Diagonal(x = w) %*% X[[k]]))
      gc(verbose = FALSE)
    }
    chol(h / n_hess)
  })
  to_b <- function(z) {
    z <- matrix(z, p, C)
    vapply(seq_len(C), function(cl) backsolve(R[[cl]], z[, cl]), numeric(p))
  }

  part <- function(chunks, b) {
    loss <- 0
    g <- matrix(0, p, C)
    for (k in chunks) {
      eta <- as.matrix(X[[k]] %*% b)
      m <- pmax(do.call(pmax, as.data.frame(eta)), 0)
      e <- exp(eta - m)
      e0 <- exp(-m)
      log_total <- log(rowSums(e) + e0)
      y <- Y[[k]]
      loss <- loss -
        sum(y[, 1:C] * (eta - m - log_total)) -
        sum(y[, C + 1] * (-m - log_total))
      g <- g +
        as.matrix(Matrix::crossprod(
          X[[k]],
          e / (rowSums(e) + e0) * rowSums(y) - y[, 1:C]
        ))
    }
    list(loss = loss, g = g)
  }
  groups <- split(seq_along(X), rep_len(seq_len(cores), length(X)))
  last <- new.env()
  evaluate <- function(z) {
    if (!identical(z, last$z)) {
      b <- to_b(z)
      parts <- parallel::mclapply(groups, part, b = b, mc.cores = cores)
      pb <- as.matrix(pen %*% b)
      gb <- Reduce(`+`, lapply(parts, `[[`, "g")) / n + pb
      last$z <- z
      last$f <- Reduce(`+`, lapply(parts, `[[`, "loss")) / n + 0.5 * sum(b * pb)
      last$gb <- gb
      last$g <- as.vector(vapply(
        seq_len(C),
        function(cl) backsolve(R[[cl]], gb[, cl], transpose = TRUE),
        numeric(p)
      ))
    }
    last
  }
  z0 <- as.vector(vapply(
    seq_len(C),
    function(cl) as.vector(R[[cl]] %*% b0[, cl]),
    numeric(p)
  ))
  opt <- stats::optim(
    z0,
    function(z) evaluate(z)$f,
    function(z) evaluate(z)$g,
    method = "L-BFGS-B",
    control = list(maxit = 2000, factr = 1e3, lmm = 20)
  )
  list(
    coefficients = to_b(opt$par),
    value = opt$value,
    convergence = opt$convergence,
    iterations = opt$counts[[1]],
    max_gradient = max(abs(evaluate(opt$par)$gb))
  )
}

# ---- data -------------------------------------------------------------------
# one season at a time: all columns of all seasons at once need several GB
pbp_cols <- c(
  "game_id",
  "season",
  "game_half",
  "posteam",
  "defteam",
  "home_team",
  "location",
  "roof",
  "down",
  "ydstogo",
  "yardline_100",
  "half_seconds_remaining",
  "posteam_timeouts_remaining",
  "defteam_timeouts_remaining",
  "score_differential",
  "play_type",
  "td_team",
  "field_goal_result",
  "safety",
  "posteam_score",
  "posteam_score_post",
  "total_home_score",
  "total_away_score",
  "home_score",
  "away_score",
  "result",
  "spread_line",
  "total_line"
)
pbp <- rbindlist(lapply(seasons, function(s) {
  setDT(nflreadr::load_pbp(s))[, c(pbp_cols, "desc"), with = FALSE]
}))

# The released data miss about 400 kneel downs (worded "Took a knee.",
# "dropped to one knee" or "kneels," until 2016, #603) and have them as runs.
# Find them as the package does now, so they aren't training plays of the
# teacher, until the released data are rebuilt. Two point tries have no down,
# so they aren't training plays anyway.
kneels <- which(pbp$play_type %chin% "run" & is_qb_kneel(pbp$desc))
cat("kneel downs among the runs of the released data:", length(kneels), "\n")
pbp[kneels, play_type := "qb_kneel"]
pbp[, desc := NULL]

# Only finished games whose running score ends at the final score. The others
# credit scores to the wrong team (see data-raw/wp_vegas_model.R: the
# Jacksonville home games of 2001 and 2002 and 2011_13_DET_NO, until the
# released data are rebuilt), which would give wrong labels.
games <- pbp[,
  .(
    ok = !is.na(result[1]) &
      max(total_home_score, na.rm = TRUE) == home_score[1] &
      max(total_away_score, na.rm = TRUE) == away_score[1]
  ),
  by = game_id
]
cat("games:", nrow(games), " dropped:", sum(!games$ok), "\n")
pbp <- pbp[game_id %chin% games$game_id[games$ok]]

# The label of a play is the next score in its half (or overtime), the play
# itself included, from the point of view of the team with the ball. Extra
# points and two point conversions are not scores here.
pbp[, row := .I]
pbp[,
  score := fcase(
    !is.na(td_team),
    "td",
    field_goal_result %chin% "made",
    "fg",
    safety %in% 1,
    "safety"
  )
]
pbp[,
  score_team := fcase(
    score == "td",
    td_team,
    score == "fg",
    posteam,
    # a few safeties are scored by the team with the ball (e.g. on a punt)
    score == "safety" & (posteam_score_post - posteam_score) %in% 2,
    posteam,
    score == "safety",
    defteam
  )
]
pbp[,
  next_score := nafill(fifelse(is.na(score), NA_integer_, row), type = "nocb"),
  by = .(game_id, game_half)
]
pbp[, `:=`(
  next_type = score[next_score],
  next_own = score_team[next_score] == posteam
)]
pbp[,
  label := fcase(
    is.na(next_score),
    "No_Score",
    next_type == "td" & next_own,
    "Touchdown",
    next_type == "td",
    "Opp_Touchdown",
    next_type == "fg" & next_own,
    "Field_Goal",
    next_type == "fg",
    "Opp_Field_Goal",
    next_type == "safety" & next_own,
    "Safety",
    next_type == "safety",
    "Opp_Safety"
  )
]

model_data <- pbp[
  !is.na(down) &
    !is.na(posteam) &
    !is.na(label) &
    !is.na(yardline_100) &
    !is.na(half_seconds_remaining) &
    !is.na(posteam_timeouts_remaining) &
    !is.na(defteam_timeouts_remaining) &
    !is.na(score_differential)
]
rm(pbp)
invisible(make_model_mutations(model_data))
# the roof of the model, for the strength weights (see strength_weights())
model_data[, model_roof := model_roof_type(roof)]
# as in the predictions of the package (see get_preds_stacked())
model_data[location %chin% "Neutral", home := 0]
model_data[, label := match(label, ep_classes) - 1L]
features <- as.matrix(model_data[, ..ep_model_features])
stopifnot(!anyNA(features))

lines_by_game <- game_lines(unique(model_data[,
  .(game_id, season, location, spread_line, total_line, model_roof)
]))
stopifnot(
  !anyDuplicated(lines_by_game$game_id),
  !anyNA(lines_by_game$home_quality)
)
model_data <- lines_by_game[, .(game_id, home_quality, total)][
  model_data,
  on = "game_id"
]
model_data[, quality := fifelse(posteam == home_team, 1, -1) * home_quality]
strength <- as.matrix(model_data[, .(quality, total)])

# ---- fit --------------------------------------------------------------------
plays <- model_data$play_type %chin% teacher_play_types
# a play is weighted by how close the score is (1 in a tied game, falling
# linearly to 0 at the largest lead) and by the strength of its teams
closeness <- 1 -
  abs(model_data$score_differential[plays]) /
    max(abs(model_data$score_differential[plays]))
strength_w <- strength_weights(
  features[plays, ],
  strength[plays, ],
  model_data$model_roof[plays],
  lines_by_game
)
cat(
  "strength weights: 1% and 99% quantiles",
  round(stats::quantile(strength_w, c(0.01, 0.99)), 2),
  " trimmed:",
  round(100 * mean(strength_w %in% c(0.2, 5)), 2),
  "%\n"
)
teacher <- fit_teacher(
  features[plays, ],
  model_data$label[plays],
  closeness * strength_w / mean(strength_w)
)

# the states of the fit: all plays and synthetic states, with the teacher's
# probabilities
set.seed(model_params$seed)
extra <- sample.int(nrow(features), round(model_params$extra * nrow(features)))
states <- rbind(features, synthetic_states(features[extra, , drop = FALSE]))
target <- teacher_probs(teacher, states)
design <- ep_design(states)
rm(states)
gc(verbose = FALSE)
fit <- ep_fit(
  design,
  target,
  ep_penalty(model_params$lambda, model_params$ridge),
  cores = model_params$cores
)
rm(design, target)
gc(verbose = FALSE)
cat(
  "fit: iterations",
  fit$iterations,
  " largest gradient",
  signif(fit$max_gradient, 2),
  "\n"
)
stopifnot(fit$convergence == 0, fit$max_gradient < 1e-6)
ep_model <- list(coefficients = fit$coefficients, classes = ep_classes)

# ---- checks -----------------------------------------------------------------
log_loss <- function(probs, label) {
  -mean(log(probs[cbind(seq_along(label), label + 1L)]))
}
points <- c(7, -7, 3, -3, 2, -2, 0)
p_teacher <- teacher_probs(teacher, features[plays, ])
p_model <- ep_model_predict(features[plays, ], ep_model)
actual <- points[model_data$label[plays] + 1L]
cat(
  "\nlog loss on the training plays (in sample): teacher",
  round(log_loss(p_teacher, model_data$label[plays]), 4),
  " model",
  round(log_loss(p_model, model_data$label[plays]), 4),
  "\nmodel and teacher, mean absolute difference of the probabilities:",
  round(mean(abs(p_model - p_teacher)), 4),
  "\nexpected points (7 / 3 / 2) minus the points of the next score: teacher",
  round(mean(p_teacher %*% points - actual), 3),
  " model",
  round(mean(p_model %*% points - actual), 3),
  "\n"
)
# the model follows the teacher
stopifnot(mean(abs(p_model - p_teacher)) < 0.01)

# EP may not rise when the ball is a yard further from the goal line or when
# there is one more yard to go. Checked on the states that occur: 1st & 10 and
# every distance on the other downs. Steps of a few hundredths of a point in
# the wrong direction remain where EP is flat (e.g. a long 4th down).
grid <- CJ(
  season = c(2005L, 2012L, 2025L),
  half_seconds_remaining = c(1500, 900, 300, 100),
  down = 1:4,
  ydstogo = 1:20,
  yardline_100 = 1:99
)[ydstogo <= yardline_100 & (down > 1 | ydstogo == 10)]
grid[, `:=`(
  posteam = "A",
  home_team = "B",
  roof = "outdoors",
  posteam_timeouts_remaining = 3,
  defteam_timeouts_remaining = 3
)]
invisible(make_model_mutations(grid))
grid_probs <- ep_model_predict(as.matrix(grid[, ..ep_model_features]), ep_model)
grid[, ep := as.vector(grid_probs %*% c(7, -7, 3, -3, 2, -2, 0))]
setkey(grid, season, half_seconds_remaining, down, ydstogo, yardline_100)
rise_yardline <- grid[,
  .(rise = ep - shift(ep)),
  by = .(season, half_seconds_remaining, down, ydstogo)
][, max(rise, na.rm = TRUE)]
setkey(grid, season, half_seconds_remaining, down, yardline_100, ydstogo)
rise_ydstogo <- grid[,
  .(rise = ep - shift(ep)),
  by = .(season, half_seconds_remaining, down, yardline_100)
][, max(rise, na.rm = TRUE)]
cat(
  "largest rise of EP for one yard further from the goal line:",
  round(rise_yardline, 3),
  " for one more yard to go:",
  round(rise_ydstogo, 3),
  "\n"
)
stopifnot(rise_yardline < 0.05, rise_ydstogo < 0.05)

# ---- save, keeping the other internal data ----------------------------------
save_sysdata(ep_model = ep_model)
