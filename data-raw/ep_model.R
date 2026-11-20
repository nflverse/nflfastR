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
# 2. The student: a small neural network (three hidden layers of 48 units)
#    that is fit to the probabilities of the teacher instead of the outcomes.
#    The trees are accurate but jagged: EP changes in steps of uneven size and
#    often in the wrong direction when the ball moves a yard, the distance
#    changes by a yard or the clock runs. The network is a smooth function of
#    its inputs and learns the same surface without the steps. Fit to the
#    outcomes directly it overfits, because a single outcome says very little
#    about the probabilities, and the teacher's probabilities don't have that
#    noise. One network depends on the random draws of its fit (its synthetic
#    states, start weights and batches): two fit with different seeds differ
#    by 0.045 EP per play on average (plays with a down of 2024 and 2025).
#    So there are three students, fit with different draws, and the model is
#    the average of their probabilities. Two such averages differ by 0.026
#    (five would differ by 0.020, but each student costs prediction time).
#
# Only the students are stored (about 50 KB of weights each, the teacher has
# 10 MB). Their predictions are matrix products, so they don't need xgboost.
#
# Model choices were made by predicting the seasons since 2021 from the seasons
# before them and with season grouped cross validation (7 folds), see the PR
# that introduced this file for the evaluation. What was tried and dropped:
# monotone constraints (the class probabilities are not monotone in field
# position, only EP is), a goal to go feature, an era for the seasons since
# 2024, kneel downs as training rows, wider and narrower networks (64 or 96
# units are slower and not better, 32 units are worse), and team strength as
# features of the teacher, with the student fit to the teacher averaged over
# the matchups of real games. That gives the same average team, but the trees
# then fit the game state less well: they overrated 3rd and long and backed up
# states by another 0.04 and 0.10 points out of sample.
#
# Run from the package root. Takes about 15 minutes and 8 GB of memory (the
# students are fit by three worker processes, see `student_params$workers`).

library(data.table)
library(xgboost)
source("R/helper_add_nflscrapr_mutations.R")
source("R/helper_ep_model.R")
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

student_params <- list(
  hidden = c(48L, 48L, 48L),
  epochs = 20L,
  batch = 4096L,
  # learning rate of Adam, from `lr` to `lr_end` on half a cosine wave
  lr = 2e-3,
  lr_end = 2e-5,
  # synthetic states per play, see synthetic_states()
  extra = 0.25,
  # the number of networks the model averages, and the seed of the first
  students = 3L,
  seed = 2013L,
  # worker processes that fit them, see fit_one_student()
  workers = 3L
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

# ---- student ----------------------------------------------------------------
# The student sees every play with a down (kneel downs too, their states are
# valid) and synthetic states: plays whose down and distance and whose yard
# line are swapped with those of other plays, while clock, timeouts, home
# field, roof and era stay as they were. They teach the student the teacher's
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

# The probabilities of the outcomes and the values of all layers for a matrix
# of standardized inputs with one row per play. This is the layout of the fit.
# ep_model_predict() has the plays in the columns.
student_forward <- function(layers, x) {
  values <- list(x)
  for (l in seq_along(layers)) {
    x <- x %*% layers[[l]]$weights + rep(layers[[l]]$bias, each = nrow(x))
    if (l < length(layers)) {
      x <- ep_model_activation(x)
    }
    values[[l + 1L]] <- x
  }
  x <- exp(x - x[cbind(seq_len(nrow(x)), max.col(x, "first"))])
  list(probs = x / rowSums(x), values = values)
}

# Fits the network to the probabilities `target` of the states `features` by
# minimizing the cross entropy with Adam on shuffled batches. Returns the model
# as it is stored.
fit_student <- function(features, target, params = student_params) {
  x <- t(ep_model_inputs(features))
  center <- colMeans(x)
  scale <- apply(x, 2, stats::sd)
  # column by column, which needs less memory than the whole matrix at once
  for (j in seq_len(ncol(x))) {
    x[, j] <- (x[, j] - center[j]) / scale[j]
  }

  sizes <- c(ncol(x), params$hidden, ncol(target))
  layers <- lapply(seq_len(length(sizes) - 1L), function(i) {
    list(
      weights = matrix(
        stats::rnorm(sizes[i] * sizes[i + 1L], sd = sqrt(1 / sizes[i])),
        sizes[i],
        sizes[i + 1L]
      ),
      bias = numeric(sizes[i + 1L])
    )
  })
  zeros <- lapply(layers, \(l) list(weights = 0 * l$weights, bias = 0 * l$bias))
  m <- zeros
  v <- zeros
  beta1 <- 0.9
  beta2 <- 0.999
  n <- nrow(x)
  batches <- ceiling(n / params$batch)
  steps <- params$epochs * batches
  step <- 0L
  for (epoch in seq_len(params$epochs)) {
    order <- sample.int(n)
    loss <- 0
    for (b in seq_len(batches)) {
      step <- step + 1L
      i <- order[((b - 1L) * params$batch + 1L):min(b * params$batch, n)]
      f <- student_forward(layers, x[i, , drop = FALSE])
      loss <- loss - sum(target[i, ] * log(f$probs))
      lr <- params$lr_end +
        0.5 * (params$lr - params$lr_end) * (1 + cos(pi * step / steps))
      # gradient of the mean cross entropy with respect to the output layer
      delta <- (f$probs - target[i, , drop = FALSE]) / length(i)
      for (l in rev(seq_along(layers))) {
        gradient <- list(
          weights = crossprod(f$values[[l]], delta),
          bias = colSums(delta)
        )
        if (l > 1L) {
          # the derivative of the activation a = x / sqrt(1 + x^2) is
          # (1 - a^2)^1.5
          slope <- 1 - f$values[[l]]^2
          delta <- tcrossprod(delta, layers[[l]]$weights) * slope * sqrt(slope)
        }
        for (part in c("weights", "bias")) {
          m[[l]][[part]] <- beta1 *
            m[[l]][[part]] +
            (1 - beta1) * gradient[[part]]
          v[[l]][[part]] <- beta2 *
            v[[l]][[part]] +
            (1 - beta2) * gradient[[part]]^2
          layers[[l]][[part]] <- layers[[l]][[part]] -
            lr *
              (m[[l]][[part]] / (1 - beta1^step)) /
              (sqrt(v[[l]][[part]] / (1 - beta2^step)) + 1e-8)
        }
      }
    }
    cat(sprintf(
      "epoch %2d  cross entropy to the teacher %.5f\n",
      epoch,
      loss / n
    ))
  }

  # The stored layers have the layout of ep_model_predict() (weights
  # transposed) and the first one takes the inputs as they are:
  # ((x - center) / scale) %*% w = x %*% (w / scale) - center %*% (w / scale)
  stored <- lapply(seq_along(layers), function(l) {
    weights <- layers[[l]]$weights
    bias <- layers[[l]]$bias
    if (l == 1L) {
      weights <- weights / scale
      bias <- bias - as.vector(center %*% weights)
    }
    list(weights = t(weights), bias = bias)
  })
  # the stored network predicts what the fit did
  check <- sample.int(n, 5000)
  last <- stored[[length(stored)]]
  stopifnot(
    isTRUE(all.equal(
      ep_model_predict(
        features[check, ],
        list(students = list(stored), classes = ep_classes)
      ),
      student_forward(layers, x[check, ])$probs,
      check.attributes = FALSE
    )),
    # exp() in ep_model_predict() can't overflow
    max(rowSums(abs(last$weights)) + abs(last$bias)) < 700
  )
  stored
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
# Every student has its own synthetic states, start weights and batches. The
# networks are fit in parallel, one core each, by worker processes that get
# the states of the plays and the teacher once and make their own synthetic
# states. Each worker needs about 2 GB of memory on top of the 3 GB of this
# session, so lower `workers` on a machine with less than 10 GB. (Forked
# workers copy the memory of this session as they run and need much more.)
fit_one_student <- function(seed) {
  set.seed(seed)
  extra <- sample.int(
    nrow(features),
    round(student_params$extra * nrow(features))
  )
  states <- rbind(features, synthetic_states(features[extra, , drop = FALSE]))
  fit_student(states, teacher_probs(teacher, states))
}
seeds <- student_params$seed + seq_len(student_params$students) - 1L
cluster <- parallel::makeCluster(
  min(student_params$workers, length(seeds), parallel::detectCores())
)
invisible(parallel::clusterEvalQ(cluster, {
  library(data.table)
  library(xgboost)
}))
parallel::clusterExport(
  cluster,
  c(
    "features",
    "fit_one_student",
    "synthetic_states",
    "teacher_matrix",
    "teacher_probs",
    "fit_student",
    "student_forward",
    "student_params",
    "ep_classes",
    "ep_model_features",
    "ep_model_inputs",
    "ep_model_activation",
    "ep_model_predict"
  )
)
# the teacher is a pointer to xgboost's memory, so it goes as its saved bytes
teacher_raw <- xgboost::xgb.save.raw(teacher)
parallel::clusterExport(cluster, "teacher_raw")
invisible(parallel::clusterEvalQ(
  cluster,
  teacher <- xgboost::xgb.load.raw(teacher_raw)
))
students <- parallel::clusterApplyLB(cluster, seeds, fit_one_student)
parallel::stopCluster(cluster)
ep_model <- list(students = students, classes = ep_classes)

# ---- checks -----------------------------------------------------------------
log_loss <- function(probs, label) {
  -mean(log(probs[cbind(seq_along(label), label + 1L)]))
}
points <- c(7, -7, 3, -3, 2, -2, 0)
p_teacher <- teacher_probs(teacher, features[plays, ])
p_student <- ep_model_predict(features[plays, ], ep_model)
actual <- points[model_data$label[plays] + 1L]
# how far one network is from the average of all
ep_student <- p_student %*% points
ep_single <- vapply(
  ep_model$students,
  function(layers) {
    single <- list(students = list(layers), classes = ep_classes)
    mean(abs(
      ep_model_predict(features[plays, ], single) %*% points - ep_student
    ))
  },
  numeric(1)
)
cat(
  "\nEP of one network minus EP of the model, mean absolute difference:",
  paste(round(ep_single, 4), collapse = " "),
  "\n"
)
cat(
  "\nlog loss on the training plays (in sample): teacher",
  round(log_loss(p_teacher, model_data$label[plays]), 4),
  " student",
  round(log_loss(p_student, model_data$label[plays]), 4),
  "\nstudent and teacher, mean absolute difference of the probabilities:",
  round(mean(abs(p_student - p_teacher)), 4),
  "\nexpected points (7 / 3 / 2) minus the points of the next score: teacher",
  round(mean(p_teacher %*% points - actual), 3),
  " student",
  round(mean(p_student %*% points - actual), 3),
  "\n"
)
# the student follows the teacher
stopifnot(mean(abs(p_student - p_teacher)) < 0.01)

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
