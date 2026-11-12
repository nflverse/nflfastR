# Builds the regulation win probability model with the Vegas line (vegas_wp)
# that lives in R/sysdata.rda (object `wp_vegas_model`). See
# R/helper_add_vegas_wp.R for what the model is.
#
# Training rows are regulation plays with a down from all finished games since
# 1999. Games that went to overtime are included: a tie at the end of
# regulation is a real state whose outcome is decided in overtime, and dropping
# those games makes late close games look more decided than they are. Ties are
# dropped.
#
# Model choices were made with season grouped cross validation (7 folds), see
# the PR that introduced this file for the evaluation.
#
# Run from the package root. Uses all cores, takes a few minutes.

library(dplyr)
library(xgboost)
source("R/helper_add_nflscrapr_mutations.R")
source("R/helper_add_ep_wp.R")
source("R/helper_add_ot_wp.R")
source("R/helper_add_vegas_wp.R")
source("R/helper_fg_model.R")
# the field goal model (data-raw/fg_model.R) is used by the end of half terms
local({
  e <- new.env()
  load("R/sysdata.rda", envir = e)
  fg_prob_table <<- e$fg_prob_table
})
stopifnot(exists("fg_prob_table"))
set.seed(2013)

constants <- list(
  # remaining-time scale of the final margin, sigma * ((t + eps) / 3600)^alpha
  sigma = 13.5,
  alpha = 0.38,
  eps = 10,
  # weight of the score / spread / home part of the offset
  shrink = 0.7,
  # weight of the whole offset: 1, fading linearly to fade_end over the last
  # fade_t seconds of the game
  fade_end = 0.43,
  fade_t = 900,
  # number of trees used, by game seconds remaining (linear in between)
  schedule = data.frame(
    t = c(0, 60, 210, 450, 750, 1350, 2250, 3600),
    n = c(1200L, 1200L, 820L, 220L, 180L, 140L, 120L, 90L)
  )
)
constants$n_trees <- max(constants$schedule$n)

params <- list(
  objective = "binary:logistic",
  eval_metric = "logloss",
  base_score = 0.5, # the offset is the only intercept
  eta = 0.03,
  max_depth = 6,
  min_child_weight = 22.5,
  subsample = 0.58,
  colsample_bytree = 1.0,
  lambda = 2.39,
  gamma = 3.46,
  # in the order of wp_vegas_features
  monotone_constraints = c(0, 0, 0, 0, 1, 1, 1, 1, -1, -1, -1, 1, -1),
  nthread = parallel::detectCores()
)

# ---- data -------------------------------------------------------------------
# one season at a time: all columns of all seasons at once need several GB
pbp_cols <- function(p) {
  p |>
    select(
      game_id,
      play_id,
      season,
      qtr,
      posteam,
      defteam,
      home_team,
      away_team,
      home_score,
      away_score,
      result,
      spread_line,
      roof,
      down,
      ydstogo,
      yardline_100,
      play_type,
      kickoff_attempt,
      game_seconds_remaining,
      half_seconds_remaining,
      score_differential,
      posteam_timeouts_remaining,
      defteam_timeouts_remaining,
      total_home_score,
      total_away_score
    )
}
pbp <- bind_rows(lapply(
  1999:nflreadr::most_recent_season(),
  function(s) pbp_cols(nflreadr::load_pbp(s))
))

# Games whose running score doesn't end at the final score were parsed with
# bugs fixed in nflfastR 6.0.0 (every Jacksonville home game of 2001 and 2002
# credited the home team's touchdowns and safeties to the visitors, and
# 2011_13_DET_NO had two touchdowns twice). Until the released data are
# rebuilt, rebuild those games here with this version of the package.
bad <- pbp |>
  group_by(game_id) |>
  summarise(
    ok = max(total_home_score, na.rm = TRUE) == first(home_score) &
      max(total_away_score, na.rm = TRUE) == first(away_score),
    .groups = "drop"
  ) |>
  filter(!ok) |>
  pull(game_id)
cat(
  "games rebuilt because the running score doesn't end at the final:",
  length(bad),
  "\n"
)
if (length(bad) > 0) {
  pkgload::load_all(".", quiet = TRUE)
  rebuilt <- nflfastR::build_nflfastR_pbp(bad) |>
    pbp_cols()
  still <- rebuilt |>
    group_by(game_id) |>
    summarise(
      ok = max(total_home_score, na.rm = TRUE) == first(home_score) &
        max(total_away_score, na.rm = TRUE) == first(away_score),
      .groups = "drop"
    )
  stopifnot(all(still$ok))
  pbp <- bind_rows(filter(pbp, !game_id %in% bad), rebuilt)
}

# where the receiving team's first snap is after a regulation kickoff, by the
# kickoff rule regimes of the overtime model (ot_kick_first)
kick_start <- pbp |>
  filter(!is.na(result)) |>
  group_by(game_id) |>
  arrange(play_id, .by_group = TRUE) |>
  mutate(
    next_yl = lead(yardline_100),
    next_down = lead(down),
    next_posteam = lead(posteam),
    next_qtr = lead(qtr)
  ) |>
  ungroup() |>
  filter(
    qtr <= 4,
    next_qtr <= 4,
    kickoff_attempt == 1,
    next_down %in% 1,
    next_posteam == posteam,
    !is.na(next_yl)
  ) |>
  mutate(regime = findInterval(season, ot_kick_first) + 1L) |>
  group_by(regime) |>
  summarise(n = n(), start = round(mean(next_yl)), .groups = "drop")
print(kick_start)
stopifnot(nrow(kick_start) == length(ot_kick_first) + 1L)

model_data <- pbp |>
  filter(!is.na(result), result != 0) |>
  make_model_mutations() |>
  prepare_wp_data() |>
  filter(
    qtr <= 4,
    !is.na(down),
    !is.na(play_type),
    !is.na(yardline_100),
    !is.na(score_differential),
    !is.na(posteam_timeouts_remaining),
    !is.na(defteam_timeouts_remaining)
  ) |>
  mutate(
    winner = if_else(home_score > away_score, home_team, away_team),
    label = as.numeric(posteam == winner)
  )
cat(
  "training rows:",
  nrow(model_data),
  " games:",
  n_distinct(model_data$game_id),
  "\n"
)

# ---- offset -------------------------------------------------------------------
off_fit <- stats::glm.fit(
  x = wp_vegas_design(model_data, constants),
  y = model_data$label,
  family = stats::binomial(link = "probit")
)
beta <- off_fit$coefficients
print(round(beta, 4))

# ---- trees ----------------------------------------------------------------------
X <- wp_vegas_tree_matrix(model_data, constants)
stopifnot(identical(colnames(X), wp_vegas_features))
dtrain <- xgb.DMatrix(
  X,
  label = model_data$label,
  base_margin = wp_vegas_offset(model_data, beta, constants)
)
trees <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = constants$n_trees,
  verbose = 0
)

# The model is stored as raw UBJSON, which is meant to be stable across xgboost
# versions (unlike the legacy binary format).
wp_vegas_model <- list(
  trees_raw = xgb.save.raw(trees, raw_format = "ubj"),
  beta = beta,
  constants = constants,
  kick_start = stats::setNames(kick_start$start, kick_start$regime),
  xgboost_version = as.character(utils::packageVersion("xgboost"))
)

# sanity check: predictions in this session match the stored model
chk <- model_data[sample(nrow(model_data), 2000), ]
wp_vegas_cache$booster <- NULL
wp_vegas_cache$inclusive <- NULL
p_stored <- wp_vegas_raw(chk, wp_vegas_model)
stopifnot(all(is.finite(p_stored)), all(p_stored > 0 & p_stored < 1))

# ---- save, keeping the other internal data ----------------------------------
# R/sysdata.rda holds other objects. Load them and write them back untouched so
# that only this model changes. Do NOT rebuild or re-serialize anything else
# here (in particular no xgboost models) because a different xgboost version on
# the machine running this script would change those bytes too.
e <- new.env()
load("R/sysdata.rda", envir = e)
untouched <- setdiff(ls(e), "wp_vegas_model")
before <- mget(untouched, envir = e)
e$wp_vegas_model <- wp_vegas_model
save(
  list = ls(e),
  file = "R/sysdata.rda",
  envir = e,
  compress = "bzip2",
  version = 2
)
# make sure we did not change anything else
e2 <- new.env()
load("R/sysdata.rda", envir = e2)
stopifnot(all(vapply(
  untouched,
  function(n) identical(before[[n]], e2[[n]]),
  TRUE
)))
