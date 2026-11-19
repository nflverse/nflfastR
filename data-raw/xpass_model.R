# Builds the expected dropback model (xpass) that lives in R/sysdata.rda
# (object `xpass_model`). See R/helper_add_xpass.R for what the model is.
#
# Training rows are all plays with a down since 1999 that are a dropback or a
# run, including plays with a penalty (`play_type` "no_play") and without kneels
# and spikes. These are the rows that get `pass_oe`.
#
# Model choices were made with season grouped cross validation (7 folds) and by
# predicting each season since 2014 from the seasons before it, see the PR that
# introduced this file for the evaluation.
#
# Run from the package root. Uses all cores, takes about a minute.

library(dplyr)
library(xgboost)
source("R/helper_add_nflscrapr_mutations.R")
source("R/helper_add_xpass.R")
`%chin%` <- data.table::`%chin%`
set.seed(2013)

nrounds <- 300
params <- list(
  objective = "binary:logistic",
  eval_metric = "logloss",
  eta = 0.1,
  max_depth = 6,
  min_child_weight = 30,
  subsample = 0.8,
  tree_method = "hist",
  nthread = parallel::detectCores()
)

# ---- data -------------------------------------------------------------------
# one season at a time: all columns of all seasons at once need several GB
pbp <- bind_rows(lapply(
  1999:nflreadr::most_recent_season(),
  function(s) {
    nflreadr::load_pbp(s) |>
      select(any_of(c(
        xpass_model_features,
        "game_id",
        "season",
        "play_type",
        "posteam",
        "home_team",
        "roof",
        "pass",
        "rush"
      )))
  }
))

model_data <- tibble::as_tibble(prepare_xpass_data(pbp)) |>
  filter(valid_play == 1, pass == 1 | rush == 1)
cat(
  "training rows:",
  nrow(model_data),
  " games:",
  n_distinct(model_data$game_id),
  " dropback rate:",
  round(mean(model_data$pass), 4),
  "\n"
)

# ---- trees ------------------------------------------------------------------
X <- as.matrix(model_data[, xpass_model_features])
trees <- xgb.train(
  params = params,
  data = xgb.DMatrix(X, label = model_data$pass),
  nrounds = nrounds,
  verbose = 0
)

# The model is stored as raw UBJSON, which is meant to be stable across xgboost
# versions (unlike the legacy binary format).
xpass_model <- list(
  trees_raw = xgb.save.raw(trees, raw_format = "ubj"),
  xgboost_version = as.character(utils::packageVersion("xgboost"))
)

# sanity check: the stored model gives the predictions of this session
chk <- sample(nrow(X), 2000)
xpass_cache$booster <- NULL
p_stored <- predict(xpass_booster(xpass_model), X[chk, ])
xpass_cache$booster <- NULL
stopifnot(
  all(p_stored > 0 & p_stored < 1),
  isTRUE(all.equal(p_stored, predict(trees, X[chk, ])))
)

# ---- save, keeping the other internal data ----------------------------------
# R/sysdata.rda holds other objects. Load them and write them back untouched so
# that only this model changes. Do NOT rebuild or re-serialize anything else
# here (in particular no xgboost models) because a different xgboost version on
# the machine running this script would change those bytes too.
e <- new.env()
load("R/sysdata.rda", envir = e)
untouched <- setdiff(ls(e), "xpass_model")
before <- mget(untouched, envir = e)
e$xpass_model <- xpass_model
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
