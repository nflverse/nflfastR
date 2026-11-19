# Builds the expected dropback model (xpass) that lives in R/sysdata.rda
# (object `xpass_model`). See R/helper_add_xpass.R for what the model is.
#
# Training rows are all plays with a down since 2001 that are a dropback or a
# run, including plays with a penalty (`play_type` "no_play") and without kneels
# and spikes. These are the rows that get `pass_oe`. 1999 and 2000 are not used
# because nflfastR doesn't support these seasons anymore (#568).
#
# Model choices were made with season grouped cross validation (7 folds) and by
# predicting each season since 2014 from the seasons before it, see the PR that
# introduced this file for the evaluation.
#
# Run from the package root. Uses all cores, takes about a minute.

library(dplyr)
library(xgboost)
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
  2001:nflreadr::most_recent_season(),
  function(s) {
    nflreadr::load_pbp(s) |>
      select(any_of(c(
        xpass_model_features,
        "game_id",
        "season",
        "play_type",
        "posteam",
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
stopifnot(isTRUE(all.equal(
  predict(xgb.load.raw(xpass_model$trees_raw), X[chk, ]),
  predict(trees, X[chk, ])
)))

# ---- save, keeping the other internal data ----------------------------------
# (see data-raw/save_sysdata.R: nothing else in R/sysdata.rda may be rebuilt or
# re-serialized here)
source("data-raw/save_sysdata.R")
save_sysdata(xpass_model = xpass_model)
