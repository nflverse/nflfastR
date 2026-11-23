################################################################################
# Author: Ben Baldwin
# Purpose: Estimate nflfastR models for CP and WP (the EP model is built by
# data-raw/ep_model.R)
################################################################################

library(tidyverse)
library(xgboost)
source('R/helper_add_ep_wp.R')
source('R/helper_add_cp_cpoe.R')
source('R/helper_add_nflscrapr_mutations.R')


# from remote
# pbp_data <- readRDS(url('https://github.com/nflverse/nflfastR-data/blob/master/models/cal_data.rds?raw=true'))

# from local
pbp_data <- readRDS('../nflfastR-data/models/cal_data.rds')

################################################################################
# Estimate CP model
################################################################################

model_vars <- pbp_data |>
  filter(season >= 2006) |>
  make_model_mutations() |>
  prepare_cp_data() |>
  filter(valid_pass == 1) |>
  select(-valid_pass)

nrounds <- 560
params <-
  list(
    booster = "gbtree",
    objective = "binary:logistic",
    eval_metric = c("logloss"),
    eta = 0.025,
    gamma = 5,
    subsample = 0.8,
    colsample_bytree = 0.8,
    max_depth = 4,
    min_child_weight = 6,
    base_score = mean(model_vars$complete_pass)
  )

full_train <- xgboost::xgb.DMatrix(
  model.matrix(~ . + 0, data = model_vars |> dplyr::select(-complete_pass)),
  label = model_vars$complete_pass
)
set.seed(2013) #GoHawks
cp_model <- xgboost::xgboost(
  params = params,
  data = full_train,
  nrounds = nrounds,
  verbose = 2
)


################################################################################
# Estimate WP model: spread
# NOTE: nflfastR no longer uses this model (fastrmodels::wp_model_spread).
# vegas_wp comes from the model built by data-raw/wp_vegas_model.R, which is
# stored in R/sysdata.rda. The data prepared here is still used by the WP model
# without the spread below.
################################################################################

model_data <-
  readRDS(url(
    'https://github.com/guga31bb/metrics/blob/master/wp_tuning/cal_data.rds?raw=true'
  )) |>
  filter(Winner != "TIE") |>
  make_model_mutations() |>
  prepare_wp_data() |>
  mutate(label = ifelse(posteam == Winner, 1, 0)) |>
  filter(
    qtr <= 4 &
      !is.na(ep) &
      !is.na(score_differential) &
      !is.na(play_type) &
      !is.na(label),
    !is.na(yardline_100)
  ) |>
  select(
    label,
    receive_2h_ko,
    spread_time,
    home,
    half_seconds_remaining,
    game_seconds_remaining,
    Diff_Time_Ratio,
    score_differential,
    down,
    ydstogo,
    yardline_100,
    posteam_timeouts_remaining,
    defteam_timeouts_remaining
  )


nrounds <- 534
params <-
  list(
    booster = "gbtree",
    objective = "binary:logistic",
    eval_metric = c("logloss"),
    eta = 0.05,
    gamma = .79012017,
    subsample = 0.9224245,
    colsample_bytree = 5 / 12,
    max_depth = 5,
    min_child_weight = 7,
    monotone_constraints = "(0, 0, 0, 0, 0, 1, 1, -1, -1, -1, 1, -1)"
  )


full_train <- xgboost::xgb.DMatrix(
  model.matrix(~ . + 0, data = model_data |> select(-label)),
  label = model_data$label
)
set.seed(2013) #GoHawks
wp_model_spread <- xgboost::xgboost(
  params = params,
  data = full_train,
  nrounds = nrounds,
  verbose = 2
)

importance <- xgboost::xgb.importance(
  feature_names = colnames(wp_model_spread),
  model = wp_model_spread
)
xgboost::xgb.ggplot.importance(importance_matrix = importance)

#xgboost::xgb.plot.tree(model = wp_model_spread, trees = 1, show_node_id = TRUE)

################################################################################
# Estimate WP model: no spread
################################################################################

model_data <- model_data |>
  select(
    -spread_time
  )

nrounds <- 65
params <-
  list(
    booster = "gbtree",
    objective = "binary:logistic",
    eval_metric = c("logloss"),
    eta = 0.2,
    gamma = 0,
    subsample = 0.8,
    colsample_bytree = 0.8,
    max_depth = 4,
    min_child_weight = 1
  )


full_train <- xgboost::xgb.DMatrix(
  model.matrix(~ . + 0, data = model_data |> select(-label)),
  label = model_data$label
)
set.seed(2013) #GoHawks
wp_model <- xgboost::xgboost(
  params = params,
  data = full_train,
  nrounds = nrounds,
  verbose = 2
)


################################################################################
# save models to use in package
################################################################################

usethis::use_data(
  wp_model,
  wp_model_spread,
  cp_model,
  internal = TRUE,
  overwrite = TRUE
)
