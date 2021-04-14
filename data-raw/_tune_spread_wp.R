library(tidyverse)
library(tidymodels)
source('R/helper_add_ep_wp.R')
source('R/helper_add_nflscrapr_mutations.R')

set.seed(2013)

model_data <-
  # readRDS('data-raw/cal_data.rds') %>%
  readRDS(url('https://github.com/nflverse/nflfastR-data/blob/master/models/cal_data.rds?raw=true')) %>%
  filter(Winner != "TIE") %>%
  make_model_mutations() %>%
  prepare_wp_data() %>%
  mutate(label = ifelse(posteam == Winner, 1, 0)) %>%
  filter(!is.na(ep) & !is.na(score_differential) & !is.na(play_type) & !is.na(label) & !is.na(yardline_100), qtr <= 4) %>%
  select(
    label,
    receive_2h_ko,
    spread_time,
    home,
    half_seconds_remaining,
    game_seconds_remaining,
    ExpScoreDiff_Time_Ratio,
    score_differential,
    # ep,
    down,
    ydstogo,
    yardline_100,
    posteam_timeouts_remaining,
    defteam_timeouts_remaining,
    season
  )


folds <- map(0:9, function(x) {
  f <- which(model_data$season %in% c(2000 + x, 2010 + x))
  return(f)
})


full_train = xgboost::xgb.DMatrix(model.matrix(~.+0, data = model_data %>% select(-label, -season)),
                                  label = model_data$label)

#params
nrounds = 5000


# #################################################################################
# try tidymodels

grid <- grid_latin_hypercube(
  finalize(mtry(), model_data),
  min_n(),
  tree_depth(),
  learn_rate(),
  loss_reduction(),
  sample_size = sample_prop(),
  size = 20
)

grid <- grid %>%
  mutate(
    # it was making dumb learn rates
    learn_rate = .1 * ((1 : nrow(grid)) / nrow(grid)),
    # has to be between 0 and 1
    mtry = mtry / length(model_data)
  )

# bonus round at the end: do more searching after finding good ones
grid <- grid %>%
  head(6) %>%
  mutate(
    learn_rate = c(0.01, 0.02, .03, .04, .05, .06),
    min_n = 14,
    tree_depth = 5,
    mtry = 0.5714286,
    loss_reduction = 3.445502e-01,
    sample_size = 0.7204741
  )

grid

# function to search over hyperparameter grid
get_metrics <- function(df, row = 1) {

  # testing only
  # df <- grid %>% dplyr::slice(1)

  params <-
    list(
      booster = "gbtree",
      objective = "binary:logistic",
      eval_metric = c("logloss"),
      eta = df$learn_rate,
      gamma = df$loss_reduction,
      subsample= df$sample_size,
      colsample_bytree= df$mtry,
      max_depth = df$tree_depth,
      min_child_weight = df$min_n
    )

  #train
  wp_cv_model <- xgboost::xgb.cv(data = full_train, params = params, nrounds = nrounds,
                                 folds = folds, metrics = list("logloss"),
                                 early_stopping_rounds = 10, print_every_n = 10)

  output <- params
  output$iter = wp_cv_model$best_iteration
  output$logloss = wp_cv_model$evaluation_log[output$iter]$test_logloss_mean
  output$error = wp_cv_model$evaluation_log[output$iter]$test_error_mean

  this_param <- bind_rows(output)

  if (row == 1) {
    saveRDS(this_param, "data-raw/modeling.rds")
  } else {
    prev <- readRDS("data-raw/modeling.rds")
    for_save <- bind_rows(prev, this_param)
    saveRDS(for_save, "data-raw/modeling.rds")
  }

  return(this_param)

}

# get results
results <- map_df(1 : nrow(grid), function(x) {

  message(glue::glue("Row {x}"))
  get_metrics(grid %>% dplyr::slice(x), row = x)

})

# plot
results %>%
  select(logloss, eta, gamma, subsample, colsample_bytree, max_depth, min_child_weight) %>%
  pivot_longer(eta:min_child_weight,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, logloss, color = parameter)) +
  geom_point(alpha = 0.8, show.legend = FALSE, size = 3) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "logloss") +
  theme_minimal()


# final best model
#
# eta 0.02
# gamma 0.3445502
# subsample 0.7204741
# colsample_bytree 0.5714286
# max_depth 5
# min_child_weight 14
# iter 760
# logloss 0.4485878


# https://parsnip.tidymodels.org/reference/boost_tree.html
# https://xgboost.readthedocs.io/en/latest/parameter.html

