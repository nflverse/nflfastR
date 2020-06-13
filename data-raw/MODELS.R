################################################################################
# Author: Ben Baldwin
# Purpose: Estimate nflfastR models for EP, CP, Field Goals, and WP
################################################################################

library(tidyverse)
library(xgboost)
source('R/helper_add_ep_wp.R')
source('R/helper_add_cp_cpoe.R')
source('R/helper_add_nflscrapr_mutations.R')

set.seed(2013) #GoHawks

################################################################################
# Estimate EP model
################################################################################

pbp_data <- readRDS('data-raw/cal_data.rds')

#function in helper_add_ep_wp.R
model_data <- pbp_data %>%
  make_model_mutations() %>%
  mutate(
    label = case_when(
      Next_Score_Half == "Touchdown" ~ 0,
      Next_Score_Half == "Opp_Touchdown" ~ 1,
      Next_Score_Half == "Field_Goal" ~ 2,
      Next_Score_Half == "Opp_Field_Goal" ~ 3,
      Next_Score_Half == "Safety" ~ 4,
      Next_Score_Half == "Opp_Safety" ~ 5,
      Next_Score_Half == "No_Score" ~ 6
    ),
    label = as.factor(label),
    # Calculate the drive difference between the next score drive and the
    # current play drive:
    Drive_Score_Dist = Drive_Score_Half - drive,
    # Create a weight column based on difference in drives between play and next score:
    Drive_Score_Dist_W = (max(Drive_Score_Dist) - Drive_Score_Dist) /
      (max(Drive_Score_Dist) - min(Drive_Score_Dist)),
    # Create a weight column based on score differential:
    ScoreDiff_W = (max(abs(score_differential), na.rm=T) - abs(score_differential)) /
      (max(abs(score_differential), na.rm=T) - min(abs(score_differential), na.rm=T)),
    # Add these weights together and scale again:
    Total_W = Drive_Score_Dist_W + ScoreDiff_W,
    Total_W_Scaled = (Total_W - min(Total_W, na.rm=T)) /
      (max(Total_W, na.rm=T) - min(Total_W, na.rm=T))
  ) %>%
  filter(
    !is.na(defteam_timeouts_remaining), !is.na(posteam_timeouts_remaining),
    !is.na(yardline_100)
  ) %>%
  select(
    label,
    half_seconds_remaining,
    yardline_100,
    home,
    retractable,
    dome,
    outdoors,
    ydstogo,
    era0, era1, era2, era3, era4,
    down1, down2, down3, down4,
    posteam_timeouts_remaining,
    defteam_timeouts_remaining,
    model_week,
    Total_W_Scaled
  )

nrounds = 70
params <-
  list(
    booster = "gbtree",
    objective = "multi:softprob",
    eval_metric = c("mlogloss"),
    num_class = 7,
    eta = 0.2,
    gamma = .2,
    subsample=0.8,
    colsample_bytree=0.8,
    max_depth = 4,
    min_child_weight = .9
  )

model_data <- model_data %>%
  mutate(label = as.numeric(label),
         label = label - 1)

full_train = xgboost::xgb.DMatrix(model.matrix(~.+0, data = model_data %>% select(-label, -Total_W_Scaled)),
                                  label = model_data$label, weight = model_data$Total_W_Scaled)
ep_model <- xgboost::xgboost(params = params, data = full_train, nrounds = nrounds, verbose = 2)

################################################################################
# Estimate FG model
################################################################################

fg_model_data <-  pbp_data %>%
  filter(play_type %in% c("field_goal","extra_point","run") &
           (!is.na(extra_point_result) | !is.na(field_goal_result))) %>%
  make_model_mutations()

#estimate model
fg_model <- mgcv::bam(sp ~ s(yardline_100, by = interaction(era, model_roof)) + model_roof + era,
                      data = fg_model_data, family = "binomial")

################################################################################
# Estimate CP model
################################################################################

model_vars <- pbp_data %>%
  filter(season >= 2006) %>%
  make_model_mutations() %>%
  prepare_cp_data() %>%
  filter(valid_pass == 1) %>%
  select(-valid_pass)

nrounds = 70
params <-
  list(
    booster = "gbtree",
    objective = "binary:logistic",
    eval_metric = c("logloss"),
    eta = 0.2,
    gamma = 5,
    subsample=0.8,
    colsample_bytree=0.8,
    max_depth = 4,
    min_child_weight = 6,
    base_score = mean(model_vars$complete_pass)
  )

full_train = xgboost::xgb.DMatrix(model.matrix(~.+0, data = model_vars %>% dplyr::select(-complete_pass)),
                                  label = model_vars$complete_pass)
cp_model <- xgboost::xgboost(params = params, data = full_train, nrounds = nrounds, verbose = 2)


################################################################################
# Estimate WP model: spread
################################################################################

model_data <- pbp_data %>%
  make_model_mutations() %>%
  prepare_wp_data() %>%
  mutate(label = ifelse(posteam == Winner, 1, 0)) %>%
  filter(qtr <= 4 & !is.na(ep) & !is.na(score_differential) & !is.na(play_type) & !is.na(label)) %>%
  select(
    label,
    receive_2h_ko,
    spread_time,
    half_seconds_remaining,
    game_seconds_remaining,
    ExpScoreDiff_Time_Ratio,
    ep,
    score_differential,
    down,
    ydstogo,
    home,
    posteam_timeouts_remaining,
    defteam_timeouts_remaining
  )


nrounds = 170
params <-
  list(
    booster = "gbtree",
    objective = "binary:logistic",
    eval_metric = c("logloss"),
    eta = 0.075,
    gamma = 3,
    subsample=0.8,
    colsample_bytree=0.8,
    max_depth = 5,
    min_child_weight = .9
  )


full_train = xgboost::xgb.DMatrix(model.matrix(~.+0, data = model_data %>% select(-label)),
                                  label = model_data$label)
wp_model_spread <- xgboost::xgboost(params = params, data = full_train, nrounds = nrounds, verbose = 2)

#importance <- xgboost::xgb.importance(feature_names = colnames(wp_model_spread), model = wp_model_spread)
#xgboost::xgb.ggplot.importance(importance_matrix = importance)

#xgboost::xgb.plot.tree(model = wp_model_spread, trees = 1, show_node_id = TRUE)


################################################################################
# Estimate WP model: no spread
################################################################################

model_data <- model_data %>%
  select(
    -spread_time
  )

nrounds = 65
params <-
  list(
    booster = "gbtree",
    objective = "binary:logistic",
    eval_metric = c("error", "logloss"),
    eta = 0.2,
    gamma = 0,
    subsample=0.8,
    colsample_bytree=0.8,
    max_depth = 4,
    min_child_weight = 1
  )


full_train = xgboost::xgb.DMatrix(model.matrix(~.+0, data = model_data %>% select(-label)),
                                  label = model_data$label)
wp_model <- xgboost::xgboost(params = params, data = full_train, nrounds = nrounds, verbose = 2)


# save models to use in package
usethis::use_data(ep_model, wp_model, wp_model_spread, fg_model, cp_model, internal = TRUE, overwrite = TRUE)



