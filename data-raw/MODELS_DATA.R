################################################################################
# Author: Ben Baldwin
# Purpose: Prepare data for nflfastR models for EP, CP, Field Goals, and WP
# This takes a long time (especially finding next score half)
# Save a pre-prepared df
################################################################################

library(tidyverse)
source('data-raw/EP_functions.R')

################################################################################
# DATA PREP
################################################################################

#read in data from data repo
pbp_data <- purrr::map_df(1999 : 2019, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
    )
  ) %>% filter(season_type == 'REG')
}) %>%
  mutate(
    Winner = if_else(home_score > away_score, home_team,
                     if_else(home_score < away_score, away_team, "TIE"))
  )

#get next score half using the provided function
pbp_next_score_half <- map_dfr(unique(pbp_data$game_id),
                               function(x) {
                                 pbp_data %>%
                                   filter(game_id == x) %>%
                                   find_game_next_score_half()
                               })

#bind to original df
pbp_data <- bind_cols(pbp_data, pbp_next_score_half)

#for estimating the models, apply some filters
pbp_data <- pbp_data %>%
  filter(Next_Score_Half %in% c("Opp_Field_Goal", "Opp_Safety", "Opp_Touchdown",
                                "Field_Goal", "No_Score", "Safety", "Touchdown") &
           play_type %in% c("field_goal", "no_play", "pass", "punt", "run",
                            "qb_spike") & is.na(two_point_conv_result) & is.na(extra_point_result) &
           !is.na(down) & !is.na(game_seconds_remaining)) %>%
  #to keep file size manageable
  select(
    game_id,
    Next_Score_Half,
    Drive_Score_Half,
    play_type,
    game_seconds_remaining,
    half_seconds_remaining,
    yardline_100,
    roof,
    posteam,
    defteam,
    home_team,
    ydstogo,
    season,
    qtr,
    down,
    week,
    drive,
    ep,
    score_differential,
    posteam_timeouts_remaining,
    defteam_timeouts_remaining,
    desc,
    receiver_player_name,
    pass_location,
    air_yards,
    complete_pass, incomplete_pass, interception,
    qb_hit,
    extra_point_result,
    field_goal_result,
    sp,
    Winner,
    spread_line
  )

#for doing calibation etc
saveRDS(pbp_data, 'data-raw/cal_data.rds')

#fix roof types
#now fixing spread_line
#delete this after re-scraping and updating
games_data <- readRDS(url("https://github.com/leesharpe/nfldata/blob/master/data/games.rds?raw=true")) %>%
  select(home_team, season, week, spread_line) %>%
  mutate(
    home_team = case_when(
      home_team == 'STL' ~ 'LA',
      home_team == 'SD' ~ 'LAC',
      home_team == 'OAK' ~ 'LV',
      TRUE ~ home_team
    )
  )

pbp_data <- readRDS('data-raw/cal_data.rds')
pbp_data <- pbp_data %>%
  left_join(games_data, by = c('home_team', 'season', 'week'))
saveRDS(pbp_data, 'data-raw/cal_data.rds')


################################################################################
# DATA PREP FOR NFLSCRAPR COMPARISON
# This is only used in the readme describing the models
# Where we compare nflscrapR and nflfastR calibration errors
################################################################################

pbp_data <- purrr::map_df(2000 : 2019, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/legacy-data/play_by_play_{x}.rds")
    )
  ) %>% filter(season_type == 'REG')
})

games <- readRDS(url("http://www.habitatring.com/games.rds")) %>%
  filter(!is.na(result)) %>%
  mutate(
    game_id = as.numeric(old_game_id),
    Winner = if_else(home_score > away_score, home_team,
                     if_else(home_score < away_score, away_team, "TIE"))
  ) %>%
  select(game_id, Winner, result, roof)

pbp_data <- pbp_data %>%
  left_join(
    games, by = c('game_id')
  )

#get next score half using the provided function
pbp_next_score_half <- map_dfr(unique(pbp_data$game_id),
                               function(x) {
                                 pbp_data %>%
                                   filter(game_id == x) %>%
                                   find_game_next_score_half()
                               })

#bind to original df
pbp_data <- bind_cols(pbp_data, pbp_next_score_half)

#apply filters
pbp_data <- pbp_data %>%
  filter(Next_Score_Half %in% c("Opp_Field_Goal", "Opp_Safety", "Opp_Touchdown",
                                "Field_Goal", "No_Score", "Safety", "Touchdown") &
           play_type %in% c("field_goal", "no_play", "pass", "punt", "run",
                            "qb_spike") & is.na(two_point_conv_result) & is.na(extra_point_result) &
           !is.na(down) & !is.na(game_seconds_remaining)) %>%
  select(posteam, wp, qtr, Winner, td_prob, opp_td_prob, fg_prob, opp_fg_prob, safety_prob, opp_safety_prob, no_score_prob, Next_Score_Half)

#for doing calibation etc
saveRDS(pbp_data, 'data-raw/cal_data_nflscrapr.rds')
