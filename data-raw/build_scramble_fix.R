library(tidyverse)

pbp <- nflfastR::load_pbp(2005) %>%
  dplyr::select(game_id, play_id, week, desc, away_team, home_team, posteam, qtr, down, ydstogo, time)

s <- readxl::read_xlsx("data-raw/scrambles_2005.xlsx") %>%
  janitor::clean_names()

dat <- s %>%
  select(
    season = year, week, qtr, away_team = away, home_team = home, posteam = offense, down, ydstogo = togo, date_time = time, desc = description
  ) %>%
  mutate(
    time = paste0(
      formatC(lubridate::hour(date_time), width = 2, flag = "0"),
      ":",
      formatC(lubridate::minute(date_time), width = 2, flag = "0")
    )
  ) %>%
  select(-date_time) %>%
  mutate_at(vars(home_team, away_team, posteam), nflfastR:::team_name_fn)

d <- dat %>%
  dplyr::left_join(
    pbp %>% select(game_id, play_id, week, away_team, home_team, posteam, qtr, down, ydstogo, time),
    by = c("week", "away_team", "home_team", "posteam", "qtr", "down", "ydstogo", "time")
  ) %>%
  mutate(scramble_id = paste0(game_id, "_", play_id)) %>%
  filter(scramble_id != "2005_09_CIN_BAL_1725")

scramble_fix <- d$scramble_id
saveRDS(scramble_fix, file = "data-raw/scramble_fix.rds")
