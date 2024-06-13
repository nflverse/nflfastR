library(tidyverse)

# yards gained needed to match some plays in older years but not available to match in 2005. set it equal to 0 for 2005

pbp <- nflfastR::load_pbp(1999 : 2005) %>%
  select(season, game_id, play_id, week, away_team, home_team, posteam, qtr, down, ydstogo, time, yards_gained) |>
  mutate(yards_gained = ifelse(season == 2005, 0, yards_gained))

# Thank you to Aaron Schatz and Football Outsiders
# For the charting data to fix scrambles in 2005
s <- readxl::read_xlsx("data-raw/scrambles_2005.xlsx") %>%
  as_tibble() |>
  janitor::clean_names() %>%
  select(
    season = year, week, qtr, away_team = away, home_team = home, posteam = offense, down, ydstogo = togo, date_time = time
  ) |>
  mutate(yards_gained = 0)

# Thank you to Aaron Schatz
# For the charting data to fix scrambles in 1999 - 2004
s2 <- readxl::read_xlsx("data-raw/Scrambles 1999-2004 UPDATE for NFLfastR.xlsx", sheet = 1) |>
  as_tibble() |>
  janitor::clean_names() |>
  filter(type %in% c("scramble", "assume scramble")) %>%
  select(
    season = year, week, qtr, away_team = away, home_team = home, posteam = offense, down, ydstogo = togo, date_time = time, yards_gained = yards
  )

dat <- bind_rows(
  s2,
  s
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
    pbp,
    by = c("week", "away_team", "home_team", "posteam", "qtr", "down", "ydstogo", "time", "season", "yards_gained")
  ) %>%
  mutate(scramble_id = paste0(game_id, "_", play_id)) %>%
  filter(scramble_id != "2005_09_CIN_BAL_1725")

scramble_fix <- d$scramble_id
saveRDS(scramble_fix, file = "data-raw/scramble_fix.rds")


