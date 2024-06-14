library(tidyverse)

pbp <- nflfastR::load_pbp(1999 : 2005) %>%
  # plays that could plausibly be scramble
  filter(
    !is.na(rusher_player_id) | penalty == 1,
    is.na(passer_player_id),
    is.na(receiver_player_id)
    ) |>
  select(season, game_id, play_id, week, away_team, home_team, posteam, qtr, down, ydstogo, time, desc) |>
  # not in scramble data this year
  mutate(
    time = case_when(
      nchar(time) == 3 ~ paste0("00", time),
      nchar(time) == 4 ~ paste0("0", time),
      TRUE ~ time
    )
    )

# Thank you to Aaron Schatz and Football Outsiders
# For the charting data to fix scrambles in 2005
s <- readxl::read_xlsx("data-raw/scrambles_2005.xlsx") %>%
  as_tibble() |>
  janitor::clean_names() %>%
  select(
    season = year, week, qtr, away_team = away, home_team = home, posteam = offense, down, ydstogo = togo, date_time = time
  )

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
    by = c("week", "away_team", "home_team", "posteam", "qtr", "down", "ydstogo", "time", "season")
  ) %>%
  mutate(scramble_id = paste0(game_id, "_", play_id)) %>%
  filter(scramble_id != "2005_09_CIN_BAL_1725")

# number non-matched by season
  nrow(d)
  d |> filter(is.na(desc)) |> group_by(season) |> summarise(n = n())

# get rid of non-match
  d <- d |>
    filter(!is.na(desc))
  d |> group_by(season) |> summarise(n = n())

scramble_fix <- d$scramble_id
scramble_fix <- scramble_fix |>
  unique()
length(scramble_fix)
saveRDS(scramble_fix, file = "data-raw/scramble_fix.rds")



