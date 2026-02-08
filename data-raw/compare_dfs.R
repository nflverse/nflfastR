library(tidyverse)
future::plan("multisession")

# function for comparing revisions against data in repo
# make sure to build package first
compare_pbp <- function(id, cols) {
  s <- substr(id[1], 1, 4) |> as.integer()
  # no idea why this is necessary
  games <- id

  new_pbp <- build_nflfastR_pbp(
    id
    # comment this out to use the "normal" way
    # , dir = "../nflfastR-raw/raw"
  ) |>
    filter(!stringr::str_detect(desc, "GAME")) |>
    select(all_of(cols)) |>
    # necessary to pass the equality checks
    mutate(
      ep = round(ep, 2),
      epa = round(epa, 2),
      vegas_home_wp = round(vegas_home_wp, 2),
      vegas_home_wpa = round(vegas_home_wpa, 2),
      home_wp = round(home_wp, 2)
    )

  repo_pbp <- readRDS(url(glue::glue(
    "https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{s}.rds"
  ))) |>
    filter(game_id %in% games) |>
    filter(!stringr::str_detect(desc, "GAME")) |>
    select(all_of(cols)) |>
    mutate(
      ep = round(ep, 2),
      epa = round(epa, 2),
      vegas_home_wp = round(vegas_home_wp, 2),
      vegas_home_wpa = round(vegas_home_wpa, 2),
      home_wp = round(home_wp, 2)
    )

  sum <- arsenal::diffs(arsenal::comparedf(
    new_pbp |> select(-desc, -game_id, -play_id),
    repo_pbp |> select(-desc, -game_id, -play_id)
  ))
  dfs <- bind_cols(
    new_pbp |> select(-desc, -game_id, -play_id),
    repo_pbp |> select(-desc, -game_id, -play_id)
  )

  dfs$desc <- new_pbp$desc
  dfs$play_id <- new_pbp$play_id
  dfs$game_id <- new_pbp$game_id

  return(
    list(sum, dfs)
  )
}

cols <- c(
  # DO NOT REMOVE THESE ONES OR THE COMPARISON WILL BREAK
  "game_id",
  "play_id",
  "desc",
  "ep",
  "epa",
  "vegas_home_wp",
  "vegas_home_wpa",
  "home_wp"

  # here is stuff you can choose whether to include
  # , "posteam_timeouts_remaining", "defteam_timeouts_remaining"
)

id <- "2002_05_PHI_JAX"
id <- "2006_01_MIA_PIT"
id <- "2006_02_PIT_JAX"
id <- "2017_08_LAC_NE"
id <- "2006_04_JAX_WAS"
id <- "2019_01_SF_TB"
id <- "2017_12_JAX_ARI"

ids <- nflfastR::fast_scraper_schedules(2020) |>
  dplyr::slice(1:20) |>
  pull(game_id)

compared <- compare_pbp(
  id = ids,
  cols = cols
)

# summary table
compared[[1]]

# get row numbers of things with differences
obs <- compared[[1]]$..row.names.. |> unique()

# dfs
compared[[2]] |> arrange(play_id)

# dfs with differences
compared[[2]][obs, ] |> arrange(play_id)

# play description of plays with differences
compared[[2]][obs, ] |> arrange(play_id) |> select(desc)
