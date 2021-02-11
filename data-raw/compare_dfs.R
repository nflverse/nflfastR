library(tidyverse)

# function for comparing revisions against data in repo
# make sure to build package first
compare_pbp <- function(id, cols) {

  s <- substr(id, 1, 4) %>% as.integer()
  # no idea why this is necessary
  game <- id

  new_pbp <- build_nflfastR_pbp(
    id
    # comment this out to use the "normal" way
    , dir = "../nflfastR-raw/raw_old"
    ) %>%
    filter(!stringr::str_detect(desc, "GAME")) %>%
    select(all_of(cols)) %>%
    # necessary to pass the equality checks
    mutate(
      ep = round(ep, 2),
      epa = round(epa, 2),
      vegas_home_wp = round(vegas_home_wp, 2)
    )

  repo_pbp <- readRDS(url(glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{s}.rds"))) %>%
    filter(game_id == game) %>%
    filter(!stringr::str_detect(desc, "GAME")) %>%
    select(all_of(cols)) %>%
    mutate(
      ep = round(ep, 2),
      epa = round(epa, 2),
      vegas_home_wp = round(vegas_home_wp, 2)
    )

  sum <- arsenal::diffs(arsenal::comparedf(
    new_pbp %>% select(-desc),
    repo_pbp %>% select(-desc)
    ))
  dfs <- bind_cols(
    new_pbp %>% select(-desc),
    repo_pbp %>% select(-desc))

  dfs$desc <- new_pbp$desc

  return(
    list(sum, dfs)
  )


}

cols <- c(
  # DO NOT REMOVE THESE ONES OR THE COMPARISON WILL BREAK
  "play_id", "desc", "ep", "epa", "vegas_home_wp",

  # here is stuff you can choose whether to include
  "posteam", "home_team", "away_team"
  # , "posteam_timeouts_remaining", "defteam_timeouts_remaining"
  )

id <- "2002_05_PHI_JAX"
id <- "2006_01_MIA_PIT"
id <- "2006_02_PIT_JAX"
id <- "2006_03_JAX_IND"
id <- "2006_04_JAX_WAS"

compared <- compare_pbp(
  id = id,
  cols = cols
)

# summary table
compared[[1]]

# get row numbers of things with differences
obs <- compared[[1]]$..row.names.. %>% unique()

# dfs
compared[[2]] %>% arrange(play_id)

# dfs with differences
compared[[2]][obs, ] %>% arrange(play_id)

# play description of plays with differences
compared[[2]][obs, ] %>% arrange(play_id) %>% select(desc)



