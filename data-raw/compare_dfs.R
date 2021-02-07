library(tidyverse)

# function for comparing revisions against data in repo
# make sure to build package first
compare_pbp <- function(id, cols) {

  s <- substr(id, 1, 4) %>% as.integer()
  # no idea why this is necessary
  game <- id

  new_pbp <- build_nflfastR_pbp(id) %>%
    select(all_of(cols)) %>%
    # necessary to pass the equality checks
    mutate(
      ep = round(ep, 2),
      epa = round(epa, 2),
      vegas_home_wp = round(vegas_home_wp, 2)
    )
  repo_pbp <- readRDS(url(glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{s}.rds"))) %>%
    filter(game_id == game) %>%
    select(all_of(cols)) %>%
    mutate(
      ep = round(ep, 2),
      epa = round(epa, 2),
      vegas_home_wp = round(vegas_home_wp, 2)
    )

  sum <- summary(arsenal::comparedf(new_pbp, repo_pbp))
  dfs <- bind_cols(new_pbp, repo_pbp)

  return(
    list(sum, dfs)
  )


}

cols <- c("game_id", "play_id", "desc", "play_type", "posteam", "yardline_100", "down", "ydstogo", "ep", "epa", "vegas_home_wp")
id <- "2020_16_MIN_NO"

compared <- compare_pbp(
  id = id,
  cols = cols
)

# summary table
compared[[1]]

# get row numbers of things with differences
obs <- compared[[1]]$diffs.table$..row.names.. %>% unique()

# dfs with differences
compared[[2]][obs, ]

