## code to prepare `CP` dataset goes here

# claculate cp models here

games <- read_csv("http://www.habitatring.com/games.csv") %>%  filter(season >= 2006 & week <= 17) %>% pull(game_id)
tictoc::tic(glue::glue('scraped {length(games)} games'))
pbp_data <- fast_scraper(games, pp = TRUE)
tictoc::toc()

## for getting the data if you don't feel like waiting for a big scrape
name = 'XXXXXXX'
pbp_data <- readRDS(glue::glue('C:/Users/{name}/Documents/github/nfl/nfl/data/pbp_all.rds')) %>%
  mutate(season = if_else(
    as.numeric(substr(as.character(game_id), 5, 6)) <= 2,
    as.numeric(substr(as.character(game_id), 1, 4)) - 1,
    as.numeric(substr(as.character(game_id), 1, 4)))
  ) %>%
  filter(season >= 2006)



  # valid pass play: at least -15 air yards, less than 70 air yards, has intended receiver, has pass location
  passes <- pbp_data %>%
    filter(complete_pass == 1 | incomplete_pass == 1 | interception == 1) %>%
    filter(!is.na(air_yards) & air_yards >= -15 & air_yards <70 & !is.na(receiver_player_id) & !is.na(pass_location)) %>%
    mutate(air_is_zero=ifelse(air_yards == 0,1,0))

  # estimate CP
  cp_models <- gam(complete_pass ~ s(air_yards) + s(yardline_100) +log(ydstogo) + air_is_zero +
                    factor(down) + factor(pass_location) + factor(season),
                  data=passes, method="REML", family = "binomial")



# Save the models using this code. It will generate the file R/sysdata.rda
usethis::use_data(cp_models, internal = TRUE, overwrite = TRUE)
