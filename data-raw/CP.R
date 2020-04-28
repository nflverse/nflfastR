# calculate cp model

#library(nflfastR)
#library(tidyverse)
seasons <- 2006:2019
pbp_data <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
    )
  )
}) %>%
  dplyr::mutate(receiver_player_name =
                 stringr::str_extract(desc, "(?<=((to)|(for))\\s[:digit:]{0,2}\\-{0,1})[A-Z][A-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?"))

  # valid pass play: at least -15 air yards, less than 70 air yards, has intended receiver, has pass location
  passes <- pbp_data %>%
    filter(complete_pass == 1 | incomplete_pass == 1 | interception == 1) %>%
    filter(!is.na(air_yards) & air_yards >= -15 & air_yards <70 & !is.na(receiver_player_name) & !is.na(pass_location)) %>%
    mutate(air_is_zero=ifelse(air_yards == 0,1,0))

  # estimate CP
  cp_models <- gam(complete_pass ~ s(air_yards) + s(yardline_100) +log(ydstogo) + air_is_zero +
                    factor(down) + factor(pass_location) + factor(season),
                  data=passes, method="REML", family = "binomial")



# Save the models using this code. It will generate the file R/sysdata.rda
usethis::use_data(cp_models, internal = TRUE, overwrite = TRUE)
