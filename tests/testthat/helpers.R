# function to take some games and return
# the first play description from each game
# on a 1st down play
extract_desc <- function(pbp) {
  pbp %>%
    dplyr::filter(down == 1) %>%
    dplyr::group_by(game_id) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::pull(desc) %>%
    return()
}

# for checking play descriptions after scraping
# the first is the same for both sources since it's a 2009 game
desc_1_nfl_source <- "(14:53) B.Roethlisberger pass short left to H.Ward to PIT 47 for 5 yards (C.Hope)."
desc_2_nfl_source <- "(15:00) 33-A.Jones left tackle to GB 25 for no gain (58-R.Smith)."

desc_2_live_source <- "(15:00) A.Jones left tackle to GB 25 for no gain (R.Smith)."

# sample games we'll use to check with
game_ids <- c("2009_01_TEN_PIT", "2019_01_GB_CHI")
old_game_ids <- c("2009091000", "2019090500")
