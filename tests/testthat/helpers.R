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
desc_1_nfl_source <- "(15:00) J.Anderson to ATL 22 for 3 yards (K.Wong, E.McDaniel)."
desc_2_nfl_source <- "(15:00) 33-A.Jones left tackle to GB 25 for no gain (58-R.Smith)."

# sample games we'll use to check with
game_ids <- c("1999_01_MIN_ATL", "2019_01_GB_CHI")

