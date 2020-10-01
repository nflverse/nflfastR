library(nflfastR)
library(tidyverse)

build_nflfastR_pbp <- function(game_ids, source = "nfl", pp = FALSE, ..., decode = FALSE, rules = TRUE) {
  if (rules){pkgdown:::rule("Build nflfastR play-by-play data")}

  game_count <- length(game_ids)

  if (game_count > 1) {
    usethis::ui_todo("Start download of {length(game_ids)} game(s)...")
  } else {
    usethis::ui_todo("Start download of {length(game_ids)} game...")
  }

  ret <- fast_scraper(game_ids = game_ids, source = source, pp = pp, ...) %>%
    add_xyac() %>%
    clean_pbp() %>%
    add_qb_epa() # message is missing for qb epa

  if(decode){ret <- decode_player_ids(ret)}
  if (rules){pkgdown:::rule("DONE")}
  return(ret)
}

test <- build_nflfastR_pbp(fast_scraper_schedules(2020) %>% head(1) %>% pull(game_id), decode = TRUE)
