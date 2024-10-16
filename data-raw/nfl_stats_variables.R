s1 <- calculate_stats(2023, "season", "player")
s2 <- calculate_stats(2023, "week",   "player")
s3 <- calculate_stats(2023, "season", "team")
s4 <- calculate_stats(2023, "week",   "team")

n1 <- names(s1)
n2 <- names(s2)
n3 <- names(s3)
n4 <- names(s4)

setdiff(n1, n2)
setdiff(n2, n1)

setdiff(n1, n3)
setdiff(n3, n1)

# tibble::tibble(
#   variable = c(n1, n2, n3, n4) %>% unique(),
#   description = ""
# ) %>%
#   jsonlite::write_json("data-raw/nfl_stats_variables.json", pretty = TRUE)

nfl_stats_variables <- jsonlite::fromJSON("data-raw/nfl_stats_variables.json")

usethis::use_data(nfl_stats_variables)

s_old_1 <- nflreadr::load_player_stats(2024, "offense")
s_old_2 <- nflreadr::load_player_stats(2024, "defense")
s_old_3 <- nflreadr::load_player_stats(2024, "kicking")
n_old_1 <- names(s_old_1)
n_old_2 <- names(s_old_2)
n_old_3 <- names(s_old_3)

setdiff(n2, n_old_1)
setdiff(n2, n_old_3)
setdiff(n_old_1, n2)
