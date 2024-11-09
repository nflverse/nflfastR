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

usethis::use_data(nfl_stats_variables, overwrite = TRUE)

s_old_1 <- nflreadr::load_player_stats(2023, "offense")
s_old_2 <- nflreadr::load_player_stats(2023, "defense")
s_old_3 <- nflreadr::load_player_stats(2023, "kicking")
n_old_1 <- names(s_old_1)
n_old_2 <- names(s_old_2)
n_old_3 <- names(s_old_3)


# Differences to old offense stats ----------------------------------------

# recent_team -> team (recent team in weekly data never made sense)
# interceptions -> passing_interceptions (all passing stats have the passing prefix)
# sacks -> sacks_suffered (to make clear it's not on defensive side)
# sack_yards -> sack_yards_lost (to make clear it's not on defensive side)
# dakota -> not implemented at the moment
setdiff(n_old_1, n2)
setdiff(n2, n_old_1)

# Differences to old defense stats ----------------------------------------

# def_tackles -> there is def_tackles_solo and def_tackles_with_assist
# def_fumble_recovery_own -> fumble_recovery_own (it is not exclusive to defense)
# def_fumble_recovery_yards_own -> fumble_recovery_yards_own (it is not exclusive to defense)
# def_fumble_recovery_opp -> fumble_recovery_opp (it is not exclusive to defense)
# def_fumble_recovery_yards_opp -> fumble_recovery_yards_opp (it is not exclusive to defense)
# def_safety -> def_safeties (we use plural everywhere)
# def_penalty -> penalties (it is not exclusive to defense)
# def_penalty_yards -> penalty_yards (it is not exclusive to defense)
setdiff(n_old_2, n2)
setdiff(n2, n_old_2)


# Differences to old kicking stats ----------------------------------------

# No differences
setdiff(n_old_3, n2)
setdiff(n2, n_old_3)
