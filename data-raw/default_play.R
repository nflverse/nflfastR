### Create datatype dataframe
### This is a db that is stored on Seb's local machine
connection <- DBI::dbConnect(duckdb::duckdb(), "../_data_cache/pbp_db")
pbp_db <- dplyr::tbl(connection, "nflfastR_pbp")

### This is heavy, only run if necessary
sk <- pbp_db |>
  dplyr::collect() |>
  skimr::skim()

readr::write_csv(sk, "data-raw/pbp_datatypes.csv")

sk <- readr::read_csv("data-raw/pbp_datatypes.csv")

random_play <- pbp_db |>
  dplyr::filter(season == 1999) |>
  dplyr::collect() |>
  dplyr::slice_sample(n = 1) |>
  as.list()

default_play <-
  purrr::map(seq_along(random_play), function(i, play, sk){
    val <- play[[i]]
    var <- names(play[i])
    if(is.character(val)){
      max_char <- sk$character.max[sk$skim_variable == var]
      rnd_char <- ifelse(is.na(val), 0L, nchar(val))
      if (is.na(max_char)){
        max_char <- switch (
          var,
          "lateral_sack_player_id" = 10L,
          "lateral_sack_player_name" = 20L,
          "tackle_with_assist_2_player_id" = 10L,
          "tackle_with_assist_2_player_name" = 20L,
          "tackle_with_assist_2_team" = 3L,
          "drive_real_start_time" = 20L
        )
      }
      if (max_char > rnd_char){
        val <- strsplit(val, character(0L))[[1]] |>
          sample(size = max_char, replace = TRUE) |>
          paste0(collapse = "")
      }
    }
    val
  }, play = random_play, sk = sk) |>
  purrr::set_names(names(random_play)) |>
  tibble::as_tibble_row() |>
  dplyr::mutate(game_id = "9999_99_DEF_TYP")

readr::write_csv(default_play, "data-raw/pbp_defaultplay.csv")
saveRDS(default_play, "data-raw/pbp_defaultplay.rds")
