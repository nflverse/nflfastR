# add_game_data() fails without the game data

    Code
      add_game_data(pbp, games = g)
    Condition
      Error in `add_game_data()`:
      ! Can't add the game data of 1 game: "2019_02_GB_CHI".
      i The schedules don't have this game.

---

    Code
      add_game_data(pbp, games = g[0, ])
    Condition
      Error in `add_game_data()`:
      ! Can't add the game data of 2 games: "2019_01_GB_CHI" and "2019_02_GB_CHI".
      i The schedules couldn't be loaded. Please try again later.

---

    Code
      add_game_data(pbp, games = data.frame())
    Condition
      Error in `add_game_data()`:
      ! `games` must be the schedules of `nflreadr::load_schedules()`.

