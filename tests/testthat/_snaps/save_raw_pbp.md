# missing_raw_pbp() returns finished games without local file

    Code
      missing <- missing_raw_pbp(dir)
    Message
      i You are missing 1 game file. The oldest missing game is "2023_01_CAR_ATL". The most recent missing game is "2023_01_CAR_ATL".

# missing_raw_pbp() handles empty dir and no missing games

    Code
      out <- missing_raw_pbp(dir)
    Message
      v No missing games!

# missing_raw_pbp() and save_raw_pbp() validate dir

    Code
      missing_raw_pbp(dir = NULL)
    Condition
      Error in `missing_raw_pbp()`:
      ! Invalid argument `dir`. Do you need to set `options(nflfastR.raw_directory)`?
    Code
      missing_raw_pbp(dir = "does/not/exist")
    Condition
      Error in `missing_raw_pbp()`:
      ! You've asked to check raw pbp in 'does/not/exist' which doesn't exist. Please create it.
    Code
      save_raw_pbp("2023_01_DET_KC", dir = NULL)
    Condition
      Error in `save_raw_pbp()`:
      ! Invalid argument `dir`. Do you need to set `options(nflfastR.raw_directory)`?
    Code
      save_raw_pbp("2023_01_DET_KC", dir = "does/not/exist")
    Condition
      Error in `save_raw_pbp()`:
      ! You've asked to save raw pbp to 'does/not/exist' which doesn't exist. Please create it.

# verify_game_ids() flags invalid game IDs

    Code
      verify_game_ids(c("2021_02_LAC_KC", "Hello World"))
    Condition
      Error in `verify_game_ids()`:
      ! The game ID "Hello World" seems to be invalid!

---

    Code
      verify_game_ids(c("Hello World", "2028_01_LAC_JAX"))
    Condition
      Error in `verify_game_ids()`:
      ! The game IDs "Hello World" and "2028_01_LAC_JAX" seem to be invalid!

