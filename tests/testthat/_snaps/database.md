# update_pbp_db() only warns about games that are still missing

    Code
      update_pbp_db(con, name = "pbp", seasons = 2024)
    Output
      -- Update nflverse Play-by-Play Data in Connected Database ---------------------
    Message
      ! <time> | Raw pbp data for the following 1 game still missing: "2024_01_PIT_ATL". Please try again in about 10 minutes.
    Output
      -- DONE ------------------------------------------------------------------------

---

    Code
      update_pbp_db(con, name = "pbp", seasons = 2024)
    Output
      -- Update nflverse Play-by-Play Data in Connected Database ---------------------
      -- DONE ------------------------------------------------------------------------

