# calculate_stats warns about an empty season_type filter

    Code
      out <- calculate_stats(2023, "season", "team", "POST", pbp = pbp)
    Message
      ! Filtering 2023 data to `season_type` == "POST" resulted in 0 rows. Returning empty tibble.

# calculate_stats validates custom pbp

    Code
      calculate_stats(pbp = pbp)
    Condition
      Error in `calculate_stats()`:
      ! You have passed custom pbp to the argument `pbp` but it is missing the following required variables: "posteam", "defteam", "special", "season_type", "away_score", "home_score", "field_goal_attempt", "fixed_drive", "score_differential", "play_type", "week", "passer_player_id", "qb_epa", "cpoe", "rusher_player_id", "epa", and "receiver_player_id"

# load_playstats checks seasons

    Code
      load_playstats(1998)
    Condition
      Error in `load_playstats()`:
      ! seasons >= 1999 is not TRUE

---

    Code
      load_playstats("2023")
    Condition
      Error in `load_playstats()`:
      ! is.numeric(seasons) is not TRUE

