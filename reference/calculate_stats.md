# Calculate NFL Stats

Compute various NFL stats based off nflverse Play-by-Play data.

## Usage

``` r
calculate_stats(
  seasons = nflreadr::most_recent_season(),
  summary_level = c("season", "week"),
  stat_type = c("player", "team"),
  season_type = c("REG", "POST", "REG+POST"),
  pbp = NULL
)
```

## Arguments

- seasons:

  A numeric vector of 4-digit years associated with given NFL seasons -
  defaults to latest season. If set to TRUE, returns all available data
  since 1999. Ignored if argument `pbp` is not `NULL`.

- summary_level:

  Summarize stats by `"season"` or `"week"`.

- stat_type:

  Calculate `"player"` level stats or `"team"` level stats.

- season_type:

  One of `"REG"`, `"POST"`, or `"REG+POST"`. Filters data to regular
  season ("REG"), post season ("POST") or keeps all data. Only applied
  if `summary_level` == `"season"`.

- pbp:

  This argument allows passing a subset of nflverse play-by-play data,
  created with
  [`build_nflfastR_pbp()`](https://nflfastr.com/reference/build_nflfastR_pbp.md)
  or loaded with
  [`load_pbp()`](https://nflreadr.nflverse.com/reference/load_pbp.html).
  Stats are then calculated based on the `game_id`s and `play_id`s in
  this subset of play-by-play data, rather then using the seasons
  specified in the `seasons` argument. The function will error if
  required variables are missing from the subset, but lists which
  variables are missing. If `pbp = NULL` (the default), all available
  games and plays from the `seasons` argument are used to calculate
  stats. Please use this responsibly, because the output is structurally
  identical to full seasons, even if plays have been filtered out. It
  may then appear as if the stats are incorrect. If `pbp` is not `NULL`,
  the function will add the attribute `"custom_pbp" = TRUE` to the
  function output to help identify stats that are possibly based on
  play-by-play subsets.

## Value

A tibble of player/team stats summarized by season/week.

## See also

[nfl_stats_variables](https://nflfastr.com/reference/nfl_stats_variables.md)
for a description of all variables.

<https://nflfastr.com/articles/stats_variables.html> for a searchable
table of the stats variable descriptions.

## Examples

``` r
# \donttest{
try({# to avoid CRAN test problems
stats <- calculate_stats(2023, "season", "player")
str(
  stats,
  list.len = ncol(stats),
  vec.len = 10,
  give.attr = FALSE,
  strict.width = "cut"
)
})
#> tibble [1,943 × 148] (S3: tbl_df/tbl/data.frame)
#>  $ player_id                  : chr [1:1943] "00-0022531" "00-0023459" "00-00"..
#>  $ player_name                : chr [1:1943] "J.Peters" "A.Rodgers" "M.Prater"..
#>  $ player_display_name        : chr [1:1943] "Jason Peters" "Aaron Rodgers" ""..
#>  $ position                   : chr [1:1943] "OT" "QB" "K" "TE" "K" "K" "LS" "..
#>  $ position_group             : chr [1:1943] "OL" "QB" "SPEC" "TE" "SPEC" "SP"..
#>  $ headshot_url               : chr [1:1943] "https://static.www.nfl.com/imag"..
#>  $ season                     : int [1:1943] 2023 2023 2023 2023 2023 2023 202..
#>  $ season_type                : chr [1:1943] "REG" "REG" "REG" "REG" "REG" "R"..
#>  $ recent_team                : chr [1:1943] "SEA" "NYJ" "ARI" "CHI" "TEN" "N"..
#>  $ games                      : int [1:1943] 2 1 17 6 17 3 2 5 1 16 5 15 2 8 1..
#>  $ completions                : int [1:1943] 0 0 0 0 0 0 0 123 0 0 0 326 23 0 ..
#>  $ attempts                   : int [1:1943] 0 1 0 0 0 0 0 204 0 0 0 521 42 0 ..
#>  $ passing_yards              : int [1:1943] 0 0 0 0 0 0 0 1616 0 0 0 3965 231..
#>  $ passing_tds                : int [1:1943] 0 0 0 0 0 0 0 13 0 0 0 24 0 0 0 0..
#>  $ passing_interceptions      : int [1:1943] 0 0 0 0 0 0 0 8 0 0 0 11 2 0 0 0 ..
#>  $ sacks_suffered             : int [1:1943] 0 1 0 0 0 0 0 8 0 0 0 30 1 0 0 0 ..
#>  $ sack_yards_lost            : int [1:1943] 0 -10 0 0 0 0 0 -57 0 0 0 -205 -8..
#>  $ sack_fumbles               : int [1:1943] 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0 0 0..
#>  $ sack_fumbles_lost          : int [1:1943] 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0..
#>  $ passing_air_yards          : int [1:1943] 0 17 0 0 0 0 0 1910 0 0 0 4033 34..
#>  $ passing_yards_after_catch  : int [1:1943] 0 0 0 0 0 0 0 678 0 0 0 1890 80 0..
#>  $ passing_first_downs        : int [1:1943] 0 0 0 0 0 0 0 60 0 0 0 187 14 0 1..
#>  $ passing_epa                : num [1:1943] NA -1.67 NA NA NA NA NA 2.25 NA N..
#>  $ passing_cpoe               : num [1:1943] NA NA NA NA NA NA NA -2.88 NA NA ..
#>  $ passing_2pt_conversions    : int [1:1943] 0 0 0 0 0 0 0 1 0 0 0 1 0 0 0 0 0..
#>  $ pacr                       : num [1:1943] NA 0 NA NA NA NA NA 0.846 NA NA N..
#>  $ passing_10                 : int [1:1943] 0 0 0 0 0 0 0 52 0 0 0 154 9 0 1 ..
#>  $ passing_16                 : int [1:1943] 0 0 0 0 0 0 0 32 0 0 0 82 2 0 1 0..
#>  $ passing_20                 : int [1:1943] 0 0 0 0 0 0 0 27 0 0 0 62 2 0 0 0..
#>  $ passing_40                 : int [1:1943] 0 0 0 0 0 0 0 9 0 0 0 8 1 0 0 0 0..
#>  $ carries                    : int [1:1943] 0 0 0 0 0 0 0 9 0 0 0 21 3 0 0 0 ..
#>  $ rushing_yards              : int [1:1943] 0 0 0 0 0 0 0 2 0 0 0 65 -3 0 0 0..
#>  $ rushing_tds                : int [1:1943] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0..
#>  $ rushing_fumbles            : int [1:1943] 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0..
#>  $ rushing_fumbles_lost       : int [1:1943] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0..
#>  $ rushing_first_downs        : int [1:1943] 0 0 0 0 0 0 0 3 0 0 0 4 0 0 0 0 0..
#>  $ rushing_epa                : num [1:1943] NA NA NA NA NA NA NA -0.112 NA NA..
#>  $ rushing_2pt_conversions    : int [1:1943] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0..
#>  $ rushing_10                 : int [1:1943] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0..
#>  $ rushing_12                 : int [1:1943] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0..
#>  $ rushing_20                 : int [1:1943] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0..
#>  $ rushing_40                 : int [1:1943] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0..
#>  $ receptions                 : int [1:1943] 0 0 0 4 0 0 0 0 0 0 0 0 0 0 0 0 0..
#>  $ targets                    : int [1:1943] 0 0 0 5 0 0 0 0 0 0 0 0 0 0 0 0 0..
#>  $ receiving_yards            : int [1:1943] 0 0 0 29 0 0 0 0 0 0 0 0 0 0 0 0 ..
#>  $ receiving_tds              : int [1:1943] 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0..
#>  $ receiving_fumbles          : int [1:1943] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0..
#>  $ receiving_fumbles_lost     : int [1:1943] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0..
#>  $ receiving_air_yards        : int [1:1943] 0 0 0 9 0 0 0 0 0 0 0 0 0 0 0 0 0..
#>  $ receiving_yards_after_catch: int [1:1943] 0 0 0 29 0 0 0 0 0 0 0 0 0 0 0 0 ..
#>  $ receiving_first_downs      : int [1:1943] 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0..
#>  $ receiving_epa              : num [1:1943] NA NA NA 2.28 NA NA NA NA NA NA N..
#>  $ receiving_2pt_conversions  : int [1:1943] 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0..
#>  $ receiving_10               : int [1:1943] 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0..
#>  $ receiving_16               : int [1:1943] 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0..
#>  $ receiving_20               : int [1:1943] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0..
#>  $ receiving_40               : int [1:1943] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0..
#>  $ racr                       : num [1:1943] NA NA NA 3.22 NA NA NA NA NA NA N..
#>  $ target_share               : num [1:1943] 0 0 0 0.0105 0 0 0 0 0 0 0 0 ...
#>  $ air_yards_share            : num [1:1943] 0 0 0 0.00238 0 0 0 0 0 0 0 0 ...
#>  $ wopr                       : num [1:1943] 0 0 0 0.0174 0 0 0 0 0 0 0 0 ...
#>  $ special_teams_tds          : int [1:1943] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0..
#>  $ def_tackles_solo           : int [1:1943] 0 0 0 0 0 0 1 0 1 25 4 0 0 0 0 0 ..
#>  $ def_tackles_with_assist    : int [1:1943] 0 0 0 0 0 0 0 0 0 7 1 0 0 0 0 0 0..
#>  $ def_tackle_assists         : int [1:1943] 0 0 0 0 0 0 1 0 0 24 1 0 0 0 0 1 ..
#>  $ def_tackles_for_loss       : int [1:1943] 0 0 0 0 0 0 0 0 0 9 0 0 0 0 0 0 0..
#>  $ def_tackles_for_loss_yards : int [1:1943] 0 0 0 0 0 0 0 0 0 35 0 0 0 0 0 0 ..
#>  $ def_fumbles_forced         : int [1:1943] 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0..
#>  $ def_sacks                  : num [1:1943] 0 0 0 0 0 0 0 0 0 6.5 0 0 0 0 0 0..
#>  $ def_sack_yards             : num [1:1943] 0 0 0 0 0 0 0 0 0 29 0 0 0 0 0 0 ..
#>  $ def_qb_hits                : int [1:1943] 0 0 0 0 0 0 0 0 0 17 0 0 0 0 0 0 ..
#>  $ def_interceptions          : int [1:1943] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0..
#>  $ def_interception_yards     : int [1:1943] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0..
#>  $ def_pass_defended          : int [1:1943] 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0..
#>  $ def_tds                    : int [1:1943] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0..
#>  $ def_fumbles                : int [1:1943] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0..
#>  $ def_safeties               : int [1:1943] 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0..
#>  $ def_punt_blocks            : int [1:1943] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0..
#>  $ def_pat_blocks             : int [1:1943] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0..
#>  $ def_fg_blocks              : int [1:1943] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0..
#>  $ def_2pt_atts               : int [1:1943] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0..
#>  $ def_2pt_made               : int [1:1943] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0..
#>  $ misc_yards                 : int [1:1943] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0..
#>  $ fumble_recovery_own        : int [1:1943] 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0..
#>  $ fumble_recovery_yards_own  : int [1:1943] 0 0 0 0 0 0 0 -2 0 0 0 0 0 0 0 0 ..
#>  $ fumble_recovery_opp        : int [1:1943] 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0..
#>  $ fumble_recovery_yards_opp  : int [1:1943] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0..
#>  $ fumble_recovery_tds        : int [1:1943] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0..
#>  $ penalties                  : int [1:1943] 2 0 0 2 0 0 0 1 0 1 2 3 0 0 0 0 0..
#>  $ penalty_yards              : int [1:1943] 9 0 0 15 0 0 0 11 0 5 15 32 0 0 0..
#>  $ fumbles_forced_by_opp      : int [1:1943] 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0..
#>  $ fumbles_not_forced         : int [1:1943] 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0..
#>  $ fumbles_out_of_bounds      : int [1:1943] 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0..
#>  $ fumbles_total              : int [1:1943] 0 0 0 0 0 0 0 4 0 0 0 0 0 0 0 0 0..
#>  $ fumbles_lost_total         : int [1:1943] 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0..
#>  $ punt_returns               : int [1:1943] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0..
#>  $ punt_return_yards          : int [1:1943] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0..
#>  $ kickoff_returns            : int [1:1943] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0..
#>  $ kickoff_return_yards       : int [1:1943] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0..
#>  $ fg_made                    : int [1:1943] 0 0 28 0 29 5 0 0 0 0 0 0 0 11 0 ..
#>  $ fg_att                     : int [1:1943] 0 0 33 0 30 7 0 0 0 0 0 0 0 17 0 ..
#>  $ fg_missed                  : int [1:1943] 0 0 5 0 1 2 0 0 0 0 0 0 0 5 0 0 0..
#>  $ fg_blocked                 : int [1:1943] 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0..
#>  $ fg_long                    : int [1:1943] NA NA 62 NA 53 52 NA NA NA NA NA ..
#>  $ fg_pct                     : num [1:1943] NA NA 0.848 NA 0.967 0.714 NA NA ..
#>  $ fg_made_0_19               : int [1:1943] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0..
#>  $ fg_made_20_29              : int [1:1943] 0 0 6 0 9 2 0 0 0 0 0 0 0 2 0 0 0..
#>  $ fg_made_30_39              : int [1:1943] 0 0 7 0 9 2 0 0 0 0 0 0 0 3 0 0 0..
#>  $ fg_made_40_49              : int [1:1943] 0 0 6 0 6 0 0 0 0 0 0 0 0 3 0 0 0..
#>  $ fg_made_50_59              : int [1:1943] 0 0 8 0 5 1 0 0 0 0 0 0 0 3 0 0 0..
#>  $ fg_made_60_                : int [1:1943] 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0..
#>  $ fg_missed_0_19             : int [1:1943] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0..
#>  $ fg_missed_20_29            : int [1:1943] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0..
#>  $ fg_missed_30_39            : int [1:1943] 0 0 1 0 0 0 0 0 0 0 0 0 0 2 0 0 0..
#>  $ fg_missed_40_49            : int [1:1943] 0 0 1 0 0 1 0 0 0 0 0 0 0 2 0 0 0..
#>  $ fg_missed_50_59            : int [1:1943] 0 0 3 0 1 1 0 0 0 0 0 0 0 1 0 0 0..
#>  $ fg_missed_60_              : int [1:1943] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0..
#>  $ fg_made_list               : chr [1:1943] NA NA "28;54;37;37;44;39;43;62;3"..
#>  $ fg_missed_list             : chr [1:1943] NA NA "55;34;56;43;51" NA "51" ""..
#>  $ fg_blocked_list            : chr [1:1943] NA NA NA NA NA NA NA NA NA NA ...
#>  $ fg_made_distance           : int [1:1943] 0 0 1174 0 1083 160 0 0 0 0 0 0 0..
#>  $ fg_missed_distance         : int [1:1943] 0 0 239 0 51 99 0 0 0 0 0 0 0 215..
#>  $ fg_blocked_distance        : int [1:1943] 0 0 0 0 0 0 0 0 0 0 0 0 0 45 0 0 ..
#>  $ pat_made                   : int [1:1943] 0 0 22 0 28 6 0 0 0 0 0 0 0 8 0 0..
#>  $ pat_att                    : int [1:1943] 0 0 23 0 30 7 0 0 0 0 0 0 0 8 0 0..
#>  $ pat_missed                 : int [1:1943] 0 0 1 0 2 1 0 0 0 0 0 0 0 0 0 0 0..
#>  $ pat_blocked                : int [1:1943] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0..
#>  $ pat_pct                    : num [1:1943] NA NA 0.957 NA 0.933 0.857 NA NA ..
#>  $ gwfg_made                  : int [1:1943] 0 0 1 0 1 0 0 0 0 0 0 0 0 1 0 0 0..
#>  $ gwfg_att                   : int [1:1943] 0 0 2 0 1 1 0 0 0 0 0 0 0 1 0 0 0..
#>  $ gwfg_missed                : int [1:1943] 0 0 1 0 0 1 0 0 0 0 0 0 0 0 0 0 0..
#>  $ gwfg_blocked               : int [1:1943] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0..
#>  $ gwfg_distance_list         : chr [1:1943] NA NA "23;51" NA "41" "54" NA NA ..
#>  $ pt_att                     : int [1:1943] 0 0 0 0 2 0 0 0 0 0 0 0 0 0 99 0 ..
#>  $ pt_blocked                 : int [1:1943] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0..
#>  $ pt_long                    : int [1:1943] NA NA NA NA 41 NA NA NA NA NA NA ..
#>  $ pt_yards                   : int [1:1943] 0 0 0 0 81 0 0 0 0 0 0 0 0 0 4831..
#>  $ pt_inside_20               : int [1:1943] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 36 0 ..
#>  $ pt_out_of_bounds           : int [1:1943] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 8 0 0..
#>  $ pt_downed                  : int [1:1943] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 7 0 0..
#>  $ pt_touchback               : int [1:1943] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 8 0 0..
#>  $ pt_fair_caught             : int [1:1943] 0 0 0 0 1 0 0 0 0 0 0 0 0 0 28 0 ..
#>  $ pt_returned                : int [1:1943] 0 0 0 0 1 0 0 0 0 0 0 0 0 0 48 0 ..
#>  $ pt_return_yards            : int [1:1943] 0 0 0 0 3 0 0 0 0 0 0 0 0 0 535 0..
#>  $ pt_return_tds              : int [1:1943] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0..
#>  $ pt_net_yards               : int [1:1943] 0 0 0 0 78 0 0 0 0 0 0 0 0 0 4136..
#>  $ fantasy_points             : num [1:1943] 0 0 0 8.9 0 0 0 100.8 0 0 0 243.1..
#>  $ fantasy_points_ppr         : num [1:1943] 0 0 0 12.9 0 0 0 100.8 0 0 0 243...
# }
```
