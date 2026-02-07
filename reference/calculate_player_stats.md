# Get Official Game Stats

**\[deprecated\]**

This function was deprecated because we have a new, much better and
harmonized approach in
[`calculate_stats()`](https://nflfastr.com/reference/calculate_stats.md).

Build columns that aggregate official passing, rushing, and receiving
stats either at the game level or at the level of the entire data frame
passed.

## Usage

``` r
calculate_player_stats(pbp, weekly = FALSE)
```

## Arguments

- pbp:

  A Data frame of NFL play-by-play data typically loaded with
  [`load_pbp()`](https://nflreadr.nflverse.com/reference/load_pbp.html)
  or
  [`build_nflfastR_pbp()`](https://nflfastr.com/reference/build_nflfastR_pbp.md).
  If the data doesn't include the variable `qb_epa`, the function
  [`add_qb_epa()`](https://nflfastr.com/reference/add_qb_epa.md) will be
  called to add it.

- weekly:

  If `TRUE`, returns week-by-week stats, otherwise, stats for the entire
  Data frame.

## Value

A data frame including the following columns (all ID columns are decoded
to the gsis ID format):

- player_id:

  ID of the player. Use this to join to other sources.

- player_name:

  Name of the player

- player_display_name:

  Full name of the player

- position:

  Position of the player

- position_group:

  Position group of the player

- headshot_url:

  URL to a player headshot image

- games:

  The number of games where the player recorded passing, rushing or
  receiving stats.

- recent_team:

  Most recent team player appears in `pbp` with.

- season:

  Season if `weekly` is `TRUE`

- week:

  Week if `weekly` is `TRUE`

- season_type:

  `REG` or `POST` if `weekly` is `TRUE`

- opponent_team:

  The player's opponent team if `weekly` is `TRUE`

- completions:

  The number of completed passes.

- attempts:

  The number of pass attempts as defined by the NFL.

- passing_yards:

  Yards gained on pass plays.

- passing_tds:

  The number of passing touchdowns.

- interceptions:

  The number of interceptions thrown.

- sacks:

  The Number of times sacked.

- sack_yards:

  Yards lost on sack plays.

- sack_fumbles:

  The number of sacks with a fumble.

- sack_fumbles_lost:

  The number of sacks with a lost fumble.

- passing_air_yards:

  Passing air yards (includes incomplete passes).

- passing_yards_after_catch:

  Yards after the catch gained on plays in which player was the passer
  (this is an unofficial stat and may differ slightly between different
  sources).

- passing_first_downs:

  First downs on pass attempts.

- passing_epa:

  Total expected points added on pass attempts and sacks. NOTE: this
  uses the variable `qb_epa`, which gives QB credit for EPA for up to
  the point where a receiver lost a fumble after a completed catch and
  makes EPA work more like passing yards on plays with fumbles.

- passing_2pt_conversions:

  Two-point conversion passes.

- pacr:

  Passing Air Conversion Ratio. PACR = `passing_yards` /
  `passing_air_yards`

- dakota:

  Adjusted EPA + CPOE composite based on coefficients which best predict
  adjusted EPA/play in the following year.

- carries:

  The number of official rush attempts (incl. scrambles and kneel
  downs). Rushes after a lateral reception don't count as carry.

- rushing_yards:

  Yards gained when rushing with the ball (incl. scrambles and kneel
  downs). Also includes yards gained after obtaining a lateral on a play
  that started with a rushing attempt.

- rushing_tds:

  The number of rushing touchdowns (incl. scrambles). Also includes
  touchdowns after obtaining a lateral on a play that started with a
  rushing attempt.

- rushing_fumbles:

  The number of rushes with a fumble.

- rushing_fumbles_lost:

  The number of rushes with a lost fumble.

- rushing_first_downs:

  First downs on rush attempts (incl. scrambles).

- rushing_epa:

  Expected points added on rush attempts (incl. scrambles and kneel
  downs).

- rushing_2pt_conversions:

  Two-point conversion rushes

- receptions:

  The number of pass receptions. Lateral receptions officially don't
  count as reception.

- targets:

  The number of pass plays where the player was the targeted receiver.

- receiving_yards:

  Yards gained after a pass reception. Includes yards gained after
  receiving a lateral on a play that started as a pass play.

- receiving_tds:

  The number of touchdowns following a pass reception. Also includes
  touchdowns after receiving a lateral on a play that started as a pass
  play.

- receiving_air_yards:

  Receiving air yards (incl. incomplete passes).

- receiving_yards_after_catch:

  Yards after the catch gained on plays in which player was receiver
  (this is an unofficial stat and may differ slightly between different
  sources).

- receiving_fumbles:

  The number of fumbles after a pass reception.

- receiving_fumbles_lost:

  The number of fumbles lost after a pass reception.

- receiving_2pt_conversions:

  Two-point conversion receptions

- racr:

  Receiver Air Conversion Ratio. RACR = `receiving_yards` /
  `receiving_air_yards`

- target_share:

  The share of targets of the player in all targets of his team

- air_yards_share:

  The share of receiving_air_yards of the player in all air_yards of his
  team

- wopr:

  Weighted Opportunity Rating. WOPR = 1.5 × `target_share` + 0.7 ×
  `air_yards_share`

- fantasy_points:

  Standard fantasy points.

- fantasy_points_ppr:

  PPR fantasy points.

## See also

The function
[`load_player_stats()`](https://nflreadr.nflverse.com/reference/load_player_stats.html)
and the corresponding examples on [the nflfastR
website](https://nflfastr.com/articles/nflfastR.html#example-11-replicating-official-stats)

## Examples

``` r
# \donttest{
try({# to avoid CRAN test problems
# pbp <- nflfastR::load_pbp(2020)

# weekly <- calculate_player_stats(pbp, weekly = TRUE)
# dplyr::glimpse(weekly)

# overall <- calculate_player_stats(pbp, weekly = FALSE)
# dplyr::glimpse(overall)
})
#> NULL
# }
```
