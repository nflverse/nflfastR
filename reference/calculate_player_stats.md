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
website](https://www.nflfastr.com/articles/nflfastR.html#example-11-replicating-official-stats)

## Examples

``` r
# \donttest{
try({# to avoid CRAN test problems
pbp <- nflfastR::load_pbp(2020)

weekly <- calculate_player_stats(pbp, weekly = TRUE)
dplyr::glimpse(weekly)

overall <- calculate_player_stats(pbp, weekly = FALSE)
dplyr::glimpse(overall)
})
#> Warning: `calculate_player_stats()` was deprecated in nflfastR 5.0.
#> ℹ Please use `calculate_stats()` instead.
#> Rows: 5,447
#> Columns: 53
#> $ player_id                   <chr> "00-0019596", "00-0019596", "00-0019596", …
#> $ player_name                 <chr> "T.Brady", "T.Brady", "T.Brady", "T.Brady"…
#> $ player_display_name         <chr> "Tom Brady", "Tom Brady", "Tom Brady", "To…
#> $ position                    <chr> "QB", "QB", "QB", "QB", "QB", "QB", "QB", …
#> $ position_group              <chr> "QB", "QB", "QB", "QB", "QB", "QB", "QB", …
#> $ headshot_url                <chr> "https://static.www.nfl.com/image/private/…
#> $ recent_team                 <chr> "TB", "TB", "TB", "TB", "TB", "TB", "TB", …
#> $ season                      <int> 2020, 2020, 2020, 2020, 2020, 2020, 2020, …
#> $ week                        <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 14,…
#> $ season_type                 <chr> "REG", "REG", "REG", "REG", "REG", "REG", …
#> $ opponent_team               <chr> "NO", "CAR", "DEN", "LAC", "CHI", "GB", "L…
#> $ completions                 <int> 23, 23, 25, 30, 25, 17, 33, 28, 22, 28, 26…
#> $ attempts                    <int> 36, 35, 38, 46, 41, 27, 45, 40, 38, 39, 48…
#> $ passing_yards               <dbl> 239, 217, 297, 369, 253, 166, 369, 279, 20…
#> $ passing_tds                 <int> 2, 1, 3, 5, 1, 2, 4, 2, 0, 3, 2, 3, 2, 2, …
#> $ interceptions               <dbl> 2, 1, 0, 1, 0, 0, 0, 0, 3, 0, 2, 2, 0, 0, …
#> $ sacks                       <dbl> 3, 0, 2, 0, 3, 0, 0, 2, 3, 1, 1, 1, 0, 3, …
#> $ sack_yards                  <dbl> 15, 0, 12, 0, 20, 0, 0, 16, 23, 7, 7, 3, 0…
#> $ sack_fumbles                <int> 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
#> $ sack_fumbles_lost           <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
#> $ passing_air_yards           <dbl> 292, 234, 311, 434, 383, 231, 399, 364, 36…
#> $ passing_yards_after_catch   <dbl> 90, 110, 111, 109, 100, 72, 134, 94, 80, 1…
#> $ passing_first_downs         <dbl> 10, 11, 12, 20, 11, 9, 22, 18, 10, 18, 14,…
#> $ passing_epa                 <dbl> -9.4968573, 0.5243795, 11.5597024, 12.6856…
#> $ passing_2pt_conversions     <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
#> $ pacr                        <dbl> 0.8184932, 0.9273504, 0.9549839, 0.8502304…
#> $ dakota                      <dbl> 0.06893691, 0.07635570, 0.15510344, 0.2116…
#> $ carries                     <int> 3, 1, 5, 3, 3, 0, 1, 1, 0, 2, 0, 1, 3, 2, …
#> $ rushing_yards               <dbl> 9, 0, 0, -3, 0, 0, 1, -1, 0, 2, 0, -1, -2,…
#> $ rushing_tds                 <int> 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, …
#> $ rushing_fumbles             <dbl> 0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
#> $ rushing_fumbles_lost        <dbl> 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
#> $ rushing_first_downs         <dbl> 2, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, …
#> $ rushing_epa                 <dbl> 1.5054478, -5.4885905, -3.8117262, -1.1660…
#> $ rushing_2pt_conversions     <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
#> $ receptions                  <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
#> $ targets                     <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
#> $ receiving_yards             <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
#> $ receiving_tds               <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
#> $ receiving_fumbles           <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
#> $ receiving_fumbles_lost      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
#> $ receiving_air_yards         <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
#> $ receiving_yards_after_catch <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
#> $ receiving_first_downs       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
#> $ receiving_epa               <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ receiving_2pt_conversions   <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
#> $ racr                        <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ target_share                <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ air_yards_share             <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ wopr                        <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ special_teams_tds           <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
#> $ fantasy_points              <dbl> 20.46, 8.68, 23.88, 32.46, 14.12, 14.64, 3…
#> $ fantasy_points_ppr          <dbl> 20.46, 8.68, 23.88, 32.46, 14.12, 14.64, 3…
#> Rows: 636
#> Columns: 50
#> $ player_id                   <chr> "00-0019596", "00-0020531", "00-0022127", …
#> $ player_name                 <chr> "T.Brady", "D.Brees", "J.Witten", "M.Schau…
#> $ player_display_name         <chr> "Tom Brady", "Drew Brees", "Jason Witten",…
#> $ position                    <chr> "QB", "QB", "TE", "QB", "P", "WR", "QB", "…
#> $ position_group              <chr> "QB", "QB", "TE", "QB", "SPEC", "WR", "QB"…
#> $ headshot_url                <chr> "https://static.www.nfl.com/image/private/…
#> $ games                       <int> 20, 14, 10, 1, 1, 13, 16, 17, 8, 18, 15, 9…
#> $ recent_team                 <chr> "TB", "NO", "LV", "ATL", "ARI", "ARI", "PI…
#> $ completions                 <int> 482, 322, 0, 0, 1, 0, 446, 396, 168, 428, …
#> $ attempts                    <int> 748, 463, 0, 0, 1, 0, 676, 589, 252, 610, …
#> $ passing_yards               <dbl> 5694, 3341, 0, 0, 26, 0, 4304, 4478, 1582,…
#> $ passing_tds                 <int> 50, 27, 0, 0, 0, 0, 37, 26, 6, 53, 0, 13, …
#> $ interceptions               <dbl> 15, 9, 0, 0, 0, 0, 14, 11, 8, 6, 0, 8, 0, …
#> $ sacks                       <dbl> 27, 13, 0, 0, 0, 0, 13, 19, 22, 25, 0, 14,…
#> $ sack_yards                  <dbl> 180, 89, 0, 0, 0, 0, 118, 118, 139, 214, 0…
#> $ sack_fumbles                <int> 2, 6, 0, 0, 0, 0, 2, 2, 1, 1, 0, 2, 0, 0, …
#> $ sack_fumbles_lost           <int> 0, 2, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, …
#> $ passing_air_yards           <dbl> 6903, 2723, 0, 0, 14, 0, 4790, 4240, 1286,…
#> $ passing_yards_after_catch   <dbl> 2256, 1700, 0, 0, 12, 0, 2139, 2341, 949, …
#> $ passing_first_downs         <dbl> 288, 169, 0, 0, 1, 0, 222, 217, 74, 250, 0…
#> $ passing_epa                 <dbl> 174.550932, 64.772987, NA, NA, 4.014011, N…
#> $ passing_2pt_conversions     <int> 0, 0, 0, 0, 0, 0, 3, 2, 0, 0, 0, 2, 0, 0, …
#> $ pacr                        <dbl> 0.8248588, 1.2269556, NA, NA, 1.8571429, N…
#> $ dakota                      <dbl> 0.16269083, 0.11352942, NA, NA, NA, NA, 0.…
#> $ carries                     <int> 43, 23, 0, 3, 0, 0, 26, 19, 10, 42, 187, 3…
#> $ rushing_yards               <dbl> 3, 3, 0, -4, 0, 0, 11, -9, 3, 146, 653, 15…
#> $ rushing_tds                 <int> 4, 2, 0, 0, 0, 0, 0, 0, 0, 4, 2, 2, 0, 0, …
#> $ rushing_fumbles             <dbl> 4, 1, 0, 0, 0, 0, 3, 0, 1, 3, 1, 0, 0, 0, …
#> $ rushing_fumbles_lost        <dbl> 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0, …
#> $ rushing_first_downs         <dbl> 8, 7, 0, 0, 0, 0, 3, 0, 0, 16, 32, 12, 0, …
#> $ rushing_epa                 <dbl> -22.6205957, -8.2220291, NA, 0.0000000, NA…
#> $ rushing_2pt_conversions     <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, …
#> $ receptions                  <int> 0, 0, 13, 0, 0, 54, 0, 0, 0, 1, 16, 1, 13,…
#> $ targets                     <int> 0, 0, 17, 0, 0, 72, 0, 0, 0, 1, 19, 1, 20,…
#> $ receiving_yards             <dbl> 0, 0, 69, 0, 0, 409, 0, 0, 0, -6, 89, 0, 1…
#> $ receiving_tds               <int> 0, 0, 2, 0, 0, 1, 0, 0, 0, 0, 0, 0, 3, 0, …
#> $ receiving_fumbles           <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
#> $ receiving_fumbles_lost      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
#> $ receiving_air_yards         <dbl> 0, 0, 91, 0, 0, 432, 0, 0, 0, -4, 34, -4, …
#> $ receiving_yards_after_catch <dbl> 0, 0, 20, 0, 0, 185, 0, 0, 0, -2, 73, 4, 7…
#> $ receiving_first_downs       <dbl> 0, 0, 8, 0, 0, 25, 0, 0, 0, 0, 3, 0, 7, 0,…
#> $ receiving_epa               <dbl> NA, NA, 2.4071780, NA, NA, -1.7713509, NA,…
#> $ receiving_2pt_conversions   <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
#> $ racr                        <dbl> NA, NA, 0.7582418, NA, NA, 0.9467593, NA, …
#> $ target_share                <dbl> NA, NA, 0.05279503, NA, NA, 0.16071429, NA…
#> $ air_yards_share             <dbl> NA, NA, 0.033641405, NA, NA, 0.117136659, …
#> $ wopr                        <dbl> NA, NA, 0.10274153, NA, NA, 0.32306709, NA…
#> $ special_teams_tds           <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
#> $ fantasy_points              <dbl> 420.06, 231.94, 18.90, -0.40, 1.04, 46.90,…
#> $ fantasy_points_ppr          <dbl> 420.06, 231.94, 31.90, -0.40, 1.04, 100.90…
# }
```
