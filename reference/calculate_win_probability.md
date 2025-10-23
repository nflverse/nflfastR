# Compute win probability

for provided plays. Returns the data with probabilities of winning the
game. The following columns must be present: receive_h2_ko (1 if game is
in 1st half and possession team will receive 2nd half kickoff, 0
otherwise), home_team, posteam, half_seconds_remaining,
game_seconds_remaining, spread_line (how many points home team was
favored by), down, ydstogo, yardline_100, posteam_timeouts_remaining,
defteam_timeouts_remaining

## Usage

``` r
calculate_win_probability(pbp_data)
```

## Arguments

- pbp_data:

  Play-by-play dataset to estimate win probability for.

## Value

The original pbp_data with the following columns appended to it:

- wp:

  win probability.

- vegas_wp:

  win probability taking into account pre-game spread.

## Details

Computes win probability for provided plays. Returns the data with
spread and non-spread-adjusted win probabilities. The following columns
must be present:

- receive_2h_ko (1 if game is in 1st half and possession team will
  receive 2nd half kickoff, 0 otherwise)

- score_differential

- home_team

- posteam

- half_seconds_remaining

- game_seconds_remaining

- spread_line (how many points home team was favored by)

- down

- ydstogo

- yardline_100

- posteam_timeouts_remaining

- defteam_timeouts_remaining

## Examples

``` r
# \donttest{
try({# to avoid CRAN test problems
library(dplyr)
data <- tibble::tibble(
"receive_2h_ko" = 0,
"home_team" = "SEA",
"posteam" = "SEA",
"score_differential" = 0,
"half_seconds_remaining" = 1800,
"game_seconds_remaining" = 3600,
"spread_line" = c(1, 3, 4, 7, 14),
"down" = 1,
"ydstogo" = 10,
"yardline_100" = 75,
"posteam_timeouts_remaining" = 3,
"defteam_timeouts_remaining" = 3
)

nflfastR::calculate_win_probability(data) |>
  dplyr::select(spread_line, wp, vegas_wp)
})
#> # A tibble: 5 × 3
#>   spread_line    wp vegas_wp
#>         <dbl> <dbl>    <dbl>
#> 1           1 0.546    0.515
#> 2           3 0.546    0.596
#> 3           4 0.546    0.638
#> 4           7 0.546    0.737
#> 5          14 0.546    0.866
# }
```
