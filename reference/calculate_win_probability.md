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

Optional: season. `vegas_wp` can't be higher than the win probability
after scoring a touchdown on the play (with more than 5 minutes left in
the game). The season decides where the next possession starts after the
kickoff. If it is missing, the current kickoff rules are assumed.

Overtime: if `pbp_data` has a `qtr` column, rows with `qtr > 4` are
valued with the rule aware overtime model that
[`build_nflfastR_pbp()`](https://nflfastr.com/reference/build_nflfastR_pbp.md)
uses (`game_seconds_remaining` is then the time left in the overtime
period). These rows also need:

- season (the overtime rules changed over time)

- ot_possession (1 for the first possession of overtime, 2 for the
  second and 3 for any later possession). If it is missing and
  `pbp_data` has `game_id` and `drive`, it is derived from the drives
  like in
  [`build_nflfastR_pbp()`](https://nflfastr.com/reference/build_nflfastR_pbp.md).

and can have `season_type` (`"REG"` or `"POST"`, derived from `game_id`
if missing, otherwise `"REG"`) and `roof` (for the field goal make
probability). Overtime rows without a possession number get `NA`.

## Examples

``` r
# \donttest{
try({# to avoid CRAN test problems
data <- data.frame(
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

wp <- nflfastR::calculate_win_probability(data)
wp[c("spread_line", "wp", "vegas_wp")]
})
#>   spread_line        wp  vegas_wp
#> 1           1 0.5462618 0.5111184
#> 2           3 0.5462618 0.5907237
#> 3           4 0.5462618 0.6359606
#> 4           7 0.5462618 0.7214520
#> 5          14 0.5462618 0.8907227
# }
```
