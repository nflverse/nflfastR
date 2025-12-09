# Load NFL Season Schedules

**\[deprecated\]**

This function was deprecated. Please use
[`nflreadr::load_schedules`](https://nflreadr.nflverse.com/reference/load_schedules.html).

## Usage

``` r
fast_scraper_schedules(...)
```

## Arguments

- ...:

  Arguments passed on to
  [`nflreadr::load_schedules`](https://nflreadr.nflverse.com/reference/load_schedules.html)

  `seasons`

  :   a numeric vector of seasons to return, default `TRUE` returns all
      available data.

## Value

A tibble of game information for past and/or future games.

## Details

See
[`nflreadr::load_schedules`](https://nflreadr.nflverse.com/reference/load_schedules.html)
for details.

## See also

For information on parallel processing and progress updates please see
[nflfastR](https://nflfastr.com/reference/nflfastR-package.md).

## Examples

``` r
# \donttest{
# Get schedules for the whole 2015 - 2018 seasons
try({# to avoid CRAN test problems
fast_scraper_schedules(2015:2018)
})
#> ── nflverse games and schedules ────────────────────────────────────────────────
#> ℹ Data updated: 2025-12-09 15:01:01 UTC
#> # A tibble: 1,068 × 46
#>    game_id  season game_type  week gameday weekday gametime away_team away_score
#>    <chr>     <int> <chr>     <int> <chr>   <chr>   <chr>    <chr>          <int>
#>  1 2015_01…   2015 REG           1 2015-0… Thursd… 20:30    PIT               21
#>  2 2015_01…   2015 REG           1 2015-0… Sunday  13:00    IND               14
#>  3 2015_01…   2015 REG           1 2015-0… Sunday  13:00    GB                31
#>  4 2015_01…   2015 REG           1 2015-0… Sunday  13:00    KC                27
#>  5 2015_01…   2015 REG           1 2015-0… Sunday  13:00    CAR               20
#>  6 2015_01…   2015 REG           1 2015-0… Sunday  13:00    CLE               10
#>  7 2015_01…   2015 REG           1 2015-0… Sunday  13:00    SEA               31
#>  8 2015_01…   2015 REG           1 2015-0… Sunday  13:00    MIA               17
#>  9 2015_01…   2015 REG           1 2015-0… Sunday  16:05    NO                19
#> 10 2015_01…   2015 REG           1 2015-0… Sunday  16:05    DET               28
#> # ℹ 1,058 more rows
#> # ℹ 37 more variables: home_team <chr>, home_score <int>, location <chr>,
#> #   result <int>, total <int>, overtime <int>, old_game_id <chr>, gsis <int>,
#> #   nfl_detail_id <chr>, pfr <chr>, pff <int>, espn <chr>, ftn <int>,
#> #   away_rest <int>, home_rest <int>, away_moneyline <int>,
#> #   home_moneyline <int>, spread_line <dbl>, away_spread_odds <int>,
#> #   home_spread_odds <int>, total_line <dbl>, under_odds <int>, …
# }
```
