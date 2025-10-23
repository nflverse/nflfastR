# Compute Division Standings and Conference Seeds from Play by Play

**\[deprecated\]**

This function was deprecated and replaced by
[`nflseedR::nfl_standings()`](https://nflseedr.com/reference/nfl_standings.html).

This function calculates division standings as well as playoff seeds per
conference based on either nflverse play-by-play data or nflverse
schedule data.

## Usage

``` r
calculate_standings(
  nflverse_object,
  tiebreaker_depth = 3,
  playoff_seeds = NULL
)
```

## Arguments

- nflverse_object:

  Data object of class `nflverse_data`. Either schedules as returned by
  [`fast_scraper_schedules()`](https://nflfastr.com/reference/fast_scraper_schedules.md)
  or
  [`nflreadr::load_schedules()`](https://nflreadr.nflverse.com/reference/load_schedules.html).
  Or play-by-play data as returned by
  [`load_pbp()`](https://nflreadr.nflverse.com/reference/load_pbp.html),
  [`build_nflfastR_pbp()`](https://nflfastr.com/reference/build_nflfastR_pbp.md),
  or [`fast_scraper()`](https://nflfastr.com/reference/fast_scraper.md).

- tiebreaker_depth:

  A single value equal to 1, 2, or 3. The default is 3. The value
  controls the depth of tiebreakers that shall be applied. The deepest
  currently implemented tiebreaker is strength of schedule. The
  following values are valid:

  tiebreaker_depth = 1

  :   Break all ties with a coinflip. Fastest variant.

  tiebreaker_depth = 2

  :   Apply head-to-head and division win percentage tiebreakers. Random
      if still tied.

  tiebreaker_depth = 3

  :   Apply all tiebreakers through strength of schedule. Random if
      still tied.

- playoff_seeds:

  Number of playoff teams per conference. If `NULL` (the default), the
  function will try to split `nflverse_object` into seasons prior 2020
  (6 seeds) and 2020ff (7 seeds). If set to a numeric, it will be used
  for all seasons in `nflverse_object`!

## Value

A tibble with NFL regular season standings

## Examples

``` r
# \donttest{
try({# to avoid CRAN test problems
  # load nflverse data both schedules and pbp
  scheds <- fast_scraper_schedules(2014)
  pbp <- load_pbp(c(2018, 2021))

  # calculate standings based on pbp
  calculate_standings(pbp)

  # calculate standings based on schedules
  calculate_standings(scheds)
})
#> Warning: `calculate_standings()` was deprecated in nflfastR 5.1.0.
#> ℹ Please use `nflseedR::nfl_standings()` instead.
#> # A tibble: 32 × 15
#>    season conf  division  div_rank  seed team  games  wins losses  ties win_pct
#>     <int> <chr> <chr>        <dbl> <dbl> <chr> <int> <int>  <int> <int>   <dbl>
#>  1   2014 AFC   AFC East         1     1 NE       16    12      4     0   0.75 
#>  2   2014 AFC   AFC East         2    NA BUF      16     9      7     0   0.562
#>  3   2014 AFC   AFC East         3    NA MIA      16     8      8     0   0.5  
#>  4   2014 AFC   AFC East         4    NA NYJ      16     4     12     0   0.25 
#>  5   2014 AFC   AFC North        1     3 PIT      16    11      5     0   0.688
#>  6   2014 AFC   AFC North        2     5 CIN      16    10      5     1   0.656
#>  7   2014 AFC   AFC North        3     6 BAL      16    10      6     0   0.625
#>  8   2014 AFC   AFC North        4    NA CLE      16     7      9     0   0.438
#>  9   2014 AFC   AFC South        1     4 IND      16    11      5     0   0.688
#> 10   2014 AFC   AFC South        2    NA HOU      16     9      7     0   0.562
#> # ℹ 22 more rows
#> # ℹ 4 more variables: div_pct <dbl>, conf_pct <dbl>, sov <dbl>, sos <dbl>
# }
```
