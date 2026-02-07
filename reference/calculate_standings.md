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
  # scheds <- fast_scraper_schedules(2014)
  # pbp <- load_pbp(c(2018, 2021))

  # calculate standings based on pbp
  # calculate_standings(pbp)

  # calculate standings based on schedules
  # calculate_standings(scheds)
})
#> NULL
# }
```
