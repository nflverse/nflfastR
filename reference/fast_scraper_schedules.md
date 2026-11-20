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
# fast_scraper_schedules(2015:2018)
})
#> NULL
# }
```
