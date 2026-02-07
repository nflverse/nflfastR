# Load Team Rosters for Multiple Seasons

**\[deprecated\]**

This function was deprecated. Please use
[`nflreadr::load_rosters`](https://nflreadr.nflverse.com/reference/load_rosters.html).

## Usage

``` r
fast_scraper_roster(...)
```

## Arguments

- ...:

  Arguments passed on to
  [`nflreadr::load_rosters`](https://nflreadr.nflverse.com/reference/load_rosters.html)

  `seasons`

  :   a numeric vector of seasons to return, defaults to returning this
      year's data if it is March or later. If set to `TRUE`, will return
      all available data. Data available back to 1920.

  `file_type`

  :   One of `c("rds", "qs", "csv", "parquet")`. Can also be set
      globally with `options(nflreadr.prefer)`

## Value

A tibble of season-level roster data.

## Details

See
[`nflreadr::load_rosters`](https://nflreadr.nflverse.com/reference/load_rosters.html)
for details.

## See also

For information on parallel processing and progress updates please see
[nflfastR](https://nflfastr.com/reference/nflfastR-package.md).

## Examples

``` r
# \donttest{
# Roster of the 2019 and 2020 seasons
try({# to avoid CRAN test problems
# fast_scraper_roster(2019:2020)
})
#> NULL
# }
```
