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
fast_scraper_roster(2019:2020)
})
#> Warning: `fast_scraper_roster()` was deprecated in nflfastR 5.2.0.
#> ℹ Please use `nflreadr::load_rosters()` instead.
#> ── nflverse roster data ────────────────────────────────────────────────────────
#> ℹ Data updated: 2023-09-13 01:04:45 UTC
#> # A tibble: 6,182 × 36
#>    season team  position depth_chart_position jersey_number status full_name    
#>     <int> <chr> <chr>    <chr>                        <int> <chr>  <chr>        
#>  1   2019 IND   K        K                                4 RES    Adam Vinatie…
#>  2   2019 NE    QB       QB                              12 ACT    Tom Brady    
#>  3   2019 NO    QB       QB                               9 ACT    Drew Brees   
#>  4   2019 ATL   K        K                                3 CUT    Matt Bryant  
#>  5   2019 PHI   QB       QB                              18 ACT    Josh McCown  
#>  6   2019 DAL   TE       TE                              82 ACT    Jason Witten 
#>  7   2019 KC    LB       OLB                             94 ACT    Terrell Suggs
#>  8   2019 HOU   DB       FS                              27 ACT    Mike Adams   
#>  9   2019 DET   LS       LS                              48 ACT    Don Muhlbach 
#> 10   2019 PHI   OL       OT                              71 ACT    Jason Peters 
#> # ℹ 6,172 more rows
#> # ℹ 29 more variables: first_name <chr>, last_name <chr>, birth_date <date>,
#> #   height <dbl>, weight <int>, college <chr>, gsis_id <chr>, espn_id <chr>,
#> #   sportradar_id <chr>, yahoo_id <chr>, rotowire_id <chr>, pff_id <chr>,
#> #   pfr_id <chr>, fantasy_data_id <chr>, sleeper_id <chr>, years_exp <int>,
#> #   headshot_url <chr>, ngs_position <chr>, week <int>, game_type <chr>,
#> #   status_description_abbr <chr>, football_name <chr>, esb_id <chr>, …
# }
```
