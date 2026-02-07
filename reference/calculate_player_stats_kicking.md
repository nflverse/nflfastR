# Summarize Kicking Stats

**\[deprecated\]**

This function was deprecated because we have a new, much better and
harmonized approach in
[`calculate_stats()`](https://nflfastr.com/reference/calculate_stats.md).

Build columns that aggregate kicking stats at the game level.

## Usage

``` r
calculate_player_stats_kicking(pbp, weekly = FALSE)
```

## Arguments

- pbp:

  A Data frame of NFL play-by-play data typically loaded with
  [`load_pbp()`](https://nflreadr.nflverse.com/reference/load_pbp.html)
  or
  [`build_nflfastR_pbp()`](https://nflfastr.com/reference/build_nflfastR_pbp.md).

- weekly:

  If `TRUE`, returns week-by-week stats, otherwise, stats for the entire
  data frame in argument `pbp`.

## Value

a dataframe of kicking stats

## See also

<https://nflreadr.nflverse.com/reference/load_player_stats.html> for the
nflreadr function to download this from repo (`stat_type = "kicking"`)

## Examples

``` r
# \donttest{
try({# to avoid CRAN test problems
    # pbp <- nflreadr::load_pbp(2021)
    # weekly <- calculate_player_stats_kicking(pbp, weekly = TRUE)
    # dplyr::glimpse(weekly)

    # overall <- calculate_player_stats_kicking(pbp, weekly = FALSE)
    # dplyr::glimpse(overall)
})
#> NULL
# }
```
