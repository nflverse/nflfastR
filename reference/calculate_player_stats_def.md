# Get Official Game Stats on Defense

**\[deprecated\]**

This function was deprecated because we have a new, much better and
harmonized approach in
[`calculate_stats()`](https://nflfastr.com/reference/calculate_stats.md).

Build columns that aggregate official defense stats either at the game
level or at the level of the entire data frame passed.

## Usage

``` r
calculate_player_stats_def(pbp, weekly = FALSE)
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

A data frame of defensive player stats. See dictionary (# TODO)

## See also

The function
[`load_player_stats()`](https://nflreadr.nflverse.com/reference/load_player_stats.html)
and the corresponding examples on [the nflfastR
website](https://nflfastr.com/articles/nflfastR.html#example-11-replicating-official-stats)

## Examples

``` r
# \donttest{
try({# to avoid CRAN test problems
  # pbp <- nflfastR::load_pbp(2020)

  # weekly <- calculate_player_stats_def(pbp, weekly = TRUE)
  # dplyr::glimpse(weekly)

  # overall <- calculate_player_stats_def(pbp, weekly = FALSE)
  # dplyr::glimpse(overall)
})
#> NULL
# }
```
