# Compute Missing Raw PBP Data on Local Filesystem

Uses
[`nflreadr::load_schedules()`](https://nflreadr.nflverse.com/reference/load_schedules.html)
to load game IDs of finished games and compares these IDs to all files
saved under `dir`. This function is intended to serve as input for
[`save_raw_pbp()`](https://nflfastr.com/reference/save_raw_pbp.md).

## Usage

``` r
missing_raw_pbp(
  dir = getOption("nflfastR.raw_directory", default = NULL),
  seasons = TRUE,
  verbose = TRUE
)
```

## Arguments

- dir:

  Path to local directory (defaults to option "nflfastR.raw_directory").
  nflfastR will download the raw game files split by season into one sub
  directory per season.

- seasons:

  a numeric vector of seasons to return, default `TRUE` returns all
  available data.

- verbose:

  If `TRUE`, will print number of missing game files as well as oldest
  and most recent missing ID to console.

## Value

A character vector of missing game IDs. If no files are missing, returns
`NULL` invisibly.

## See also

[`save_raw_pbp()`](https://nflfastr.com/reference/save_raw_pbp.md)

## Examples

``` r
# \donttest{
try(
missing <- missing_raw_pbp(tempdir())
)
#> ℹ You are missing 7099 game files. The oldest missing game is "1999_01_MIN_ATL". The most recent missing game is "2025_07_HOU_SEA".
# }
```
