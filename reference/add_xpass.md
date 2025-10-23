# Add expected pass columns

Build columns from the expected dropback model. Will return `NA` on data
prior to 2006 since that was before NFL started marking scrambles. Must
be run on a dataframe that has already had
[`clean_pbp()`](https://nflfastr.com/reference/clean_pbp.md) run on it.
Note that the functions
[`build_nflfastR_pbp()`](https://nflfastr.com/reference/build_nflfastR_pbp.md)
and the database function
[`update_db()`](https://nflfastr.com/reference/update_db.md) already
include this function.

## Usage

``` r
add_xpass(pbp, ...)
```

## Arguments

- pbp:

  is a Data frame of play-by-play data scraped using
  [`fast_scraper()`](https://nflfastr.com/reference/fast_scraper.md).

- ...:

  Additional arguments passed to a message function (for internal use).

## Value

The input Data Frame of the parameter `pbp` with the following columns
added:

- xpass:

  Probability of dropback scaled from 0 to 1.

- pass_oe:

  Dropback percent over expected on a given play scaled from 0 to 100.
