# Compute QB epa

Compute QB epa

## Usage

``` r
add_qb_epa(pbp, ...)
```

## Arguments

- pbp:

  is a Data frame of play-by-play data scraped using
  [`fast_scraper()`](https://nflfastr.com/reference/fast_scraper.md).

- ...:

  Additional arguments passed to a message function (for internal use).

## Details

Add the variable 'qb_epa', which gives QB credit for EPA for up to the
point where a receiver lost a fumble after a completed catch and makes
EPA work more like passing yards on plays with fumbles
