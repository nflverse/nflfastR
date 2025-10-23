# Add expected yards after completion (xyac) variables

Add expected yards after completion (xyac) variables

## Usage

``` r
add_xyac(pbp, ...)
```

## Arguments

- pbp:

  is a Data frame of play-by-play data scraped using
  [`fast_scraper()`](https://nflfastr.com/reference/fast_scraper.md).

- ...:

  Additional arguments passed to a message function (for internal use).

## Value

The input Data Frame of the parameter 'pbp' with the following columns
added:

- xyac_epa:

  Expected value of EPA gained after the catch, starting from where the
  catch was made. Zero yards after the catch would be listed as zero
  EPA.

- xyac_success:

  Probability play earns positive EPA (relative to where play started)
  based on where ball was caught.

- xyac_fd:

  Probability play earns a first down based on where the ball was
  caught.

- xyac_mean_yardage:

  Average expected yards after the catch based on where the ball was
  caught.

- xyac_median_yardage:

  Median expected yards after the catch based on where the ball was
  caught.

## Details

Build columns that capture what we should expect after the catch.
