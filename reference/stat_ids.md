# NFL Stat IDs and their Meanings

NFL Stat IDs and their Meanings

## Usage

``` r
stat_ids
```

## Format

A data frame including NFL stat IDs, names and descriptions used in an
nflfastR dataset.

## Source

[http://www.nflgsis.com/gsis/Documentation/Partners/StatIDs.html](http://www.nflgsis.com/gsis/Documentation/Partners/StatIDs.md)

## Examples

``` r
# \donttest{
stat_ids
#> # A tibble: 119 × 3
#>    stat_id name                       comment                                   
#>      <int> <chr>                      <chr>                                     
#>  1       1 Rushing Yards - Minus      "Used in addition to the other Rushing st…
#>  2       2 Punt Blocked (Offense)     "Punt was blocked. A blocked punt is a pu…
#>  3       3 1st Down Rushing           "A first down or TD occurred due to a rus…
#>  4       4 1st Down Passing           "A first down or TD occurred due to a pas…
#>  5       5 1st Down Penalty           "A first down or TD occurred due to a pen…
#>  6       6 3rd Down Attempt Converted "3rd down play resulted in a first down o…
#>  7       7 3rd Down Attempt Failed    "3rd down play did NOT result in a first …
#>  8       8 4th Down Attempt Converted "4th down play resulted in a first down o…
#>  9       9 4th Down Attempt Failed    "4th down play did NOT result in a first …
#> 10      10 Rushing Yards              "Rushing yards and credit for a rushing a…
#> # ℹ 109 more rows
# }
```
