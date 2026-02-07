# Compute expected points

for provided plays. Returns the data with probabilities of each scoring
event and EP added. The following columns must be present: season,
home_team, posteam, roof (coded as 'open', 'closed', or 'retractable'),
half_seconds_remaining, yardline_100, ydstogo,
posteam_timeouts_remaining, defteam_timeouts_remaining

## Usage

``` r
calculate_expected_points(pbp_data)
```

## Arguments

- pbp_data:

  Play-by-play dataset to estimate expected points for.

## Value

The original pbp_data with the following columns appended to it:

- ep:

  expected points.

- no_score_prob:

  probability of no more scoring this half.

- opp_fg_prob:

  probability next score opponent field goal this half.

- opp_safety_prob:

  probability next score opponent safety this half.

- opp_td_prob:

  probability of next score opponent touchdown this half.

- fg_prob:

  probability next score field goal this half.

- safety_prob:

  probability next score safety this half.

- td_prob:

  probability text score touchdown this half.

## Details

Computes expected points for provided plays. Returns the data with
probabilities of each scoring event and EP added. The following columns
must be present:

- season

- home_team

- posteam

- roof (coded as 'outdoors', 'dome', or 'open'/'closed'/NA
  (retractable))

- half_seconds_remaining

- yardline_100

- down

- ydstogo

- posteam_timeouts_remaining

- defteam_timeouts_remaining

## Examples

``` r
# \donttest{
try({# to avoid CRAN test problems
library(dplyr)
data <- tibble::tibble(
"season" = 1999:2019,
"home_team" = "SEA",
"posteam" = "SEA",
"roof" = "outdoors",
"half_seconds_remaining" = 1800,
"yardline_100" = c(rep(80, 17), rep(75, 4)),
"down" = 1,
"ydstogo" = 10,
"posteam_timeouts_remaining" = 3,
"defteam_timeouts_remaining" = 3
)

nflfastR::calculate_expected_points(data) |>
  dplyr::select(season, yardline_100, td_prob, ep)
})
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
#>    season yardline_100   td_prob        ep
#> 1    1999           80 0.3342112 0.6378878
#> 2    2000           80 0.3342112 0.6378878
#> 3    2001           80 0.3342112 0.6378878
#> 4    2002           80 0.3431796 0.8167660
#> 5    2003           80 0.3431796 0.8167660
#> 6    2004           80 0.3431796 0.8167660
#> 7    2005           80 0.3431796 0.8167660
#> 8    2006           80 0.3445111 0.8136176
#> 9    2007           80 0.3445111 0.8136176
#> 10   2008           80 0.3445111 0.8136176
#> 11   2009           80 0.3445111 0.8136176
#> 12   2010           80 0.3445111 0.8136176
#> 13   2011           80 0.3445111 0.8136176
#> 14   2012           80 0.3445111 0.8136176
#> 15   2013           80 0.3445111 0.8136176
#> 16   2014           80 0.3522740 0.9822985
#> 17   2015           80 0.3522740 0.9822985
#> 18   2016           75 0.3771672 1.4573911
#> 19   2017           75 0.3771672 1.4573911
#> 20   2018           75 0.4067504 1.4740978
#> 21   2019           75 0.4067504 1.4740978
# }
```
