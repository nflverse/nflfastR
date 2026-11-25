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

  probability next score touchdown this half.

## Details

Computes expected points for provided plays. Returns the data with
probabilities of each scoring event and EP added. Scoring events are
valued as the points scored minus the expected points of the possession
the other team gets afterwards (a kickoff touchback, which depends on
the season and the clock). The clock is `half_seconds_remaining`, which
is all that is needed, since it is equivalent to the quarter and the
time remaining in the quarter (a half is two quarters). In overtime, the
value of a score also depends on the overtime rules and the possession,
which is used if the data has `qtr`, `game_id`, `drive` and
`score_differential`. The possessions are counted from the lowest
`drive` of each overtime game, so all of the overtime plays of a game
from its start have to be in the data. The following columns must be
present:

- season

- home_team

- posteam

- location (optional: 'Neutral' for a neutral site game, in which both
  teams are treated as away teams)

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
data <- data.frame(
"season" = 2001:2019,
"home_team" = "SEA",
"posteam" = "SEA",
"roof" = "outdoors",
"half_seconds_remaining" = 1800,
"yardline_100" = c(rep(80, 15), rep(75, 4)),
"down" = 1,
"ydstogo" = 10,
"posteam_timeouts_remaining" = 3,
"defteam_timeouts_remaining" = 3
)

ep <- nflfastR::calculate_expected_points(data)
ep[c("season", "yardline_100", "td_prob", "ep")]
})
#>    season yardline_100   td_prob        ep
#> 1    2001           80 0.3365089 0.5386615
#> 2    2002           80 0.3365089 0.5386615
#> 3    2003           80 0.3365089 0.5386615
#> 4    2004           80 0.3365089 0.5386615
#> 5    2005           80 0.3365089 0.5386615
#> 6    2006           80 0.3452529 0.6592721
#> 7    2007           80 0.3452529 0.6592721
#> 8    2008           80 0.3452529 0.6592721
#> 9    2009           80 0.3452529 0.6592721
#> 10   2010           80 0.3452529 0.6592721
#> 11   2011           80 0.3452529 0.7016030
#> 12   2012           80 0.3452529 0.7016030
#> 13   2013           80 0.3452529 0.7016030
#> 14   2014           80 0.3546035 0.8787173
#> 15   2015           80 0.3546035 0.8735679
#> 16   2016           75 0.3748126 1.1397971
#> 17   2017           75 0.3748126 1.1397971
#> 18   2018           75 0.3983951 1.2143765
#> 19   2019           75 0.3983951 1.2143765
# }
```
