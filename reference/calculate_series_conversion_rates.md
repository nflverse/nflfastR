# Compute Series Conversion Information from Play by Play

A "Series" begins on a 1st and 10 and each team attempts to either earn
a new 1st down (on offense) or prevent the offense from converting a new
1st down (on defense). Series conversion rate represents how many series
have been either converted to a new 1st down or ended in a touchdown.
This function computes series conversion rates on offense and defense
from nflverse play-by-play data along with other series results. The
function automatically removes series that ended in a QB kneel down.

## Usage

``` r
calculate_series_conversion_rates(pbp, weekly = FALSE)
```

## Arguments

- pbp:

  Play-by-play data as returned by
  [`load_pbp()`](https://nflreadr.nflverse.com/reference/load_pbp.html),
  [`build_nflfastR_pbp()`](https://nflfastr.com/reference/build_nflfastR_pbp.md),
  or [`fast_scraper()`](https://nflfastr.com/reference/fast_scraper.md).

- weekly:

  If `TRUE`, returns week-by-week stats, otherwise, season-by-season
  stats in argument `pbp`.

## Value

A data frame of series information including the following columns:

- season:

  The NFL season

- team:

  NFL team abbreviation

- week:

  Week if `weekly` is `TRUE`

- off_n:

  The number of series the offense played (excludes QB kneel downs,
  kickoffs, extra point/two point conversion attempts, non-plays, and
  plays that do not list a "posteam")

- off_scr:

  The rate at which a series ended in either new 1st down or touchdown
  while the offense was on the field

- off_scr_1st:

  The rate at which an offense earned a 1st down or scored a touchdown
  on 1st down

- off_scr_2nd:

  The rate at which an offense earned a 1st down or scored a touchdown
  on 2nd down

- off_scr_3rd:

  The rate at which an offense earned a 1st down or scored a touchdown
  on 3rd down

- off_scr_4th:

  The rate at which an offense earned a 1st down or scored a touchdown
  on 4th down

- off_1st:

  The rate of series that ended in a new 1st down while the offense was
  on the field (does not include offensive touchdown)

- off_td:

  The rate of series that ended in an offensive touchdown while the
  offense was on the field

- off_fg:

  The rate of series that ended in a field goal attempt while the
  offense was on the field

- off_punt:

  The rate of series that ended in a punt while the offense was on the
  field

- off_to:

  The rate of series that ended in a turnover (including on downs), in
  an opponent score, or at the end of half (or game) while the offense
  was on the field

- def_n:

  The number of series the defense played (excludes QB kneel downs,
  kickoffs, extra point/two point conversion attempts, non-plays, and
  plays that do not list a "posteam")

- def_scr:

  The rate at which a series ended in either new 1st down or touchdown
  while the defense was on the field

- def_scr_1st:

  The rate at which a defense allowed a 1st down or touchdown on 1st
  down

- def_scr_2nd:

  The rate at which a defense allowed a 1st down or touchdown on 2nd
  down

- def_scr_3rd:

  The rate at which a defense allowed a 1st down or touchdown on 3rd
  down

- def_scr_4th:

  The rate at which a defense allowed a 1st down or touchdown on 4th
  down

- def_1st:

  The rate of series that ended in a new 1st down while the defense was
  on the field (does not include offensive touchdown)

- def_td:

  The rate of series that ended in an offensive touchdown while the
  defense was on the field

- def_fg:

  The rate of series that ended in a field goal attempt while the
  defense was on the field

- def_punt:

  The rate of series that ended in a punt while the defense was on the
  field

- def_to:

  The rate of series that ended in a turnover (including on downs), in
  an opponent score, or at the end of half (or game) while the defense
  was on the field

## Examples

``` r
# \donttest{
try({# to avoid CRAN test problems
  pbp <- nflfastR::load_pbp(2021)

  weekly <- calculate_series_conversion_rates(pbp, weekly = TRUE)
  dplyr::glimpse(weekly)

  overall <- calculate_series_conversion_rates(pbp, weekly = FALSE)
  dplyr::glimpse(overall)
})
#> Rows: 570
#> Columns: 25
#> $ season      <int> 2021, 2021, 2021, 2021, 2021, 2021, 2021, 2021, 2021, 2021…
#> $ team        <chr> "ARI", "ARI", "ARI", "ARI", "ARI", "ARI", "ARI", "ARI", "A…
#> $ week        <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 14, 15, 16, 17, 18,…
#> $ off_n       <int> 28, 29, 31, 32, 27, 29, 31, 27, 28, 21, 36, 20, 31, 31, 27…
#> $ off_scr     <dbl> 0.7857143, 0.7241379, 0.7419355, 0.8437500, 0.7407407, 0.8…
#> $ off_scr_1st <dbl> 0.1428571, 0.2413793, 0.1935484, 0.1250000, 0.1851852, 0.1…
#> $ off_scr_2nd <dbl> 0.3571429, 0.3448276, 0.4516129, 0.4375000, 0.4444444, 0.3…
#> $ off_scr_3rd <dbl> 0.28571429, 0.10344828, 0.03225806, 0.28125000, 0.11111111…
#> $ off_scr_4th <dbl> 0.00000000, 0.03448276, 0.06451613, 0.00000000, 0.00000000…
#> $ off_1st     <dbl> 0.6071429, 0.5862069, 0.6451613, 0.7187500, 0.6666667, 0.6…
#> $ off_td      <dbl> 0.17857143, 0.13793103, 0.09677419, 0.12500000, 0.07407407…
#> $ off_fg      <dbl> 0.07142857, 0.06896552, 0.03225806, 0.09375000, 0.07407407…
#> $ off_punt    <dbl> 0.10714286, 0.13793103, 0.16129032, 0.06250000, 0.14814815…
#> $ off_to      <dbl> 0.03571429, 0.06896552, 0.06451613, 0.00000000, 0.03703704…
#> $ def_n       <int> 26, 30, 30, 31, 25, 23, 19, 30, 30, 33, 24, 32, 24, 27, 27…
#> $ def_scr     <dbl> 0.6538462, 0.7333333, 0.6666667, 0.7741935, 0.6800000, 0.6…
#> $ def_scr_1st <dbl> 0.1923077, 0.2666667, 0.2000000, 0.2580645, 0.2400000, 0.2…
#> $ def_scr_2nd <dbl> 0.2307692, 0.3666667, 0.3666667, 0.3225806, 0.2800000, 0.2…
#> $ def_scr_3rd <dbl> 0.19230769, 0.06666667, 0.06666667, 0.19354839, 0.12000000…
#> $ def_scr_4th <dbl> 0.03846154, 0.03333333, 0.03333333, 0.00000000, 0.04000000…
#> $ def_1st     <dbl> 0.5769231, 0.6333333, 0.6000000, 0.7096774, 0.6400000, 0.5…
#> $ def_td      <dbl> 0.07692308, 0.10000000, 0.06666667, 0.06451613, 0.04000000…
#> $ def_fg      <dbl> 0.03846154, 0.10000000, 0.00000000, 0.09677419, 0.04000000…
#> $ def_punt    <dbl> 0.11538462, 0.16666667, 0.20000000, 0.03225806, 0.08000000…
#> $ def_to      <dbl> 0.19230769, 0.00000000, 0.13333333, 0.09677419, 0.20000000…
#> Rows: 32
#> Columns: 24
#> $ season      <int> 2021, 2021, 2021, 2021, 2021, 2021, 2021, 2021, 2021, 2021…
#> $ team        <chr> "ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE", "D…
#> $ off_n       <int> 510, 441, 538, 568, 474, 472, 568, 463, 540, 449, 463, 504…
#> $ off_scr     <dbl> 0.7431373, 0.6825397, 0.7267658, 0.7834507, 0.6666667, 0.6…
#> $ off_scr_1st <dbl> 0.1843137, 0.2040816, 0.2416357, 0.2605634, 0.2025316, 0.2…
#> $ off_scr_2nd <dbl> 0.3039216, 0.2380952, 0.2918216, 0.2799296, 0.2468354, 0.2…
#> $ off_scr_3rd <dbl> 0.2176471, 0.2108844, 0.1561338, 0.2147887, 0.1856540, 0.1…
#> $ off_scr_4th <dbl> 0.03725490, 0.02947846, 0.03717472, 0.02816901, 0.03164557…
#> $ off_1st     <dbl> 0.6431373, 0.6122449, 0.6524164, 0.6637324, 0.6012658, 0.6…
#> $ off_td      <dbl> 0.10000000, 0.07029478, 0.07434944, 0.11971831, 0.06540084…
#> $ off_fg      <dbl> 0.07254902, 0.06575964, 0.06877323, 0.05633803, 0.06118143…
#> $ off_punt    <dbl> 0.11176471, 0.14512472, 0.13197026, 0.09859155, 0.15611814…
#> $ off_to      <dbl> 0.07254902, 0.10657596, 0.07249071, 0.06161972, 0.11603376…
#> $ def_n       <int> 483, 490, 477, 489, 451, 439, 592, 451, 509, 434, 485, 477…
#> $ def_scr     <dbl> 0.7142857, 0.7734694, 0.7127883, 0.6748466, 0.6851441, 0.7…
#> $ def_scr_1st <dbl> 0.2380952, 0.2183673, 0.2641509, 0.1881391, 0.2283814, 0.2…
#> $ def_scr_2nd <dbl> 0.2525880, 0.2979592, 0.2599581, 0.2433538, 0.2372506, 0.2…
#> $ def_scr_3rd <dbl> 0.1904762, 0.2306122, 0.1635220, 0.1963190, 0.1929047, 0.2…
#> $ def_scr_4th <dbl> 0.03312629, 0.02653061, 0.02515723, 0.04703476, 0.02660754…
#> $ def_1st     <dbl> 0.6252588, 0.6714286, 0.6205451, 0.5971370, 0.5920177, 0.6…
#> $ def_td      <dbl> 0.08902692, 0.10204082, 0.09224319, 0.07770961, 0.09312639…
#> $ def_fg      <dbl> 0.06211180, 0.05714286, 0.05870021, 0.05725971, 0.08203991…
#> $ def_punt    <dbl> 0.13043478, 0.10816327, 0.17400419, 0.16155419, 0.16407982…
#> $ def_to      <dbl> 0.09316770, 0.06122449, 0.05450734, 0.10633947, 0.06873614…
# }
```
