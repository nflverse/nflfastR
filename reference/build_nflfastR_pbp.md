# Build a Complete nflfastR Data Set

`build_nflfastR_pbp` is a convenient wrapper around 6 nflfastR
functions:

- [`fast_scraper()`](https://nflfastr.com/reference/fast_scraper.md)

- [`clean_pbp()`](https://nflfastr.com/reference/clean_pbp.md)

- [`add_qb_epa()`](https://nflfastr.com/reference/add_qb_epa.md)

- [`add_xyac()`](https://nflfastr.com/reference/add_xyac.md)

- [`add_xpass()`](https://nflfastr.com/reference/add_xpass.md)

- [`decode_player_ids()`](https://nflfastr.com/reference/decode_player_ids.md)

Please see either the documentation of each function or [the nflfastR
Field Descriptions
website](https://www.nflfastr.com/articles/field_descriptions.html) to
learn about the output.

## Usage

``` r
build_nflfastR_pbp(
  game_ids,
  dir = getOption("nflfastR.raw_directory", default = NULL),
  ...,
  decode = TRUE,
  rules = TRUE
)
```

## Arguments

- game_ids:

  Vector of character ids or a data frame including the variable
  `game_id` (see details for further information).

- dir:

  Path to local directory (defaults to option "nflfastR.raw_directory")
  where nflfastR searches for raw game play-by-play data. See
  [`save_raw_pbp()`](https://nflfastr.com/reference/save_raw_pbp.md) for
  additional information.

- ...:

  Additional arguments passed to the scraping functions (for internal
  use)

- decode:

  If `TRUE`, the function
  [`decode_player_ids()`](https://nflfastr.com/reference/decode_player_ids.md)
  will be executed.

- rules:

  If `FALSE`, printing of the header and footer in the console output
  will be suppressed.

## Value

An nflfastR play-by-play data frame like it can be loaded from
<https://github.com/nflverse/nflverse-data>.

## Details

To load valid game_ids please use the package function
[`fast_scraper_schedules()`](https://nflfastr.com/reference/fast_scraper_schedules.md).

## See also

For information on parallel processing and progress updates please see
[nflfastR](https://nflfastr.com/reference/nflfastR-package.md).

## Examples

``` r
# \donttest{
# Build nflfastR pbp for the 2018 and 2019 Super Bowls
try({# to avoid CRAN test problems
build_nflfastR_pbp(c("2018_21_NE_LA", "2019_21_SF_KC"))
})
#> ── Build nflfastR Play-by-Play Data ───────────── nflfastR version 5.1.0.9009 ──
#> • 01:28:26 | Start download of 2 games...
#> ℹ It is recommended to use parallel processing when trying to load multiple games.Please consider running `future::plan("multisession")`! Will go on sequentially...
#> ✔ 01:28:29 | Download finished. Adding variables...
#> ✔ 01:28:29 | added game variables
#> ✔ 01:28:29 | added nflscrapR variables
#> ✔ 01:28:30 | added ep variables
#> ✔ 01:28:30 | added air_yac_ep variables
#> ✔ 01:28:30 | added wp variables
#> ✔ 01:28:30 | added air_yac_wp variables
#> ✔ 01:28:30 | added cp and cpoe
#> ✔ 01:28:31 | added fixed drive variables
#> ✔ 01:28:31 | added series variables
#> • 01:28:31 | Cleaning up play-by-play...
#> ✔ 01:28:31 | Cleaning completed
#> ✔ 01:28:31 | added qb_epa
#> • 01:28:31 | Computing xyac...
#> ✔ 01:28:33 | added xyac variables
#> • 01:28:33 | Computing xpass...
#> ✔ 01:28:33 | added xpass and pass_oe
#> • 01:28:33 | Decode player ids...
#> ✔ 01:28:34 | Decoding of player ids completed
#> ── DONE ────────────────────────────────────────────────────────────────────────
#> ── nflverse play by play ───────────────────────────────────────────────────────
#> ℹ Data updated: 2026-01-25 01:28:34 UTC
#> # A tibble: 349 × 372
#>    play_id game_id     old_game_id home_team away_team season_type  week posteam
#>      <dbl> <chr>       <chr>       <chr>     <chr>     <chr>       <int> <chr>  
#>  1       1 2018_21_NE… 2019020300  LA        NE        POST           21 NA     
#>  2      38 2018_21_NE… 2019020300  LA        NE        POST           21 NE     
#>  3      67 2018_21_NE… 2019020300  LA        NE        POST           21 NE     
#>  4      89 2018_21_NE… 2019020300  LA        NE        POST           21 NE     
#>  5     111 2018_21_NE… 2019020300  LA        NE        POST           21 NE     
#>  6     133 2018_21_NE… 2019020300  LA        NE        POST           21 NE     
#>  7     155 2018_21_NE… 2019020300  LA        NE        POST           21 NE     
#>  8     182 2018_21_NE… 2019020300  LA        NE        POST           21 LA     
#>  9     204 2018_21_NE… 2019020300  LA        NE        POST           21 LA     
#> 10     226 2018_21_NE… 2019020300  LA        NE        POST           21 LA     
#> # ℹ 339 more rows
#> # ℹ 364 more variables: posteam_type <chr>, defteam <chr>, side_of_field <chr>,
#> #   yardline_100 <dbl>, game_date <chr>, quarter_seconds_remaining <dbl>,
#> #   half_seconds_remaining <dbl>, game_seconds_remaining <dbl>,
#> #   game_half <chr>, quarter_end <dbl>, drive <dbl>, sp <dbl>, qtr <dbl>,
#> #   down <dbl>, goal_to_go <int>, time <chr>, yrdln <chr>, ydstogo <dbl>,
#> #   ydsnet <dbl>, desc <chr>, play_type <chr>, yards_gained <dbl>, …

# It is also possible to directly use the
# output of `fast_scraper_schedules` as input
try({# to avoid CRAN test problems
library(dplyr, warn.conflicts = FALSE)
fast_scraper_schedules(2020) |>
  slice_tail(n = 3) |>
  build_nflfastR_pbp()
 })
#> Warning: `fast_scraper_schedules()` was deprecated in nflfastR 5.2.0.
#> ℹ Please use `nflreadr::load_schedules()` instead.
#> ── Build nflfastR Play-by-Play Data ───────────── nflfastR version 5.1.0.9009 ──
#> • 01:28:34 | Start download of 3 games...
#> ℹ It is recommended to use parallel processing when trying to load multiple games.Please consider running `future::plan("multisession")`! Will go on sequentially...
#> ✔ 01:28:38 | Download finished. Adding variables...
#> ✔ 01:28:38 | added game variables
#> ✔ 01:28:38 | added nflscrapR variables
#> ✔ 01:28:38 | added ep variables
#> ✔ 01:28:38 | added air_yac_ep variables
#> ✔ 01:28:39 | added wp variables
#> ✔ 01:28:39 | added air_yac_wp variables
#> ✔ 01:28:39 | added cp and cpoe
#> ✔ 01:28:39 | added fixed drive variables
#> ✔ 01:28:39 | added series variables
#> • 01:28:39 | Cleaning up play-by-play...
#> ✔ 01:28:39 | Cleaning completed
#> ✔ 01:28:39 | added qb_epa
#> • 01:28:39 | Computing xyac...
#> ✔ 01:28:41 | added xyac variables
#> • 01:28:41 | Computing xpass...
#> ✔ 01:28:41 | added xpass and pass_oe
#> • 01:28:41 | Decode player ids...
#> ✔ 01:28:41 | Decoding of player ids completed
#> ── DONE ────────────────────────────────────────────────────────────────────────
#> ── nflverse play by play ───────────────────────────────────────────────────────
#> ℹ Data updated: 2026-01-25 01:28:41 UTC
#> # A tibble: 539 × 372
#>    play_id game_id     old_game_id home_team away_team season_type  week posteam
#>      <dbl> <chr>       <chr>       <chr>     <chr>     <chr>       <int> <chr>  
#>  1       1 2020_20_BU… 2021012401  KC        BUF       POST           20 NA     
#>  2      42 2020_20_BU… 2021012401  KC        BUF       POST           20 BUF    
#>  3      57 2020_20_BU… 2021012401  KC        BUF       POST           20 BUF    
#>  4      78 2020_20_BU… 2021012401  KC        BUF       POST           20 BUF    
#>  5     102 2020_20_BU… 2021012401  KC        BUF       POST           20 BUF    
#>  6     123 2020_20_BU… 2021012401  KC        BUF       POST           20 BUF    
#>  7     145 2020_20_BU… 2021012401  KC        BUF       POST           20 BUF    
#>  8     174 2020_20_BU… 2021012401  KC        BUF       POST           20 BUF    
#>  9     207 2020_20_BU… 2021012401  KC        BUF       POST           20 BUF    
#> 10     236 2020_20_BU… 2021012401  KC        BUF       POST           20 BUF    
#> # ℹ 529 more rows
#> # ℹ 364 more variables: posteam_type <chr>, defteam <chr>, side_of_field <chr>,
#> #   yardline_100 <dbl>, game_date <chr>, quarter_seconds_remaining <dbl>,
#> #   half_seconds_remaining <dbl>, game_seconds_remaining <dbl>,
#> #   game_half <chr>, quarter_end <dbl>, drive <dbl>, sp <dbl>, qtr <dbl>,
#> #   down <dbl>, goal_to_go <int>, time <chr>, yrdln <chr>, ydstogo <dbl>,
#> #   ydsnet <dbl>, desc <chr>, play_type <chr>, yards_gained <dbl>, …

# }
```
