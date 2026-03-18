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
website](https://nflfastr.com/articles/field_descriptions.html) to learn
about the output.

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
#> ── Build nflfastR Play-by-Play Data ───────────── nflfastR version 5.2.0.9010 ──
#> • 19:51:32 | Start download of 2 games...
#> ℹ It is recommended to use parallel processing when trying to load multiple games.Please consider running `future::plan("multisession")`! Will go on sequentially...
#> ✔ 19:51:35 | Download finished. Adding variables...
#> ✔ 19:51:36 | added game variables
#> ✔ 19:51:36 | added nflscrapR variables
#> ✔ 19:51:36 | added ep variables
#> ✔ 19:51:37 | added air_yac_ep variables
#> ✔ 19:51:37 | added wp variables
#> ✔ 19:51:37 | added air_yac_wp variables
#> ✔ 19:51:37 | added cp and cpoe
#> ✔ 19:51:37 | added fixed drive variables
#> ✔ 19:51:37 | added series variables
#> • 19:51:37 | Cleaning up play-by-play...
#> ✔ 19:51:37 | Cleaning completed
#> ✔ 19:51:37 | added qb_epa
#> • 19:51:37 | Computing xyac...
#> ✔ 19:51:39 | added xyac variables
#> • 19:51:39 | Computing xpass...
#> ✔ 19:51:39 | added xpass and pass_oe
#> • 19:51:39 | Decode player ids...
#> ✔ 19:51:40 | Decoding of player ids completed
#> ── DONE ────────────────────────────────────────────────────────────────────────
#> ── nflverse play by play ───────────────────────────────────────────────────────
#> ℹ Data updated: 2026-03-18 19:51:40 UTC
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
#> #   down <dbl>, goal_to_go <dbl>, time <chr>, yrdln <chr>, ydstogo <dbl>,
#> #   ydsnet <dbl>, desc <chr>, play_type <chr>, yards_gained <dbl>, …

# It is also possible to directly use the
# output of `load_schedules` as input
try({# to avoid CRAN test problems
nflreadr::load_schedules(2025) |>
  dplyr::slice_tail(n = 3) |>
  build_nflfastR_pbp()
})
#> ── Build nflfastR Play-by-Play Data ───────────── nflfastR version 5.2.0.9010 ──
#> • 19:51:40 | Start download of 3 games...
#> ℹ It is recommended to use parallel processing when trying to load multiple games.Please consider running `future::plan("multisession")`! Will go on sequentially...
#> ✔ 19:51:43 | Download finished. Adding variables...
#> ✔ 19:51:43 | added game variables
#> ✔ 19:51:43 | added nflscrapR variables
#> ✔ 19:51:44 | added ep variables
#> ✔ 19:51:44 | added air_yac_ep variables
#> ✔ 19:51:44 | added wp variables
#> ✔ 19:51:44 | added air_yac_wp variables
#> ✔ 19:51:44 | added cp and cpoe
#> ✔ 19:51:44 | added fixed drive variables
#> ✔ 19:51:44 | added series variables
#> • 19:51:44 | Cleaning up play-by-play...
#> ✔ 19:51:44 | Cleaning completed
#> ✔ 19:51:44 | added qb_epa
#> • 19:51:44 | Computing xyac...
#> ✔ 19:51:45 | added xyac variables
#> • 19:51:45 | Computing xpass...
#> ✔ 19:51:45 | added xpass and pass_oe
#> • 19:51:45 | Decode player ids...
#> ✔ 19:51:46 | Decoding of player ids completed
#> ── DONE ────────────────────────────────────────────────────────────────────────
#> ── nflverse play by play ───────────────────────────────────────────────────────
#> ℹ Data updated: 2026-03-18 19:51:46 UTC
#> # A tibble: 533 × 372
#>    play_id game_id     old_game_id home_team away_team season_type  week posteam
#>      <dbl> <chr>       <chr>       <chr>     <chr>     <chr>       <int> <chr>  
#>  1       1 2025_21_LA… 2026012501  SEA       LA        POST           21 NA     
#>  2      39 2025_21_LA… 2026012501  SEA       LA        POST           21 LA     
#>  3      62 2025_21_LA… 2026012501  SEA       LA        POST           21 LA     
#>  4      84 2025_21_LA… 2026012501  SEA       LA        POST           21 LA     
#>  5     109 2025_21_LA… 2026012501  SEA       LA        POST           21 LA     
#>  6     132 2025_21_LA… 2026012501  SEA       LA        POST           21 LA     
#>  7     152 2025_21_LA… 2026012501  SEA       LA        POST           21 SEA    
#>  8     174 2025_21_LA… 2026012501  SEA       LA        POST           21 SEA    
#>  9     197 2025_21_LA… 2026012501  SEA       LA        POST           21 SEA    
#> 10     222 2025_21_LA… 2026012501  SEA       LA        POST           21 SEA    
#> # ℹ 523 more rows
#> # ℹ 364 more variables: posteam_type <chr>, defteam <chr>, side_of_field <chr>,
#> #   yardline_100 <dbl>, game_date <chr>, quarter_seconds_remaining <dbl>,
#> #   half_seconds_remaining <dbl>, game_seconds_remaining <dbl>,
#> #   game_half <chr>, quarter_end <dbl>, drive <dbl>, sp <dbl>, qtr <dbl>,
#> #   down <dbl>, goal_to_go <dbl>, time <chr>, yrdln <chr>, ydstogo <dbl>,
#> #   ydsnet <dbl>, desc <chr>, play_type <chr>, yards_gained <dbl>, …

# }
```
