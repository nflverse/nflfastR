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
#> ── Build nflfastR Play-by-Play Data ────────────────── nflfastR version 5.2.0 ──
#> • 12:06:47 | Start download of 2 games...
#> ℹ It is recommended to use parallel processing when trying to load multiple games.Please consider running `future::plan("multisession")`! Will go on sequentially...
#> ✔ 12:06:50 | Download finished. Adding variables...
#> ✔ 12:06:51 | added game variables
#> ✔ 12:06:51 | added nflscrapR variables
#> ✔ 12:06:52 | added ep variables
#> ✔ 12:06:52 | added air_yac_ep variables
#> ✔ 12:06:52 | added wp variables
#> ✔ 12:06:52 | added air_yac_wp variables
#> ✔ 12:06:52 | added cp and cpoe
#> ✔ 12:06:52 | added fixed drive variables
#> ✔ 12:06:53 | added series variables
#> • 12:06:53 | Cleaning up play-by-play...
#> ✔ 12:06:53 | Cleaning completed
#> ✔ 12:06:53 | added qb_epa
#> • 12:06:53 | Computing xyac...
#> ✔ 12:06:55 | added xyac variables
#> • 12:06:55 | Computing xpass...
#> ✔ 12:06:55 | added xpass and pass_oe
#> • 12:06:55 | Decode player ids...
#> ✔ 12:06:56 | Decoding of player ids completed
#> ── DONE ────────────────────────────────────────────────────────────────────────
#> ── nflverse play by play ───────────────────────────────────────────────────────
#> ℹ Data updated: 2026-02-08 12:06:56 UTC
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
# output of `load_schedules` as input
try({# to avoid CRAN test problems
nflreadr::load_schedules(2025) |>
  dplyr::slice_tail(n = 3) |>
  build_nflfastR_pbp()
})
#> ── Build nflfastR Play-by-Play Data ────────────────── nflfastR version 5.2.0 ──
#> • 12:06:56 | Start download of 3 games...
#> ℹ It is recommended to use parallel processing when trying to load multiple games.Please consider running `future::plan("multisession")`! Will go on sequentially...
#> ! The requested GameID "2025_22_SEA_NE" is not loaded yet, please try
#> again later!
#> ✔ 12:06:59 | Download finished. Adding variables...
#> ✔ 12:06:59 | added game variables
#> ✔ 12:06:59 | added nflscrapR variables
#> ✔ 12:06:59 | added ep variables
#> ✔ 12:06:59 | added air_yac_ep variables
#> ✔ 12:07:00 | added wp variables
#> ✔ 12:07:00 | added air_yac_wp variables
#> ✔ 12:07:00 | added cp and cpoe
#> ✔ 12:07:00 | added fixed drive variables
#> ✔ 12:07:00 | added series variables
#> • 12:07:00 | Cleaning up play-by-play...
#> ✔ 12:07:00 | Cleaning completed
#> ✔ 12:07:00 | added qb_epa
#> • 12:07:00 | Computing xyac...
#> ✔ 12:07:01 | added xyac variables
#> • 12:07:01 | Computing xpass...
#> ✔ 12:07:01 | added xpass and pass_oe
#> • 12:07:01 | Decode player ids...
#> ✔ 12:07:01 | Decoding of player ids completed
#> ── DONE ────────────────────────────────────────────────────────────────────────
#> ── nflverse play by play ───────────────────────────────────────────────────────
#> ℹ Data updated: 2026-02-08 12:07:01 UTC
#> # A tibble: 340 × 372
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
#> # ℹ 330 more rows
#> # ℹ 364 more variables: posteam_type <chr>, defteam <chr>, side_of_field <chr>,
#> #   yardline_100 <dbl>, game_date <chr>, quarter_seconds_remaining <dbl>,
#> #   half_seconds_remaining <dbl>, game_seconds_remaining <dbl>,
#> #   game_half <chr>, quarter_end <dbl>, drive <dbl>, sp <dbl>, qtr <dbl>,
#> #   down <dbl>, goal_to_go <int>, time <chr>, yrdln <chr>, ydstogo <dbl>,
#> #   ydsnet <dbl>, desc <chr>, play_type <chr>, yards_gained <dbl>, …

# }
```
