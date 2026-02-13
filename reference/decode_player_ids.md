# Decode the player IDs in nflfastR play-by-play data

Takes all columns ending with `'player_id'` as well as the variables
`'passer_id'`, `'rusher_id'`, `'fantasy_id'`, `'receiver_id'`, and
`'id'` of an nflfastR play-by-play data set and decodes the player IDs
to the commonly known GSIS ID format 00-00xxxxx.

The function uses by default the high efficient
[decode_ids](https://rdrr.io/pkg/gsisdecoder/man/decode_ids.html) of the
package [`gsisdecoder`](https://cran.r-project.org/package=gsisdecoder).
In the unlikely event that there is a problem with this function, an
nflfastR internal decoder can be used with the option `fast = FALSE`.

The 2022 play by play data introduced new player IDs that can't be
decoded with gsisdecoder. In that case, IDs are joined through
[nflreadr::load_players](https://nflreadr.nflverse.com/reference/load_players.html).

## Usage

``` r
decode_player_ids(pbp, ..., fast = TRUE)
```

## Arguments

- pbp:

  is a Data frame of play-by-play data scraped using
  [`fast_scraper()`](https://nflfastr.com/reference/fast_scraper.md).

- ...:

  Additional arguments passed to a message function (for internal use).

- fast:

  If `TRUE` the IDs will be decoded with the high efficient function
  [decode_ids](https://rdrr.io/pkg/gsisdecoder/man/decode_ids.html). If
  `FALSE` an nflfastR internal function will be used for decoding (it is
  generally not recommended to do this, unless there is a problem with
  [decode_ids](https://rdrr.io/pkg/gsisdecoder/man/decode_ids.html)
  which can take several days to fix on CRAN.)

## Value

The input data frame of the parameter `pbp` with decoded player IDs.

## Examples

``` r
# \donttest{
# Decode data frame consisting of some names and ids
decode_player_ids(data.frame(
  name = c("P.Mahomes", "B.Baldwin", "P.Mahomes", "S.Carl", "J.Jones"),
  id = c(
    "32013030-2d30-3033-3338-3733fa30c4fa",
    NA_character_,
    "00-0033873",
    NA_character_,
    "32013030-2d30-3032-3739-3434d4d3846d"
  )
))
#> • 16:36:03 | Decode player ids...
#> ✔ 16:36:03 | Decoding of player ids completed
#>        name         id
#> 1 P.Mahomes 00-0033873
#> 2 B.Baldwin       <NA>
#> 3 P.Mahomes 00-0033873
#> 4    S.Carl       <NA>
#> 5   J.Jones 00-0027944
# }
```
