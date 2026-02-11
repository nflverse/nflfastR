# Update or Create a nflverse Play-by-Play Data Table in a Connected Database

The nflfastR play-by-play era dates back to 1999. To analyze all the
data efficiently, there is practically no alternative to working with a
database.

This function helps to create and maintain a table containing all
play-by-play data of the nflfastR era in a connected database.
Primarily, the preprocessed data from
[load_pbp](https://nflreadr.nflverse.com/reference/load_pbp.html) is
written to the database and, if necessary, supplemented with the latest
games using
[build_nflfastR_pbp](https://nflfastr.com/reference/build_nflfastR_pbp.md).

## Usage

``` r
update_pbp_db(conn, ..., name = "nflverse_pbp", seasons = most_recent_season())
```

## Arguments

- conn:

  A `DBIConnection` object, as returned by
  [`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html)

- ...:

  These dots are for future extensions and must be empty.

- name:

  The table name, passed on to
  [`dbQuoteIdentifier()`](https://dbi.r-dbi.org/reference/dbQuoteIdentifier.html).
  Options are:

  - a character string with the unquoted DBMS table name, e.g.
    `"table_name"`,

  - a call to [`Id()`](https://dbi.r-dbi.org/reference/Id.html) with
    components to the fully qualified table name, e.g.
    `Id(schema = "my_schema", table = "table_name")`

  - a call to [`SQL()`](https://dbi.r-dbi.org/reference/SQL.html) with
    the quoted and fully qualified table name given verbatim, e.g.
    `SQL('"my_schema"."table_name"')`

- seasons:

  Hybrid argument (logical or numeric) to update parts of or the
  complete play by play table within the database.

  It can update the play by play data table either for the whole
  nflfastR era (with `seasons = TRUE`) or just for specified seasons
  (e.g. `seasons = 2024:2025`).

  Defaults to
  [most_recent_season](https://nflreadr.nflverse.com/reference/latest_season.html).
  Please see details for further information.

## Value

Always returns the database connection invisibly.

## Details

### The `seasons` argument

The `seasons` argument controls how the table in the connected database
is handled.

With `seasons = TRUE`, the table in argument `name` will be removed
completely (by calling
[DBI::dbRemoveTable](https://dbi.r-dbi.org/reference/dbRemoveTable.html))
and all seasons of the nflfastR era will be added to a fresh table. This
is helpful when new columns are added during the offseason.

With a numerical vector, e.g. `seasons = 2024:2025`, the table in
argument `name` will be preserved and only rows from the given seasons
will be deleted and re-added (by calling
[DBI::dbAppendTable](https://dbi.r-dbi.org/reference/dbAppendTable.html)).
This is intended to be used for ongoing seasons because the NFL fixes
bugs in the underlying data during the week and we recommend rebuilding
the current season every Thursday during the season.

The default behavior is `seasons = most_recent_season()`, which means
that only the most recent season is updated or added.

To keep the table, and thus also the schema, but update all play-by-play
data of the nflfastR era, set

    seasons = seq(1999, most_recent_season())

If `seasons` contains multiple seasons, it is possible to control
whether the seasons are loaded individually and written to the database,
or whether multiple seasons should be processed in chunks. The latter is
more efficient because fewer write operations are required, but at the
same time, the data must first be stored in memory. The option
`“nflfastR.db_chunk_size”` can be used to control how many seasons are
loaded together in a chunk and written to the database. With the
following option, for example, 5 seasons are always loaded together and
written to the database.

    options("nflfastR.db_chunk_size" = 5L)

## Examples

``` r
# \donttest{
con <- DBI::dbConnect(duckdb::duckdb())
try({# to avoid CRAN test problems
update_pbp_db(con, seasons = 2024)
})
#> ── Update nflverse Play-by-Play Data in Connected Database ─────────────────────
#> ℹ Table "nflverse_pbp" does not yet exist in your connected database.
#> Do you wish to create it? (Y/n)
#> 
#> ℹ 07:55:16 | Initiate table "nflverse_pbp" with nflverse pbp schema
#> ℹ 07:55:16 | Drop 2024 season from table "nflverse_pbp"
#> ℹ 07:55:16 | Append 2024 season to table "nflverse_pbp"
#> ✔ 07:55:19 | Database update completed
#> ── DONE ────────────────────────────────────────────────────────────────────────
# }
```
