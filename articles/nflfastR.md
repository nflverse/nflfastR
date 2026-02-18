# Get started with nflfastR

If you are new to R or are having trouble understanding the code in the
below sections we highly recommend the **nflfastR beginner’s guide** in
[`vignette("beginners_guide")`](https://nflfastr.com/articles/beginners_guide.md).

## The Main Functions

nflfastR comes with a set of functions to access NFL play-by-play data.
This section provides a brief introduction to the essential functions.

nflfastR processes and cleans up play-by-play data and adds variables
through [it’s
models](https://www.opensourcefootball.com/posts/2020-09-28-nflfastr-ep-wp-and-cp-models/).
Since some of these tasks are performed by separate functions, the
easiest way to compute the complete nflfastR dataset is
[`build_nflfastR_pbp()`](https://nflfastr.com/reference/build_nflfastR_pbp.md).
The main input for that function is a set of game ids which can be
accessed with
[`load_schedules()`](https://nflreadr.nflverse.com/reference/load_schedules.html).
The following code demonstrates how to build the nflfastR dataset for
the Super Bowls of the 2017 - 2019 seasons.

``` r
ids <- nflreadr::load_schedules(2017:2019) |>
  dplyr::filter(game_type == "SB") |>
  dplyr::pull(game_id)
pbp <- nflfastR::build_nflfastR_pbp(ids)
#> ── Build nflfastR Play-by-Play Data ───────────── nflfastR version 5.2.0.9005 ──
#> • 07:38:26 | Start download of 3 games...
#> ✔ 07:38:29 | Download finished. Adding variables...
#> ✔ 07:38:29 | added game variables
#> ✔ 07:38:30 | added nflscrapR variables
#> ✔ 07:38:30 | added ep variables
#> ✔ 07:38:30 | added air_yac_ep variables
#> ✔ 07:38:31 | added wp variables
#> ✔ 07:38:31 | added air_yac_wp variables
#> ✔ 07:38:31 | added cp and cpoe
#> ✔ 07:38:31 | added fixed drive variables
#> ✔ 07:38:31 | added series variables
#> • 07:38:31 | Cleaning up play-by-play...
#> ✔ 07:38:31 | Cleaning completed
#> ✔ 07:38:31 | added qb_epa
#> • 07:38:31 | Computing xyac...
#> ✔ 07:38:33 | added xyac variables
#> • 07:38:33 | Computing xpass...
#> ✔ 07:38:33 | added xpass and pass_oe
#> • 07:38:33 | Decode player ids...
#> ✔ 07:38:35 | Decoding of player ids completed
#> ── DONE ────────────────────────────────────────────────────────────────────────
```

In most cases, however, it is not necessary to use this function for
individual games, because nflverse provides both a [data
release](https://github.com/nflverse/nflverse-data/releases/tag/pbp) and
two main play-by-play functions:
[`load_pbp()`](https://nflreadr.nflverse.com/reference/load_pbp.html)
and
[`update_pbp_db()`](https://nflfastr.com/reference/update_pbp_db.md). We
cover
[`load_pbp()`](https://nflreadr.nflverse.com/reference/load_pbp.html)
below, and please see \[Example 8: Using the built-in database
function\] for how to work with the database function
[`update_pbp_db()`](https://nflfastr.com/reference/update_pbp_db.md).

The easiest way to access the data from the release is the function
[`load_pbp()`](https://nflreadr.nflverse.com/reference/load_pbp.html).
It can load multiple seasons directly into memory and supports multiple
data formats. Loading all play-by-play data of the 2022-2024 seasons is
as easy as

``` r
pbp <- nflfastR::load_pbp(2022:2024)
```

Joining roster data to the play-by-play data set is possible as well.
The data can be accessed with the function
[`load_rosters()`](https://nflreadr.nflverse.com/reference/load_rosters.html)
and its application is demonstrated in \[Example 10: Working with roster
and position data\].

## Application Examples

All examples listed below assume that the following libraries are
installed (and loaded).

``` r
library(nflfastR)
library(nflplotR)
library(dplyr)
library(ggplot2)
```

### Example 1: Completion Percentage Over Expected (CPOE)

Let’s look at CPOE leaders from the 2009 regular season.

As discussed above, `nflfastR` has a data release for all available
seasons, so there’s no need to actually build them. Let’s use that here
with the convenience function
[`load_pbp()`](https://nflreadr.nflverse.com/reference/load_pbp.html)
which fetches data from the release (for non-R users, .csv and .parquet
are also available in the [data
release](https://github.com/nflverse/nflverse-data/releases/tag/pbp)).

``` r
games_2009 <- nflfastR::load_pbp(2009) |> dplyr::filter(season_type == "REG")
games_2009 |>
  dplyr::filter_out(is.na(cpoe)) |>
  dplyr::summarize(
    passer = nflreadr::stat_mode(passer_player_name),
    cpoe = mean(cpoe),
    Atts = n(),
    .by = passer_player_id
  ) |>
  dplyr::filter(Atts > 200) |>
  dplyr::slice_max(cpoe, n = 5) |>
  knitr::kable(digits = 1)
```

| passer_player_id | passer           | cpoe | Atts |
|:-----------------|:-----------------|-----:|-----:|
| 00-0020531       | D.Brees          |  7.5 |  509 |
| 00-0022942       | P.Rivers         |  6.6 |  474 |
| 00-0010346       | P.Manning        |  6.5 |  569 |
| 00-0005106       | B.Favre          |  6.0 |  527 |
| 00-0022924       | B.Roethlisberger |  5.4 |  503 |

### Example 2: Using Drive Information

When working with `nflfastR`, drive results are automatically included.
We use `fixed_drive` and `fixed_drive_result` since the NFL-provided
information is a bit wonky. Let’s look at how much more likely teams
were to score starting from 1st & 10 at their own 20 yard line in 2015
(the last year before touchbacks on kickoffs changed to the 25) than in
2000.

``` r
pbp <- nflfastR::load_pbp(c(2003, 2015))

out <- pbp |>
  dplyr::filter(
    season_type == "REG" & down == 1 & ydstogo == 10 & yardline_100 == 80
  ) |>
  dplyr::mutate(
    drive_score = dplyr::case_when(
      fixed_drive_result %in% c("Touchdown", "Field goal") ~ 1L,
      TRUE ~ 0L
    )
  ) |>
  dplyr::summarize(drive_score = mean(drive_score), .by = season)

out |>
  knitr::kable(digits = 3)
```

| season | drive_score |
|-------:|------------:|
|   2003 |       0.206 |
|   2015 |       0.305 |

So 20.6% of 1st & 10 plays from teams’ own 20 would see the drive end up
in a score in 2003, compared to 30.5% in 2015. This has implications for
Expected Points models (see [this
article](https://www.opensourcefootball.com/posts/2020-09-28-nflfastr-ep-wp-and-cp-models/)).

### Example 3: Plot offensive and defensive EPA per play for a given season

Let’s build the **[NFL team tiers](https://rbsdm.com/stats/stats/)**
using offensive and defensive expected points added per play for the
2005 regular season. Creating data viz including NFL team logos (or
wordmarks, or headshots), we recommend the nflverse R package
[nflplotR](https://nflplotr.nflverse.com).

When using
[`load_pbp()`](https://nflreadr.nflverse.com/reference/load_pbp.html),
the helper function
[`clean_pbp()`](https://nflfastr.com/reference/clean_pbp.md) has already
been run, which creates “rush” and “pass” columns that (a) properly
count sacks and scrambles as pass plays and (b) properly include plays
with penalties. Using this, we can keep only rush or pass plays.

``` r
pbp <- nflfastR::load_pbp(2005) |>
  dplyr::filter(season_type == "REG") |>
  dplyr::filter(!is.na(posteam) & (rush == 1 | pass == 1))
offense <- pbp |>
  dplyr::group_by(team = posteam) |>
  dplyr::summarise(off_epa = mean(epa, na.rm = TRUE))
defense <- pbp |>
  dplyr::group_by(team = defteam) |>
  dplyr::summarise(def_epa = mean(epa, na.rm = TRUE))
offense |>
  dplyr::inner_join(defense, by = "team") |>
  ggplot(aes(x = off_epa, y = def_epa)) +
  geom_abline(
    slope = -1.5,
    intercept = c(.4, .3, .2, .1, 0, -.1, -.2, -.3),
    alpha = .2
  ) +
  nflplotR::geom_mean_lines(aes(y0 = off_epa, x0 = def_epa)) +
  nflplotR::geom_nfl_logos(aes(team_abbr = team), width = 0.07, alpha = 0.7) +
  labs(
    x = "Offense EPA/play",
    y = "Defense EPA/play",
    caption = "Data: @nflfastR",
    title = "2005 NFL Offensive and Defensive EPA per Play"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5, face = "bold")
  ) +
  scale_y_reverse()
```

![](nflfastR_files/figure-html/ex5-1.png)

### Example 4: Expected Points calculator

We have provided a calculator for working with the Expected Points
model. Here is an example of how to use it, looking for how the Expected
Points on a drive beginning following a touchback has changed over time.

While I have put in `'SEA'` for `home_team` and `posteam`, this only
matters for figuring out whether the team with the ball is the home team
(there’s no actual effect for given team; it would be the same no matter
what team is supplied).

``` r
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
  dplyr::select(season, yardline_100, td_prob, ep) |>
  knitr::kable(digits = 2)
```

| season | yardline_100 | td_prob |   ep |
|-------:|-------------:|--------:|-----:|
|   1999 |           80 |    0.33 | 0.64 |
|   2000 |           80 |    0.33 | 0.64 |
|   2001 |           80 |    0.33 | 0.64 |
|   2002 |           80 |    0.34 | 0.82 |
|   2003 |           80 |    0.34 | 0.82 |
|   2004 |           80 |    0.34 | 0.82 |
|   2005 |           80 |    0.34 | 0.82 |
|   2006 |           80 |    0.34 | 0.81 |
|   2007 |           80 |    0.34 | 0.81 |
|   2008 |           80 |    0.34 | 0.81 |
|   2009 |           80 |    0.34 | 0.81 |
|   2010 |           80 |    0.34 | 0.81 |
|   2011 |           80 |    0.34 | 0.81 |
|   2012 |           80 |    0.34 | 0.81 |
|   2013 |           80 |    0.34 | 0.81 |
|   2014 |           80 |    0.35 | 0.98 |
|   2015 |           80 |    0.35 | 0.98 |
|   2016 |           75 |    0.38 | 1.46 |
|   2017 |           75 |    0.38 | 1.46 |
|   2018 |           75 |    0.41 | 1.47 |
|   2019 |           75 |    0.41 | 1.47 |

Not surprisingly, offenses have become much more successful over time,
with the kickoff touchback moving from the 20 to the 25 in 2016
providing an additional boost. Note that the `td_prob` in this example
is the probability that the next score within the same half will be a
touchdown scored by team with the ball, **not** the probability that the
current drive will end in a touchdown (this is why the numbers are
different from Example 4 above).

We could compare the most recent four years to the expectation for
playing in a dome by inputting all the same things and changing the
`roof` input:

``` r
data <- tibble::tibble(
  "season" = 2016:2019,
  "week" = 5,
  "home_team" = "SEA",
  "posteam" = "SEA",
  "roof" = "dome",
  "half_seconds_remaining" = 1800,
  "yardline_100" = c(rep(75, 4)),
  "down" = 1,
  "ydstogo" = 10,
  "posteam_timeouts_remaining" = 3,
  "defteam_timeouts_remaining" = 3
)
nflfastR::calculate_expected_points(data) |>
  dplyr::select(season, yardline_100, td_prob, ep) |>
  knitr::kable(digits = 2)
```

| season | yardline_100 | td_prob |   ep |
|-------:|-------------:|--------:|-----:|
|   2016 |           75 |    0.41 | 1.81 |
|   2017 |           75 |    0.41 | 1.81 |
|   2018 |           75 |    0.44 | 1.84 |
|   2019 |           75 |    0.44 | 1.84 |

So for 2018 and 2019, 1st & 10 from a home team’s own 25 yard line had
higher EP in domes than at home, which is to be expected.

### Example 5: Win probability calculator

We have also provided a calculator for working with the win probability
models. Here is an example of how to use it, looking for how the win
probability to begin the game depends on the pre-game spread.

While I have put in `'SEA'` for `home_team` and `posteam`, this only
matters for figuring out whether the team with the ball is the home team
(there’s no actual effect for given team; it would be the same no matter
what team is supplied).

``` r
data <- tibble::tibble(
  "receive_2h_ko" = 0,
  "home_team" = "SEA",
  "posteam" = "SEA",
  "score_differential" = 0,
  "half_seconds_remaining" = 1800,
  "game_seconds_remaining" = 3600,
  "spread_line" = c(1, 3, 4, 7, 14),
  "down" = 1,
  "ydstogo" = 10,
  "yardline_100" = 75,
  "posteam_timeouts_remaining" = 3,
  "defteam_timeouts_remaining" = 3
)
nflfastR::calculate_win_probability(data) |>
  dplyr::select(spread_line, wp, vegas_wp) |>
  knitr::kable(digits = 2)
```

| spread_line |   wp | vegas_wp |
|------------:|-----:|---------:|
|           1 | 0.55 |     0.51 |
|           3 | 0.55 |     0.60 |
|           4 | 0.55 |     0.64 |
|           7 | 0.55 |     0.74 |
|          14 | 0.55 |     0.87 |

Not surprisingly, `vegas_wp` increases with the amount a team was coming
into the game favored by.

### Example 6: Using the built-in database function

If you’re comfortable using `dplyr` functions to manipulate and tidy
data, you’re ready to use a database. Why should you use a database?

- The provided function in `nflfastR` makes it extremely easy to build a
  database and keep it updated
- Play-by-play data over 25+ seasons takes up a lot of memory: working
  with a database allows you to only bring into memory what you actually
  need
- R makes it *extremely* easy to work with databases.

#### Start: install and load packages

To start, we need to install the two packages required for this that
aren’t installed automatically when `nflfastR` installs: `DBI` and
`duckdb` (advanced users can use other types of databases, but this
example will use duckdb). The `if` statements make sure the packages
won’t be updated if they are already installed:

``` r
if (!require("DBI")) install.packages("DBI")
if (!require("duckdb")) install.packages("duckdb")
```

#### Overview

There’s exactly one function in `nflfastR` that works with databases:
[`update_pbp_db()`](https://nflfastr.com/reference/update_pbp_db.md).
Some notes:

- [`update_pbp_db()`](https://nflfastr.com/reference/update_pbp_db.md)
  follows the DBI argument naming convention and order. It requires an
  open connection created with
  [`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html).
- You can specify a different table name with `name`.
- The `seasons` argument controls how the table in the connected
  database is handled. This is a hybrid argument, and its behavior is
  described in detail [in the function
  documentation](https://nflfastr.com/reference/update_pbp_db.html#the-seasons-argument).
- If larger parts of the DB need to be updated, then you should
  definitely consider doing so in chunks. The `"nflfastR.db_chunk_size"`
  option is available for this purpose. Further details can also be
  found in the function documentation.

#### Connect to a database

Working with databases always requires an open connection. In this
example, we will focus solely on duckdb databases, as duckdb has
essentially become the state of the art for this type of data. duckdb
can easily create a database in your memory. Of course, this doesn’t
make sense for large amounts of data, because they shouldn’t be stored
in memory, but the process is practically identical with a locally
stored database.

So let’s connect to an in-memory duckdb database:

``` r
connection <- DBI::dbConnect(duckdb::duckdb())
connection
#> <duckdb_connection 66b40 driver=<duckdb_driver dbdir=':memory:' read_only=FALSE bigint=numeric>>
```

#### Write data to the database

Let’s say I just want to dump play-by-play data of the 2021 - 2024
seasons in my database. Here we go!

``` r
nflfastR::update_pbp_db(connection, seasons = 2021:2024)
#> ── Update nflverse Play-by-Play Data in Connected Database ─────────────────────
#> ℹ Table "nflverse_pbp" does not yet exist in your connected database.
#> Do you wish to create it? (Y/n)
#> ℹ 07:39:03 | Initiate table "nflverse_pbp" with nflverse pbp schema
#> ℹ 07:39:03 | Drop 2021, 2022, 2023, and 2024 seasons from table "nflverse_pbp"
#> ℹ 07:39:03 | Append 2021, 2022, 2023, and 2024 seasons to table "nflverse_pbp"
#> ✔ 07:39:18 | Database update completed
#> ── DONE ────────────────────────────────────────────────────────────────────────
```

This created a table named “nflverse_pbp” in the connected database and
appended 2024 play-by-play data to it.

Wait, that’s it? That’s it! What if it’s partway through the season and
you want to make sure all the new games are added to the database to
allow for data corrections from the NFL to propagate into your database?
What do you run?
[`update_pbp_db()`](https://nflfastr.com/reference/update_pbp_db.md)!

``` r
nflfastR::update_pbp_db(connection)
#> ── Update nflverse Play-by-Play Data in Connected Database ─────────────────────
#> ℹ 07:39:19 | Drop 2025 season from table "nflverse_pbp"
#> ℹ 07:39:19 | Append 2025 season to table "nflverse_pbp"
#> ✔ 07:39:26 | Database update completed
#> ── DONE ────────────────────────────────────────────────────────────────────────
```

#### Work with the database

Now we’re ready to do stuff. If you aren’t familiar with databases,
they’re organized around tables. Here’s how to see which tables are
present in our database:

``` r
DBI::dbListTables(connection)
#> [1] "nflverse_pbp"
```

Since we went with the defaults, there’s a table called `nflverse_pbp`.
Another useful function is to see the fields (i.e., columns) in a table:

``` r
DBI::dbListFields(connection, "nflverse_pbp") |>
  utils::head(10)
#>  [1] "play_id"      "game_id"      "old_game_id"  "home_team"    "away_team"   
#>  [6] "season_type"  "week"         "posteam"      "posteam_type" "defteam"
```

This is the same list as the list of columns in `nflfastR` play-by-play.
Notice we had to supply the name of the table above (`"nflverse_pbp"`).

With all that out of the way, there’s only a couple more things to
learn. The main driver here is `tbl`, which helps get output with a
specific table in a database:

``` r
pbp_db <- dplyr::tbl(connection, "nflverse_pbp")
```

And now, everything will magically just “work”: you can forget you’re
even working with a database!

``` r
pbp_db |>
  dplyr::group_by(season) |>
  dplyr::summarize(n = dplyr::n())
#> # Source:   SQL [?? x 2]
#> # Database: DuckDB 1.4.4 [unknown@Linux 6.14.0-1017-azure:R 4.5.2/:memory:]
#>   season     n
#>    <int> <dbl>
#> 1   2021 49922
#> 2   2022 49434
#> 3   2023 49665
#> 4   2024 49492
#> 5   2025 48771
pbp_db |>
  dplyr::filter(
    rush == 1 | pass == 1,
    down <= 2,
    !is.na(epa),
    !is.na(posteam)
  ) |>
  dplyr::group_by(pass) |>
  dplyr::summarize(mean_epa = mean(epa, na.rm = TRUE))
#> # Source:   SQL [?? x 2]
#> # Database: DuckDB 1.4.4 [unknown@Linux 6.14.0-1017-azure:R 4.5.2/:memory:]
#>    pass mean_epa
#>   <dbl>    <dbl>
#> 1     0  -0.0865
#> 2     1   0.0717
```

So far, everything has stayed in the database. If you want to bring a
query into memory, just use
[`collect()`](https://dplyr.tidyverse.org/reference/compute.html) at the
end:

``` r
russ <- pbp_db |>
  dplyr::filter(name == "R.Wilson" & posteam == "SEA") |>
  dplyr::select(desc, epa) |>
  dplyr::collect()
russ
#> # A tibble: 502 × 2
#>    desc                                                                      epa
#>    <chr>                                                                   <dbl>
#>  1 (4:54) 3-R.Wilson scrambles right end pushed ob at SEA 25 for 2 yards… -0.275
#>  2 (4:11) (Shotgun) 3-R.Wilson scrambles right tackle to SEA 31 for 11 y…  2.05 
#>  3 (1:37) (No Huddle, Shotgun) 3-R.Wilson pass incomplete deep right.     -0.591
#>  4 (1:31) (Shotgun) 3-R.Wilson pass deep middle to 16-T.Lockett for 23 y…  3.41 
#>  5 (14:50) 3-R.Wilson pass short right to 81-G.Everett to 50 for 11 yard…  0.751
#>  6 (13:48) (No Huddle, Shotgun) 3-R.Wilson pass short left to 89-W.Dissl…  0.663
#>  7 (12:51) 3-R.Wilson pass deep right to 89-W.Dissly to IND 13 for 22 ya…  1.89 
#>  8 (11:27) (Shotgun) 3-R.Wilson pass short middle to 81-G.Everett for 9 …  2.18 
#>  9 (7:04) 3-R.Wilson pass short right to 32-C.Carson pushed ob at SEA 41…  0.961
#> 10 (6:04) 3-R.Wilson pass short right to 16-T.Lockett to SEA 47 for 4 ya… -0.195
#> # ℹ 492 more rows
```

So we’ve searched through 247,284 rows of data across 300+ columns and
only brought about 500 rows and two columns into memory. Pretty neat!
This is how we supply the data to the shiny apps on rbsdm.com without
running out of memory on the server. Now there’s only one more thing to
remember. When you’re finished doing what you need with the database:

``` r
DBI::dbDisconnect(connection)
```

For more details on using a database with `nflfastR`, see [Thomas Mock’s
life-changing post
here](https://themockup.blog/posts/2019-04-28-nflfastr-dbplyr-rsqlite/).
More detailed information on dbplyr (the dplyr database back-end) are
given in the second edition of [Hadley Wickham’s R for Data Science
(2e)](https://r4ds.hadley.nz/databases.html).

### Example 7: working with the expected yards after catch model

The variables in `xyac` are as follows:

- `xyac_epa`: The expected value of EPA gained after the catch,
  **starting from where the catch was made**.
- `xyac_success`: The probability the play earns positive EPA (relative
  to where play started) based on where ball was caught.
- `xyac_fd`: Probability play earns a first down based on where the ball
  was caught.
- `xyac_mean_yardage` and `xyac_median_yardage`: Average and median
  expected yards after the catch based on where the ball was caught.

Some other notes:

- `epa` = `air_epa` + `yac_epa`, where `air_epa` is the EPA associated
  with a catch at the target location. If a receiver loses a fumble, it
  is removed from his `yac_epa`
- Expected value of EPA at catch point = `air_epa` + `xyac_epa`
- So if we want to get YAC EPA over expected, we need to compare
  `yac_epa` to `xyac_epa`, as in the example below
- To get first downs over expected, we could compare `first_down` to
  `xyac_fd`
- These fields are populated for all pass attempts, whether caught or
  not, but restrict to completed passes when measuring, for example, YAC
  EPA over expected
- The expected YAC EPA model doesn’t take receiver fumbles into account,
  so actual minus expected YAC is slightly negative due to fumbles
  happening

Let’s create measures for EPA and first downs over expected in 2015:

``` r
nflfastR::load_pbp(2015) |>
  dplyr::group_by(receiver, receiver_id, posteam) |>
  dplyr::mutate(tgt = sum(complete_pass + incomplete_pass)) |>
  dplyr::filter(tgt >= 50) |>
  dplyr::filter(
    complete_pass == 1,
    air_yards < yardline_100,
    !is.na(xyac_epa)
  ) |>
  dplyr::summarize(
    epa_oe = mean(yac_epa - xyac_epa),
    actual_fd = mean(first_down),
    expected_fd = mean(xyac_fd),
    fd_oe = mean(first_down - xyac_fd),
    rec = dplyr::n()
  ) |>
  dplyr::ungroup() |>
  dplyr::select(
    receiver,
    posteam,
    actual_fd,
    expected_fd,
    fd_oe,
    epa_oe,
    rec
  ) |>
  dplyr::slice_max(epa_oe, n = 10) |>
  knitr::kable(digits = 3)
```

| receiver      | posteam | actual_fd | expected_fd |  fd_oe | epa_oe | rec |
|:--------------|:--------|----------:|------------:|-------:|-------:|----:|
| D.Johnson     | ARI     |     0.500 |       0.391 |  0.109 |  0.334 |  50 |
| J.White       | NE      |     0.489 |       0.434 |  0.055 |  0.264 |  47 |
| T.Ginn        | CAR     |     0.800 |       0.734 |  0.066 |  0.249 |  45 |
| D.Lewis       | NE      |     0.472 |       0.309 |  0.163 |  0.239 |  36 |
| L.Green       | LAC     |     0.629 |       0.526 |  0.103 |  0.216 |  35 |
| O.Beckham Jr. | NYG     |     0.692 |       0.706 | -0.014 |  0.207 |  91 |
| G.Bernard     | CIN     |     0.373 |       0.289 |  0.083 |  0.204 |  51 |
| T.Riddick     | DET     |     0.400 |       0.304 |  0.096 |  0.203 |  80 |
| D.Woodhead    | LAC     |     0.468 |       0.354 |  0.114 |  0.172 |  77 |
| T.Lockett     | SEA     |     0.588 |       0.548 |  0.040 |  0.163 |  51 |

The presence of so many running backs on this list suggests that even
though it takes into account target depth and pass direction, the model
doesn’t do a great job capturing space. Alternatively, running backs
might be better at generating yards after the catch since running with
the football is their primary role.

### Example 8: Working with roster and position data

At long last, there’s a way to merge the new play-by-play data with
roster information. Use the function to get the rosters:

``` r
roster <- nflfastR::load_rosters(2019)
```

Now let’s load play-by-play data from 2019:

``` r
games_2019 <- nflfastR::load_pbp(2019)
```

Here is what the player IDs look like because `nflfastR` now
automatically decodes IDs to look like the old format with GSIS IDs:

``` r
games_2019 |>
  dplyr::filter(rush == 1 | pass == 1, posteam == "SEA") |>
  dplyr::select(name, id)
#> ── nflverse play by play data ──────────────────────────────────────────────────
#> ℹ Data updated: 2025-07-31 18:16:40 UTC
#> # A tibble: 1,207 × 2
#>    name     id        
#>    <chr>    <chr>     
#>  1 C.Carson 00-0033594
#>  2 R.Wilson 00-0029263
#>  3 R.Wilson 00-0029263
#>  4 C.Carson 00-0033594
#>  5 R.Wilson 00-0029263
#>  6 C.Carson 00-0033594
#>  7 R.Wilson 00-0029263
#>  8 C.Carson 00-0033594
#>  9 R.Wilson 00-0029263
#> 10 R.Wilson 00-0029263
#> # ℹ 1,197 more rows
```

Now we’re ready to join to the roster data using these IDs:

``` r
joined <- games_2019 |>
  dplyr::filter(!is.na(receiver_id)) |>
  dplyr::select(posteam, season, desc, receiver, receiver_id, epa) |>
  dplyr::left_join(roster, by = c("receiver_id" = "gsis_id"))
```

``` r
# the real work is done, this just makes a table and has it look nice
joined |>
  dplyr::filter(position %in% c("WR", "TE", "RB")) |>
  dplyr::group_by(receiver_id, receiver, position) |>
  dplyr::summarize(tot_epa = sum(epa), n = n()) |>
  dplyr::arrange(-tot_epa) |>
  dplyr::ungroup() |>
  dplyr::group_by(position) |>
  dplyr::mutate(position_rank = 1:n()) |>
  dplyr::filter(position_rank <= 5) |>
  dplyr::rename(
    Pos_Rank = position_rank,
    Player = receiver,
    Pos = position,
    Tgt = n,
    EPA = tot_epa
  ) |>
  dplyr::select(Player, Pos, Pos_Rank, Tgt, EPA) |>
  knitr::kable(digits = 0)
```

| Player      | Pos | Pos_Rank | Tgt | EPA |
|:------------|:----|---------:|----:|----:|
| M.Thomas    | WR  |        1 | 199 | 105 |
| T.Kelce     | TE  |        1 | 179 | 100 |
| C.Godwin    | WR  |        2 | 123 |  87 |
| D.Adams     | WR  |        3 | 161 |  77 |
| T.Lockett   | WR  |        4 | 140 |  76 |
| J.Jones     | WR  |        5 | 164 |  72 |
| G.Kittle    | TE  |        2 | 129 |  56 |
| C.McCaffrey | RB  |        1 | 148 |  52 |
| D.Waller    | TE  |        3 | 124 |  45 |
| A.Ekeler    | RB  |        2 | 113 |  44 |
| J.Cook      | TE  |        4 |  75 |  43 |
| Z.Ertz      | TE  |        5 | 147 |  42 |
| J.White     | RB  |        3 | 105 |  27 |
| D.Cook      | RB  |        4 |  77 |  26 |
| K.Juszczyk  | RB  |        5 |  28 |  23 |

Not surprisingly, all 5 of the top 5 WRs in terms of EPA added come in
ahead of the top RB. Note that the number of targets won’t match
official stats because we’re including plays with penalties.

### Example 9: Replicating official stats

The columns like `name`, `passer`, `fantasy` etc are `nflfastR`-created
columns that mimic “real” football: i.e., excluding plays with spikes,
counting scrambles and sacks as pass plays, etc. But if you’re trying to
replicate official statistics – perhaps for fantasy purposes – use the
`*_player_name` and `*_player_id` columns.

[Let’s try to replicate this page of passing
leaders](https://www.nfl.com/stats/player-stats/).

``` r
nflfastR::load_pbp(2020) |>
  dplyr::filter(
    season_type == "REG",
    complete_pass == 1 | incomplete_pass == 1 | interception == 1,
    !is.na(down)
  ) |>
  dplyr::group_by(passer_player_name, posteam) |>
  dplyr::summarize(
    yards = sum(passing_yards, na.rm = T),
    tds = sum(touchdown == 1 & td_team == posteam),
    ints = sum(interception),
    att = dplyr::n()
  ) |>
  dplyr::arrange(-yards) |>
  utils::head(10) |>
  knitr::kable(digits = 0)
```

| passer_player_name | posteam | yards | tds | ints | att |
|:-------------------|:--------|------:|----:|-----:|----:|
| D.Watson           | HOU     |  4823 |  33 |    7 | 544 |
| P.Mahomes          | KC      |  4740 |  38 |    6 | 588 |
| T.Brady            | TB      |  4633 |  40 |   12 | 610 |
| M.Ryan             | ATL     |  4581 |  26 |   11 | 626 |
| J.Allen            | BUF     |  4544 |  37 |   10 | 572 |
| J.Herbert          | LAC     |  4336 |  31 |   10 | 595 |
| A.Rodgers          | GB      |  4299 |  48 |    5 | 526 |
| K.Cousins          | MIN     |  4265 |  35 |   13 | 516 |
| R.Wilson           | SEA     |  4212 |  40 |   13 | 558 |
| P.Rivers           | IND     |  4169 |  24 |   11 | 543 |

These match the official stats on NFL.com (note the filter for
`season_type == "REG"` since official stats only count regular season
games). Note that we’re using `passing_yards` here because
`yards_gained` is not equal to passing yards on plays with laterals.

While the above code works in this case, there are several special cases
where it is nearly impossible to get official player stats from nflfastR
play-by-play data. The reason for this is that the idea of nflfastR
play-by-play data is a “tidy” data structure. In other words, the aim is
to have one row per play in the data. This can lead to problems if, for
example, there are several changes of possession per play (i.e. several
fumbles) or if the ball is lateraled in a play. These are just two
examples of “abnormal” plays that are not fully captured in a tidy data
structure. We have solved this problem with the function
[`calculate_stats()`](https://nflfastr.com/reference/calculate_stats.md).
This function uses playstats of the raw play-by-play data before it is
parsed into a tidy structure by nflfastR.

This function has the following features:

- It determines stats in offense, defense, and special teams,
- either on player level or on team level,
- and can summarize them on season level (separately for regular season
  and post season) or on week level.

For more information see the function documentation of
[`calculate_stats()`](https://nflfastr.com/reference/calculate_stats.md).
Again, **don’t try to get an exact match with official stats based on
nflfastR play-by-play data**. It usually works, but fails because of
details that are unsolvable.

Now let’s replicate the above table using
[`calculate_stats()`](https://nflfastr.com/reference/calculate_stats.md):

``` r
s <- nflfastR::calculate_stats(
  seasons = 2020,
  summary_level = "season",
  stat_type = "player",
  season_type = "REG"
)
s |>
  dplyr::slice_max(passing_yards, n = 10) |>
  dplyr::select(
    player_name,
    recent_team,
    completions,
    attempts,
    passing_yards,
    passing_tds,
    passing_interceptions,
    attempts
  ) |>
  knitr::kable(digits = 0)
```

| player_name | recent_team | completions | attempts | passing_yards | passing_tds | passing_interceptions |
|:------------|:------------|------------:|---------:|--------------:|------------:|----------------------:|
| D.Watson    | HOU         |         382 |      544 |          4823 |          33 |                     7 |
| P.Mahomes   | KC          |         390 |      588 |          4740 |          38 |                     6 |
| T.Brady     | TB          |         401 |      610 |          4633 |          40 |                    12 |
| M.Ryan      | ATL         |         407 |      626 |          4581 |          26 |                    11 |
| J.Allen     | BUF         |         396 |      572 |          4544 |          37 |                    10 |
| J.Herbert   | LAC         |         396 |      595 |          4336 |          31 |                    10 |
| A.Rodgers   | GB          |         372 |      526 |          4299 |          48 |                     5 |
| K.Cousins   | MIN         |         349 |      516 |          4265 |          35 |                    13 |
| R.Wilson    | SEA         |         384 |      558 |          4212 |          40 |                    13 |
| P.Rivers    | IND         |         369 |      543 |          4169 |          24 |                    11 |

The same applies to stats data as to pbp data. Its computation is
costly, but can be automated. There is therefore rarely a reason to call
[`calculate_stats()`](https://nflfastr.com/reference/calculate_stats.md)
directly. Instead, nflverse offers the functions
[`nflfastR::load_player_stats()`](https://nflreadr.nflverse.com/reference/load_player_stats.html)
and
[`nflfastR::load_team_stats()`](https://nflreadr.nflverse.com/reference/load_team_stats.html)
to load precomputed data from data releases.

## Frequent issues

### The `drive` column looks wacky

Use `fixed_drive` and `fixed_drive_result` instead. See [Example 2:
Using Drive Information](#example-2-using-drive-information).

### Why are there so many win probability columns?

`vegas_wp` and `vegas_home_wp` incorporate the pregame spread and are
much better models.

### Need more help?

Please ask [in the nflverse Discord
server](https://discord.com/invite/5Er2FBnnQa).
