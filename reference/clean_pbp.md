# Clean Play by Play Data

Clean Play by Play Data

## Usage

``` r
clean_pbp(pbp, ...)
```

## Arguments

- pbp:

  is a Data frame of play-by-play data scraped using
  [`fast_scraper()`](https://nflfastr.com/reference/fast_scraper.md).

- ...:

  Additional arguments passed to a message function (for internal use).

## Value

The input Data Frame of the parameter 'pbp' with the following columns
added:

- success:

  Binary indicator wheter epa \> 0 in the given play.

- passer:

  Name of the dropback player (scrambles included) including plays with
  penalties.

- passer_jersey_number:

  Jersey number of the passer.

- rusher:

  Name of the rusher (no scrambles) including plays with penalties.

- rusher_jersey_number:

  Jersey number of the rusher.

- receiver:

  Name of the receiver including plays with penalties.

- receiver_jersey_number:

  Jersey number of the receiver.

- pass:

  Binary indicator if the play was a pass play (sacks and scrambles
  included).

- rush:

  Binary indicator if the play was a rushing play.

- special:

  Binary indicator if the play was a special teams play.

- first_down:

  Binary indicator if the play ended in a first down.

- aborted_play:

  Binary indicator if the play description indicates "Aborted".

- play:

  Binary indicator: 1 if the play was a 'normal' play (including
  penalties), 0 otherwise.

- passer_id:

  ID of the player in the 'passer' column.

- rusher_id:

  ID of the player in the 'rusher' column.

- receiver_id:

  ID of the player in the 'receiver' column.

- name:

  Name of the 'passer' if it is not 'NA', or name of the 'rusher'
  otherwise.

- fantasy:

  Name of the rusher on rush plays or receiver on pass plays.

- fantasy_id:

  ID of the rusher on rush plays or receiver on pass plays.

- fantasy_player_name:

  Name of the rusher on rush plays or receiver on pass plays (from
  official stats).

- fantasy_player_id:

  ID of the rusher on rush plays or receiver on pass plays (from
  official stats).

- jersey_number:

  Jersey number of the player listed in the 'name' column.

- id:

  ID of the player in the 'name' column.

- out_of_bounds:

  = 1 if play description contains "ran ob", "pushed ob", or "sacked
  ob"; = 0 otherwise.

- home_opening_kickoff:

  = 1 if the home team received the opening kickoff, 0 otherwise.

## Details

Build columns that capture what happens on all plays, including
penalties, using string extraction from play description. Loosely based
on Ben's nflfastR guide
(<https://www.nflfastr.com/articles/beginners_guide.html>) but updated
to work with the RS data, which has a different player format in the
play description; e.g. 24-M.Lynch instead of M.Lynch. The function also
standardizes team abbreviations so that, for example, the Chargers are
always represented by 'LAC' regardless of which year it was. Starting in
2022, play-by-play data was missing gsis player IDs of rookies. This
functions tries to fix as many as possible.

## See also

For information on parallel processing and progress updates please see
[nflfastR](https://nflfastr.com/reference/nflfastR-package.md).
