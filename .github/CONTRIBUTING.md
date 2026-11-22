# Contributing to nflfastR

Thanks for your interest in contributing! Issues and pull requests are welcome, from people and from AI agents.

If the play-by-play data of nflfastR is wrong, there are two possible reasons:

1. **The parser has a bug** or doesn't handle a situation correctly. Please open an issue that shows the problem (game ID, play ID, column, expected and actual value) or a pull request that fixes the parser.
2. **The raw data is wrong.** If the error follows a rule that can apply to other games too, it is fixed in the parser as well. If the raw data has a few wrong values in a few games and no rule finds them, they are fixed with a patch, see below.

## Patching errors of the raw data

[`inst/pbp_patches.dcf`](https://github.com/nflverse/nflfastR/blob/master/inst/pbp_patches.dcf) lists single errors of the raw data. nflfastR applies these patches while it parses a game, before it computes anything from the data, so a patch also changes everything derived from the patched value (e.g. EPA and win probability).

### Patch or parser fix?

- Fix the **parser** if the error follows a rule that can apply to other games too (e.g. all Jacksonville home games of 2001 and 2002 name the same team on every play), or if the fix has to compute values (e.g. the first downs of a game in which the NFL didn't record them).
- Add a **patch** if the raw data has a few wrong values in a few games and no rule finds them.

### Format

The file has the format of an R `DESCRIPTION` file (DCF): one record per patch, records separated by an empty line, one `field: value` per line. A long value can continue on the next lines if they are indented. Leave out the fields a patch doesn't use; a field that is left out is `NA`. Keep the records sorted by `game_id`, `play_id`, `action`, `stat_id` and `column` (the tests check this).

| field | meaning |
|---|---|
| `game_id` | nflfastR game ID |
| `play_id` | play ID. Left out in a `set` patch: all plays of the game |
| `action` | `drop_play`, `drop_stat` or `set`, see below |
| `stat_id` | the stat ID of a `drop_stat` patch |
| `column` | the column of a `set` patch |
| `from` | the value of `column` the raw data has now, left out for `NA` |
| `to` | the correct value, left out for `NA` |
| `reason` | what is wrong, with the issue number if there is one |

For example:

```
game_id: 2011_13_DET_NO
play_id: 1152
action: drop_play
reason: duplicated touchdown record (one for each team) that counts the
  points twice

game_id: 2012_04_NO_GB
play_id: 1085
action: set
column: time
from: 03:33
to: 3:34
reason: wrong game clock
```

- `drop_play` removes the play from the raw plays, before anything is derived from it. Use it for plays the raw data has twice.
- `drop_stat` removes all rows of the stat ID on the play from the raw play stats, before the stat columns (`rush_attempt`, `passer_player_id`, `td_team`, ...) are derived from them. Prefer it to `set` patches of stat columns, because one stat ID sets several columns that must agree.
- `set` changes the value of `column` after the plays are parsed. Some columns have other names at this point than in the final data:

  | final data | patch file |
  |---|---|
  | `qtr` | `quarter` |
  | `yrdln` | `yardline` |
  | `ydstogo` | `yards_to_go` |
  | `desc` | `play_description` |
  | `play_type` | `play_type_nfl` |

  `from` and `to` are converted to the class of the column. The value is only changed if it is `from` on all patched plays.

A patch that doesn't fit the raw data (a missing play or stat, a value other than `from`) warns with class `nflfastR_patch_not_applied` and changes nothing. `build_nflfastR_pbp()` suppresses these warnings, so `data-raw/pbp_patches/check_patches.R` (below) and an online test find patches that don't fit anymore, e.g. because the NFL fixed the raw data. `Rscript data-raw/pbp_patches/check_patches.R --prune` removes the patches that the raw data made obsolete: `set` patches where the raw data has the value `to` now, and `drop_stat` patches where the play still has stats, but not this stat ID. It only lists the other patches that don't fit (a missing play, a value other than `from` and `to`), because the error may still be there with other play IDs or values. A `set` patch of a column that doesn't exist or with a value that can't be converted is an error, and `build_nflfastR_pbp()` skips the game, so the tests check both for the patches in the file.

### Adding patches

1. Fork the repository and create a branch.
2. Add the records to `inst/pbp_patches.dcf`.
3. `devtools::test(filter = "helper_patch_pbp")` checks the format.
4. `Rscript data-raw/pbp_patches/check_patches.R` checks that every patch fits the raw data.
5. Build the patched games with `devtools::load_all()` and `build_nflfastR_pbp()`, and check the result.
6. Open a pull request that names the patched games and why the raw data is wrong.

If a patched game is one of the games of the regression test, the test fails because the output changes on purpose. The maintainers update the expected output, see [`data-raw/test_game_selection/README.md`](https://github.com/nflverse/nflfastR/blob/master/data-raw/test_game_selection/README.md).
