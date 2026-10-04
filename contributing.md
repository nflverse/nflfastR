# Contributing to nflfastR

Thanks for your interest in contributing! There are many ways to make a valuable 
contribution to an open-source project, such as issues or pull requests.

This guide focuses specifically on how users (or AI agents) can make specific 
data corrections if the raw data contains minor errors.

Basically, there are two possible reasons why data in nflfastR might be incorrect:

1. The parser has a bug and does not handle a specific situation correctly
1. The raw data is incorrect

In the first case, please submit issues or pull requests that explain the 
problem and, if necessary, propose changes to the parser.

For the second case, nflfastR provides a mechanism for manually overwriting the 
data before the parser applies its usual modifications to the raw data.

## Contribute manual data corrections

Manual corrections should be the last resort when we have no other way to fix 
an error. Alternatively, we can also use them if a fix in the parser would be 
very time-consuming or potentially prone to errors. A typical example is rare 
errors in the raw data, particularly in the raw data from older seasons. So if 
there are only a handful of incorrect values, this might be the right approach.

Here's how to submit manual corrections:

1. fork the repo
1. create new branch (we have no naming conventions)
1. add patch data to `"inst/patch_pbp.json"` and save the file
1. run  `devtools::load_all()`
1. run `.patch_clean_data()` interactively to ensure correct sorting and to
remove duplicates
1. run `devtools::test(filter = "patch_pbp")`. This will run tests aimed at the 
file format of the patch file in `"inst/patch_pbp.json"`.
1. run `build_nflfastR_pbp()` with the `game_id`s  affected by your correction 
to confirm that your patch works as intended
1. commit and push
1. create the pull request

A patch in the `"inst/patch_pbp.json"` looks like this

```json
[
  {
    "game_id": "2015_01_IND_BUF",
    "play_id": 1207,
    "column": "qtr",
    "value": [2]
  },
  {
    "game_id": "2014_07_ATL_BAL",
    "play_id": 99999,
    "column": "start_time",
    "value": ["10/19/14, 13:02:00"]
  }
]
```

Some notes:

- The first entry overwrites the `qtr` variable in the play-by-play data of 
`game_id 2015_01_IND_BUF` and `play_id 1207` with the integer `2`.
- The second entry overwrites the `start_time` variable in the play-by-play data 
of the complete `game_id 2015_01_IND_BUF` with the string `"10/19/14, 13:02:00"`.
- Please note the `[]` brackets in the value field. They allow us to save `value`
in different classes, i.e. `integer`, `numeric`, `character`.
- Integers and numerics don't need quotes. We want to preserve their classes.
- The field `play_id` is also unquoted and should be an integer. 
- The special `play_id 99999` is a placeholder and tells nflfastR to apply the 
patch to the complete column, i.e. all `play_id`s of the specified `game_id`. 
- Only `value` is allowed to be `NA`, which means you would like to overwrite 
specifically with `NA`.

Package tests should enforce the described format.
