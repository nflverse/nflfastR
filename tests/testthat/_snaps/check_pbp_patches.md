# check_pbp_patches() finds and prunes obsolete and unused patches

    Code
      check_pbp_patches(dir = test_dir, prune = TRUE, file = file, games = g)
    Message
      Checking the patches of 1 game
      v Removed 2 obsolete or unused patches:
      * obsolete: 2011_13_DET_NO set play 771 time: time of play_id 771 is "02:42",
        not "02:43" as the patch expects
      * unused: 2011_13_DET_NO drop_play play 842: the pbp is the same without it
    Condition
      Error in `check_pbp_patches()`:
      ! 1 patch needs a look:
      x not_applied: 2011_13_DET_NO drop_play play 99999: play_id 99999 doesn't exist (check the raw data and fix or remove the patch)

# check_pbp_patches() only prunes the file of the checked patches

    Code
      check_pbp_patches(dir = test_dir, prune = TRUE, file = other)
    Condition
      Error in `check_pbp_patches()`:
      ! '<file>' doesn't have the patches that are checked. Run `devtools::load_all()` from the root of the package first.

