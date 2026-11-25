# set patches only change the value they expect

    Code
      patch_set_values(pbp, "2019_01_GB_CHI")
    Condition
      Warning:
      Patch of "2019_01_GB_CHI" not applied:
      x down is 2, 3, 4, 5, and NA on some plays, not 1 as the patch expects
    Code
      patch_set_values(pbp, "2019_01_GB_CHI", late = TRUE)
    Condition
      Warning:
      Patch of "2019_01_GB_CHI" not applied:
      x posteam of play_id 1 is "KC", not "CHI" as the patch expects
      Warning:
      Patch of "2019_01_GB_CHI" not applied:
      x play_id 9 doesn't exist

# set patches with a wrong column or value fail

    Code
      patch_set_values(pbp, "2019_01_GB_CHI")
    Condition
      Error in `patch_set_values()`:
      ! Patch of "2019_01_GB_CHI": column downs doesn't exist when `set` patches are applied.
      i See the section on patches in '.github/CONTRIBUTING.md'.

---

    Code
      patch_set_values(pbp, "2019_01_GB_CHI")
    Condition
      Error in `patch_set_values()`:
      ! Patch of "2019_01_GB_CHI": column yardline_100 doesn't exist when `set` patches are applied.
      i Patch yardline instead.

---

    Code
      patch_set_values(pbp, "2019_01_GB_CHI")
    Condition
      Error in `patch_value()`:
      ! Patch of "2019_01_GB_CHI": "two" isn't a valid value of column down (<double>).

# patch files with unknown fields or IDs that aren't numbers fail

    Code
      local_pbp_patches(
        "game_id: 2019_01_GB_CHI; play_id: 12a4; action: set; column: down; from: 1; to: 2; reason: r")
    Condition
      Error in `read_pbp_patches()`:
      ! play_id must be a number, not "12a4", in the patch file.

---

    Code
      local_pbp_patches("game_id: 2019_01_GB_CHI; play: 1; reason: r")
    Condition
      Error in `read_pbp_patches()`:
      ! Unknown field play in the patch file.

