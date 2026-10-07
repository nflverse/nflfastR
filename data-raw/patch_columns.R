# patch columns is a vector of column names
# we will allow manual data corrections of these column names

patch_columns <- yaml::read_yaml("data-raw/patch_valid_column_names.yaml")$column
final_pbp_columns <- names(default_play)

diffs <- setdiff(patch_columns, full_names)
# we use the following columns in the parser and either rename them or drop them
# before final pbp output
allowed_diffs <- c(
  "two_point_",
  "extra_point_",
  "play_description",
  "quarter",
  "yardline",
  "yards_to_go",
  "field_goal_",
  "safety_team"
)

# This should be empty if we are doing it right
diffs[!stringr::str_detect(diffs, paste(allowed_diffs, collapse = "|"))]

usethis::use_data(patch_columns, internal = TRUE, overwrite = TRUE)
