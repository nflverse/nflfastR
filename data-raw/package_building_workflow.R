# Package workflow
# library(devtools)

# add all package dependencies to DESCRIPTION
# alphabetical order
usethis::use_package("dplyr", type = "Imports", min_version = NULL)
usethis::use_package("furrr", type = "Suggests", min_version = NULL)
usethis::use_package("future", type = "Suggests", min_version = NULL)
usethis::use_package("glue", type = "Imports", min_version = NULL)
usethis::use_package("httr", type = "Imports", min_version = NULL)
usethis::use_package("janitor", type = "Imports", min_version = NULL)
usethis::use_package("jsonlite", type = "Imports", min_version = NULL)
usethis::use_package("lubridate", type = "Imports", min_version = NULL)
usethis::use_package("magrittr", type = "Imports", min_version = NULL)
usethis::use_package("mgcv", type = "Imports", min_version = NULL)
usethis::use_package("nflscrapR", type = "Imports", min_version = NULL)
usethis::use_package("purrr", type = "Imports", min_version = NULL)
usethis::use_package("stringr", type = "Imports", min_version = NULL)
usethis::use_package("tibble", type = "Imports", min_version = NULL)
usethis::use_package("tidyr", type = "Imports", min_version = NULL)
usethis::use_package("tidyselect", type = "Imports", min_version = NULL)
usethis::use_tidy_description()

# add license
usethis::use_mit_license("Sebastian Carl; Ben Baldwin")

# change version number to dev version
usethis::use_dev_version()

# change version number to release version
usethis::use_version()
