library(dplyr)
library(tidyr)
library(stringr)
library(usethis)

x <- readLines("data-raw/variable_list.txt")

field_descriptions <- tibble(x = x) |>
  separate(x,"{",into = c(NA,"Field","Description")) |>
  mutate_all(str_remove_all,"\\}")

usethis::use_data(field_descriptions,overwrite = TRUE)
# save(field_descriptions, file = "vignettes/field_descriptions.rda")
