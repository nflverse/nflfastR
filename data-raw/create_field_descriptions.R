library(tidyverse)
library(unglue)

x <- read_lines("data-raw/variable_list.txt")

field_descriptions <- tibble(x = x) %>%
  separate(x,"{",into = c(NA,"Field","Description")) %>%
  mutate_all(str_remove_all,"\\}")

usethis::use_data(field_descriptions)
