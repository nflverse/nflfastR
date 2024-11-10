stat_ids <- "https://www.nflgsis.com/gsis/Documentation/Partners/StatIDs_files/sheet001.html" |>
  xml2::read_html() |>
  rvest::html_table(fill = TRUE) |>
  as.data.frame() |>
  dplyr::rename("stat_id" = X1, "name" = X2, "comment" = X3) |>
  dplyr::select(1:3) |>
  dplyr::slice(-1) |>
  dplyr::mutate(stat_id = as.integer(stat_id)) |>
  dplyr::filter(!is.na(stat_id)) |>
  dplyr::group_by(stat_id, name) |>
  dplyr::summarise(comment = paste0(comment, collapse = " ")) |>
  dplyr::ungroup() |>
  dplyr::mutate(comment = stringr::str_squish(comment))

usethis::use_data(stat_ids, overwrite = TRUE)
