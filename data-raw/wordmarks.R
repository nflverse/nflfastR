library(dplyr)

teams <- nflfastR::teams_colors_logos %>%
  dplyr::filter(!team_abbr %in% c("LAR", "OAK", "SD", "STL"))

purrr::walk(teams$team_abbr, function(x) {
  load <- glue::glue(
    "https://static.www.nfl.com/league/apps/clubs/wordmarks/{x}_fullcolor.png"
  ) %>%
    magick::image_read() %>%
    magick::image_trim()

  info <- magick::image_info(load)

  rl <- (700 - info$width) / 2
  tb <- (192 - info$height) / 2

  image <- magick::image_border(load, "transparent", glue::glue("{rl}x{tb}"))

  magick::image_write(image, path = glue::glue("wordmarks/{x}.png"), format = "png")

  if (x == "LA") {
    magick::image_write(image, path = "wordmarks/LAR.png", format = "png")
    magick::image_write(image, path = "wordmarks/STL.png", format = "png")
  } else if (x == "LAC") {
    magick::image_write(image, path = "wordmarks/SD.png", format = "png")
  } else if (x == "LV") {
    magick::image_write(image, path = "wordmarks/OAK.png", format = "png")
  }
})
