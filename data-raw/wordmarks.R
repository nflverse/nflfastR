library(dplyr)

teams <- nflreadr::load_teams()

purrr::walk(teams$team_abbr, function(x) {
  load <- glue::glue(
    "https://upload.wikimedia.org/wikipedia/commons/4/44/LA_Rams_wordmark.png"
  ) |>
    magick::image_read() |>
    magick::image_trim()

  info <- magick::image_info(load)

  rl <- (700 - info$width) / 2
  tb <- (192 - info$height) / 2

  image <- magick::image_border(load, "transparent", glue::glue("{rl}x{tb}"))

  magick::image_write(
    image,
    path = glue::glue("wordmarks/{x}.png"),
    format = "png"
  )

  if (x == "LA") {
    magick::image_write(image, path = "wordmarks/LAR.png", format = "png")
    magick::image_write(image, path = "wordmarks/STL.png", format = "png")
  } else if (x == "LAC") {
    magick::image_write(image, path = "wordmarks/SD.png", format = "png")
  } else if (x == "LV") {
    magick::image_write(image, path = "wordmarks/OAK.png", format = "png")
  }
})
