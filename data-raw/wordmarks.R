library(dplyr)

teams <- nflfastR::teams_colors_logos %>%
  dplyr::filter(!team_abbr %in% c("LAR", "OAK", "SD", "STL"))

purrr::walk(teams$team_abbr, function(x){

  image <- magick::image_trim(magick::image_read(glue::glue(
    "https://static.www.nfl.com/league/apps/clubs/wordmarks/{x}_fullcolor.png"
    )))

  magick::image_write(image, path = glue::glue("wordmarks/{x}.png"), format = "png")

  if(x == "LA") {
    magick::image_write(image, path = "wordmarks/LAR.png", format = "png")
    magick::image_write(image, path = "wordmarks/STL.png", format = "png")
  } else if(x == "LAC"){
    magick::image_write(image, path = "wordmarks/SD.png", format = "png")
  } else if(x == "LV"){
    magick::image_write(image, path = "wordmarks/OAK.png", format = "png")
  }

})
