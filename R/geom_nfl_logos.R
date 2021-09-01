#' ggplot2 Layer for Visualizing NFL Team Logos
#' @inheritParams ggplot2::geom_point
#' @export
#' @examples
#' \donttest{
#' library(ggplot2)
#' library(nflfastR)
#'
#' teams <- nflfastR::teams_colors_logos %>%
#'   dplyr::filter(!team_abbr %in% c("OAK", "SD", "STL", "LAR"))
#'
#' df <- data.frame(
#'   a = runif(nrow(teams)),
#'   b = runif(nrow(teams)),
#'   teams = teams$team_abbr
#' ) %>%
#'   dplyr::mutate(
#'     alpha = ifelse(teams %in% c("CLE", "PIT", "NE"), 1, 0.2)
#'   )
#'
#' df %>%
#'   ggplot(aes(x = a, y = b)) +
#'   geom_nfl_logos(aes(team_abbr = teams, alpha = alpha)) +
#'   theme(minimal)
#'
#' }
geom_nfl_logos <- function(mapping = NULL, data = NULL,
                           stat = "identity", position = "identity",
                           ...,
                           na.rm = FALSE,
                           show.legend = FALSE,
                           inherit.aes = TRUE) {

  if (!is_installed("ggplot2") | !is_installed("magick")) {
    cli::cli_abort("{my_time()} | Packages {.val ggplot2} and {.val magick} required for {.var geom_nfl_logos}. Please install them.")
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomNFL,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

GeomNFL <- ggplot2::ggproto("GeomNFL", ggplot2::Geom,
                            required_aes = c("x", "y", "team_abbr"),
                            # non_missing_aes = c(""),
                            default_aes = ggplot2::aes(
                              alpha = NULL, angle = 0, hjust = 0.5,
                              vjust = 0.5, width = 0.1, height = 0.1
                            ),

                            draw_panel = function(data, panel_params, coord, na.rm = FALSE) {
                              urls <- nflfastR::teams_colors_logos %>%
                                dplyr::filter(team_abbr %in% data$team_abbr) %>%
                                dplyr::pull(team_logo_espn)

                              data <- coord$transform(data, panel_params)

                              grobs <- purrr::map(seq_along(urls), function(i, urls, alpha, data) {
                                if(is.null(alpha)){
                                  grid <- grid::rasterGrob(magick::image_read(urls[i]))
                                } else if (length(alpha) == 1L) {
                                  if (as.numeric(alpha) <= 0 || as.numeric(alpha) >= 1) {
                                    cli::cli_abort("aesthetic {.var alpha} requires a value between {.val 0} and {.val 1}")
                                  }
                                  img <- magick::image_read(urls[i])
                                  new <- magick::image_fx(img, expression = paste0(alpha, "*a"), channel = "alpha")
                                  grid <- grid::rasterGrob(new)
                                } else{
                                  if (any(as.numeric(alpha) < 0) || any(as.numeric(alpha) > 1)) {
                                    cli::cli_abort("aesthetics {.var alpha} require values between {.val 0} and {.val 1}")
                                  }
                                  img <- magick::image_read(urls[i])
                                  new <- magick::image_fx(img, expression = paste0(alpha[i], "*a"), channel = "alpha")
                                  grid <- grid::rasterGrob(new)
                                }

                                grid$vp <- grid::viewport(
                                  x = grid::unit(data$x[i], "native"),
                                  y = grid::unit(data$y[i], "native"),
                                  width = grid::unit(data$width[i], "npc"),
                                  height = grid::unit(data$height[i], "npc"),
                                  just = c(data$hjust[i], data$vjust[i]),
                                  angle = data$angle[i],
                                  name = paste("geom_nfl.panel", data$PANEL[i],
                                               "row", i, sep = ".")
                                )

                                grid$name <- paste("nfl.grob", i, sep = ".")

                                grid

                              }, urls = urls, alpha = data$alpha, data = data)

                              class(grobs) <- "gList"

                              grid::gTree(children = grobs)
                            },

                            draw_key = function(...) grid::nullGrob()
)
