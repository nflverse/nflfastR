#' @export
geom_nfl_logos <- function(mapping = NULL, data = NULL,
                           stat = "identity", position = "identity",
                           ...,
                           na.rm = FALSE,
                           show.legend = FALSE,
                           inherit.aes = TRUE,
                           alpha = NULL) {
  required <- c("x", "y", "team_abbr")
  missing_aes <- required[!required %in% names(mapping)]
  if (!rlang::is_empty(missing_aes)){
    string <- glue::glue_collapse(missing_aes, sep = ", ", last = " and ")
    cli::cli_abort("{.var geom_nfl_logo} requires the following missing aesthetics: {string}")
  }

  x_var <- as.character(rlang::quo_get_expr(mapping$x))
  y_var <- as.character(rlang::quo_get_expr(mapping$y))
  team_abbr_var <- as.character(rlang::quo_get_expr(mapping$team_abbr))

  teams <- data[,team_abbr_var]
  dots <- list(...)

  if ("alpha" %in% names(dots)) alpha <- dots$alpha

  if ("alpha" %in% names(mapping)) alpha <- data[,as.character(rlang::quo_get_expr(mapping$alpha))]

  urls <- teams_colors_logos |>
    dplyr::filter(team_abbr %in% teams) |>
    dplyr::pull(team_logo_espn)

  print(alpha)

  grobs <- purrr::map(seq_along(urls), function(x, urls, alpha) {
    if(is.null(alpha)){
      grid::rasterGrob(magick::image_read(urls[x]))
    } else if (length(alpha) == 1L) {
      if (as.numeric(alpha) <= 0 || as.numeric(alpha) >= 1) {
        cli::cli_abort("aesthetic {.var alpha} requires a value between {.val 0} and {.val 1}")
      }
      img <- magick::image_read(urls[x])
      new <- magick::image_fx(img, expression = paste0(alpha, "*a"), channel = "alpha")
      grid::rasterGrob(new)
    } else{
      if (any(as.numeric(alpha) < 0) || any(as.numeric(alpha) > 1)) {
        cli::cli_abort("aesthetics {.var alpha} require values between {.val 0} and {.val 1}")
      }
      img <- magick::image_read(urls[x])
      new <- magick::image_fx(img, expression = paste0(alpha[x], "*a"), channel = "alpha")
      grid::rasterGrob(new)
    }
  }, urls = urls, alpha = alpha)

  data$label_var <- grobs

  mapping$label <- ggplot2::aes(label = label_var)$label

  ggpp::geom_grob(
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    ...,
    na.rm = na.rm,
    show.legend = show.legend,
    inherit.aes = inherit.aes
  )
}
