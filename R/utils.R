# The function `message_completed` to create the green "...completed" message
# only exists to hide the option `in_builder` in dots

message_completed <- function(x, in_builder = FALSE) {
  if (!in_builder) {
    usethis::ui_done("{usethis::ui_field(x)}")
  } else if (in_builder) {
    usethis::ui_done(x)
  }
}

# custom mode function from https://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode/8189441
custom_mode <- function(x, na.rm = TRUE) {
  if(na.rm){x <- x[!is.na(x)]}
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}


# This function is borrowed from pkgdown:::rule and slightly modified
rule <- function(x = NULL, line = "-") {
  width <- getOption("width")

  if (!is.null(x)) {
    prefix <- paste0(line, line, " ")
    suffix <- " "
  } else {
    prefix <- ""
    suffix <- ""
    x <- ""
  }

  bold_text <- paste0("\033[1m", x, "\033[22m")

  line_length <- width - nchar(x) - nchar(prefix) - nchar(suffix)
  cat_line(prefix, bold_text, suffix, strrep(line, line_length))
}

# This function is borrowed from pkgdown:::cat_line
cat_line <- function(...) {
  cat(paste0(..., "\n"), sep = "")
}
