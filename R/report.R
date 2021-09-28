#' Get a Situation Report on System, nflverse Package Versions and Dependencies
#'
#' This function gives a quick overview of the versions of R and the operating
#' system as well as the versions of nflverse packages and their dependencies.
#' It's primarily designed to help you get a quick idea of what's going on when
#' you're helping someone else debug a problem.
#'
#' @param pkg a character vector naming installed packages, or `NULL`
#'   (the default) meaning all nflverse packages. The function checks internally
#'   if all packages are installed and informs if that is not the case.
#' @param recursive a logical indicating whether dependencies of `pkg` and their
#'  dependencies (and so on) should be included.
#'  Can also be a character vector listing the types of dependencies, a subset
#'  of `c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")`.
#'  Character string `"all"` is shorthand for that vector, character string
#'  `"most"` for the same vector without `"Enhances"`, character string `"strong"`
#'  (default) for the first three elements of that vector.
#' @examples
#' \donttest{
#' report()
#' report("nflreadr", TRUE)
#' }
#' @export
report <- function(pkg = NULL, recursive = FALSE){
  if (is.null(pkg)){
    packages <- c(
      "nflreadr",
      "nflfastR",
      "nflseedR",
      "nfl4th",
      "nflplotR",
      "ffsimulator",
      "ffscrapr"
    )
    header <- "nflverse "
  } else {
    packages <- pkg
    header <- ""
  }

  installed <- lapply(packages, is_installed) %>% unlist()

  if (!is.null(pkg)){
    not_installed <- packages[!installed]
    if (length(not_installed) >= 1){
      cli::cli_alert_info("You've asked for the package{?s} {not_installed} which {?is/are} not installed. {?It/They} {?is/are} skipped.")
    }
  }

  packages <- packages[installed]

  s <- utils::sessionInfo(packages)

  cli::cat_rule("System Info")
  cli::cat_bullet(s$R.version$version.string)
  cli::cat_bullet("OS: ", s$platform)

  cli::cat_rule(paste0(header, "Packages"))
  packages <- s$otherPkgs %>% lapply(function(pkg) pkg$Package) %>% unlist() %>% format()
  versions <- s$otherPkgs %>% lapply(function(pkg) pkg$Version) %>% unlist()
  cli::cat_bullet(paste0(packages, " (", versions, ")"))

  # Exit here if we don't want recursive deps
  if (isFALSE(recursive)) return(invisible(NULL))

  cli::cat_rule(paste0(header, "Dependencies"))

  # The checks failed because the repo option is empty sometimes
  # so we set it here to the rstudio mirror and restore the options
  # after the call to package_dependencies()
  old <- options(repos = "https://cran.rstudio.com/")
  deps <-
    tools::package_dependencies(packages, recursive = TRUE) %>%
    unlist(use.names = FALSE) %>%
    sort() %>%
    unique()

  # restore old options
  options(old)

  deps <- deps[!deps %in% packages]

  s <- utils::sessionInfo(deps)

  packages <- s$otherPkgs %>% lapply(function(pkg) pkg$Package) %>% unlist()
  versions <- s$otherPkgs %>% lapply(function(pkg) pkg$Version) %>% unlist()

  if (is.null(packages)){
    cli::cat_bullet("No non-base dependencies")
    return(invisible())
  }

  p <- split(packages, rep_len(1:3, length(packages)))
  v <- split(versions, rep_len(1:3, length(versions)))

  if (length(p) == 1){
    cli::cat_bullet(paste0(
      p[[1]] %>% format(),
      paste0(" (", v[[1]], ")") %>% format()
    ))
  } else if (length(p) == 2){
    cli::cat_bullet(paste0(
      p[[1]] %>% format(),
      paste0(" (", v[[1]], ")") %>% format(),
      "   ",
      cli::symbol$bullet,
      " ",
      p[[2]] %>% format(),
      paste0(" (", v[[2]], ")") %>% format()
    ))
  } else {
    cli::cat_bullet(paste0(
      p[[1]] %>% format(),
      paste0(" (", v[[1]], ")") %>% format(),
      "   ",
      cli::symbol$bullet,
      " ",
      p[[2]] %>% format(),
      paste0(" (", v[[2]], ")") %>% format(),
      "   ",
      cli::symbol$bullet,
      " ",
      p[[3]] %>% format(),
      paste0(" (", v[[3]], ")") %>% format()
    ))
  }
}
