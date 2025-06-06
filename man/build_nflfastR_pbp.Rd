% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/build_nflfastR_pbp.R
\name{build_nflfastR_pbp}
\alias{build_nflfastR_pbp}
\title{Build a Complete nflfastR Data Set}
\usage{
build_nflfastR_pbp(
  game_ids,
  dir = getOption("nflfastR.raw_directory", default = NULL),
  ...,
  decode = TRUE,
  rules = TRUE
)
}
\arguments{
\item{game_ids}{Vector of character ids or a data frame including the variable
\code{game_id} (see details for further information).}

\item{dir}{Path to local directory (defaults to option "nflfastR.raw_directory")
where nflfastR searches for raw game play-by-play data.
See \code{\link[=save_raw_pbp]{save_raw_pbp()}} for additional information.}

\item{...}{Additional arguments passed to the scraping functions (for internal use)}

\item{decode}{If \code{TRUE}, the function \code{\link[=decode_player_ids]{decode_player_ids()}} will be executed.}

\item{rules}{If \code{FALSE}, printing of the header and footer in the console output will be suppressed.}
}
\value{
An nflfastR play-by-play data frame like it can be loaded from \url{https://github.com/nflverse/nflverse-data}.
}
\description{
\code{build_nflfastR_pbp} is a convenient wrapper around 6 nflfastR functions:

\itemize{
\item{\code{\link[=fast_scraper]{fast_scraper()}}}
\item{\code{\link[=clean_pbp]{clean_pbp()}}}
\item{\code{\link[=add_qb_epa]{add_qb_epa()}}}
\item{\code{\link[=add_xyac]{add_xyac()}}}
\item{\code{\link[=add_xpass]{add_xpass()}}}
\item{\code{\link[=decode_player_ids]{decode_player_ids()}}}
}

Please see either the documentation of each function or
\href{https://www.nflfastr.com/articles/field_descriptions.html}{the nflfastR Field Descriptions website}
to learn about the output.
}
\details{
To load valid game_ids please use the package function \code{\link[=fast_scraper_schedules]{fast_scraper_schedules()}}.
}
\examples{
\donttest{
# Build nflfastR pbp for the 2018 and 2019 Super Bowls
try({# to avoid CRAN test problems
build_nflfastR_pbp(c("2018_21_NE_LA", "2019_21_SF_KC"))
})

# It is also possible to directly use the
# output of `fast_scraper_schedules` as input
try({# to avoid CRAN test problems
library(dplyr, warn.conflicts = FALSE)
fast_scraper_schedules(2020) |>
  slice_tail(n = 3) |>
  build_nflfastR_pbp()
 })

\dontshow{
# Close open connections for R CMD Check
future::plan("sequential")
}
}
}
\seealso{
For information on parallel processing and progress updates please
see \link{nflfastR}.
}
