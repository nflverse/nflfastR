% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ep_wp_calculators.R
\name{calculate_expected_points}
\alias{calculate_expected_points}
\title{Compute expected points}
\usage{
calculate_expected_points(pbp_data)
}
\arguments{
\item{pbp_data}{Play-by-play dataset to estimate expected points for.}
}
\value{
The original pbp_data with the following columns appended to it:
\describe{
\item{ep}{expected points.}
\item{no_score_prob}{probability of no more scoring this half.}
\item{opp_fg_prob}{probability next score opponent field goal this half.}
\item{opp_safety_prob}{probability next score opponent safety  this half.}
\item{opp_td_prob}{probability of next score opponent touchdown this half.}
\item{fg_prob}{probability next score field goal this half.}
\item{safety_prob}{probability next score safety this half.}
\item{td_prob}{probability text score touchdown this half.}
}
}
\description{
for provided plays. Returns the data with
probabilities of each scoring event and EP added. The following columns
must be present: season, home_team, posteam, roof (coded as 'open',
'closed', or 'retractable'), half_seconds_remaining, yardline_100,
ydstogo, posteam_timeouts_remaining, defteam_timeouts_remaining
}
\details{
Computes expected points for provided plays. Returns the data with
probabilities of each scoring event and EP added. The following columns
must be present:
\itemize{
\item{season}
\item{home_team}
\item{posteam}
\item{roof (coded as 'outdoors', 'dome', or 'open'/'closed'/NA (retractable))}
\item{half_seconds_remaining}
\item{yardline_100}
\item{down}
\item{ydstogo}
\item{posteam_timeouts_remaining}
\item{defteam_timeouts_remaining}
}
}
\examples{
\donttest{
try({# to avoid CRAN test problems
library(dplyr)
data <- tibble::tibble(
"season" = 1999:2019,
"home_team" = "SEA",
"posteam" = "SEA",
"roof" = "outdoors",
"half_seconds_remaining" = 1800,
"yardline_100" = c(rep(80, 17), rep(75, 4)),
"down" = 1,
"ydstogo" = 10,
"posteam_timeouts_remaining" = 3,
"defteam_timeouts_remaining" = 3
)

nflfastR::calculate_expected_points(data) |>
  dplyr::select(season, yardline_100, td_prob, ep)
})
}
}
