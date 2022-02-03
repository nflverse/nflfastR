################################################################################
# Author: Sebastian Carl
# Purpose: Function for loading schedules and rosters from nflfastR repos
# Code Style Guide: styler::tidyverse_style()
################################################################################

get_scheds_and_rosters <- function(season, type) {

  type <- match.arg(type,choices = c("schedule","roster"))

  switch(type,
         "schedule" = nflreadr::load_schedules(season),
         "roster" = nflreadr::load_rosters(season))
}
