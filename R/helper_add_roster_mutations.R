################################################################################
# Author: Sebastian Carl
# Purpose: Build roster data frame and add some variables
# Code Style Guide: styler::tidyverse_style()
################################################################################

add_roster_mutations <- function(pbp) {
  out <-
    pbp %>%
    # for some older seasons the grab function returns less information about
    # teams and players (e.g. team.conferenceAbbr, team.divisionAbbr or
    # teamPlayers.suffix) which is the reason why we can't select the
    # variables by index on the one hand and need to catch errors while selecting
    # on the other hand (using matches)
    dplyr::select(
      tidyselect::matches(
        c(
          "team.season",
          "teamPlayers.displayName",
          "teamPlayers.firstName",
          "teamPlayers.middleName",
          "teamPlayers.lastName",
          "teamPlayers.suffix",
          "teamPlayers.status",
          "teamPlayers.position",
          "teamPlayers.positionGroup",
          "teamPlayers.nflId",
          "teamPlayers.esbId",
          "teamPlayers.gsisId",
          "teamPlayers.birthDate",
          "teamPlayers.homeTown",
          "teamPlayers.collegeId",
          "teamPlayers.collegeName",
          "teamPlayers.jerseyNumber",
          "teamPlayers.height",
          "teamPlayers.weight",

          # variables commented out because they appear to be unnecessary

          # "teamPlayers.yearsOfExperience",
          # "teamPlayers.teamAbbr",
          # "teamPlayers.teamSeq",
          # "teamPlayers.teamId",
          # "teamPlayers.teamFullName",

          "team.teamId",
          "team.abbr",
          "team.cityState",
          "team.fullName",
          "team.nick",

          # team type is 'TEAM' for 'normal teams' and 'PRO' for ProBowl teams
          # seems to be unnecessary to include it

          # "team.teamType",

          "team.conferenceAbbr",
          "team.divisionAbbr"
        )
      )
    ) %>%

    # Some Variables have empty observations. Replace them with 'NA'
    dplyr::mutate_all(dplyr::na_if, "") %>%
    dplyr::mutate(team.abbr = dplyr::if_else(team.abbr == "JAC", "JAX", team.abbr))

  # add the two new variables headshot_url and profile_url
  # since this only works if the variables esbId and nflId are available
  # the expressions are wrapped in a try structure

  if (!"teamPlayers.esbId" %in% colnames(out)) {
    out$teamPlayers.headshot_url <- NA_character_
  } else {
    out <- out %>%
      dplyr::mutate(
        teamPlayers.headshot_url = glue::glue(
          "http://static.nfl.com/static/content/public/static/img/fantasy/transparent/200x200/{teamPlayers.esbId}.png"
        )
      )
  }

  if (!"teamPlayers.nflId" %in% colnames(out)) {
    out$teamPlayers.profile_url <- NA_character_
  } else {
    out <- out %>%
      dplyr::mutate(
        teamPlayers.profile_url = glue::glue(
          "http://www.nfl.com/player/{tolower(teamPlayers.firstName)}{tolower(teamPlayers.lastName)}/{teamPlayers.nflId}/profile"
        )
      )
  }
  return(out)
}
