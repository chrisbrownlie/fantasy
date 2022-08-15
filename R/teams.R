#' Get list of teams
#'
#' Use general data endpoint to get teams as dataframe
#'
#' @return dataframe of information on teams
get_teams <- function() {

  # Query API or cache
  general <- query_general_data()

  # Get teams
  general$teams |>
    bind_rows() |>
    # Rename id col
    rename(team_id = id)
}


#' Convert team ID to abbreviation
#'
#' @param id the team id
#' @param full if TRUE return the full name, if FALSE
#' (the default), return abbreviation
#'
#' @return a string representing a team name
team_abbr <- function(id, full = FALSE) {
  teams <- get_teams() |>
    arrange(team_id) |>
    pull(all_of(ifelse(full, "name", "short_name")))
  teams[id]
}
