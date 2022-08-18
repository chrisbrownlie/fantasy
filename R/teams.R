#' Get list of teams
#'
#' Use general data endpoint to get teams as dataframe
#'
#' @return dataframe of information on teams
#'
#' @export
get_teams <- function() {

  # Query API or cache
  general <- query_general_data()

  # Get teams
  general$teams |>
    bind_rows() |>
    # Rename id col
    rename(team_id = .data$id)
}


#' Convert team ID to abbreviation
#'
#' @param id the team id
#' @param full if TRUE return the full name, if FALSE
#' (the default), return abbreviation
#'
#' @return a string representing a team name
#'
#' @export
team_from_id <- function(id, full = FALSE) {
  teams <- get_teams() |>
    arrange(.data$team_id) |>
    pull(all_of(ifelse(full, "name", "short_name")))
  if (any(!id %in% seq_along(teams))) cli::cli_abort("Invalid team ID(s) - must be between 1-20 inclusive")
  teams[id]
}
