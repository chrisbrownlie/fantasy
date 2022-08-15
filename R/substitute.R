#' Switch the position of two players
#'
#' Make a substitution in your team by selecting two players to swap
#'
#' @param team an object with class team
#' @param p1 a numeric ID of a player in the team
#' @param p2 a numeric ID of another player in the team
#'
#' @return the team object with the two players switched and the resulting
#' team validated to ensure no rules were broken in the substitution
#'
#' @export
team_substitute <- function(team, p1, p2) {
  UseMethod("team_substitute")
}

#' @export
team_substitute.team <- function(team, p1, p2) {

  # Check that both supplied IDs are in the team
  if (!p1 %in% team$id) cli::cli_abort("{.arg p1} ({.val {p1}}) is not the ID of a player in the team")
  if (!p2 %in% team$id) cli::cli_abort("{.arg p2} ({.val {p2}}) is not the ID of a player in the team")

  # Make substitution
  p1_pos <- which(team$id == p1)
  p2_pos <- which(team$id == p2)
  new_team <- team$id
  new_team[p1_pos] <- p2
  new_team[p2_pos] <- p1

  # If swapping a starting player for a benched player, check the starting player is not captain or vice captain
  if (p1_pos %in% 1:11 & p2_pos %in% 12:15) {
    if (p1 == attr(team, "captain")) cli::cli_abort("{.arg p1} is the team captain so cannot be swapped for a benched player. Use {.fun assign_captain} to pick a new captain first.")
    if (p1 == attr(team, "vc")) cli::cli_abort("{.arg p1} is the team vice-captain so cannot be swapped for a benched player. Use {.fun assign_vice_captain} to pick a new captain first.")
  }
  if (p2_pos %in% 1:11 & p1_pos %in% 12:15) {
    if (p2 == attr(team, "captain")) cli::cli_abort("{.arg p2} is the team captain so cannot be swapped for a benched player. Use {.fun assign_captain} to pick a new captain first.")
    if (p2 == attr(team, "vc")) cli::cli_abort("{.arg p2} is the team vice-captain so cannot be swapped for a benched player. Use {.fun assign_vice_captain} to pick a new captain first.")
  }

  # Create and validate the new team object
  team(new_team,
                 captain = attr(team, "captain"),
                 vc = attr(team, "vc"))

}

#' @export
team_substitute.default <- function(team, p1, p2) {
  cli::cli_abort("{.fun team_substitute} can only be used with an object of class {.cls team}")
}
