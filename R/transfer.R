#' Replace a player in your team with a different valid player
#'
#' Supply the ID of a player in your team and a player not in your team.
#' The function will ensure that the transfer can be completed and then
#' make the transfer.
#'
#' @param x the team in which the transfer is to occur
#' @param p_out the ID of a player in your team to transfer out
#' @param p_in the ID of a player NOT in your team, to transfer in
#'
#' @return the input team with the transfer made, if it is a valid transfer
#'
#' @export
team_transfer <- function(x, p_out, p_in) {
  UseMethod("team_transfer")
}

#' @export
team_transfer.team <- function(x, p_out, p_in) {

  # Check argument types
  if (!is.numeric(p_out)|length(p_out)!=1|p_out<0) cli::cli_abort("{.arg p_out} must be a single non-negative integer player ID")
  if (!is.numeric(p_in)|length(p_in)!=1|p_in<0) cli::cli_abort("{.arg p_in} must be a single non-negative integer player ID")


  # Check that the player to be transferred out is in the team and does not have a role
  if (!p_out %in% x$id) cli::cli_abort("{.arg p_out} is not the ID of a player in your team")
  if (attr(x, "captain")==p_out) cli::cli_abort("You cannot transfer out the team captain, please reassign first using {.fun assign_role}")
  if (attr(x, "vc")==p_out) cli::cli_abort("You cannot transfer out the team vice-captain, please reassign first using {.fun assign_role}")

  # Check the player to be transferred in is NOT in the team
  if (p_in %in% x$id) cli::cli_abort("{.arg p_in} must not be the ID of a player already in your team")

  # Get players
  players <- get_players()

  # Get useful info
  outgoing <- filter(players, id == p_out)
  incoming <- filter(players, id == p_in)

  # Check that the cost of p_out plus the bank balance of the team, at least exceeds the cost of the new player
  new_bank <- (outgoing$cost + attr(x, "bank")) - incoming$cost
  if (new_bank < 0) cli::cli_abort("You do not have the funds to make this transfer.")

  # Create a new team object with the change made
  new_players <- x$id
  new_players[new_players == p_out] <- p_in

  new_team <- team(players = new_players,
                   captain = attr(x, "captain"),
                   vc = attr(x, "vc"),
                   bank = new_bank,
                   transfers = attr(x, "transfers") - 1,
                   chips = attr(x, "chips"))

  # Return the new team
  new_team
}

#' @export
team_transfer.default <- function(x, p_out, p_in) {
  cli::cli_abort("{.fun team_transfer} can only be used with an object of class {.cls team}")
}

#' Show all potential replacements for a particular player
#'
#' Show all players which could replace a particular player in a team. This
#' will return a filtered version of what is returned by get_players(), that
#' allows you to see all the players that the team can afford to replace an
#' existing player.
#'
#' @param x an object with class team, the team containing the player to replace
#' @param p_out a single integer, the ID of the player in the team to look for replacements for
#'
#' @return a tibble of players who could potentially be transferred in for the
#' player
show_transfer_targets <- function(x, p_out) {

  # Check argument types
  if (!is_team(x)) cli::cli_abort("{.arg x} must be an object of class {.cls team}")
  if (!is.numeric(p_out)|length(p_out)!=1) cli::cli_abort("{.arg p_out} must be a single integer player ID")

  # Check that the player is in the team
  if (!p_out %in% x$id) cli::cli_abort("The selected player is not in the team")

  # Get all players
  players <- get_players()

  # Get total funds available
  available_funds <- players$cost[players$id == p_out] + attr(x, "bank")

  # Make sure the transfer wouldn't result in too many players from the same team
  existing_teams <- table(players$team[players$id %in% x$id[x$id != p_out]])
  invalid_teams <- names(existing_teams[existing_teams == 3])

  players |>
    filter(cost <= available_funds,
           position == players$position[players$id == p_out],
           !team %in% invalid_teams,
           !id %in% x$id) |>
    arrange(desc(cost), desc(points_total), desc(selected_by_pct))
}
