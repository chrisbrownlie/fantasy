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

}

#' @export
team_transfer.default <- function(x, p_out, p_in) {
  cli::cli_abort("{.fun team_transfer} can only be used with an object of class {.cls team}")
}
