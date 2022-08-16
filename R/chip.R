#' Use one of your chips
#'
#' Use this function to activate one of your chips (wildcard,
#' bench boost, free hit or triple captain)
#'
#' @param x a team object
#'
#' @return
#'
#' @export
activate_chip <- function(x) {
  UseMethod("activate_chip")
}

activate_chip.team <- function(x) {

}

activate_chip.default <- function(x) {
  cli::cli_abort("{.fun activate_chip} can only be used with an object of class {.cls team}")
}
