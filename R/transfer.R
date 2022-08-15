squad_transfer <- function(x, p1, p2) {
  UseMethod("squad_transfer")
}

squad_transfer.team_selection <- function(x, p1, p2) {

}

squad_transfer.default <- function(x, p1, p2) {
  cli::cli_abort("{.fun squad_transfer} can only be used with an object of class {.cls team_selection}")
}
