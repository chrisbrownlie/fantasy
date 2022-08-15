team_transfer <- function(x, p1, p2) {
  UseMethod("team_transfer")
}

team_transfer.team <- function(x, p1, p2) {

}

team_transfer.default <- function(x, p1, p2) {
  cli::cli_abort("{.fun team_transfer} can only be used with an object of class {.cls team}")
}
