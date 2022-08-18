#' Use one of your chips
#'
#' Use this function to activate one of your chips (wildcard,
#' bench boost, free hit or triple captain)
#'
#' @param x a team object
#' @param chip the name of the chip to activate
#'
#' @return the input team with the chip activated
#'
#' @export
activate_chip <- function(x, chip) {
  UseMethod("activate_chip")
}

#' @export
activate_chip.team <- function(x, chip) {

  # Check inputs
  if (!is.character(chip)|!chip %in% c("3xc", "bboost", "wildcard", "freehit")) cli::cli_abort("{.arg chip} must be one of: {.val 3xc}, {.val bboost}, {.val wildcard} or {.val freehit}")

  # If it hasn't already been activated, activate it
  if (is.na(attr(x, "chips")[[chip]])) {
    cli::cli_abort("{.val {chip}} has already been used in a previous gameweek.")
  } else if (isFALSE(attr(x, "chips")[[chip]])) {
    cli::cli_abort("{.val {chip}} is already activated for this gameweek.")
  } else {
    # Don't allow wildcard if freehit and vice versa
    if (isFALSE(attr(x, "chips")[["freehit"]]) & chip == "wildcard") cli::cli_abort("You cannot activate {.val wildcard} as {.val freehit} is already active, use {.fun deactivate_chip} to deactivate {.val freehit} first")
    if (isFALSE(attr(x, "chips")[["wildcard"]]) & chip == "freehit") cli::cli_abort("You cannot activate {.val freehit} as {.val wildcard} is already active, use {.fun deactivate_chip} to deactivate {.val wildcard} first")

    # Otherwise, set the chip to FALSE to indicate it is currently active
    attr(x, "chips")[[chip]] <- FALSE
  }

  # Validate the team again to be sure
  x <- validate_team(x)

  # Return the team
  x
}

#' @export
activate_chip.default <- function(x, chip) {
  cli::cli_abort("{.fun activate_chip} can only be used with an object of class {.cls team}")
}

#' @export
deactivate_chip <- function(x, chip) {
  UseMethod("deactivate_chip")
}

#' @export
deactivate_chip.team <- function(x, chip) {

  # Check inputs
  if (!is.character(chip)|!chip %in% c("3xc", "bboost", "wildcard", "freehit")) cli::cli_abort("{.arg chip} must be one of: {.val 3xc}, {.val bboost}, {.val wildcard} or {.val freehit}")

  # If it hasn't already been activated, activate it
  if (is.na(attr(x, "chips")[[chip]])) {
    cli::cli_abort("{.val {chip}} has already been used in a previous gameweek.")
  } else if (isTRUE(attr(x, "chips")[[chip]])) {
    cli::cli_abort("{.val {chip}} has not been activated for this gameweek.")
  } else {
    attr(x, "chips")[[chip]] <- TRUE
  }

  # Validate the team to make sure
  x <- validate_team(x)

  # Return team
  x
}

#' @export
deactivate_chip.default <- function(x, chip) {
  cli::cli_abort("{.fun deactivate_chip} can only be used with an object of class {.cls team}")
}

#' Mark all chips as valid
#'
#' Simple helper for returning a logical vector where all chips are valid -
#' used mainly for testing.
#'
#' @return logical vector that can be passed to the chips argument of the
#' team constructor
#' @keywords internal
all_chips <- function(triple_cap = TRUE,
                      bboost = TRUE,
                      wildcard = TRUE,
                      freehit = TRUE) {
  c("3xc" = triple_cap, "bboost" = bboost, "wildcard" = wildcard, "freehit" = freehit)
}
