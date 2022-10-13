#' Get one number as a percentile of another
#'
#' Given two numbers, will return what percentile the first is of the second
#' e.g. 9/10 is the 90th percentile, 2/3 is the 66th percentile, etc.
#'
#' @param rank a number denoting the numerator
#' @param out_of a number denoting the denominator
#' @param nearest a number, either 0, 1, 5 or 10, indicating what to round to.
#' 0 indicates rounding to 2 decimal places
#'
#' @return a number denoting the percentile
#'
#' @keywords internal
percentile <- function(rank,
                       out_of,
                       nearest = 5) {

  if (out_of < rank) cli::cli_abort("{.arg out_of} must be greater than or equal to {.arg rank}")

  n <- 100-((rank*100)/out_of)

  if (nearest == 0) {
    round(n, digits = 2)
  } else if (nearest == 1) {
    round(n, digits = 0)
  } else if (nearest == 5) {
    round(n/5, digits = 0)*5
  } else if (nearest == 10) {
    round(n/10, digits = 0)*10
  } else {
    cli::cli_abort("{.arg nearest} must be one of: 0, 1, 5, 10")
  }
}

#' Make an abbreviation user friendly using a lookup
#'
#' @param abbr the abbreviation to get the long form for
#'
#' @return a string that is more user friendly than abbr
#' @keywords internal
user_friendly <- function(abbr) {
  switch(abbr,
         "bboost" = "Bench Boost",
         "3xc" = "Triple Captain",
         "wildcard" = "Wildcard",
         "freehit" = "Free Hit")
}

#' If empty then replace
#'
#' @param object to check for emptiness
#' @param replace what to use if object is empty
#'
#' @return object, or replace if object is empty
#' @keywords internal
if_empty <- function(object, replace = NA) {
  ifelse(!length(object), replace, object)
}
