#' Construct an API URL
#'
#' Construct an FPL API endpoint
#'
#' @param endpoint the endpoint to use
#'
#' @return the full URL of the endpoint
construct <- function(endpoint) {
  sprintf("https://fantasy.premierleague.com/api/%s", endpoint)
}

#' Perform API query
#'
#' Query an API endpoint. Lightweight wrapper round httr2 funcs
#'
#' @param endpoint_url the URL to query
#'
#' @return a list
perform_query <- function(endpoint_url) {

  # Query API
  response <- httr2::request(endpoint_url) |>
    httr2::req_perform()

  # Extract content
  httr2::resp_body_json(response)
}
