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
#' @param ... any headers to assign to the request
#' @param type one of the 'json', 'html' or 'string'. How the content
#' of the response should be returned
#'
#' @return a list
perform_query <- function(endpoint_url,
                          ...,
                          type = "json") {

  # Check valid type
  if (!type %in% c("json", "html", "string")) cli::cli_abort("{.arg type} must be one of 'json', 'html' or 'string'")

  # Query API
  response <- httr2::request(endpoint_url) |>
    httr2::req_retry(max_tries = getOption("FANTASY_MAX_RETRIES", default = 3)) |>
    httr2::req_headers(...) |>
    httr2::req_perform()

  # Extract content
  if (type == "json") {
    httr2::resp_body_json(response)
  } else if (type == "html") {
    httr2::resp_body_html(response)
  } else if (type == "string") {
    httr2::resp_body_string(response)
  }
}
