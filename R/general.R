#' Query general info endpoint
#'
#' Get the overall info data from the API, this function is cached.
#'
#' @return a list of the bootstrap-static endpoint data
#'
#' @keywords internal
query_general_data <- function() {

  # Get general info endpoint
  basic_ep <- construct("bootstrap-static/")

  # Query API
  perform_query(basic_ep)
}
