#' Get gameweek information
#'
#' Get a list of gameweeks with information on each
#'
#' @param gw an optional integer vector indicating which gameweeks to return information
#' for
#'
#' @return a tibble of information about gameweeks
#'
#' @export
get_gameweek_info <- function(gw = NULL) {

  # Get general endpoint data
  general <- query_general_data()

  # Extract useful gameweek info
  all_weeks <- purrr::map_dfr(general$events,
                 function(el) {
                   tibble::tibble(id = el$id,
                                  gameweek = as.numeric(gsub(el$name, pattern = "Gameweek ", replacement = "")),
                                  deadline = as.POSIXct(gsub(el$deadline_time, pattern = "[[:alpha:]]", replacement = " ")),
                                  average_team_score = el$average_entry_score,
                                  highest_team_score = el$highest_score,
                                  finished = el$finished,
                                  current = el$is_current,
                                  "next" = el$is_next,
                                  most_selected = player_from_id(el$most_selected),
                                  most_transferred_in = player_from_id(el$most_transferred_in),
                                  most_captained = player_from_id(el$most_captained),
                                  most_vice_captained = player_from_id(el$most_vice_captained),
                                  highest_scoring_player = player_from_id(el$top_element))
                 })

  if (length(gw)) {
    filter(all_weeks, gameweek %in% gw)
  } else {
    all_weeks
  }
}

#' Get current gameweek
#'
#' Get the current gameweek ID
#'
#' @return integer denoting the current gameweek ID
get_current_gameweek <- function() {
  get_gameweek_info() %>%
    dplyr::filter(deadline > Sys.time()) %>%
    dplyr::pull(id) %>%
    min()
}
