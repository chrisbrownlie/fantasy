#' Get raw fixtures data
#'
#' Get the raw underlying fixtures data. This function is
#' cached.
#'
#' @return the raw fixtures data as a list
query_fixtures_data <- function() {

  # Get fixtures endpoint
  fixtures_ep <- construct("fixtures")

  # Query API
  perform_query(fixtures_ep)
}

#' Get fixture list
#'
#' Query the API for a list of fixtures and return as
#' a tibble with one row per fixture.
#'
#' @return a tibble of premier league fixtures
#'
#' @export
get_fixture_list <- function() {

  # Query API or cache
  fixtures_data <- query_fixtures_data()

  # Construct tibble from all non-stats info
  purrr::map_dfr(fixtures_data,
                 ~purrr::list_modify(.x, stats = NULL)) |>
    # Subset and rename columns for clarity
    transmute(id,
              gameweek = event,
              finished,
              kickoff_time = as.POSIXct(kickoff_time),
              minutes,
                     home_team = team_abbr(team_h),
                     home_score = team_h_score,
                     home_difficulty = team_h_difficulty,
                     away_team = team_abbr(team_a),
                     away_score = team_a_score,
                     away_difficulty = team_a_difficulty)

}

#' Get fixture stats
#'
#' Get a tibble of stats for a given fixture ID
#'
#' @param fixture_id (optional) a fixture ID(s) to get stats for. If NULL,
#' the default, then all fixtures' stats are returned.
#'
#' @return a dataframe with columns 'id', 'stat', 'team', 'player' and 'value'
#'
#' @export
get_fixture_stats <- function(fixture_id = NULL) {

  # Query API or cache
  fixtures_data <- query_fixtures_data()

  # If only a single fixture then filter
  if (length(fixture_id)) {
    fixtures_data <- purrr::keep(fixtures_data, ~.x$id %in% fixture_id)
  }

  # For each fixtures' stats element, format as a tibble
  purrr::map_dfr(fixtures_data,
                 ~purrr::map_dfr(.x$stats, format_stat) |>
                   mutate(fixture_id = .x$id,
                                 .before = 1))
}

#' Format a stat
#'
#' Format a list of information about a particular stat as a tibble
#'
#' @param stat a list containing information on a particular stat in a particular fixture
#'
#' @return a dataframe with columns 'type', 'team', 'player' and 'value'
format_stat <- function(stat) {

  # Name of stat
  stat_name <- stat$identifier

  # Get home and away values
  if (length(stat$h)) {
    home_vals <- purrr::map_dfr(stat$h,
                                tibble::as_tibble_row) |>
      transmute(
        team = "home",
        player = element,
        value
      )
  } else {
    home_vals <- tibble::tibble()
  }
  if (length(stat$a)) {
    away_vals <- purrr::map_dfr(stat$a,
                                tibble::as_tibble_row) |>
      transmute(
        team = "away",
        player = element,
        value
      )
  } else {
    away_vals <- tibble::tibble()
  }

  # Combine home and away and add stat type
  bind_rows(home_vals,
                   away_vals) |>
    mutate(stat = stat_name,
                  .before = 1)
}
