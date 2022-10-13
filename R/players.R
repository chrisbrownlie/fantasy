#' Get players info
#'
#' Use general endpoint to get players data
#'
#' @return a dataframe of player info
#'
#' @export
get_players <- function() {

  # Get data from API or cache
  general <- query_general_data()

  # Get player data
  general$elements %>%
    bind_rows() %>%
    # Subset and ensure correct type
    mutate(
      name = sprintf("%s %s", .data$first_name, .data$second_name),
      position = case_when(
        .data$element_type == 1 ~ "GKP",
        .data$element_type == 2 ~ "DEF",
        .data$element_type == 3 ~ "MID",
        .data$element_type == 4 ~ "FWD",
        TRUE ~ "UNK"
      ),
      status = case_when(
        .data$status == "a" ~ "available",
        .data$status == "u" ~ "unavailable",
        .data$status == "i" ~ "injured",
        .data$status == "d" ~ "doubtful",
        TRUE ~ "unknown"
      ),
      cost = as.numeric(.data$now_cost)/10,
    ) %>%
    select(!any_of(c("first_name", "second_name", "now_cost", "element_type", "photo"))) %>%
    select(
      .data$id,
      .data$name,
      known_as = .data$web_name,
      .data$position,
      .data$team,
      .data$team_code,
      .data$cost,
      points = .data$event_points,
      points_total = .data$total_points,
      .data$points_per_game,
      cost_change_recent = .data$cost_change_event,
      cost_change_from_start = .data$cost_change_start,
      .data$form,
      .data$value_form,
      .data$value_season,
      selected_by_pct = .data$selected_by_percent,
      transfers_in_recent = .data$transfers_in_event,
      transfers_in_total = .data$transfers_in,
      transfers_out_recent = .data$transfers_out_event,
      transfers_out_total = .data$transfers_out,
      expected_points_this_week = .data$ep_this,
      expected_points_next_week = .data$ep_next,
      .data$minutes:.data$penalties_order) %>%
    mutate(
        across(
          !any_of(c("name", "known_as", "status", "position", "news", "news_added"))&!contains("text"),
          as.numeric,
          na.rm = T
        )
      )
}

#' Get player summary
#'
#' Get a players summary for the current season
#'
#' @param player_id the ID of the player
#'
#' @return dataframe of player summary for the current season
#'
#' @export
get_player_summary <- function(player_id) {

  # Query API for individual player info
  player_ep <- construct(paste0("element-summary/", player_id, "/"))

  # Get the data
  player_summary <- perform_query(player_ep)

  # Get player team
  player_team <- get_players() %>%
    filter(.data$id == player_id) %>%
    pull(.data$team) %>%
    team_from_id()

  future_fixtures <- player_summary$fixtures %>%
    bind_rows() %>%
    mutate(element_id = player_id,
           team_h = team_from_id(.data$team_h),
           team_a = team_from_id(.data$team_a),
           kickoff_time = as.POSIXct(.data$kickoff_time)) %>%
    rename(fixture_id = .data$id,
           at_home = .data$is_home,
           round = .data$event)

  played_fixtures <- player_summary$history %>%
    bind_rows() %>%
    mutate(team_h = if_else(.data$was_home, player_team, team_from_id(.data$opponent_team)),
           team_a = if_else(.data$was_home, team_from_id(.data$opponent_team), player_team),
           kickoff_time = as.POSIXct(.data$kickoff_time)) %>%
    rename(element_id = .data$element,
           fixture_id = .data$fixture,
           at_home = .data$was_home)

  # Combine future and previous fixtures
  bind_rows(played_fixtures,
            future_fixtures) %>%
    select(.data$element_id,
           .data$fixture_id,
           .data$round,
           .data$kickoff_time,
           .data$at_home,
           .data$team_h,
           .data$team_h_score,
           .data$team_a,
           .data$team_a_score,
           .data$total_points,
           .data$difficulty,
           .data$minutes:.data$transfers_out)

}


#' Convert player ID to name
#'
#' @param id the player id, an integer
#' @param full if TRUE return the full name, if FALSE
#' (the default), return the 'known as' name
#'
#' @return a string representing a player name
#'
#' @export
player_from_id <- function(id, full = FALSE) {

  if (!length(id)) return(NA)

  players <- get_players()
  single_player <- function(pl_id, plyrs, fll) {
    if (fll) {
      plyrs$name[plyrs$id == pl_id]
    } else {
      plyrs$known_as[plyrs$id == pl_id]
    }
  }
  if (length(id) > 1) {
    invalid <- id[!id %in% players$id]
    if (!length(invalid)) {
      purrr::map_chr(id, single_player, plyrs = players, fll = full)
    } else if (length(invalid)==1) {
      cli::cli_abort("There is no player with ID {.val {id}}")
    } else if (length(invalid < 10)) {
      cli::cli_abort("There are no players with the following IDs: {{paste(id, collapse = ';')}}")
    } else {
      cli::cli_abort("Many of the player IDs you have supplied are invalid")
    }
  } else {
    single_player(id, plyrs = players, fll = full)
  }
}


#' Search for a player ID by name
#'
#' @param search the string to use to match against the players name
#'
#' @return a tibble of potential matches and their player ID
#'
#' @export
search_for_player <- function(search) {

  # Get all players
  players <- get_players()

  # Filter for search
  players %>%
    filter(grepl(pattern = search, x = .data$name, ignore.case = TRUE)) %>%
    transmute(.data$id, .data$name, team = team_from_id(.data$team, full = TRUE))
}

#' Get the cost for a given player
#'
#' @param pid the ID of the player to get the cost for
#'
#' @return the current value of the player
#'
#' @export
get_player_cost <- function(pid) {
  get_players() %>%
    filter(id == pid) %>%
    pull(cost)
}
