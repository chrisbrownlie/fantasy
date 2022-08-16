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
  general$elements |>
    bind_rows() |>
    # Subset and ensure correct type
    mutate(
      name = sprintf("%s %s", first_name, second_name),
      position = case_when(
        element_type == 1 ~ "GKP",
        element_type == 2 ~ "DEF",
        element_type == 3 ~ "MID",
        element_type == 4 ~ "FWD",
        TRUE ~ "UNK"
      ),
      status = case_when(
        status == "a" ~ "available",
        status == "u" ~ "unavailable",
        status == "i" ~ "injured",
        status == "d" ~ "doubtful",
        TRUE ~ "unknown"
      ),
      cost = as.numeric(now_cost)/10,
    ) |>
    select(!any_of(c("first_name", "second_name", "now_cost", "element_type", "photo"))) |>
    select(
      id,
      name,
      known_as = web_name,
      position,
      team,
      team_code,
      cost,
      points = event_points,
      points_total = total_points,
      points_per_game,
      cost_change_recent = cost_change_event,
      cost_change_from_start = cost_change_start,
      form,
      value_form,
      value_season,
      selected_by_pct = selected_by_percent,
      transfers_in_recent = transfers_in_event,
      transfers_in_total = transfers_in,
      transfers_out_recent = transfers_out_event,
      transfers_out_total = transfers_out,
      expected_points_this_week = ep_this,
      expected_points_next_week = ep_next,
      minutes:penalties_order) |>
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
  player_team <- get_players() |>
    filter(id == player_id) |>
    pull(team) |>
    team_from_id()

  future_fixtures <- player_summary$fixtures |>
    bind_rows() |>
    mutate(element_id = player_id,
           team_h = team_from_id(team_h),
           team_a = team_from_id(team_a),
           kickoff_time = as.POSIXct(kickoff_time)) |>
    rename(fixture_id = id,
           at_home = is_home,
           round = event)

  played_fixtures <- player_summary$history |>
    bind_rows() |>
    mutate(team_h = if_else(was_home, player_team, team_from_id(opponent_team)),
           team_a = if_else(was_home, team_from_id(opponent_team), player_team),
           kickoff_time = as.POSIXct(kickoff_time)) |>
    rename(element_id = element,
           fixture_id = fixture,
           at_home = was_home)

  # Combine future and previous fixtures
  bind_rows(played_fixtures,
            future_fixtures) |>
    select(element_id,
           fixture_id,
           round,
           kickoff_time,
           at_home,
           team_h,
           team_h_score,
           team_a,
           team_a_score,
           total_points,
           difficulty,
           minutes:transfers_out)

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
