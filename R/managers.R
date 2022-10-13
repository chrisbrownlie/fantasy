#' Query manager info
#'
#' @param manager_id an integer manager ID
#'
#' @return list of manager details
#'
#' @keywords internal
query_manager_data <- function(manager_id) {
  paste0("entry/", manager_id, "/") %>%
    construct() %>%
    perform_query()
}

#' Get information on a manager
#'
#' Get information about a manager, such as their team name, the value of
#' their team, how much money they had in the bank at their last deadline,
#' their rank and their points.
#'
#' @param manager_id an integer manager ID, if logged in this does not need
#' to be supplied
#'
#' @return tibble of information about the manager
#'
#' @export
get_manager_info <- function(manager_id) {

  # Use env variable if authenticated
  if (missing(manager_id)) {
    if (Sys.getenv("FPL_MANAGER_ID") == "") {
      cli::cli_abort("Must provide a manager ID, or login using {.fun authenticate}")
    } else {
      manager_id <- Sys.getenv("FPL_MANAGER_ID")
    }
  }

  # Get number of players for percentiles
  total_players <- query_general_data()$total_players

  # Query endpoint for information about manager
  manager_data <- query_manager_data(manager_id = manager_id)
  manager_info <- tibble::tibble(id = manager_data$id,
                                 player_name = paste(manager_data$player_first_name, manager_data$player_last_name),
                                 team_name = manager_data$name,
                                 favourite_team = team_from_id(manager_data$favourite_team, T),
                                 region = manager_data$player_region_name,
                                 last_deadline_bank = manager_data$last_deadline_bank,
                                 last_deadline_team_value = manager_data$last_deadline_value/10,
                                 overall_points = manager_data$summary_overall_points,
                                 overall_rank = manager_data$summary_overall_rank,
                                 overall_percentile = percentile(manager_data$summary_overall_rank, total_players, nearest = 1),
                                 gameweek_points = manager_data$summary_event_points,
                                 gameweek_rank = manager_data$summary_event_rank,
                                 gameweek_percentile = percentile(manager_data$summary_event_rank, total_players, nearest = 1))

  manager_info
}

#' Get leagues
#'
#' Get information on your currently entered leagues, such as where you are
#'
#' @param private_only logical, if TRUE (the default), will ignore leagues which you
#' have not requested to join (e.g. the open leagues that everyone is automatically
#' added to will be ignored). If FALSE, info for all leagues will be returned
#'
#' @return a list of information about each league you are participating in
#'
#' @export
get_my_leagues <- function(private_only = TRUE) {

  # Require authentication
  require_authentication()

  # Get basic manager info
  league_info <- query_manager_data(manager_id = Sys.getenv("FPL_MANAGER_ID"))$leagues

  # Classic leagues
  classic_leagues <- league_info$classic
  classic_leagues[is.null(classic_leagues)] <- NA
  classic_league_tbl <- classic_leagues %>%
    bind_rows()

  if (private_only) {
    filter(classic_league_tbl, league_type == "x")
  } else {
    classic_league_tbl
  }
}

#' Get league
#'
#' Return a league as a tibble, with information on points scored
#'
#' @param league_id the ID of the league to return
#' @param return_rows the number of rows to return, defaults to 50 to avoid
#' long-running queries
#'
#' @return a tibble of the league
get_league <- function(league_id, return_rows = 50) {

  # Get league
  league_raw <- paste0("leagues-classic/", league_id, "/standings") %>%
    construct() %>%
    perform_query() %>%
    `$`(standings)

  all_standings <- league_raw$results %>%
    bind_rows() %>%
    select(position = rank,
           player_id = id,
           player_name = player_name,
           team_name = entry_name,
           points = total,
           gameweek_points = event_total)

  cli::cli_warn(paste0("Iterating through league pages to get top ", return_rows, " entries."))
  while(league_raw$has_next & nrow(all_standings) <= return_rows) {
    cli::cli_alert(paste0("Page ", page, "..."))
    league_raw <- paste0("leagues-classic/", league_id, "/standings?page_standings=", page) %>%
      construct() %>%
      perform_query() %>%
      `$`(standings)

    standings <- league_raw$results %>%
      bind_rows() %>%
      select(position = rank,
             player_id = id,
             player_name = player_name,
             team_name = entry_name,
             points = total,
             gameweek_points = event_total)

    all_standings <- bind_rows(all_standings, bind_rows(standings))
    page <- league_raw$page+1
  }
  if (nrow(all_standings) < return_rows) cli::cli_warn("{.arg return_rows} was greater than the total number of entries.")
  all_standings
}
