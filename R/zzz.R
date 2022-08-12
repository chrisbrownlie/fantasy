.onLoad <- function(libname, pkgname) {
  cli::cli_alert_info("Memoising functions...")
  query_general_data <- memoise::memoise(query_general_data)
  query_fixtures_data <- memoise::memoise(query_fixtures_data)
  get_teams <- memoise::memoise(get_teams)
  get_fixture_list <- memoise::memoise(get_fixture_list)
  get_players <- memoise::memoise(get_players)
}
