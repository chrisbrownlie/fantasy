#' Get predicted lineups for the next game
#'
#' Scrape data from fantasyfootballscout.co.uk to get
#' a prediction for who is likely to be starting in the
#' next gameweek.
#'
#' @return a dataframe of teams and their likely starting XI
get_predicted_lineups <- function() {

  # Get the page
  content <- perform_query("https://www.fantasyfootballscout.co.uk/team-news/",
                           type = "html")

  # Extract raw player lineups
  players <- content |>
    rvest::html_elements(css = ".formation") |>
    rvest::html_text2() |>
    strsplit("\n")

  # Get corresponding team names
  teams <- content |>
    rvest::html_elements(css = ".story-wrap") |>
    rvest::html_element("h2") |>
    rvest::html_text2()

  names(players) <- teams

  # Convert to tibble
  lineups <- players |>
    bind_rows() |>
    mutate(across(everything(), ~paste0(row_number(), .x))) |>
    tidyr::pivot_longer(everything(),
                        names_to = "Team",
                        values_to = "Player") %>%
    mutate(Selection = as.numeric(gsub(pattern = "^(\\d+).*",
                            replacement = "\\1",
                            Player)),
           Player = gsub(pattern = "^\\d+(.*)",
                         replacement = "\\1",
                         Player)) |>
    select(Team, Selection, Player) |>
    arrange(Team, Selection)

  lineups
}
