#' Get my team
#'
#' Get dataframe of my current team
#'
#' @param manager_id the manager ID of the team to show
#'
#' @return tibble of information about current picks
get_my_team <- function(manager_id = 7330951) {

  # Construct endpoint URL for my team
  my_team_ep <- construct(paste0("my-team/", manager_id, "/"))

  # Get team data
  rep <- httr2::request(my_team_ep) |>
    httr2::req_headers(
      "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8",
      "Accept-Encoding" = "gzip, deflate, br",
      "Accept-Language" = "en-GB,en;q=0.5",
      "Connection" = "keep-alive",
      "Cookie" = "datadome=H42HXuFNIBXvyFC8xBWbpUhyVV5QNB52T9C.iOA_3kSFZM-xfP9.WeTBP7Ijzl~W6RB6t5rr8uy_XGP8UFvUEIF99Wr9~~JzVYl75u~yqH1bKsJcs72_QRcIrJ-N9Z4; _ga=GA1.2.424242963.1659705746; _ga_844XQSF4K8=GS1.1.1660332201.7.1.1660334929.58; pl_euconsent-v2=CPdQv_BPdQv_BFCABAENCbCsAP_AAH_AAAwIF5wAQF5gXnABAXmAAAAA.YAAAAAAAAAAA; pl_euconsent-v2-intent-confirmed={%22tcf%22:[755]%2C%22oob%22:[]}; pl_oob-vendors={}; csrftoken=d1HEnQhQ0t5HA8Iv6TRzaBYqi244meXr3T3lCwPP5PoIh8ufAKFdTTq1bHfjX78d; _gid=GA1.2.478089430.1660310081; pl_profile='eyJzIjogIld6SXNOamt3TXpVNU1USmQ6MW9NYXhjOm9UQ3NxNHhHUkZka01OYXE3aDk3a3FOYk1vZkllaTVWa2kxRFdpQnRkT00iLCAidSI6IHsiaWQiOiA2OTAzNTkxMiwgImZuIjogIkNocmlzIiwgImxuIjogIkJyb3dubGllIiwgImZjIjogMTd9fQ=='; sessionid=.eJxVy8sKwjAQheF3yVrKZCaXxp17QaG4LtMkQ8RSirEr8d1Nd7o8fOd_q5G3Vxm3mp_jPamjcgHIBo3q8EsTx0dedl9nWedul-56vjWrw3A5tfkfFK6lvVFIC2jOBsQGg0hO0Ds00nMPEB1yjnYK2vpotDggiZIyBfLJcwD1-QLjITKd:1oMaxc:LV7MNBmDiiLPng-A6issYdUMFcCZ1ti_KfEE0ZqEYMw",
      "Host" = "fantasy.premierleague.com",
      "Sec-Fetch-Dest" = "document",
      "Sec-Fetch-Mode" = "navigate",
      "Sec-Fetch-Site" = "none",
      "Sec-Fetch-User" = "?1",
      "TE" = "trailers",
      "Upgrade-Insecure-Requests" = "1",
      "User-Agent" = "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:103.0) Gecko/20100101 Firefox/103.0"
    ) |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  players <- get_players()

  rep$picks |>
    bind_rows() |>
    left_join(players,
              by = c("element" = "id")) |>
    transmute(id = element,
              team_position = position.x,
              name,
              known_as,
              team = team_abbr(team, T),
              points,
              points_total,
              form,
              position = position.y,
              selling_price,
              multiplier,
              purchase_price,
              is_captain,
              is_vice_captain)
}

#' Update your team
#'
#' Send a request to the API to update your team details
#'
#' @param player_ids a numeric vector of player IDs, ordered in
#' their desired squad positions 1-15
#' @param captain_position_id number from 1-11. The *position* ID
#' of your desired captain (e.g. 1 would set your GK to captain)
#' @param vc_position_id number from 1-11. The *position* ID of your
#' desired vice-captain.
#'
#' @return informs the user whether the request was successful or not
update_team <- function(player_ids,
                        captain_position_id,
                        vc_position_id,
                        verbose = 0) {

  # Validate inputs
  validated <- validate_team_selection(player_ids,
                                       captain_position_id,
                                       vc_position_id)
  player_ids <- validated$ids
  captain_position_id <- validated$capt
  vc_position_id <- validated$vc

  # Get team payload in format for server
  cli::cli_alert("Creating payload...")
  picks_payload_lists <- purrr::map(1:15, ~list(element = player_ids[.x],
                                                 is_captain = .x==captain_position_id,
                                                 is_vice_captain = .x==vc_position_id,
                                                 position = .x))

  payload <- list(
    'picks' = picks_payload_lists,
    'chips' = NULL
  )

  cli::cli_alert("Sending POST request...")
  rep <- httr2::request("https://fantasy.premierleague.com/api/my-team/7330951/") |>
    httr2::req_headers(
      'content-type' = 'application/json',
      'origin' = 'https://fantasy.premierleague.com',
      'referer' = 'https://fantasy.premierleague.com/my-team',
      "Cookie" = "datadome=H42HXuFNIBXvyFC8xBWbpUhyVV5QNB52T9C.iOA_3kSFZM-xfP9.WeTBP7Ijzl~W6RB6t5rr8uy_XGP8UFvUEIF99Wr9~~JzVYl75u~yqH1bKsJcs72_QRcIrJ-N9Z4; _ga=GA1.2.424242963.1659705746; _ga_844XQSF4K8=GS1.1.1660332201.7.1.1660334929.58; pl_euconsent-v2=CPdQv_BPdQv_BFCABAENCbCsAP_AAH_AAAwIF5wAQF5gXnABAXmAAAAA.YAAAAAAAAAAA; pl_euconsent-v2-intent-confirmed={%22tcf%22:[755]%2C%22oob%22:[]}; pl_oob-vendors={}; csrftoken=d1HEnQhQ0t5HA8Iv6TRzaBYqi244meXr3T3lCwPP5PoIh8ufAKFdTTq1bHfjX78d; _gid=GA1.2.478089430.1660310081; pl_profile='eyJzIjogIld6SXNOamt3TXpVNU1USmQ6MW9NYXhjOm9UQ3NxNHhHUkZka01OYXE3aDk3a3FOYk1vZkllaTVWa2kxRFdpQnRkT00iLCAidSI6IHsiaWQiOiA2OTAzNTkxMiwgImZuIjogIkNocmlzIiwgImxuIjogIkJyb3dubGllIiwgImZjIjogMTd9fQ=='; sessionid=.eJxVy8sKwjAQheF3yVrKZCaXxp17QaG4LtMkQ8RSirEr8d1Nd7o8fOd_q5G3Vxm3mp_jPamjcgHIBo3q8EsTx0dedl9nWedul-56vjWrw3A5tfkfFK6lvVFIC2jOBsQGg0hO0Ds00nMPEB1yjnYK2vpotDggiZIyBfLJcwD1-QLjITKd:1oMaxc:LV7MNBmDiiLPng-A6issYdUMFcCZ1ti_KfEE0ZqEYMw"
    ) |>
    httr2::req_body_json(data = payload) |>
    httr2::req_method("POST") |>
    httr2::req_perform(verbosity = verbose)

  if (rep$status_code == 200) {
    cli::cli_alert_success("Team changes made!")
  } else {
    cli::cli_abort("Team changes could not be made")
  }
}

#' Validate a team selection
#'
#' Takes in a vector of player IDs that are to be passed to update_team, and
#' ensures they are in the right order, printing the team to the console for
#' the users benefit
#'
#' @param player_ids a numeric vector of length 15 indicating the player IDs of
#' players in your squad, with the first 11 being your starting XI
#'
#' @return the same numeric vector in an order that is acceptable for the API
validate_team_selection <- function(player_ids,
                                    cpt_pos,
                                    vc_pos) {

  selection <- get_players() %>%
    filter(id %in% player_ids) %>%
    mutate(selection_pos = match(id, player_ids),
           bench = !selection_pos %in% 1:11,
           position = ordered(position, levels = c("GKP", "DEF", "MID", "FWD")),
           is_captain = selection_pos == cpt_pos,
           is_vice_captain = selection_pos == vc_pos) %>%
    arrange(bench, position, desc(points_total))



  # Print team info to console
  print_team_selection(selection)

  # Return new order of IDs
  list(ids = selection$id,
       capt = which(selection$captain),
       vc = which(selection$vc))
}

#' Print a nicely formatted team selection
#'
#' @param selection_df a dataframe with 15 rows and at least
#' columns 'id', 'position', 'known_as', 'points_total',
#' 'is_captain' and 'is_vice_captain' (i.e. this can simply
#' be the result of a call to get_my_team())
#'
#' @return print nicely formatted team sheet to console
print_team_selection <- function(selection_df) {

  selection <- selection_df %>%
    rename(any_of(c("selection_pos" = "team_position"))) %>%
    mutate(bench = !selection_pos %in% 1:11,
           position = ordered(position, levels = c("GKP", "DEF", "MID", "FWD")),
           sel_string = paste0(cli::col_cyan(known_as),
                               "-",
                               cli::col_yellow(points_total),
                               ifelse(is_captain,
                                      cli::style_bold(" (C)"),
                                      ifelse(is_vice_captain,
                                             cli::style_bold(" (VC)"),
                                             "")))) %>%
    arrange(bench, position, desc(points_total))


  # Print team info to console
  starting_gkp <- paste(selection$sel_string[!selection$bench&selection$position=="GKP"], collapse = ";  ")
  starting_defs <- paste(selection$sel_string[!selection$bench&selection$position=="DEF"], collapse = ";  ")
  starting_mids <- paste(selection$sel_string[!selection$bench&selection$position=="MID"], collapse = ";  ")
  starting_fwds <- paste(selection$sel_string[!selection$bench&selection$position=="FWD"], collapse = ";  ")
  benched <- paste(selection$sel_string[selection$bench], collapse = "; ")
  cli::cli_alert_info("Your selected team:")
  cli::cli_bullets(paste0("GKP: ", starting_gkp))
  cli::cli_bullets(paste0("DEF: ", starting_defs))
  cli::cli_bullets(paste0("MID: ", starting_mids))
  cli::cli_bullets(paste0("FWD: ", starting_fwds))
  cli::cli_bullets(paste0("(Bench): ", benched))
}
