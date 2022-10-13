#' Update your team
#'
#' Send a request to the API to update a team.
#'
#' @param team either an object with class team, or a list of length
#' three with the following positional elements: (1) a numeric vector of length
#' 15 indicating the 15 player IDs of the team; (2) a single numeric ID of the
#' captain; (3) a single numeric ID of the vice-captain.
#' If a list is supplied, it will be converted to a team object first
#' before the update request is sent.
#' @param verbose an integer from 0 to 3 denoting how verbose the request should be -
#' passed to httr2::req_perform (0 = no output, 3 = maximum output)
#' @param report_changes logical - should the overall changes to the team be
#' summarised and printed to the console?
#'
#' @return informs the user whether the update request was successful or not
#'
#' @export
update_team <- function(team,
                        verbose = 0,
                        report_changes = TRUE) {
  UseMethod("update_team")
}

#' @export
update_team.team <- function(team,
                             verbose = 0,
                             report_changes = TRUE) {

  # Check authentication
  require_authentication()

  # Validate inputs
  validated <- validate_team(team)

  # Get current team before update
  pre_changes <- get_my_team()

  # Check chips and notify
  new_chips <- attr(team, "chips")
  old_chips <- attr(pre_changes, "chips")
  chips_used <- purrr::map(
    names(new_chips),
    function(x) {
      if (isTRUE(new_chips[x]) & !isTRUE(old_chips[x])) {
        cli::cli_alert_danger("You are using your {.val user_friendly(.x)} chip")
        x
      }
    }
  ) %>%
    unlist()

  # Identify any transfers
  transfers <- summarise_team_changes(previous = pre_changes,
                                      current = team,
                                      type = "transfer_list")
  if (length(transfers)) {
    cli::cli_alert("Creating transfer payload...")

    transfer_chip <- chips_used[chips_used %in% c("freehit", "wildcard")]
    transfer_payload <- list("transfers" = transfers,
                             "chip" = if_empty(as.list(transfer_chip)),
                             "entry" = Sys.getenv("FPL_MANAGER_ID"),
                             "event" = get_current_gameweek())

    cli::cli_alert("Sending transfers POST request...")
    transf_rep <- httr2::request("https://fantasy.premierleague.com/api/transfers/") %>%
      httr2::req_headers(
        'content-type' = 'application/json',
        'origin' = 'https://fantasy.premierleague.com',
        'referer' = 'https://fantasy.premierleague.com/transfers',
        "Cookie" = paste0("pl_profile=", getOption("FANTASY_COOKIE"))
      ) %>%
      httr2::req_body_json(data = transfer_payload) %>%
      httr2::req_method("POST") %>%
      httr2::req_perform(verbosity = verbose)

    if (transf_rep$status_code == 200) {
      cli::cli_alert("Transfer(s) successful!")
    } else {
      cli::cli_abort("Team changes could not be made - transfer(s) failed")
    }
  }

  # Get team payload in format for server
  picks_payload_lists <- purrr::map(1:15, ~list(element = attr(team, "submission_order")[.x],
                                                is_captain = attr(team, "submission_order")[.x]==attr(team, "captain"),
                                                is_vice_captain = attr(team, "submission_order")[.x]==attr(team, "vc"),
                                                position = .x))

  payload <- list(
    'picks' = picks_payload_lists,
    'chips' = as.list(attr(team, "chips"))
  )

  cli::cli_alert("Sending team POST request...")
  rep <- httr2::request("https://fantasy.premierleague.com/api/my-team/7330951/") %>%
    httr2::req_headers(
      'content-type' = 'application/json',
      'origin' = 'https://fantasy.premierleague.com',
      'referer' = 'https://fantasy.premierleague.com/my-team',
      "Cookie" = paste0("pl_profile=", getOption("FANTASY_COOKIE"))
    ) %>%
    httr2::req_body_json(data = payload) %>%
    httr2::req_method("POST") %>%
    httr2::req_perform(verbosity = verbose)

  if (rep$status_code == 200) {
    if (report_changes) {
      cli::cli_alert_success("Team update successful!")
      summarise_team_changes(previous = pre_changes,
                             current = team)
    } else {
      cli::cli_alert_success("Team update successful!")
    }
  } else {
    cli::cli_abort("Team changes could not be made")
  }
}

#' @export
update_team.default <- function(team,
                                verbose = 0,
                                report_changes = TRUE) {
  cli::cli_abort("{.fun update_team} can only be used with objects of class {.cls team}.")
}
