#' Get my team
#'
#' Get current fantasy premier league team for a manager
#'
#' @param manager_id the manager ID of the team to show - this does
#' not need to be supplied once you have logged in using authenticate()
#'
#' @return an object of class team, representing your current team
#'
#' @export
get_my_team <- function(manager_id) {

  # If manager_id ommitted, check
  if (missing(manager_id)) manager_id <- Sys.getenv("FPL_MANAGER_ID")
  if (manager_id == "") cli::cli_abort("Must supply a manager ID or login using {.fun authenticate}")

  # Check authentication
  require_authentication()

  # Construct endpoint URL for my team
  my_team_ep <- construct(paste0("my-team/", manager_id, "/"))

  # Get team data
  rep <- perform_query(my_team_ep,
                       Cookie = paste0("pl_profile=", getOption("FANTASY_COOKIE")),
                       type = "json")

  raw_team <- rep$picks |>
    bind_rows() |>
    arrange(position) |>
    select(id = element,
           is_captain,
           is_vice_captain)

  # Convert to team object and return
  team(raw_team$id,
                 captain = raw_team$id[raw_team$is_captain],
                 vc = raw_team$id[raw_team$is_vice_captain])
}

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

  # If changes to be summarised, get current team before update
  if (report_changes) {
    pre_changes <- get_my_team(manager_id = Sys.getenv("FPL_MANAGER_ID"))
  }

  # Get team payload in format for server
  cli::cli_alert("Creating payload...")
  picks_payload_lists <- purrr::map(1:15, ~list(element = attr(team, "submission_order")[.x],
                                                is_captain = attr(team, "submission_order")[.x]==attr(team, "captain"),
                                                is_vice_captain = attr(team, "submission_order")[.x]==attr(team, "vc"),
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
      "Cookie" = paste0("pl_profile=", getOption("FANTASY_COOKIE"))
    ) |>
    httr2::req_body_json(data = payload) |>
    httr2::req_method("POST") |>
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
update_team.list <- function(team,
                             verbose = 0,
                             report_changes = TRUE) {

  # Check that team is valid
  if (length(team) != 3) cli::cli_abort("If using a list to update a team, must be a named list of length 3")
  if (length(team[[1]]) != 15|!is.numeric(team[[1]])) cli::cli_abort("If using a list to update a team, the first element must be a numeric vector of 15 player IDs")
  if (length(team[[2]]) != 1 |!is.numeric(team[[2]])) cli::cli_abort("If using a list to update a team, the second element must be a single numeric player ID of the captain")
  if (length(team[[3]]) != 1 |!is.numeric(team[[3]])) cli::cli_abort("If using a list to update a team, the third element must be a single numeric player ID of the vice-captain")

  # Convert to team
  cli::cli_alert("Converting list to {.cls team} object...")
  valid_team <- team(players = team[[1]],
                               captain = team[[2]],
                               vc = team[[3]])

  update_team(valid_team,
              verbose = verbose)
}

#' @export
update_team.default <- function(team,
                                verbose = 0,
                                report_changes = TRUE) {
  cli::cli_abort("{.fun update_team} can only be used with objects of class {.cls team} or lists.")
}

#' Assign a captain or vice-captain
#'
#' @param team an object with class team
#' @param pid the ID of the player to assign a role to
#' @param role either 'c' to assign them the role of
#' captain, or 'vc' to assign them the role of
#' vice-captain.
#'
#' @return the team object with the new role assigned
assign_role <- function(team,
                        pid,
                        role = 'c') {

  # Check initial arguments
  if (!is_team(team)) cli::cli_abort("{.fun assign_role} can only be used with an object of class {.cls team}")
  if (!is.numeric(pid)|length(pid) != 1) cli::cli_abort("{.arg pid} must be a single numeric player ID")
  if (!role %in% c("c", "vc")) cli::cli_abort("{.arg role} must be one of 'c' or 'vc'")

  # Check that player is in team, doesn't already have a role and is in starting XI
  if (!pid %in% team$id) cli::cli_abort("{.arg pid} must be the ID of a player in the team")
  if (!pid %in% attr(team, "submission_order")[1:11]) cli::cli_abort("Cannot assign a role to a non-starting player")
  if (attr(team, "captain") == pid & role == "vc") cli::cli_abort("Selected player is already captain so cannot be assigned as vice-captain")
  if (attr(team, "vc") == pid & role == "c") cli::cli_abort("Selected player is already vice-captain so cannot be assigned as captain")

  # If all checks pass, assign new role
  if (role == "c") {
    attr(team, "captain") <- pid
  } else if (role == "vc") {
    attr(team, "vc") <- pid
  }

  # Return team with new role
  team
}

#' Summarise the difference between two team objects
#'
#' Take in two team objects and summarise the changes between the two,
#' with the changes being printed to the console.
#'
#' @param previous the previous team object
#' @param current the current team object
#'
#' @return prints information to the console on the differences between the two teams
summarise_team_changes <- function(previous,
                                   current) {

  # Check argument types
  if (!is_team(previous)) cli::cli_abort("{.arg previous} must be an object of class {.cls team}")
  if (!is_team(current)) cli::cli_abort("{.arg current} must be an object of class {.cls team}")

  # List transfers
  transfers_in <- current$id[!current$id %in% previous$id]
  transfers_out <- previous$id[!previous$id %in% current$id]

  # List substitutions
  subs_in <- current$id[current$id %in% attr(current, "submission_order")[1:11] & !current$id %in% attr(previous, "submission_order")[1:11]]
  subs_out <- previous$id[previous$id %in% attr(previous, "submission_order")[1:11] & !previous$id %in% attr(current, "submission_order")[1:11]]

  cli::cli_alert_info("Team changes:")

  # Print information on transfers
  if (length(transfers_in)) {
    transf_in <- paste(cli::col_yellow(current$known_as[current$id %in% transfers_in]), collapse = "; ")
    cli::cli_text(paste0("{cli::symbol$arrow_right}", cli::col_cyan("Transfers In: "), transf_in))
  }
  if (length(transfers_out)) {
    transf_out <- paste(cli::col_yellow(previous$known_as[previous$id %in% transfers_out]), collapse = "; ")
    cli::cli_text(paste0("{cli::symbol$arrow_left}", cli::col_cyan("Transfers Out: "), transf_out))
  }
  if (!length(transfers_in) & !length(transfers_out)) {
    cli::cli_text("{cli::symbol$line} No transfers.")
  }

  # Print information on substitutions
  if (length(subs_in)) {
    subin <- paste(cli::col_yellow(current$known_as[current$id %in% subs_in]), collapse = "; ")
    cli::cli_text(paste0("{cli::symbol$arrow_up}", cli::col_cyan("Subs In: "), subin))
  }
  if (length(subs_out)) {
    subout <- paste(cli::col_yellow(current$known_as[current$id %in% subs_out]), collapse = "; ")
    cli::cli_text(paste0("{cli::symbol$arrow_down}", cli::col_cyan("Subs In: "), subout))
  }
  if (!length(subs_in) & !length(subs_out)) {
    cli::cli_text("{cli::symbol$line} No substitutes.")
  }

  # Print information on role changes
  if (attr(previous, "captain") != attr(current, "captain")) {
    prev_cap <- cli::col_red(previous$known_as[previous$id == attr(previous, "captain")])
    current_cap <- cli::col_green(current$known_as[current$id == attr(current, "captain")])
    cli::cli_text(paste0("{cli::symbol$circle_filled} Captain: ", prev_cap, " -> ", current_cap))
  }
  if (attr(previous, "vc") != attr(current, "vc")) {
    prev_vc <- cli::col_red(previous$known_as[previous$id == attr(previous, "vc")])
    current_vc <- cli::col_green(current$known_as[current$id == attr(current, "vc")])
    cli::cli_text(paste0("{cli::symbol$circle_double} Vice-Captain: ", prev_vc, " -> ", current_vc))
  }
  if (attr(previous, "captain") == attr(current, "captain") & attr(previous, "vc") == attr(current, "vc")) {
    cli::cli_text("{cli::symbol$line} No captaincy changes.")
  }

}
