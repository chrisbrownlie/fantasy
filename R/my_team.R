#' Get my team
#'
#' Get current fantasy premier league team for a logged-in user
#'
#' @return an object of class team, representing your current team
#'
#' @export
get_my_team <- function() {

  # Check authentication
  require_authentication()

  # Get manager ID (this environment variable must exist if authenticated)
  manager_id <- Sys.getenv("FPL_MANAGER_ID")

  # Construct endpoint URL for my team
  my_team_ep <- construct(paste0("my-team/", manager_id, "/"))

  # Get team data
  rep <- perform_query(my_team_ep,
                       Cookie = paste0("pl_profile=", getOption("FANTASY_COOKIE")),
                       type = "json")

  raw_team <- rep$picks %>%
    bind_rows() %>%
    arrange(position) %>%
    select(id = element,
           is_captain,
           is_vice_captain)

  bank <- rep$transfers$bank
  chips <- sapply(rep$chips, function(x) setNames(ifelse(!length(x$played_by_entry),
                                                         TRUE,
                                                         NA), nm = x$name))
  transfers <- rep$transfers$limit - rep$transfers$made

  # Convert to team object and return
  team(raw_team$id,
       captain = raw_team$id[raw_team$is_captain],
       vc = raw_team$id[raw_team$is_vice_captain],
       bank = bank,
       transfers = transfers,
       chips = chips)
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
#'
#' @export
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
#' @param current the current team object
#' @param previous the previous team object
#' @param type the return type, if 'report' (the default), differences between
#' the two teams will be printed prettily to the console. If 'transfer_list', will return
#' a list of transfers
#'
#' @return prints information to the console on the differences between the two teams
#'
#' @export
summarise_team_changes <- function(current,
                                   previous,
                                   type = "report") {

  if (!type %in% c("report", "transfer_list")) cli::cli_abort("{.arg type} must be one of '{.val report}' or '{.val transfer_list}'")

  # If previous is missing and user is authenticated, compare new team to their current team
  if (missing(previous)) {
    if (is_authenticated()) {
      cli::cli_alert_warning("You have not supplied a previous team, so using your current FPL team for comparison")
    } else {
      cli::cli_abort("You have not supplied a {.arg previous} team to compare to")
    }
  }

  # Check argument types
  if (!is_team(previous)) cli::cli_abort("{.arg previous} must be an object of class {.cls team}")
  if (!is_team(current)) cli::cli_abort("{.arg current} must be an object of class {.cls team}")

  # List transfers
  transfers_in <- current$id[!current$id %in% previous$id]
  transfers_out <- previous$id[!previous$id %in% current$id]
  transfer_list <- purrr::imap(transfers_in,
                               ~list(element_in = .x,
                                     element_out = transfers_out[.y],
                                     purchase_price = get_player_cost(.x)*10,
                                     selling_price = get_player_cost(transfers_out[.y])*10))

  # List substitutions
  subs_in <- current$id[current$id %in% attr(current, "submission_order")[1:11] & !current$id %in% attr(previous, "submission_order")[1:11] & !current$id %in% transfers_in]
  subs_out <- previous$id[previous$id %in% attr(previous, "submission_order")[1:11] & !previous$id %in% attr(current, "submission_order")[1:11]  & !previous$id %in% transfers_out]

  if (type == "report") {
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
      subin <- paste(paste0(cli::col_yellow(current$known_as[current$id %in% subs_in]), " (", cli::col_blue(current$position[current$id %in% subs_in]), ")"), collapse = "; ")
      cli::cli_text(paste0("{cli::symbol$arrow_up}", cli::col_cyan("Subs In: "), subin))
    }
    if (length(subs_out)) {
      subout <- paste(paste0(cli::col_yellow(current$known_as[current$id %in% subs_out]), " (", cli::col_blue(current$position[current$id %in% subs_out]), ")"), collapse = "; ")
      cli::cli_text(paste0("{cli::symbol$arrow_down}", cli::col_cyan("Subs Out: "), subout))
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
  } else if (type == "transfer_list") {
    return(transfer_list)
  } else {
    invisible(NULL)
  }

}
