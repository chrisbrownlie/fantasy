#' Construct new team
#'
#' @param players a numeric vector of 15 player IDs
#' @param captain the ID of the teams captain
#' @param vc the ID of the teams vice captain
#' @param bank a numeric value indicating the teams remaining balance (in FPL£m)
#' @param transfers an integer indicating the number of free transfers a
#' team has remaining
#' @param chips a named logical vector of length 4, indicating whether each of
#' the teams chips is available or not
#'
#' @return a new tibble of class team
#'
#' @keywords internal
new_team <- function(players,
                     captain,
                     vc,
                     bank,
                     transfers,
                     chips) {

  # Create new team object
  tibble::new_tibble(
    tibble::as_tibble_col(players, column_name = "id"),
    captain = captain,
    vc = vc,
    submission_order = players,
    bank = bank,
    transfers = transfers,
    chips = chips,
    class = "team"
  )
}

#' Validate new team
#'
#' @param x an object of class team
#'
#' @return the input team object, or throws an error if the
#' team is not valid.
#'
#' @keywords internal
validate_team <- function(x) {

  # 15 players
  if (nrow(x)!=15) cli::cli_abort("Teams must be length 15 (11 starting players and 4 subs)")

  # Only valid players allowed (name will be NA for invalid players because of the
  # left_join in team())
  if (any(is.na(x$name))) {
    cli::cli_abort(paste0("Some of the IDs you have selected are not valid player IDs. Invalid IDs: ",
                          paste(x$id[is.na(x$name)], collapse = "; ")))
  }

  # Check a single valid captain has been selected
  if (!is.numeric(attr(x, "captain"))) cli::cli_abort("Captain must be single numeric ID")
  if (length(attr(x, "captain")) == 0) cli::cli_abort("A captain must be selected")
  if (length(attr(x, "captain")) > 1) cli::cli_abort("Only one captain can be selected")
  if (!attr(x, "captain") %in% x$id) cli::cli_abort("Selected captain is not one of the players in the team")

  # Check a single valid vice-captain has been selected
  if (!is.numeric(attr(x, "vc"))) cli::cli_abort("Vice-captain must be single numeric ID")
  if (length(attr(x, "vc")) == 0) cli::cli_abort("A vice-captain must be selected")
  if (length(attr(x, "vc")) > 1) cli::cli_abort("Only one vice-captain can be selected")
  if (!attr(x, "vc") %in% x$id) cli::cli_abort("Selected vice-captain is not one of the players in the team")

  # Check the number of players of each position is valid
  if (sum(x$position == "GKP") != 2) cli::cli_abort(paste0("You must select two goalkeepers in your team, you currently have ",
                                                           sum(x$position == "GKP"), ": ", paste(x$known_as[x$position == "GKP"],
                                                                                                 collapse = "; ")))
  if (sum(x$position == "DEF") != 5) cli::cli_abort(paste0("You must select five defenders in your team, you currently have ",
                                                           sum(x$position == "DEF"), ": ", paste(x$known_as[x$position == "DEF"],
                                                                                                 collapse = "; ")))
  if (sum(x$position == "MID") != 5) cli::cli_abort(paste0("You must select five midfielders in your team, you currently have ",
                                                           sum(x$position == "MID"), ": ", paste(x$known_as[x$position == "MID"],
                                                                                                 collapse = "; ")))
  if (sum(x$position == "FWD") != 3) cli::cli_abort(paste0("You must select three forwards in your team, you currently have ",
                                                           sum(x$position == "FWD"), ": ", paste(x$known_as[x$position == "FWD"],
                                                                                                 collapse = "; ")))

  # Check maximum of three from a single club
  teams <- count(x, team)
  if (any(teams$n>3)) cli::cli_abort(paste0("You can only select a maximum of three players from any one club. ",
                                            "You have selected more than three players from the following club(s): ",
                                            paste(team_from_id(teams$team[teams$n>3], T), collapse = "; ")))

  # Ensure team is in submission order for remaining checks
  x <- x[match(attr(x, "submission_order"), x$id),]

  # Check the number of each position in starting XI
  if (sum(x$position[1:11] == "GKP") != 1) cli::cli_abort(paste0("You must select {.val {1}} goalkeeper in your starting XI, you currently have {.val {sum(x$position[1:11] == 'GKP')}}."))
  if (sum(x$position[1:11] == "DEF") < 3) cli::cli_abort("You must select at least {.val {3}} defenders in your starting XI, you currently have {.val {sum(x$position[1:11] == 'DEF')}}.")
  if (sum(x$position[1:11] == "MID") < 2) cli::cli_abort("You must select at least {.val {2}} midfielders in your starting XI, you currently have {.val {sum(x$position[1:11] == 'MID')}}.")
  if (sum(x$position[1:11] == "FWD") < 1) cli::cli_abort("You must select at least {.val {1}} forwards in your starting XI, you currently have {.val {sum(x$position[1:11] == 'FWD')}}.")

  # Check the order of players in the starting XI and change to make it correct
  starting_pos_order <- order(x$position[1:11])
  if (!identical(starting_pos_order, 1:11)) {
    x <- x[c(starting_pos_order, 12:15),]
  }

  # Check the order of players on the bench and change to make it correct
  bench_pos_order <- order(x$position[12:15])
  if (!identical(bench_pos_order, 1:4)) {
    x <- x[c(1:11, 11+bench_pos_order),]
  }

  # Update submission order attribute with position-correct order
  attr(x, "submission_order") <- x$id

  # OTHER ATTRIBUTES
  # Check that bank balance is not negative
  if (!is.numeric(attr(x, "bank"))) cli::cli_abort("Bank balance must be a single non-negative numeric value")
  if (length(attr(x, "bank"))!=1) cli::cli_abort("Bank balance must be a single non-negative numeric value")
  if (attr(x, "bank") < 0) cli::cli_abort("Bank balance cannot be negative.")

  # Check that transfers not used up
  if (!is.numeric(attr(x, "transfers"))) cli::cli_abort("Transfers must be a single integer value")
  if (attr(x, "transfers") < 0) cli::cli_warn("This action is not covered by free transfers so will cost {.strong {abs(attr(x, 'transfers'))*4}} points.")

  # Check that chips has all expected names and is logical
  if (!is.logical(attr(x, "chips"))|length(attr(x, "chips"))!=4|!length(names(attr(x, "chips")))) cli::cli_abort("Chips must be a named logical vector of length 4")
  if (!setequal(names(attr(x, "chips")), c("bboost", "3xc", "wildcard", "freehit"))) cli::cli_abort("Chips must be named: 'bboost', '3xc', 'wildcard', 'freehit'")

  x
}


#' Create a new team and validate it
#'
#' Typically you will not have to use this function explicitly, but it can
#' be used to build on top of the {fantasy} package.
#'
#' This function takes in 15 player IDs and a captain/vice-captain designation,
#' enriches this into a tibble with information on each player, then validates
#' this against a set of criteria to ensure the selected 15 is valid. It then
#' returns the enriched tibble as an object of class team.
#'
#' @param players a numeric vector of length 15 indicating 15 player IDs
#' @param captain a numeric value indicating the ID of the team's captain
#' @param vc a numeric value indicating the ID of the team's vice-captain
#' @param bank a numeric value indicating the teams remaining balance (in FPL£m)
#' @param transfers an integer indicating the number of free transfers a
#' team has remaining
#' @param chips a named logical vector of length 4, indicating the availability of each
#' chip. TRUE indicates the chip has not been used and is available. FALSE indicates
#' that the chip is being used for the current gameweek. NA indicates the chip
#' is not available to use (i.e. has been used earlier in the season).
#'
#' @return a tibble with class `<team>`
#'
#' @export
team <- function(players,
                 captain,
                 vc,
                 bank,
                 transfers,
                 chips) {

  # Create new raw team tibble
  team <- new_team(players = players,
                   captain = captain,
                   vc = vc,
                   bank = bank,
                   transfers = transfers,
                   chips = chips)

  # Get all valid player info for enriching
  valid_players <- get_players()

  # Enrich raw team with players info
  enriched_team <- team %>%
    left_join(valid_players, by = "id") %>%
    mutate(position = ordered(.data$position, levels = c("GKP", "DEF", "MID", "FWD")),
           team_position = row_number()) %>%
    # Subset the columns to retain most useful
    select(
      .data$id,
      .data$name,
      .data$known_as,
      .data$position,
      .data$team,
      .data$points,
      .data$points_total,
      .data$form,
      .data$cost
    )

  # Validate team (this potentially changes the order of the players)
  # and return
  validate_team(enriched_team)
}

#' Format function for team selection
#' @keywords internal
#' @export
print.team <- function(x, ...) {

  # If team is enriched, print nicely
  if (all(c("known_as", "position", "points_total") %in% names(x))&!isTRUE(getOption("FANTASY_UNFORMAT"))) {

    # Ensure team is in submission order for printing
    x <- x[match(attr(x, "submission_order"), x$id),]

    # Format names and IDs nicely
    selection <- x %>%
      mutate(sel_string = paste0(cli::col_yellow(.data$id), "-",
                                 cli::col_cyan(.data$known_as),
                                 ifelse(id == attr(x, "captain"),
                                        cli::style_bold(" (C)"),
                                        ifelse(.data$id == attr(x, "vc"),
                                               cli::style_bold(" (VC)"),
                                               ""))))

    # Print team info to console
    starting_gkp <- paste(selection$sel_string[1:11][selection$position[1:11]=="GKP"], collapse = ";  ")
    starting_defs <- paste(selection$sel_string[1:11][selection$position[1:11]=="DEF"], collapse = ";  ")
    starting_mids <- paste(selection$sel_string[1:11][selection$position[1:11]=="MID"], collapse = ";  ")
    starting_fwds <- paste(selection$sel_string[1:11][selection$position[1:11]=="FWD"], collapse = ";  ")
    benched <- paste(selection$sel_string[12:15], collapse = "; ")
    cli::cli_alert_info("Team selection:")
    cli::cli_bullets(paste0("GKP: ", starting_gkp))
    cli::cli_bullets(paste0("DEF: ", starting_defs))
    cli::cli_bullets(paste0("MID: ", starting_mids))
    cli::cli_bullets(paste0("FWD: ", starting_fwds))
    cli::cli_bullets(paste0("(Bench): ", benched))
  } else {
    # If team is not enriched, print normally as tibble
    NextMethod()
  }
}

#' Is this object a team object?
#'
#' @param x an object to test
#'
#' @export
is_team <- function(x) inherits(x, "team")

#' Remove all formatting of team objects for the current session
#'
#' Strip formatting from objects of class `<team>` so that the underlying
#' tibble is returned instead.
#' This function sets an options("FANTASY_UNFORMAT" = TRUE), which you
#' can add to your .Rprofile to activate for every session.
#'
#' @return invisibly sets an option to avoid custom formatting of team objects
#'
#' @rdname formatting
#' @export
global_remove_formatting <- function() {
  options("FANTASY_UNFORMAT" = TRUE)
  cli::cli_alert("{.cls team} objects will no longer be custom formatted. To revert to formatted output use {.fun global_restore_formatting}")
}
#' Restore formatting
#' @rdname formatting
#' @export
global_restore_formatting <- function() {
  options("FANTASY_UNFORMAT" = FALSE)
  cli::cli_alert("{.cls team} objects will now be custom formatted. To revert to unformatted output use {.fun global_remove_formatting}")
}

#' Convert team to data.frame
#'
#' Useful for testing or to access the underlying data of a team object
#'
#' @param x a team object to convert to a dataframe
#'
#' @return the team as a data.frame -i.e. the underlying
#' team data
#'
#' @keywords internal
team_to_df <- function(x) {
  class(x) <- "data.frame"
  attributes(x)[!attributes(x) %in% c("names", "class", "row.names")] <- NULL
  x
}
