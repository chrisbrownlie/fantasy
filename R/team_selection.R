#' Construct new team selection
#'
#' @param players a numeric vector of 15 player IDs
#' @param captain the ID of the teams captain
#' @param vc the ID of the teams vice captain
#'
#' @return a new tibble of class team_selection
new_team_selection <- function(players,
                               captain,
                               vc) {

  # Create new team_selection object
  tibble::new_tibble(
    tibble::as_tibble_col(players, column_name = "id"),
    captain = captain,
    vc = vc,
    submission_order = players,
    class = "team_selection"
  )
}

#' Validate new team selection
#'
#' @param x an object of class team_selection
#'
#' @return the input team_selection object, or throws an error if the
#' selection is not valid.
validate_team_selection <- function(x) {

  # 15 players
  if (nrow(x)!=15) cli::cli_abort("Team selections must be length 15 (11 starting players and 4 subs)")

  # Only valid players allowed (name will be NA for invalid players because of the
  # left_join in team_selection())
  if (any(is.na(x$name))) {
    cli::cli_abort(paste0("Some of the IDs you have selected are not valid player IDs. Invalid IDs: ",
                          paste(x$id[is.na(x$name)], collapse = "; ")))
  }

  # Check a single valid captain has been selected
  if (!is.numeric(attr(x, "captain"))) cli::cli_abort("Captain must be single numeric ID")
  if (length(attr(x, "captain")) == 0) cli::cli_abort("A captain must be selected")
  if (length(attr(x, "captain")) > 1) cli::cli_abort("Only one captain can be selected")
  if (!attr(x, "captain") %in% x$id) cli::cli_abort("Selected captain is not one of the players in the squad")

  # Check a single valid vice-captain has been selected
  if (!is.numeric(attr(x, "vc"))) cli::cli_abort("Vice-captain must be single numeric ID")
  if (length(attr(x, "vc")) == 0) cli::cli_abort("A vice-captain must be selected")
  if (length(attr(x, "vc")) > 1) cli::cli_abort("Only one vice-captain can be selected")
  if (!attr(x, "vc") %in% x$id) cli::cli_abort("Selected vice-captain is not one of the players in the squad")

  # Check the number of players of each position is valid
  if (sum(x$position == "GKP") != 2) cli::cli_abort(paste0("You must select two goalkeepers in your squad, you currently have ",
                                                           sum(x$position == "GKP"), ": ", paste(x$known_as[x$position == "GKP"],
                                                                                                 collapse = "; ")))
  if (sum(x$position == "DEF") != 5) cli::cli_abort(paste0("You must select five defenders in your squad, you currently have ",
                                                           sum(x$position == "DEF"), ": ", paste(x$known_as[x$position == "DEF"],
                                                                                                 collapse = "; ")))
  if (sum(x$position == "MID") != 5) cli::cli_abort(paste0("You must select five midfielders in your squad, you currently have ",
                                                           sum(x$position == "MID"), ": ", paste(x$known_as[x$position == "MID"],
                                                                                                 collapse = "; ")))
  if (sum(x$position == "FWD") != 3) cli::cli_abort(paste0("You must select three forwards in your squad, you currently have ",
                                                           sum(x$position == "FWD"), ": ", paste(x$known_as[x$position == "FWD"],
                                                                                                 collapse = "; ")))

  # Check maximum of three from a single club
  teams <- count(x, team)
  if (any(teams$n>3)) cli::cli_abort(paste0("You can only select a maximum of three players from any one club. ",
                                            "You have selected more than three players from the following club(s): ",
                                            paste(team_abbr(teams$team[teams$n>3], T), collapse = "; ")))

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

  x
}


#' Create a new team selection and validate it
#'
#' This function takes in 15 player IDs and a captain/vice-captain designation,
#' enriches this into a tibble with information on each player, then validates
#' this against a set of criteria to ensure the selected 15 is valid. It then
#' returns the enriched tibble as an object of class team_selection
#'
#' @param players a numeric vector of length 15 indicating 15 player IDs
#' @param captain a numeric value indicating the ID of the team's captain
#' @param vc a numeric value indicating the ID of the team's vice-captain
#'
#' @return a vector with class team_selection
#'
#' @export
team_selection <- function(players,
                           captain,
                           vc) {

  # Create new raw team selection tibble
  team_selection <- new_team_selection(players = players,
                                       captain = captain,
                                       vc = vc)

  # Get all valid player info for enriching
  valid_players <- get_players()

  # Enrich raw selection with players info
  enriched_selection <- team_selection |>
    left_join(valid_players, by = "id") |>
    mutate(position = ordered(position, levels = c("GKP", "DEF", "MID", "FWD")),
           team_position = row_number()) |>
    # Subset the columns to retain most useful
    select(
      id,
      name,
      known_as,
      position,
      team,
      points,
      points_total,
      form,
      cost
    )

  # Validate selection (this potentially changes the order of the players)
  # and return
  validate_team_selection(enriched_selection)
}

#' Format function for team selection
#' @export
print.team_selection <- function(x, ...) {

  # If selection is enriched, print nicely
  if (all(c("known_as", "position", "points_total") %in% names(x))) {

    # Ensure team is in submission order for printing
    x <- x[match(attr(x, "submission_order"), x$id),]

    # Format names and IDs nicely
    selection <- x |>
      mutate(sel_string = paste0(cli::col_yellow(id), "-",
                                 cli::col_cyan(known_as),
                                 ifelse(id == attr(x, "captain"),
                                        cli::style_bold(" (C)"),
                                        ifelse(id == attr(x, "vc"),
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
    # If selection is not enriched, print simply
    cli::cli_alert_info("Team selection (IDs only):")
    cli::cli_bullets(paste(x$id, collapse = "; "))
  }
}

#' Is this object a team selection?
is_team_selection <- function(x) inherits(x, "team_selection")
