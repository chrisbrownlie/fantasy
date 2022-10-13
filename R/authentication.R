#' Authenticate a user for the Fantasy Premier League API
#'
#' Use a supplied login email and password to get the necessary authentication
#' cookie to allow a user to get information about and update their team. The
#' value of the resulting cookie is saved for the session to options("FANTASY_COOKIE")
#'
#' @param email the email address of the FPL user
#' @param password the password of the FPL user
#'
#' @return saves the users authentication token to options("FANTASY_COOKIE")
#'
#' @export
authenticate <- function(email,
                         password) {

  # If inputs are missing and use is interactive, ask for inputs
  if (missing(email)) {
    if (Sys.getenv("FPL_EMAIL") == "") {
      if (interactive()) {
        email <- rstudioapi::showPrompt(title = "Login", message = "Enter your FPL email address:")
      } else {
        cli::cli_abort("{.arg email} must be supplied")
      }
    } else {
      email <- Sys.getenv("FPL_EMAIL")
    }
  }
  if (missing(password)) {
    if (Sys.getenv("FPL_PASSWORD") == "") {
      if (interactive()) {
        password <- rstudioapi::askForPassword(prompt = "Enter your FPL password:")
      } else {
        cli::cli_abort("{.arg password} must be supplied")
      }
    } else {
      password <- Sys.getenv("FPL_PASSWORD")
    }
  }

  # Send a login request to get the cookie required for authentication
  cli::cli_alert_info(paste0("Sending login request for user ", cli::col_magenta(email), "..."))
  login_response <- httr::POST(
    url = 'https://users.premierleague.com/accounts/login/',
    body = list(
      "login" = email,
      "password" = password,
      "redirect_uri" = "https://fantasy.premierleague.com/",
      "app" = "plfpl-web"),
    encode = "form",
    httr::add_headers(
      "Accept-Language" = "en-GB,en;q=0.5",
      "User-Agent" = "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:103.0) Gecko/20100101 Firefox/103.0"
    ))

  if (login_response$status_code != 200) cli::cli_abort("Login request was unsuccessful.")

  profile_cookie <- httr::cookies(login_response) %>%
    filter(.data$name == "pl_profile") %>%
    pull(.data$value)

  if (!length(profile_cookie)) cli::cli_abort("Login unsuccessful - incorrect email or password.")

  if (options("FANTASY_COOKIE") != "") {
    cli::cli_alert_success("Login successful, overwriting existing cookie...")
  } else {
    cli::cli_alert_success("Login successful, saving cookie for this session...")
  }

  options("FANTASY_COOKIE" = profile_cookie)
  cli::cli_alert_success("Authentication successful!")

  # Identify user manager ID and save for later use
  identify()

  invisible()
}


#' Ensure a user has logged in
#'
#' Check the value of the FANTASY_COOKIE option to see if user
#' has authenticated in this session and a valid cookie has
#' been stored.
#'
#' @return throws an error if no valid cookie has been stored
#'
#' @keywords internal
require_authentication <- function() {
  if (!length(getOption("FANTASY_COOKIE"))) authenticate()
  invisible(NULL)
}

#' Get a logged in users manager ID and save to environment variable
#'
#' @return saves the users manager ID to the environment variable FPL_MANAGER_ID
#'
#' @keywords internal
identify <- function() {

  # Check that user is authenticated
  require_authentication()

  # User info endpoint
  user_ep <- construct("me/")

  # Query the endpoint
  user_info <- perform_query(user_ep,
                             Cookie = paste0("pl_profile=", getOption("FANTASY_COOKIE")),
                             type = "json")

  # Save users manager ID to environment variable
  Sys.setenv("FPL_MANAGER_ID" = user_info$player$entry)
}
