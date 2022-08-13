#' Authenticate a user for the Fantasy Premier League API
#'
#' Use a supplied login email and password to get the necessary authentication
#' cookie to allow a user to get information about and update their team. The
#' value of the resulting cookie
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
  if (missing(email)&interactive()) email <- rstudioapi::showPrompt(title = "Login", message = "Enter your FPL email address:")
  if (missing(password)&interactive()) password <- rstudioapi::askForPassword(prompt = "Enter your FPL password:")

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
      "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8",
      "Accept-Encoding" = "gzip, deflate, br",
      "Accept-Language" = "en-GB,en;q=0.5",
      "Connection" = "keep-alive",
      "Content-Type" = "application/x-www-form-urlencoded",
      "Host" = "users.premierleague.com",
      "Origin" = "https://fantasy.premierleague.com",
      "Referer" = "https://fantasy.permierleague.com/",
      "Sec-Fetch-Dest" = "document",
      "Sec-Fetch-Mode" = "navigate",
      "Sec-Fetch-Site" = "same-site",
      "Sec-Fetch-User" = "?1",
      "TE" = "trailers",
      "Upgrade-Insecure-Requests" = "1",
      "User-Agent" = "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:103.0) Gecko/20100101 Firefox/103.0"
    ))

  if (login_response$status_code != 200) cli::cli_abort("Login request was unsuccessful.")

  profile_cookie <- httr::cookies(login_response) |>
    filter(name == "pl_profile") |>
    pull(value)

  if (!length(profile_cookie)) cli::cli_abort("Login unsuccessful - incorrect email or password.")

  if (options("FANTASY_COOKIE") != "") {
    cli::cli_alert_success("Login successful, overwriting existing cookie...")
  } else {
    cli::cli_alert_success("Login successful, saving cookie for this session...")
  }

  options("FANTASY_COOKIE" = profile_cookie)
  cli::cli_alert_success("Authentication successful!")
  invisible()
}


#' Ensure a user has logged in
#'
#' Check the value of the FANTASY_COOKIE option to see if user
#' has authenticated in this session and a valid cookie has
#' been stored.
#'
#' @return throws an error if no valid cookie has been stored
require_authentication <- function() {
  if (!length(getOption("FANTASY_COOKIE"))) cli::cli_abort("You need to login using {.fun authenticate} first.")
}
