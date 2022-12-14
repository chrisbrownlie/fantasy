% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/update.R
\name{update_team}
\alias{update_team}
\title{Update your team}
\usage{
update_team(team, verbose = 0, report_changes = TRUE)
}
\arguments{
\item{team}{either an object with class team, or a list of length
three with the following positional elements: (1) a numeric vector of length
15 indicating the 15 player IDs of the team; (2) a single numeric ID of the
captain; (3) a single numeric ID of the vice-captain.
If a list is supplied, it will be converted to a team object first
before the update request is sent.}

\item{verbose}{an integer from 0 to 3 denoting how verbose the request should be -
passed to httr2::req_perform (0 = no output, 3 = maximum output)}

\item{report_changes}{logical - should the overall changes to the team be
summarised and printed to the console?}
}
\value{
informs the user whether the update request was successful or not
}
\description{
Send a request to the API to update a team.
}
