% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulate_bleaching.R
\name{simulate_bleaching}
\alias{simulate_bleaching}
\title{Assign Bleaching Status to Coral Population}
\usage{
simulate_bleaching(
  populations,
  mortality = NULL,
  seed,
  silent = FALSE,
  plot = FALSE
)
}
\arguments{
\item{mortality}{A data frame with columns \code{species} and \code{mortality}
(values between 0 and 1).}

\item{population}{An \code{sf} object with a \code{species} column.}
}
\value{
An \code{sf} object with an additional column \code{status}.
}
\description{
Randomly assigns bleaching status ("bleached" or "unbleached") to individuals
in a population dataset based on species-specific mortality rates.
}
