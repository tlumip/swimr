% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{county_controls}
\alias{county_controls}
\title{County control totals}
\format{
A data frame with 396 observations.
\describe{
  \item{county}{name of the county in Oregon}
  \item{year}{the year for the control; 1990-2010 are estimates, past 2010
  are forecasts}
  \item{var}{The type of control: population or employment.}
  \item{y}{The value of the control.}
}
}
\source{
Reconstructed from data provided by Alex Bettinardi
}
\usage{
county_controls
}
\description{
A dataset containing the control totals for employment and population for
counties in the Oregon statewide model.
}
\keyword{datasets}
