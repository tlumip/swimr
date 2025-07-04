% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/population.R
\name{compare_sevar}
\alias{compare_sevar}
\title{Compare population and employment}
\usage{
compare_sevar(db1, db2, ...)
}
\arguments{
\item{db1}{The swim database for the "Reference" scenario.}

\item{db2}{The swim database for the "Current" scenario.}

\item{...}{
  Arguments passed on to \code{\link[=extract_se]{extract_se}}
  \describe{
    \item{\code{color_var}}{Field to color by: either "MPO" or "COUNTY".}
    \item{\code{color_levels}}{A character vector of the color variable specifiying
which levels to include.}
    \item{\code{controls}}{Plot against the control totals. Defaults to TRUE, cannot
currently run with FALSE.}
    \item{\code{index}}{Whether to show the variables as indexed against the base year.}
  }}
}
\value{
A \code{ggplot2} plot object showing the modeled change in employment
  and population over time.
}
\description{
Compare population and employment
}
