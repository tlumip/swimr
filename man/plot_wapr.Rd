% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/laborforce.R
\name{plot_wapr}
\alias{plot_wapr}
\title{Plot the labor force participation rate.}
\usage{
plot_wapr(
  db,
  color_var = c("BZONE", "MPO", "COUNTY", "DOT_REGION", "STATE"),
  color_levels = NULL,
  facet_var = NULL,
  facet_levels = NULL,
  index_year = 2000
)
}
\arguments{
\item{db}{The scenario sqlite database.}

\item{color_var}{Field to color by: c("BZONE", "MPO", "COUNTY", "DOT_REGION",
"STATE")}

\item{color_levels}{A character vector of the variable specifiying
which levels to include.}

\item{facet_var}{Field to facet by: c("MPO", "COUNTY", "DOT_REGION",
"STATE")}

\item{facet_levels}{A character vector of the variable specifiying
which levels to include.}

\item{index_year}{[Optional] index year that should be used as the starting year for data or plots;}
}
\value{
a ggplot2 plot object.
}
\description{
BLS defines the civilian labor force participation rate as the percent of
individuals age 16 and over who are either working or who are looking for
work. This is not something we ask about in travel surveys generally, so it is
not possible to make an apples-to-apples comparison.
}
\details{
This function returns the number of workers in each facet region divided by
the number of individuals fifteen years and older in that region. The
definition of a "worker" in a travel model is different than the BLS
definition in that travel modesl do not consider unemployed individuals who
are looking for work.

We call our definition "workers to adult population ratio," or "WAPR".
}
