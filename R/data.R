#' County control totals
#'
#' A dataset containing the control totals for employment and population for
#' counties in the Oregon statewide model.
#'
#' @format A data frame with 396 observations.
#' \describe{
#'   \item{county}{name of the county in Oregon}
#'   \item{year}{the year for the control; 1990-2010 are estimates, past 2010 are forecasts}
#'   \item{population}
#'   \item{employment}
#' }
#' @source Reconstructed from data provided by Alex Bettinardi
"county_controls"

#' Reference counts
#'
#' A dataset containing the reference highway counts for comparing modeled
#' traffic volumes in the calibration year.
#'
#' @format A data frame with 167 observations.
#' \describe{
#'   \item{site}{The site location of the count.}
#'   \item{AADT}{Observed traffic flow.}
#'   \item{year}{The year of the count.}
#'   \item{FROMNODENO}{The from node of the link in the SWIM highway network.}
#'   \item{TONODENO}{The from node of the link in the SWIM highway network.}
#' }
#' @source Calculated by Rick Donnelly from data supplied by ODOT.
"ref_counts"
