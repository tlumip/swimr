#' County control totals
#'
#' A dataset containing the control totals for employment and population for
#' counties in the Oregon statewide model.
#'
#' @format A data frame with 396 observations.
#' \describe{
#'   \item{county}{name of the county in Oregon}
#'   \item{year}{the year for the control; 1990-2010 are estimates, past 2010
#'   are forecasts}
#'   \item{var}{The type of control: population or employment.}
#'   \item{y}{The value of the control.}
#' }
#' @source Reconstructed from data provided by Alex Bettinardi
"county_controls"


#' Floor types
#'
#' A dataset mapping the floor types in the database to consolidated categories.
#' @format A data frame with 16 observations.
#' \describe{
#'   \item{commodity}{The floorspace commodity types in the db.}
#'   \item{floor_type}{The consolidated categories.}
#' }
#'
"floor_types"

#' Employment types
#'
#' A dataset mapping the NAICS sectors in the buy/sell from the database to
#' consolidated categories.
#' @format A data frame with 43 observations.
#' \describe{
#'   \item{sector}{The employment sector in the db.}
#'   \item{naics}{The two-digit NAICS code largely corresponding to the sector.}
#'   \item{naics1}{The one-digit NAICS code aggregation level.}
#'   \item{naics_label}{Label for the one-digit NAICS Code.}
#' }
"employment_types"

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


#' Link table
#'
#' A fortified shapefile containing link attributes
"links"

#' Zone table
#'
#' A fortified shapefile containing the zone geometries
"zones"
