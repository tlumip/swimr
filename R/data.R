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

#' Historical county population
#'
#' A dataset containing the historical population from the decennial census of
#' population and housing for Oregon from 1930-2010
#'
#' @format A data frame with 324 observations.
#' \describe{
#'   \item{county}{name of the county in Oregon}
#'   \item{year}{1930-2010 by decennial.}
#'   \item{y}{The population count (coded as y for joining to interior data).}
#' }
#' @source Reconstructed from data provided by Rick Donnelly
"historical_pop"


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


#' Facility types
#'
#' A dataset mapping the facility types in the database to consolidated
#' categories.
#' @format A data frame with 17 observations.
#' \describe{
#'   \item{PLANNO}{The facility classifications in the db.}
#'   \item{FacType}{The consolidated classifications.}
#' }
#'
"fac_types"


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
#' traffic volumes in multiple historical years.
#'
#' @format A data frame with 2,170 observations.
#' \describe{
#'   \item{site}{The site location of the count.}
#'   \item{FROMNODENO}{The from node of the link in the SWIM highway network.}
#'   \item{TONODENO}{The from node of the link in the SWIM highway network.}
#'   \item{aawdt}{Observed traffic flow; calculated AAWDT by ODOT data analysis
#'   division.}
#'   \item{year}{The year of the count.}
#' }
#' @source Calculated by Rick Donnelly from data supplied by ODOT.
"ref_counts"


#' Consolidated transport modes
#'
#' A dataset mapping modes in the trip matrix to consolidated values for plotting.
#'
#' @format A data frame with 9 observations.
#' \describe{
#'   \item{mode}{The mode in the TRIPMATRIX} \item{consolidated_mode}{One of
#'   five modes: auto, transit, school, truck, and non-motorized.}
#' }
#'
"mode_types"

#' Link table
#'
#' A fortified shapefile containing link attributes
"links"

#' Zone table
#'
#' A fortified shapefile containing the zone geometries
"zones"

#' Zone Shapefile
#'
#' A SpatialPolygonsDataFrame of the zone system for plotting in leaflet.
"zones_shp"
