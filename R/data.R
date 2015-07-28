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
#'   \item{population}{The population control}
#'   \item{employment}{The employment control}
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
