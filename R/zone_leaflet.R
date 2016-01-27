#' Construct a base leaflet zone plot
#'
#' This function creates a standard leaflet base image that we can add
#' information to.
#'
#' @param shp An object of class \code{SpatialPolygonsDataFrame} with the
#'   plotting fields already appended.
#'
#' @import leaflet
#'
zone_leaflet <- function(shp){

  leaflet(shp) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(color = "grey", weight = 0.5, fill = FALSE)

}

