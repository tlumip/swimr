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


#' Build popup tag for leaflet
#'
#' @inheritParams zone_leaflet
#' @inheritParams change_leaflet
#' @param var Variable to create popup tag for (e.g., \code{"pop"})
#'
change_popup <- function(shp, var, year1, year2){
  # build popup
  zone_info <- paste0("<strong>Alpha Zone: </strong>", shp@data$AZONE)

  var_info <- paste0(
    "<strong>", year1, " ", var, ": </strong>",
    shp@data[, paste0("", var, "_", year1)], "<br>",
    "<strong>", year2, " ", var, ": </strong>",
    shp@data[, paste0("", var, "_", year2)], "<br>",
    "<strong>Growth rate </strong>",
    round(shp@data[, paste0(var, "_rate")], digits = 3), "%"
  )

  paste(zone_info, var_info, sep = "</br>")

}

