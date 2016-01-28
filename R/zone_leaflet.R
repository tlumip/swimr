#' Extract se data for leaflet zone plots
#'
#' @inheritParams change_leaflet
#'
#' @details If \code{year2 = NULL}, then will return only the values from year 1
#'   and will not calculate the implied exponential growth rate.
#'
extract_changedata <- function(db, year1, year2 = NULL){

  if(is.null(year2)){
    year2 <- year1
  }

  # pre-construct growth rate function call in order to mix variables
  # and field names. See vignettes("nse") for details.
  grt_exp <- lazyeval::interp(
    ~ calc_exprate(p1, p2, year1, year2) * 100,
    p1 = as.name(year1), p2 = as.name(year2))

  # Get socioeconomic data from database.
  se <- tbl(db, "AZONE") %>%
    transmute(AZONE, pop = POPULATION, emp = EMPLOYMENT,
              hh = TOTALHHS, year = TSTEP + 1990) %>%
    filter(year %in% c(year1, year2)) %>%
    collect() %>%
    gather(variable, value, -AZONE, -year) %>%
    spread(year, value)

  if(year2 != year1){
    se <- se %>%
      # calculate implied growth rate
      mutate_("rate" = grt_exp)
  }

  # reformat
  se %>%
    gather(period, value, -AZONE, -variable) %>%
    unite(var, c(variable, period)) %>%
    spread(var, value, fill = NA)

}


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

