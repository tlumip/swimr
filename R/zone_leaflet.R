#' Leaflet plot of change over time in one scenario.
#'
#' @param db The scenario database
#' @param year1 T
#'
#' @details This function creates an interactive Leaflet plot of the alpha zone
#'   system, showing annualized implied growth rates of key socioeconomic
#'   variables between two years. For instance, the user can select 2010 and
#'   2030 as analysis years, and the plot will show the implied average
#'   exponential growth rate between them.
#'
#'   As a note, SWIM does not generate alpha-zone level statistics in years when
#'   the transport model does not run. If the user asks for year that does not
#'   exist, then the plot will use the nearest available year.
#'
#' @import leaflet
#'
#' @export
change_leaflet <- function(db, year1 = 2010, year2 = 2030){

  # Get nearest years in db to year1 and year2
  #
  # The database doesn't have population and employment at
  # alpha zone level in years where the transport model doesn't run.
  # This simply corrects the value of year1 and year 2 if the user asks for
  # a year that doesn't exist.
  years <- 1990 + as.numeric(names(table(
    tbl(db, "AZONE") %>% select(TSTEP) %>% collect(n=Inf) %>% .$TSTEP)))
  year1 <- years[which(abs(years - year1) == min(abs(years - year1)))]
  year2 <- years[which(abs(years - year2) == min(abs(years - year2)))]

  # Get scenario information and put it onto the shapefile for
  # leaflet plotting.
  shp <- zones_shp
  shp@data <- shp@data %>%
    left_join(extract_zonedata(db, year1, year2))

  # Create a palette for
  palq <- colorFactor(
    palette = "RdBu",
    domain = cut_diverror(shp@data$pop_rate)
  )

  zone_leaflet(shp) %>%
    addPolygons(
      group = "Population", fill = TRUE, color = FALSE,
      fillColor = ~palq(cut_diverror(pop_rate)),
      popup = change_popup(shp, "pop", year1, year2)
    ) %>%
    addPolygons(
      group = "Employment", fill = TRUE, color = FALSE,
      fillColor = ~palq(cut_diverror(emp_rate)),
      popup = change_popup(shp, "emp", year1, year2)
    ) %>%
    addPolygons(
      group = "HH", fill = TRUE, color = FALSE,
      fillColor = ~palq(cut_diverror(hh_rate)),
      popup = change_popup(shp, "hh", year1, year2)
    ) %>%
    addLayersControl(
      overlayGroups = c("Population", "Employment", "HH"),
      options = layersControlOptions(collapsed = FALSE)
    )

}

#' Leaflet plot of difference between two scenarios.
#'
#' @param db1 Reference scenario VIZ database connection.
#' @param db2 Current scenario VIZ database connection.
#' @param year Year in which to compare the two scenarios.
#' @param scen_names Names of the scenarios in the comparison. Defaults to
#'   "Reference", "Current".
#'
#' @import leaflet
#'
#' @export
diff_leaflet <- function(db1, db2, year,
                         variable = c("Population", "Employment", "HH"),
                         scen_names = c("Reference", "Current")){

  if(variable == "Population"){
    crit <- "variable == 'pop'"
  } else if(variable == "Employment"){
    crit <- "variable == 'emp'"
  } else {
    crit <- "variable == 'hh'"
  }

  # Get nearest years in db to year
  #
  # The database doesn't have population and employment at
  # alpha zone level in years where the transport model doesn't run.
  # This simply corrects the value of year if the user asks for
  # a year that doesn't exist.
  years <- 1990 + as.numeric(names(table(
    tbl(db1, "AZONE") %>% select(TSTEP) %>% collect(n=Inf) %>% .$TSTEP)))
  year <- years[which(abs(years - year) == min(abs(years - year)))]

  # get se data from both scenarios and join together ----
  se <- extract_zonedata(db1, year) %>%
    left_join(extract_zonedata(db2, year), by = "AZONE") %>%
    gather(var, value, -AZONE) %>%
    separate(var, c("variable", "year", "sc")) %>%
    select(-year) %>%
    spread(sc, value) %>%
    filter_(.dots = crit) %>%

    # calculate absolute difference between scenarios
    mutate(
      diff = y - x,
      pct = diff / x * 100
    )


  # Get scenario information and put it onto the shapefile for
  # leaflet plotting.
  shp <- zones_shp
  shp@data <- shp@data %>%
    left_join(se, by = "AZONE")

  # Create a palette for
  palq <- colorFactor(
    palette = "PRGn",
    domain = cut_abserror(shp@data$diff)
  )

  zone_leaflet(shp) %>%
    addPolygons(
      group = "Absolute", stroke = FALSE,
      color = ~palq(cut_abserror(diff)),
      popup = diff_popup(shp, variable, scen_names)
    ) %>%
    addPolygons(
      group = "Percent", stroke = FALSE,
      color = ~palq(cut_abserror(pct)),
      popup = diff_popup(shp, variable, scen_names)
    ) %>%
    addLayersControl(
      baseGroups = c("Absolute", "Percent"),
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    addLegend(
      "bottomright", pal = palq, values = ~cut_abserror(diff),
      title = paste0("Change in ", variable, " per sqmi<br>",
                     scen_names[1], " - ", scen_names[2] )
    )

}

#' Extract se data for leaflet zone plots
#'
#' @inheritParams change_leaflet
#'
#' @details If \code{year2 = NULL}, then will return only the values from year 1
#'   and will not calculate the implied exponential growth rate.
#'
extract_zonedata <- function(db, year1, year2 = NULL){

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

    # return density of variable
    left_join(
      tbl(db, "ALLZONES") %>%
        transmute(AZONE = Azone, sqmi = AREASQFT * 3.587048e-8),
      by = "AZONE") %>%
    collect(n=Inf) %>%
    mutate_each(funs( ./sqmi), pop:hh) %>%
    select(-sqmi) %>%

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


#' Build change popup tag for leaflet
#'
#' @inheritParams zone_leaflet
#' @inheritParams change_leaflet
#' @param var Variable to create popup tag for (e.g., \code{"pop"})
#'
change_popup <- function(shp, var, year1, year2){
  # build popup
  zone_info <- paste0("<strong>Alpha Zone: </strong>", shp@data$AZONE)

  var_info <- paste0(
    "<strong>", year1, " ", var, " density: </strong>",
    round(shp@data[, paste0("", var, "_", year1)], digits = 2), "<br>",
    "<strong>", year2, " ", var, " density: </strong>",
    round(shp@data[, paste0("", var, "_", year2)], digits = 2), "<br>",
    "<strong>Growth rate </strong>",
    round(shp@data[, paste0(var, "_rate")], digits = 3), "%"
  )

  paste(zone_info, var_info, sep = "</br>")

}


#' Build diff popup tag for leaflet
#'
#' @inheritParams zone_leaflet
#' @inheritParams diff_leaflet
#' @param var Variable to create popup tag for (e.g., \code{"pop"})
#'
diff_popup <- function(shp, var, scen_names){
  # build popup
  zone_info <- paste0("<strong>Alpha Zone: </strong>", shp@data$AZONE)

  var_info <- paste0(
    "<strong>", scen_names[1], " ", var, " per sqmi: </strong>",
    round(shp@data[, "x"], digits = 3), "<br>",
    "<strong>", scen_names[2], " ", var, " per sqmi: </strong>",
    round(shp@data[, "y"], digits = 3), "<br>",
    "<strong>Difference </strong>",
    round(shp@data[, ("diff")], digits = 3), "<br>",
    "<strong>Percent diff </strong>",
    round(shp@data[, "pct"], digits = 3)
  )

  paste(zone_info, var_info, sep = "</br>")

}

