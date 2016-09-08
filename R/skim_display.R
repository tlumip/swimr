#' Plot a skim chloropleth
#'
#' @param zone The focus zone.
#' @param skim The name of the skim file in the \code{dir1/outputs/t*/}
#'   directory.
#' @param dir1 The base or reference directory.
#' @param dir2 The alternative or analysis scenario. Default \code{NULL}
#'   scenario.
#' @param year1 The base or reference year to show, e.g., \code{t20}.
#' @param year2 The comparison or analysis year. Default \code{NULL}.
#' @param from If \code{TRUE}, gets values from \code{zone} to other zones.
#'
#' @details If \code{dir2} and \code{year2} are both \code{NULL}, will show an
#'   absolute measure of travel time. If one or the other is given, will show
#'   a percent change from \code{dir1, year1}.
#'
#' @export
#'
skim_leaflet <- function(zone, skim,
                         dir1, dir2 = NULL, year1, year2 = NULL,
                         from = TRUE) {

  # get reference skim
  s1 <- get_skim(file.path(dir1, "outputs", year1, skim), zone, from)

  # if year2 or dir2 given, get alternate skim
  if(!is.null(dir2) | !is.null(year2)){
    if(is.null(year2)) year2 <- year1
    if(is.null(dir2)) dir2 <- dir1

    s2 <- get_skim(file.path(dir2, "outputs", year2, skim), zone, from)
  }

  if(exists("s2")){
    # Calculate percent diff between between s1 and s2
    s <- dplyr::left_join(s1, s2, by = "zone") %>%
      dplyr::mutate(
        value = pct_diff(value.x, value.y),
        plot_value = cut_diverror(value)
      )

    # error diff pallette
    palq <- colorFactor(
      palette = "PuOr",
      domain = s$plot_value
    )

    legend_title <- "Percent difference"

  } else {
    # just return value from s1 if no comparison
    s <- s1 %>%
      dplyr::mutate(plot_value = value)
    # error diff pallette

    palq <- colorBin(
      palette = "PuRd",
      domain = s$plot_value
    )
    legend_title <- "Travel Time"

  }

  # Join skim information to zones shapefile
  shp <- zones_shp
  shp@data <- shp@data %>%
    dplyr::left_join(s, by = c("AZONE" = "zone"))

  # Make and return leaflet plot
  zone_leaflet(shp) %>%
    addPolygons(
      group = "", fill = TRUE, color = FALSE,
      fillColor = ~palq(plot_value),
      popup = skim_popup(shp, zone)
    ) %>%
    addLegend(
      "bottomright", pal = palq, values = ~plot_value,
      title = legend_title
    )


}


#' Process skim values from one zone.
#'
#' @param skim Path to zmx skim file
#' @param zone Focus zone for the skim calculation.
#' @param from If \code{TRUE}, gets values from \code{zone} to other zones.
#'
#'
#'
get_skim <- function(skim, zone, from = TRUE){

  message("Reading zone ", zone, " from ", skim)
  s <- omxr::read_zmx(skim)

  # Get origin or destination row
  if(from){
    r <- s[zone, ]
  } else {
    r <- s[, zone]
  }

  dplyr::data_frame(
    zone = as.numeric(names(r)),
    value = r
  )

}

#' Build skim popup tag for leaflet
#'
#' @inheritParams skim_leaflet
#' @param shp A shapefile containing data table attributes
#' 
#'
skim_popup <- function(shp, zone){
  # build popup
  zone_info <- paste0("<strong>From Zone: </strong>", zone, "<br>",
                      "<strong>To Zone: </strong>", shp@data$AZONE)

  if("value.x" %in% names(shp@data)){
    var_info <- paste0(
      "<strong>", "Base: </strong>", round(shp@data[, "value.x"], 3), "<br>",
      "<strong>", "Alternative: </strong>", round(shp@data[, "value.y"], 3),
      "<br>",
      "<strong>Pct Change: </strong>", round(shp@data[, "value"], digits = 3),
      "%"
    )
  } else {
    var_info <- paste0(
      "<strong>", "Value: </strong>", round(shp@data[, "value"], 3), "<br>"
    )
  }

  paste(zone_info, var_info, sep = "</br>")

}
