# link and zone shapefiles converted to R objects
library(maptools)
library(ggmap)
library(rgdal)

ogic <- CRS("+proj=lcc +lat_1=43 +lat_2=45.5 +lat_0=41.75 +lon_0=-120.5 +x_0=399999.9999999999 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=ft +no_defs")
wgs84 <- CRS("+proj=longlat +datum=WGS84 +no_defs")

# links
links_shp <- readShapeLines(
  "data-raw/links_link_link.SHP",
  proj4string = ogic
) %>%
  spTransform(., wgs84)


links_data <- links_shp@data %>%
  mutate(id = as.character(row_number() - 1)) %>%
  tbl_df()

links <- fortify(links_shp) %>%
  left_join(links_data, by = "id") %>%
  filter(!(TSYSSET %in% c("m", "n"))) %>%
  tbl_df()

devtools::use_data(links)

# zones
zones_shp <- readShapePoly(
  "data-raw/zones_zone.SHP",
  proj4string = ogic
) %>%
  spTransform(., wgs84)


zones_data <- zones_shp@data %>%
  mutate(id = as.character(row_number() - 1)) %>%
  tbl_df() %>%
  select(NO, ALDREGION, AZONE, BZONE, AREASQFT, id)

zones <- fortify(zones_shp) %>% tbl_df() %>%
  left_join(zones_data, by = "id")

devtools::use_data(zones)


