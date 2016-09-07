# link and zone shapefiles converted to R objects
library(maptools)
library(readr)
library(rgdal)
library(dplyr)

options(stringsAsFactors = FALSE)

ogic <- CRS("+proj=lcc +lat_1=43 +lat_2=45.5 +lat_0=41.75 +lon_0=-120.5 +x_0=399999.9999999999 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=ft +no_defs")
wgs84 <- CRS("+proj=longlat +datum=WGS84 +no_defs")

# links
links_shp <- readShapeLines(
  "data-raw/links_link.shp",
  proj4string = ogic
) %>%
  spTransform(., wgs84)


links_data <- links_shp@data %>%
  tbl_df() %>%
  mutate(
    id = as.character(row_number() - 1),

    # there's a really annoying feature of R that reads in data as factors. But
    # even more annoying is the fact that the length fields in visum are written
    # as 0.159mi, which means that the numeric field gets read in as a factor.
    LENGTH = as.character(levels(LENGTH)[LENGTH])
  ) %>%
  mutate(
    LENGTH = as.numeric(gsub("[^0-9$.]", "", LENGTH))
  )

links <- fortify(links_shp) %>%
  left_join(links_data, by = "id") %>%
  filter(!(TSYSSET %in% c("m", "n"))) %>%
  tbl_df()

devtools::use_data(links)

# zones
zones_shp <- readShapePoly(
  "data-raw/zones_zone.shp",
  proj4string = ogic
) %>%
  spTransform(., wgs84)

regions <- read_csv("data-raw/regions.csv")

zones_shp@data <- zones_shp@data %>%
  left_join(regions) %>%
  transmute(
    NO,
    id = as.character(row_number() - 1),
    AZONE = as.numeric(as.character(AZONE)),
    BZONE,
    COUNTY = as.character(COUNTY),
    ALDREGION = as.character(ALDREGION),
    MPO = as.character(MPO),
    DOT_REGION,
    STATE = as.character(STATE)
  )


zones <- fortify(zones_shp) %>% tbl_df() %>%
  left_join(zones_shp@data, by = "id")

zones_data <- zones_shp@data

devtools::use_data(zones, overwrite = TRUE)
devtools::use_data(zones_data, overwrite = TRUE)
devtools::use_data(zones_shp, overwrite = TRUE)


