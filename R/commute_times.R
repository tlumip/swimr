#' Calculate trip times by residence region
#'
#' @param dir Scenario directory
#' @param year Analysis year, e.g. \code{t20}.
#' @param region_var
#'
#' @importFrom readr read_csv
#'
#' @details Note that this function does not work on a VIZ database, but rather
#'   on the scenario outputs directly.
get_commute_times <- function(dir, year,
                           region_var = c("MPO", "COUNTY", "STATE"),
                           regions = NULL) {

  region <- read_csv(file.path(dir, "outputs", year, "AllZones.csv")) %>%
    filter(STATE == "OR") %>%
    select_("azone" = "Azone", "region_var" = region_var)

  # if no regions specified, then keep all.
  if(!is.null(regions)){
    if(length(regions) == 1){
      region <- region %>%
        filter(region_var == regions)
    } else {
      region <- region %>%
        filter(region_var %in% regions)
    }
  }

  hh <- read_csv(file.path(dir, "outputs", year, "householdData.csv")) %>%
    group_by(hh = HH_ID) %>% slice(1) %>% select(HH_ID, azone = TAZ)


  trips <- read_csv(file.path(dir, "outputs", year, "Trips_SDTPerson.csv"))

  trips %>% filter(tourPurpose %in% c("WORK", "WORK_BASED")) %>%
    select(hh = hhID, time) %>%
    left_join(hh) %>%
    left_join(region) %>%
    group_by(region_var) %>%
    summarise(
      mean = mean(time, na.rm = TRUE),
      median = median(time, na.rm = TRUE)
    )



}

