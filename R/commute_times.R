#' Calculate trip times by residence region
#'
#' @param dir Scenario directory
#' @param year Analysis year, e.g. \code{t20}.
#' @param region_var One of \code{c("MPO", "COUNTY", "STATE")}
#' @param regions A list of regions to include; defaults to \code{NULL} (all).
#'
#' @details Note that this function does not work on a VIZ database, but rather
#'   on the scenario outputs directly.
get_commute_times <- function(dir, year,
                           region_var = c("MPO", "COUNTY", "STATE"),
                           regions = NULL) {

  region <- read_csv(file.path(dir, "outputs", year, "AllZones.csv")) %>%
    dplyr::filter(STATE == "OR") %>%
    dplyr::select_("azone" = "Azone", "region_var" = region_var)

  # if no regions specified, then keep all.
  if(!is.null(regions)){
    if(length(regions) == 1){
      region <- region %>%
        dplyr::filter(region_var == regions)
    } else {
      region <- region %>%
        dplyr::filter(region_var %in% regions)
    }
  }

  hh <- read_csv(file.path(dir, "outputs", year, "householdData.csv")) %>%
    dplyr::group_by(hh = HH_ID) %>% dplyr::slice(1) %>% dplyr::select(HH_ID, azone = TAZ)


  trips <- read_csv(file.path(dir, "outputs", year, "Trips_SDTPerson.csv"))

  trips %>% dplyr::filter(tourPurpose %in% c("WORK", "WORK_BASED")) %>%
    dplyr::select(hh = hhID, time) %>%
    dplyr::left_join(hh) %>%
    dplyr::left_join(region) %>%
    dplyr::group_by(region_var) %>%
    dplyr::summarize(
      mean = mean(time, na.rm = TRUE),
      median = stats::median(time, na.rm = TRUE)
    )



}
