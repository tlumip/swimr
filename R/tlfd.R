#' Extract trip length frequency distribution data
#'
#' @param db The scenario database.
#' @param region_var The region to aggregate to.
#' @param regions The regions to return.  If \code{NULL}, returns all.
#' @param bin_width The width of bins to use in the trip length frequency
#'   distribution. Defaults to 1.
#' @param max_bin The maximum bin size to return.
#'
#' @return A data frame with the trip length frequency distribution by year
#'   and region.
#'
#' @importFrom plyr round_any
#' @export
#'
extract_tlfd <- function(db,
                         region_var = c("MPO", "COUNTY", "STATE"),
                         regions = NULL,
                         bin_width = 1, max_bin = 100){

  # get travel distance skim between zones
  skim <- tbl(db, "SKIM") %>%
    left_join(
      tbl(db, "BZONE") %>% select_("FROMBZONE" = "BZONE", region_var)
    ) %>%
    select_(
      region_var, "FROMBZONE", "TOBZONE", "TSTEP",
      "pk_dist" = "pkautodist",  "op_dist" = "opautodist"
    )

  # if no regions specified, then keep all.
  if(!is.null(regions)){
    if(length(regions) == 1){
      skim <- skim %>%
        mutate_("region" = region_var) %>%
        filter(region == regions)
    } else {
      skim <- skim %>%
        mutate_("region" = region_var) %>%
        filter(region %in% regions)
    }
  }

  # get number of auto trips between zones
  trips <- tbl(db, "TRIPMATRIX") %>%
    transmute(
      FROMBZONE, TOBZONE, TSTEP,
      pk_auto =
        am_DA + am_SR2 + am_SR3P +
        pm_DA + pm_SR2 + pm_SR3P,
      op_auto =
        md_DA + md_SR2 + md_SR3P +
        nt_DA + nt_SR2 + nt_SR3P
    )

  tlfd <- inner_join(skim, trips) %>%
    collect() %>%

    # bin distances
    mutate(
      pk_dist = plyr::round_any(pk_dist, bin_width, floor),
      pk_dist = pmin(pk_dist, max_bin),
      op_dist = plyr::round_any(op_dist, bin_width, floor),
      op_dist = pmin(op_dist, max_bin)
    ) %>%

    # reshape data
    gather(var, value, pk_dist, op_dist, pk_auto, op_auto, convert = TRUE) %>%
    separate(var, c("period", "var")) %>%
    spread(var, value) %>%

    # calculate number of autos in each distance bin in each year
    group_by_(region_var, "TSTEP", "dist") %>%
    summarize(auto = sum(auto)) %>%

    # get frequency
    group_by_(region_var, "TSTEP") %>%
    mutate(
      freq = auto / sum(auto),
      year = as.numeric(TSTEP) + 1990
    )


  return(tlfd)

}

