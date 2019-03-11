#' Extract trip length frequency distribution data
#'
#' @param db The scenario database.
#' @param region_var The region to aggregate to.
#' @param regions The regions to return.  If \code{NULL}, returns all.
#' @param years The years to show in the plot. If null, will show the first
#' @param max_bin The maximum bin size to return.
#' @param cumulative Return a cumulative PDF?
#'
#' @return A data frame with the trip length frequency distribution by year
#'   and region.
#'
#' @export
extract_tlfd <- function(db,
                         region_var = c("MPO", "COUNTY", "STATE"),
                         regions = NULL, cumulative = FALSE, years = NULL){

  region <- dplyr::tbl(db, "ALLZONES") %>%
    dplyr::select_("azone" = "Azone", "region_var" = region_var)

  # if no regions specified, then keep all.
  if(!is.null(regions)){
    region <- region %>% dplyr::filter(region_var %in% regions)
  }

  # get TLFD
  tlfd <- dplyr::tbl(db, "TLFD_SDT") %>%
    inner_join(region) %>%
    dplyr::group_by(region_var, TSTEP, distance) %>%
    dplyr::summarize(trips = sum(trips, na.rm=TRUE)) %>%
    dplyr::collect(n=Inf) %>%

    dplyr::mutate(
      freq = trips / sum(trips),
      cum_freq = cumsum(freq),
      year = as.numeric(TSTEP) + 1990
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-TSTEP)

  # dplyr::filter to the years you want.
  if(is.null(years)){
    tlfd <- tlfd %>% dplyr::filter(year == min(year) | year == max(year))
  } else {
    tlfd <- tlfd %>% dplyr::filter(year %in% years)
  }

  # return cumulative if wanted
  if(cumulative){
    tlfd <- tlfd %>% dplyr::mutate(freq = cum_freq)
  }


  return(tlfd)

}


#' Plot a trip length frequency distribution
#'
#' @param db The scenario database.
#' @param region_var The region to aggregate to.
#' @param regions The regions to return.  If \code{NULL}, returns all.
#' @param cumulative Plot cumulative instead of probability density distribution.
#' @param years The years to show in the plot. If null, will show the first
#'   and last years.
#'
#' @return A ggplot2 object.
#'
#' @export
plot_tlfd <- function(db,
                      region_var = c("MPO", "COUNTY", "STATE"),
                      regions = NULL, cumulative = FALSE, years = NULL){

  tlfd <- extract_tlfd(db, region_var, regions, cumulative, years)

  p <-
    ggplot2::ggplot(tlfd,
         ggplot2::aes(x = distance, y = freq, group = factor(year), color = factor(year))) +
    geom_line() +
    ggplot2::scale_color_discrete("Year") +
    ggplot2::facet_wrap(~ region_var) +
    ggplot2::ylab("Frequency") + ggplot2::xlab("Miles") +
    ggplot2::theme_bw() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30))

  return(p)

}


#' Compare Trip Length Frequency Distributions
#'
#' @param db1 The swim database for the "Reference" scenario.
#' @param db2 The swim database for the "Current" scenario.
#' @inheritParams plot_tlfd
#'
#' @return A ggplot2 object.
#'
#' @export
compare_tlfd <- function(db1, db2,
                         region_var = c("MPO", "COUNTY", "STATE"),
                         regions = NULL, cumulative = FALSE, years = NULL){
  if(cumulative){
    freq <- "freq"
  } else {
    freq <- "cum_freq"
  }

  tlfd1 <- extract_tlfd(db1, region_var, regions, cumulative, years) %>%
    dplyr::select_("region_var", "distance", "year", "ref" = freq)
  tlfd2 <- extract_tlfd(db2, region_var, regions, cumulative, years) %>%
    dplyr::select_("region_var", "distance", "year", "com" = freq)


  df <- dplyr::left_join(tlfd1, tlfd2, by = c("region_var", "year", "distance")) %>%
    dplyr::mutate(diff = com - ref)


  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = distance, y = diff, group = factor(year), color = factor(year))
  ) +
    geom_line() +
    ggplot2::scale_color_discrete("Year") +
    ggplot2::facet_wrap(~ region_var) +
    ggplot2::ylab(ifelse(cumulative, "Difference in Cumulative Frequency (ref - cur)",
                "Difference in Frequency")) + ggplot2::xlab("Miles") +
    ggplot2::theme_bw() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30))

  return(p)

}
