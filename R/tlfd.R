#' Extract trip length frequency distribution data
#'
#' @param db The scenario database.
#' @param region_var The region to aggregate to.
#' @param regions The regions to return.  If \code{NULL}, returns all.
#' @param years The years to show in the plot. If null, will show the first
#' @param max_bin The maximum bin size to return.
#'
#' @return A data frame with the trip length frequency distribution by year
#'   and region.
#'
#' @importFrom plyr round_any
#' @export
extract_tlfd <- function(db,
                         region_var = c("MPO", "COUNTY", "STATE"),
                         regions = NULL, cumulative = FALSE, years = NULL){

  region <- tbl(db, "ALLZONES") %>%
    select_("azone" = "Azone", "region_var" = region_var)

  # if no regions specified, then keep all.
  if(!is.null(regions)){
    region <- region %>% filter(region_var %in% regions)
  }

  # get TLFD
  tlfd <- tbl(db, "TLFD_SDT") %>%
    inner_join(region) %>%
    group_by(region_var, TSTEP, distance) %>%
    summarise(trips = sum(trips)) %>%
    collect(n=Inf) %>%

    mutate(
      freq = trips / sum(trips),
      cum_freq = cumsum(freq),
      year = as.numeric(TSTEP) + 1990
    ) %>%
    ungroup() %>%
    select(-TSTEP)

  # filter to the years you want.
  if(is.null(years)){
    tlfd <- tlfd %>% filter(year == min(year) | year == max(year))
  } else {
    tlfd <- tlfd %>% filter(year %in% years)
  }

  # return cumulative if wanted
  if(cumulative){
    tlfd <- tlfd %>% mutate(freq = cum_freq)
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
    ggplot(tlfd,
         aes(x = distance, y = freq, group = factor(year), color = factor(year))) +
    geom_line() +
    scale_color_discrete("Year") +
    facet_wrap(~ region_var) +
    ylab("Frequency") + xlab("Miles") +
    theme_bw() + theme(axis.text.x = element_text(angle = 30))

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
    select_("region_var", "distance", "year", "ref" = freq)
  tlfd2 <- extract_tlfd(db2, region_var, regions, cumulative, years) %>%
    select_("region_var", "distance", "year", "com" = freq)


  df <- left_join(tlfd1, tlfd2, by = c("region_var", "year", "distance")) %>%
    mutate(diff = com - ref)


  p <- ggplot(
    df,
    aes(x = distance, y = diff, group = factor(year), color = factor(year))
  ) +
    geom_line() +
    scale_color_discrete("Year") +
    facet_wrap(~ region_var) +
    ylab(ifelse(cumulative, "Difference in Cumulative Frequency (ref - cur)",
                "Difference in Frequency")) + xlab("Miles") +
    theme_bw() + theme(axis.text.x = element_text(angle = 30))

  return(p)

}
