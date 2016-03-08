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
extract_tlfd <- function(db,
                         region_var = c("MPO", "COUNTY", "STATE"),
                         regions = NULL){

  region <- tbl(db, "ALLZONES") %>%
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

  # get TLFD
  tlfd <- tbl(db, "TLFD_SDT") %>%
    left_join(region) %>%
    group_by(region_var, TSTEP, distance) %>%
    summarise(trips = sum(trips)) %>%
    collect() %>%

    mutate(
      freq = trips / sum(trips),
      cum_freq = cumsum(freq),
      year = as.numeric(TSTEP) + 1990
    ) %>%
    ungroup() %>%
    select(-TSTEP)


  return(tlfd)

}


#' Plot a trip length frequency distribution
#'
#' @param db The scenario database.
#' @param region_var The region to aggregate to.
#' @param regions The regions to return.  If \code{NULL}, returns all.
#' @param cumulative Plot cumulative instead of probability density distribution.
#'
#' @return A ggplot2 object.
#'
#' @export
plot_tlfd <- function(db,
                      region_var = c("MPO", "COUNTY", "STATE"),
                      regions = NULL, cumulative = FALSE){

  tlfd <- extract_tlfd(db, region_var, regions) %>%
    filter(year == min(year) | year == max(year))

  if(cumulative){
    tlfd <- tlfd %>% mutate(freq = cum_freq)
  }

  p <- ggplot(tlfd,
         aes(x = distance, y = freq, group = year, color = year)) +
    geom_line(alpha = 0.5) +
    scale_color_gradient(low = "red", high = "blue") +
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
                         regions = NULL, cumulative = FALSE){

  tlfd1 <- extract_tlfd(db1, region_var, regions)
  tlfd2 <- extract_tlfd(db2, region_var, regions)

  if(cumulative){
    tlfd1 <- tlfd1 %>% mutate(ref = cum_freq)
    tlfd2 <- tlfd2 %>% mutate(com = cum_freq)
  } else {
    tlfd1 <- tlfd1 %>% mutate(ref = freq)
    tlfd2 <- tlfd2 %>% mutate(com = freq)
  }

  df <- left_join(tlfd1, tlfd2) %>%
    mutate(
      diff = com - ref
    ) %>%
    filter(year == min(year) | year == max(year))


  p <- ggplot(df,
              aes(x = distance, y = diff, group = year, color = year)) +
    geom_line(alpha = 0.5) +
    scale_color_gradient(low = "red", high = "blue") +
    facet_wrap(~ region_var) +
    ylab("Difference in Frequency (reference - current)") + xlab("Miles") +
    theme_bw() + theme(axis.text.x = element_text(angle = 30))

  return(p)

}
