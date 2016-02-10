#' Plot base-year traffic validation.
#'
#' Create a plot of the base scenario link volumes compared with observed AADT.
#'
#' @param db The scenario database.
#' @return A ggplot2 object.
#'
#' @export
plot_validation <- function(db){


  # get base year link traffic volumes
  link_vols <- tbl(db, "LINK_DATA") %>%
    select(ANODE, BNODE, TSTEP, DAILY_VOL_TOTAL) %>%
    filter(TSTEP == "23") %>%

    collect() %>%
    mutate(
      ANODE = as.character(ANODE),
      BNODE = as.character(BNODE)
    )  %>%

    # Join count locations
    left_join(ref_counts %>% filter(year == 2010),
              by = c("ANODE" = "FROMNODENO", "BNODE" = "TONODENO")) %>%

    # compute percent error
    filter(!is.na(aawdt)) %>%
    mutate(
      error = DAILY_VOL_TOTAL - aawdt,
      pct_error = abs(error) / aawdt
    )

  p <- ggplot(link_vols,
              aes(x = aawdt, y = DAILY_VOL_TOTAL)) +
    geom_point() +
    geom_smooth(method = "lm") +
    geom_abline(aes(intercept = 0, slope = 1), alpha = 0.5) +

    ylab("Assigned Volume") +
    xlab("Observed AAWDT") +
    theme_bw() + theme(axis.text.x = element_text(angle = 30))

  p
}


#' Plot historical and projected traffic counts
#'
#' @param db The scenario database.
#' @param atr A numeric vector indicating the traffic recorders to plot track.
#' @return A ggplot2 object
#'
#' @export
#'
plot_traffic_count <- function(db, atr = c(01001, 01011, 01012)){

  # protect type
  atr <- as.numeric(atr)

  pc <- ref_counts %>%
    filter(site %in% atr) %>%
    mutate(
      data = "ODOT Count",
      year = as.numeric(year)
    )

  # get base year link traffic volumes
  link_vols <- tbl(db, "LINK_DATA") %>%
    select(ANODE, BNODE, TSTEP, DAILY_VOL_TOTAL) %>%

    collect() %>%
    mutate(
      ANODE = as.character(ANODE),
      BNODE = as.character(BNODE)
    )  %>%

    # Join count locations
    inner_join(
      pc %>% filter(year == 2010) %>% select(site, FROMNODENO, TONODENO),
      by = c("ANODE" = "FROMNODENO", "BNODE" = "TONODENO")
    ) %>%

    transmute(
      site,
      year = as.numeric(TSTEP) + 1990,
      aawdt = DAILY_VOL_TOTAL,
      data = "SWIM"
    )

  d <- bind_rows(pc, link_vols)


  p <- ggplot(
    d,
    aes(x = year, y = aawdt, color = factor(site), lty = data) ) +
    geom_path()

  p +
    xlab("Year") + ylab("AAWDT") +
    scale_color_discrete("ATR Location") +
    scale_linetype("Source") +
    theme_bw()

}


