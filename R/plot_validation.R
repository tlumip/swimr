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
    left_join(ref_counts,
              by = c("ANODE" = "FROMNODENO", "BNODE" = "TONODENO")) %>%

    # compute percent error
    filter(!is.na(AADT)) %>%
    mutate(
      error = DAILY_VOL_TOTAL - AADT,
      pct_error = abs(error) / AADT
    )

  p <- ggplot(link_vols,
              aes(x = AADT, y = DAILY_VOL_TOTAL)) +
    geom_point() +
    geom_smooth(method = "lm") +
    geom_abline(aes(intercept = 0, slope = 1), alpha = 0.5) +

    ylab("Assigned Volume") +
    xlab("Observed AADT") +
    theme_bw()

  p
}



