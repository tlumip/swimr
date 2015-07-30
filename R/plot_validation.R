#' Plot base-year traffic validation.
#'
#' Create a plot of the base scenario link volumes compared with observed AADT.
#'
#' @param db The scenario database.
#' @return A ggplot2 object.
#'
plot_validation <- function(db){


  # get base year link traffic volumes
  link_vols <- tbl(db, "LINK_DATA") %>%
    select(FROMNODENO, TONODENO, TSTEP, DAILY_VOL_TOTAL) %>%
    filter(TSTEP == "23") %>%

    collect() %>%
    mutate(
      FROMNODENO = as.character(FROMNODENO),
      TONODENO = as.character(TONODENO)
    )  %>%

    # Join count locations
    left_join(ref_counts, by = c("FROMNODENO", "TONODENO")) %>%

    # compute percent error
    mutate(
      error = DAILY_VOL_TOTAL - AADT,
      pct_error = abs(error) / AADT
    )

  p <- ggplot(link_vols,
              aes(x = AADT, y = DAILY_VOL_TOTAL)) +
    geom_point() +
    geom_smooth() +
    geom_abline(aes(intercept = 0, slope = 1), alpha = 0.5) +
    scale_y_log10() +
    scale_x_log10() +

    ylab("Assigned Volume") +
    xlab("Observed AADT") +
    theme_bw()

  p
}



