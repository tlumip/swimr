#' Plot base-year traffic validation.
#'
#'
plot_validation <- function(db, facet_var = "MPO", facet_levels = NULL){

  grouping <- tbl(db, "ALLZONES") %>%
    select_("Azone", facet_var) %>%
    rename(AZONE = Azone) %>%
    rename_("facet_var" = facet_var)

  # get base year link traffic volumes
  link_vols <- tbl(db, "LINK_DATA") %>%
    select(AZONE, FROMNODENO, TONODENO, TSTEP, DAILY_VOL_TOTAL) %>%
    filter(TSTEP == "23") %>%

    # Join facet_var
    left_join(grouping, by = "AZONE") %>%
    collect() %>%
    mutate(
      FROMNODENO = as.character(FROMNODENO),
      TONODENO = as.character(TONODENO)
    )  %>%

    # Join count locations
    right_join(ref_counts, by = c("FROMNODENO", "TONODENO")) %>%

    # compute percent error
    mutate(
      error = DAILY_VOL_TOTAL - AADT,
      pct_error = abs(error) / AADT
    )

  p <- ggplot(link_vols,
              aes(x = AADT, y = DAILY_VOL_TOTAL)) +
    geom_point(shape = 21, aes(fill = cut_error(pct_error))) +
    geom_smooth() +
    geom_abline(aes(intercept = 0, slope = 1), alpha = 0.5) +
    scale_y_log10() +
    scale_x_log10() +
    scale_fill_brewer(type = "seq")

  p + facet_wrap(~ facet_var)
}



#' Cut percent error into ranges
#'
#' @param x a vector of error measurements
#' @return a factor showing the bin
#'
cut_error <- function(x){
  brks <- c(0.05, 0.1, 0.2, 0.5, 1)
  cut(x, breaks = c(0, brks, Inf))
}
