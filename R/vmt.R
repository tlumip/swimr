#' Plot VMT over time.
#'
#' This function plots the sum of the daily link volumes over time, colored and
#' faceted by arbitrary variables in the link table.
#'
#' @param db The scenario database.
#' @param color_var The variable to use in coloring the data.
#' @param color_levels A character vector giving the levels of the coloring
#'   variable to use. Levels not called are dropped from the plot; default is
#'   \code{NULL}, meaning print all level.
#'
#' @return A ggplot2 figure object.
#'
#' @import dplyr ggplot2 dplyrExtras
#'
#'
#' @export
plot_vmt <- function(db, facet_var = "MPO", facet_levels = NULL){

  # Get lookup table of zones to grouping variable.
  grouping <- tbl(db, "ALLZONES") %>%
    select_("Azone", facet_var) %>%
    rename(AZONE = Azone) %>%
    rename_("facet_var" = facet_var)

  # Link distances
  link_dist <- links %>%
    # reverse link distance
    mutate_if(
      order == 2,
      hold = FROMNODENO,
      FROMNODENO = TONODENO,
      TONODENO = hold,
      LENGTH = R_LENGTH
    ) %>%
    select(FROMNODENO, TONODENO, LENGTH)


  # If no levels are specified, show all but external stations.
  if(is.null(facet_levels)){
    a <- grouping %>%
      collect() %>%
      filter(facet_var != "EXTSTA")

    facet_levels = names(table(a$facet_var))
  }

  link_vmt <- tbl(db, "LINK_DATA") %>%
    select(AZONE, ANODE, BNODE, TSTEP, PLANNO, DAILY_VOL_TOTAL) %>%
    left_join(grouping, by = "AZONE") %>%

    # filter out regions you don't want
    filter(facet_var %in% facet_levels) %>%
    collect()  %>%

    # get link distance from table
    left_join(link_dist, by = c("ANODE" = "FROMNODENO", "BNODE" = "TONODENO")) %>%

    # calculate total by year and group
    group_by(TSTEP, facet_var, PLANNO) %>%
    summarise(vmt = sum(DAILY_VOL_TOTAL * LENGTH, na.rm = TRUE)) %>%

    # bring it locally
    ungroup() %>%
    mutate(TSTEP = as.numeric(TSTEP))

  p <- ggplot(
    link_vmt,
    aes(x = TSTEP + 1990, y = vmt, color = factor(PLANNO))
  ) +
    geom_path() +
    scale_y_log10() +
    facet_wrap(~ facet_var) +
    scale_color_discrete("Facility Type")

  p +
    xlab("Year") + ylab("Vehicle Miles Traveled") +
    theme_bw()

}


#' Plot percent congested links over time.
#'
#' This function plots the percent of congested links, colored and
#' faceted by arbitrary variables in the link table.
#'
#' @param db The scenario database.
#' @param color_var The variable to use in coloring the data.
#' @param color_levels A character vector giving the levels of the coloring
#'   variable to use. Levels not called are dropped from the plot; default is
#'   \code{NULL}, meaning print all level.
#'
#' @return A ggplot2 figure object.
#'
#' @export
#' @import dplyr ggplot2
#'
plot_pct_cong <- function(db, color_var = "MPO", color_levels = NULL,
                          congested_voc = 0.9){

  # Get lookup table of zones to grouping variable.
  grouping <- tbl(db, "ALLZONES") %>%
    select_("Azone", color_var) %>%
    rename(AZONE = Azone) %>%
    rename_("color_var" = color_var)

  # If no levels are specified, show all but external stations.
  if(is.null(color_levels)){
    a <- grouping %>%
      collect() %>%
      filter(color_var != "EXTSTA")

    color_levels = names(table(a$color_var))
  }

  links <- tbl(db, "LINK_DATA") %>%
    select(AZONE, TSTEP, PM_VOL_TOTAL, TEMP_CAPACITY) %>%
    left_join(grouping, by = "AZONE") %>%

    # filter out regions you don't want
    filter(color_var %in% color_levels) %>%

    filter(TEMP_CAPACITY > 0) %>%
    collect() %>%  #need to collect sooner because of ifelse below.

    # Calculate the percent of congested links
    mutate(
      congested = ifelse(PM_VOL_TOTAL/TEMP_CAPACITY > congested_voc,
                         1, 0)
    ) %>%
    group_by(TSTEP, color_var) %>%
    summarise(percent_congested = (sum(congested) / n()) * 100) %>%

    ungroup() %>%
    mutate(TSTEP = as.numeric(TSTEP))

  p <- ggplot(
    links,
    aes(x = TSTEP + 1990, y = percent_congested, color = color_var)
  ) +
    geom_path() +
    scale_color_discrete(color_var)

  p +
    xlab("Year") + ylab("Percent Congested Links") +
    theme_bw()

}



#' Plot VHT over time.
#'
#' This function plots the sum of the daily vehicle travel time, colored and
#' faceted by arbitrary variables in the link or zone table.
#'
#' @param db The scenario database.
#' @param color_var The variable to use in coloring the data.
#' @param color_levels A character vector giving the levels of the coloring
#'   variable to use. Levels not called are dropped from the plot; default is
#'   \code{NULL}, meaning print all level.
#'
#' @return A ggplot2 figure object.
#'
#' @export
#' @import dplyr ggplot2
#'
plot_vht <- function(db, color_var = "MPO", color_levels = NULL){

  # Get lookup table of zones to grouping variable.
  grouping <- tbl(db, "ALLZONES") %>%
    select_("Azone", color_var) %>%
    rename(AZONE = Azone) %>%
    rename_("color_var" = color_var)

  # If no levels are specified, show all but external stations.
  if(is.null(color_levels)){
    a <- grouping %>%
      collect() %>%
      filter(color_var != "EXTSTA")

    color_levels = names(table(a$color_var))
  }

  links <- tbl(db, "LINK_DATA") %>%
    select(AZONE, TSTEP, DAILY_TIME_AUTO) %>%
    left_join(grouping, by = "AZONE") %>%

    # filter out regions you don't want
    filter(color_var %in% color_levels) %>%

    # calculate total by year and group
    group_by(TSTEP, color_var) %>%
    summarise(vht = sum(DAILY_TIME_AUTO)) %>%

    # bring it locally
    collect() %>%
    ungroup() %>%
    mutate(TSTEP = as.numeric(TSTEP))

  p <- ggplot(
    links,
    aes(x = TSTEP + 1990, y = vht, color = color_var)
  ) +
    geom_path() +
    scale_y_log10() +
    scale_color_discrete(color_var)

  p +
    xlab("Year") + ylab("Vehicle Hours Traveled") +
    theme_bw()

}
