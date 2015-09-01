#' Extract VMT
#'
#' @export
extract_vmt <- function(db, facet_var = "MPO", facet_levels = NULL){

  # Get lookup table of zones to grouping variable.
  grouping <- tbl(db, "ALLZONES") %>%
    select_("Azone", facet_var) %>%
    rename(AZONE = Azone) %>%
    rename_("facet_var" = facet_var)

  # Link distances
  link_dist <- links %>%
    # reverse link distance
    mutate(
      ANODE = ifelse(order == 2, TONODENO, FROMNODENO),
      BNODE = ifelse(order == 2, FROMNODENO, TONODENO),
      LENGTH = R_LENGTH
    ) %>%
    select(ANODE, BNODE, LENGTH)


  # TODO: group by facility type

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
    mutate(year = as.numeric(TSTEP) + 1990) %>%

    # get link distance from table
    left_join(link_dist) %>%

    # calculate total by year and group
    group_by(year, facet_var, PLANNO) %>%
    summarise(vmt = sum(DAILY_VOL_TOTAL * LENGTH, na.rm = TRUE))

  return(link_vmt)
}

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

  link_vmt <- extract_vmt(db, facet_var, facet_levels)

  ggplot(link_vmt,
         aes(x = year, y = vmt, color = factor(PLANNO))
         ) +
    geom_path() +
    scale_y_log10() +
    facet_wrap(~ facet_var) +
    scale_color_discrete("Facility Type") +
    xlab("Year") + ylab("Vehicle Miles Traveled") +
    theme_bw()

}

#' Compare VMT between scenarios
#'
#' @param db1 The swim database for the "Reference" scenario.
#' @param db2 The swim database for the "Current" scenario.
#' @param facet_var Field to facet by: either "MPO" or "COUNTY".
#' @param facet_levels A character vector of the facet variable specifiying
#'   which levels to include.
#'
#' @return A \code{ggplot2} plot object showing the modeled change in employment
#'   and population over time.
#'
#' @export
#'
#' @import ggplot2
#' @import tidyr
#' @import dplyr
compare_vmt <- function(db1, db2, facet_var = c("MPO", "COUNTY"),
                          facet_levels = NULL){


  vmtref <- extract_vmt(db1, facet_var, facet_levels)  %>%
    rename(ref = vmt)
  vmtcom <- extract_vmt(db2, facet_var, facet_levels)  %>%
    rename(com = vmt)

  df <- left_join(vmtref, vmtcom) %>%
    mutate(diff = (com - ref) / ref * 100)

  ggplot(df,
         aes(x = year, y = diff, color = factor(PLANNO))) +
    geom_path() +
    facet_wrap(~facet_var) +
    scale_fill_discrete("Facility Type") +
    xlab("Year") + ylab("Percent difference in VMT.") +
    theme_bw()
}


#' Extract congestion
#'
#'
#'
extract_cong <- function(db, facet_var = "MPO", facet_levels = NULL,
                         congested_voc = 0.9){

  # Get lookup table of zones to grouping variable.
  grouping <- tbl(db, "ALLZONES") %>%
    select_("Azone", facet_var) %>%
    rename(AZONE = Azone) %>%
    rename_("facet_var" = facet_var)

  # If no levels are specified, show all but external stations.
  if(is.null(facet_levels)){
    a <- grouping %>%
      collect() %>%
      filter(facet_var != "EXTSTA")

    facet_levels = names(table(a$facet_var))
  }

  link_con <- tbl(db, "LINK_DATA") %>%
    select(AZONE, PLANNO, TSTEP, PM_VOL_TOTAL, TEMP_CAPACITY) %>%
    left_join(grouping, by = "AZONE") %>%

    # filter out regions you don't want
    filter(facet_var %in% facet_levels) %>%

    filter(TEMP_CAPACITY > 0) %>%
    collect() %>%  #need to collect sooner because of ifelse below.

    # Calculate the percent of congested links
    mutate(
      congested = ifelse(PM_VOL_TOTAL/TEMP_CAPACITY > congested_voc,
                         1, 0),
      year = as.numeric(TSTEP) + 1990
    ) %>%
    group_by(year, facet_var, PLANNO) %>%
    summarise(percent_congested = (sum(congested) / n()) * 100)
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
plot_pct_cong <- function(db, facet_var = "MPO", facet_levels = NULL,
                          congested_voc = 0.9){


  links_con <- extract_cong(db, facet_var, facet_levels, congested_voc)

  ggplot(
    links_con,
    aes(x = year, y = percent_congested, color = factor(PLANNO))
  ) +
    geom_path() +
    scale_color_discrete("Facility Type") +
    xlab("Year") + ylab("Percent Congested Links") +
    facet_wrap(~ facet_var) +
    theme_bw()

}

#' Plot percent congested links over time.
#'
#' This function plots the percent of congested links, colored and
#' faceted by arbitrary variables in the link table.
#'
#' @param db1 The swim database for the "Reference" scenario.
#' @param db2 The swim database for the "Current" scenario.
#' @param facet_var The variable to use in faceting the data.
#' @param facet_levels A character vector giving the levels of the faceting
#'   variable to use. Levels not called are dropped from the plot; default is
#'   \code{NULL}, meaning print all level.
#'
#' @return A ggplot2 figure object.
#'
#' @export
#' @import dplyr ggplot2
#'
compare_pct_cong <- function(db1, db2, facet_var = "MPO", facet_levels = NULL,
                             congested_voc = 0.9){


  conref <- extract_cong(db1, facet_var, facet_levels)  %>%
    rename(ref = percent_congested)
  concom <- extract_cong(db2, facet_var, facet_levels)  %>%
    rename(com = percent_congested)

  df <- left_join(conref, concom) %>%
    mutate(diff = (com - ref))


  ggplot(df,
         aes(x = year, y = diff, fill = factor(PLANNO))) +
    geom_path() +
    facet_wrap(~ facet_var) +
    scale_fill_discrete("Facility Type") +
    xlab("Year") + ylab("Difference in percent of congested links.") +
    theme_bw()

}

#' Extract VHT
#' @param db The scenario database.
#' @param facet_var The variable to use in faceting the data.
#' @param facet_levels A character vector giving the levels of the faceting
#'   variable to use. Levels not called are dropped from the plot; default is
#'   \code{NULL}, meaning print all level.
#'
#' @return A data frame with the
#'
#' @export
#'
#' @import dplyr tidyr
#'
extract_vht <- function(db, facet_var, facet_levels){

  # Get lookup table of zones to grouping variable.
  grouping <- tbl(db, "ALLZONES") %>%
    select_("Azone", facet_var) %>%
    rename(AZONE = Azone) %>%
    rename_("facet_var" = facet_var)

  # If no levels are specified, show all but external stations.
  if(is.null(facet_levels)){
    a <- grouping %>%
      collect() %>%
      filter(facet_var != "EXTSTA")

    facet_levels = names(table(a$facet_var))
  }

  links <- tbl(db, "LINK_DATA") %>%
    select(AZONE, TSTEP, DAILY_TIME_AUTO, PLANNO) %>%
    left_join(grouping, by = "AZONE") %>%

    # filter out regions you don't want
    filter(facet_var %in% facet_levels) %>%

    # calculate total by year and group
    group_by(TSTEP, PLANNO, facet_var) %>%
    summarise(vht = sum(DAILY_TIME_AUTO)) %>%

    # bring it locally
    collect() %>%
    ungroup() %>%
    mutate(year = as.numeric(TSTEP) + 1990)
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

  links <- extract_vht(db, facet_var, facet_levels)

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

#' Compare VHT between scenarios
#'
#' @param db1 The swim database for the "Reference" scenario.
#' @param db2 The swim database for the "Current" scenario.
#' @param facet_var Field to facet by: either "MPO" or "COUNTY".
#' @param facet_levels A character vector of the facet variable specifiying
#'   which levels to include.
#'
#' @return A ggplot2 object showing VHT by facility type in each facet level
#'   over time.
#'
#' @export
#'
#' @import ggplot2
#' @import tidyr
#' @import dplyr
compare_vht <- function(db1, db2, facet_var = c("MPO", "COUNTY"),
                        facet_levels = NULL){


  vhtref <- extract_vht(db1, facet_var, facet_levels)  %>%
    rename(ref = vht)
  vhtcom <- extract_vht(db2, facet_var, facet_levels)  %>%
    rename(com = vht)

  df <- left_join(vhtref, vhtcom) %>%
    mutate(diff = (com - ref) / ref * 100)

  ggplot(df,
         aes(x = year, y = diff, color = factor(PLANNO))) +
    geom_path() +
    facet_wrap(~facet_var) +
    scale_fill_discrete("Facility Type") +
    xlab("Year") + ylab("Percent difference in VHT.") +
    theme_bw()
}

