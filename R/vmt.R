#' Extract VMT
#'
#' @param db The scenario database.
#' @param facet_var The variable to use in facetting the data.
#' @param facet_levels A character vector giving the levels of the facet
#'   variable to use. Levels not called are dropped from the plot; default is
#'   \code{NULL}, meaning print all levels.
#'
#' @return A data frame with VMT summarized by facility type and facet variable.
#' @import dplyr dplyrExtras
#'
#' @export
extract_vmt <- function(db, facet_var = "MPO", facet_levels = NULL){

  # Get lookup table of zones to grouping variable.
  grouping <- tbl(db, "ALLZONES") %>%
    select_("Azone", facet_var) %>%
    rename(AZONE = Azone) %>%
    rename_("facet_var" = facet_var)

  # Link distances
  data("links", package = "swimr")
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


    # consolidate facility types
    left_join(fac_types) %>%

    # calculate total by year and group
    group_by(year, facet_var, FacType) %>%
    summarise(vmt = sum(DAILY_VOL_TOTAL * LENGTH, na.rm = TRUE))

  return(link_vmt)
}

#' Plot VMT over time.
#'
#' This function plots the sum of the daily link volumes over time, colored and
#' faceted by arbitrary variables in the link table.
#'
#' @param db The scenario database.
#' @param facet_var The variable to use in facetting the data.
#' @param facet_levels A character vector giving the levels of the facet
#'   variable to use. Levels not called are dropped from the plot; default is
#'   \code{NULL}, meaning print all levels.
#'
#' @return A ggplot2 object showing VMT by facility type in each facet level
#'   over time.
#'
#' @import dplyr ggplot2
#'
#'
#' @export
plot_vmt <- function(db, facet_var = "MPO", facet_levels = NULL){

  link_vmt <- extract_vmt(db, facet_var, facet_levels)

  ggplot(link_vmt,
         aes(x = year, y = vmt, color = factor(FacType))
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
#' @return A ggplot2 object showing VMT by facility type in each facet level
#'   over time.
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
         aes(x = year, y = diff, color = factor(FacType))) +
    geom_path() +
    facet_wrap(~facet_var) +
    scale_color_discrete("Facility Type") +
    xlab("Year") + ylab("Percent difference in VMT.") +
    theme_bw()
}


#' Extract congestion
#'
#' @param db The scenario database.
#' @param facet_var Field to facet by: either "MPO" or "COUNTY".
#' @param facet_levels A character vector of the facet variable specifiying
#'   which levels to include.
#' @param congested_voc the volume to capacity ratio considered as "congested."
#'   Default value is \code{0.9}.
#'
#' @return A data frame with the percent of congested links by facet level and
#'   facility type in each year.
#'
#' @import dplyr tidyr
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

  # Link distances
  data("links", package = "swimr")
  link_dist <- links %>%
    # reverse link distance
    mutate(
      ANODE = ifelse(order == 2, TONODENO, FROMNODENO),
      BNODE = ifelse(order == 2, FROMNODENO, TONODENO),
      LENGTH = R_LENGTH
    ) %>%
    select(ANODE, BNODE, LENGTH)

  link_con <- tbl(db, "LINK_DATA") %>%
    select(AZONE, ANODE, BNODE, PLANNO, TSTEP, PM_VOL_TOTAL, TEMP_CAPACITY) %>%
    left_join(grouping, by = "AZONE") %>%

    # filter out regions you don't want
    filter(facet_var %in% facet_levels) %>%

    filter(TEMP_CAPACITY > 0) %>%
    collect() %>%  #need to collect sooner because of ifelse below.

    # get link distance from table
    left_join(link_dist) %>%

    # Calculate the percent of congested links
    mutate(
      # is the link congested?
      congested = ifelse(PM_VOL_TOTAL/TEMP_CAPACITY > congested_voc, 1, 0),
      # get period vmt
      vmt = PM_VOL_TOTAL * LENGTH,
      congested_vmt = congested * vmt,
      year = as.numeric(TSTEP) + 1990
    ) %>%

    # consolidate facility types
    left_join(fac_types) %>%

    group_by(year, facet_var, FacType) %>%
    summarise(percent_congested = (sum(congested_vmt) / sum(vmt)) * 100)
}

#' Plot percent congested links over time.
#'
#' This function plots the percent of PM VMT traveling on congested links.,
#' faceted by arbitrary variables in the link table.
#'
#' @param db The scenario database.
#' @param facet_var The variable to use in faceting the data.
#' @param facet_levels A character vector giving the levels of the faceting
#'   variable to use. Levels not called are dropped from the plot; default is
#'   \code{NULL}, meaning print all level.
#' @param congested_voc the volume to capacity ratio considered as "congested."
#'   Default value is \code{0.9}.
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
    aes(x = year, y = percent_congested, color = factor(FacType))
  ) +
    geom_path() +
    scale_color_discrete("Facility Type") +
    xlab("Year") + ylab("Percent of PM VMT on Congested Links") +
    facet_wrap(~ facet_var) +
    theme_bw()

}

#' Plot percent congested links over time.
#'
#' This function plots the percent of PM VMT travelling on congested links,
#' colored and faceted by arbitrary variables in the link table.
#'
#' @param db1 The swim database for the "Reference" scenario.
#' @param db2 The swim database for the "Current" scenario.
#' @param facet_var The variable to use in faceting the data.
#' @param facet_levels A character vector giving the levels of the faceting
#'   variable to use. Levels not called are dropped from the plot; default is
#'   \code{NULL}, meaning print all level.
#' @param congested_voc the volume to capacity ratio considered as "congested."
#'   Default value is \code{0.9}.
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
         aes(x = year, y = diff, color = factor(FacType))) +
    geom_path() +
    facet_wrap(~ facet_var) +
    scale_color_discrete("Facility Type") +
    xlab("Year") + ylab("Difference in Percent of PM VMT on Congested Links.") +
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
extract_vht <- function(db, facet_var, facet_levels = NULL){

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
    select(AZONE, TSTEP, DAILY_TIME_AUTO, DAILY_VOL_AUTO, PLANNO) %>%
    left_join(grouping, by = "AZONE") %>%

    # filter out regions you don't want
    filter(facet_var %in% facet_levels) %>%
    # bring it locally
    collect() %>%

    # consolidate facility types
    left_join(fac_types) %>%

    # calculate total by year and group
    group_by(TSTEP, facet_var, FacType) %>%
    summarise(vht = sum(DAILY_TIME_AUTO * DAILY_VOL_AUTO)) %>%

    ungroup() %>%
    mutate(year = as.numeric(TSTEP) + 1990)
}

#' Plot VHT over time.
#'
#' This function plots the sum of the daily vehicle travel time, colored and
#' faceted by arbitrary variables in the link or zone table.
#'
#' @param db The scenario database.
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
plot_vht <- function(db, facet_var = "MPO", facet_levels = NULL){

  df <- extract_vht(db, facet_var, facet_levels)

  p <- ggplot(
    df,
    aes(x = year, y = vht, color = factor(FacType))
  ) +
    geom_path() +
    facet_wrap(~ facet_var) +
    scale_y_log10() +
    scale_color_discrete("Facility Type")

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
         aes(x = year, y = diff, color = factor(FacType))) +
    geom_path() +
    facet_wrap(~facet_var) +
    scale_color_discrete("Facility Type") +
    xlab("Year") + ylab("Percent difference in VHT.") +
    theme_bw()
}

