#' Extract VMT
#'
#' @param db The scenario database.
#' @param facet_var The variable to use in facetting the data.
#' @param facet_levels A character vector giving the levels of the facet
#'   variable to use. Levels not called are dropped from the plot; default is
#'   \code{NULL}, meaning print all levels.
#' @param index Whether to show the variables as indexed against the base year.
#'
#'
#'
#' @return A data frame with VMT summarized by facility type and facet variable.
#'
#' @export
extract_vmt <- function(db, facet_var = "MPO", facet_levels = NULL, index = FALSE){

  # Get lookup table of zones to grouping variable.
  grouping <- tbl(db, "ALLZONES") %>%
    select_("AZONE" = "Azone", facet_var)

  l <- tbl(db, "LINK_DATA") %>%
    left_join(grouping) %>%
  collect(n = Inf) %>%
    # consolidate facility types
    left_join(fac_types) %>%
    mutate(year = as.numeric(TSTEP) + 1990) %>%

    group_by_(facet_var, "FacType", "year") %>%
    summarise(vmt = sum(LENGTH * DAILY_VOL_TOTAL))


  if(index){
    l <- l %>%
      group_by_(facet_var, "FacType") %>%
      mutate(vmt = calc_index(vmt))
  }

  if(!is.null(facet_levels)){
    crit <- lazyeval::interp(~x %in% facet_levels, x = as.name(facet_var))
    l %>% filter_(.dots = crit)
  } else {
    l
  }

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
#' @param index Whether to show the variables as indexed against the base year.
#'
#' @return A ggplot2 object showing VMT by facility type in each facet level
#'   over time.
#'
#' @export
plot_vmt <- function(db, facet_var = "MPO", facet_levels = NULL, index = TRUE){

  link_vmt <- extract_vmt(db, facet_var, facet_levels, index)

  ggplot(link_vmt,
    aes(x = year, y = vmt, color = factor(FacType))
  ) +
    geom_path() +
    facet_wrap(as.formula(paste("~", facet_var))) +
    scale_color_discrete("Facility Type") +
    xlab("Year") + ylab(ifelse(index, "VMT Indexed to Base Year",
                               "Vehicle Miles Traveled")) +
    theme_bw() + theme(axis.text.x = element_text(angle = 30))

}

#' Compare VMT between scenarios
#'
#' @param db1 The swim database for the "Reference" scenario.
#' @param db2 The swim database for the "Current" scenario.
#' @param facet_var Field to facet by: either "MPO" or "COUNTY".
#' @param facet_levels A character vector of the facet variable specifiying
#'   which levels to include.
#' @param index Whether to show the variables as indexed against the base year.
#'
#' @return A ggplot2 object showing VMT by facility type in each facet level
#'   over time.
#'
#' @export
compare_vmt <- function(db1, db2, facet_var = c("MPO", "COUNTY"),
                          facet_levels = NULL, index = TRUE){


  vmtref <- extract_vmt(db1, facet_var, facet_levels, index)  %>%
    rename(ref = vmt)
  vmtcom <- extract_vmt(db2, facet_var, facet_levels, index)  %>%
    rename(com = vmt)

  df <- left_join(vmtref, vmtcom) %>%
    mutate(diff = (com - ref) / ref * 100)

  ggplot(df,
         aes(x = year, y = diff, color = factor(FacType))) +
    geom_path() +
    facet_wrap(as.formula(paste("~", facet_var))) +
    scale_color_discrete("Facility Type") +
    xlab("Year") + ylab("Percent difference in VMT.") +
    theme_bw() + theme(axis.text.x = element_text(angle = 30))
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
extract_vht <- function(db, facet_var, facet_levels = NULL){

  # Get lookup table of zones to grouping variable.
  grouping <- tbl(db, "ALLZONES") %>%
    select_("Azone", facet_var) %>%
    rename(AZONE = Azone) %>%
    rename_("facet_var" = facet_var)

  # If no levels are specified, show all but external stations.
  if(is.null(facet_levels)){
    a <- grouping %>%
      collect(n=Inf) %>%
      filter(facet_var != "EXTSTA")

    facet_levels = names(table(a$facet_var))
  }

  links <- tbl(db, "LINK_DATA") %>%
    select(AZONE, TSTEP, DAILY_TIME_AUTO, DAILY_VOL_AUTO, PLANNO) %>%
    left_join(grouping, by = "AZONE") %>%

    # filter out regions you don't want
    filter(facet_var %in% facet_levels) %>%
    # bring it locally
    collect(n=Inf) %>%

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
    theme_bw() + theme(axis.text.x = element_text(angle = 30))

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
    theme_bw() + theme(axis.text.x = element_text(angle = 30))
}

#' Compare VMT between multiple scenarios
#'
#' @param dbset A list of connections to SWIM databases.
#' @param db_names A character vector naming the scenarios.
#' @param facet_var The region to summarize by.
#' @param facet_levels Regions to include in summary.
#'
#' @return A ggplot2 object showing VHT by facility type in each facet level
#'   over time.
#'
#' @export
multiple_vmt <- function(dbset, db_names,
                           facet_var = c("MPO", "COUNTY", "STATE"),
                           facet_levels = NULL ){

  # get the trips table for every scenario.
  names(dbset) <- db_names
  df <- rbind_all(
    lapply(seq_along(dbset), function(i)
      extract_vmt(dbset[[i]], facet_var, facet_levels) %>%
        mutate(scenario = names(dbset)[[i]])
    )
  )

  p <- ggplot(
    df,
    aes(x = year, y = vmt, color = scenario)
  )

  p + geom_path() +
    facet_grid(facet_var ~ FacType, scales = "free_y") +
    ylab("VMT") + xlab("Year") +
    theme_bw() + theme(axis.text.x = element_text(angle = 30))

}
