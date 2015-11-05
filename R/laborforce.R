#' Extract labor force participation
#'
#' BLS defines the civilian labor force participation rate as the percent of
#' individuals age 16 and over who are either working or who are looking for
#' work. This is not something we ask about in travel surveys generally, so
#' it is not possible to make an apples-to-apples comparison.
#'
#' This function returns the number of workers in each facet region divided
#' by the number of individuals fifteen years and older in that region. The
#' definition of a "worker" in a travel model is different than the BLS definition
#' in that travel models do not consider unemployed individuals who are looking
#' for work.
#'
#' We call our definition "workers to adult population ratio," or "WAPR". Again,
#' it corresponds to the labor force participation rate but is not precisely
#' defined.
#'
#' @param db The scenario sqlite database.
#' @param facet_var Field to facet by: "MPO", "COUNTY", or "STATE".
#' @param facet_levels A character vector of the variable specifiying
#'   which levels to include.
#'
#' @return A \code{data_frame} with the participation rate in each facet region
#'   in each transport model year.
#'
#' @export
extract_wapr <- function(db,
                         facet_var = c("BZONE", "MPO", "COUNTY",
                                       "DOT_REGION", "STATE"),
                         facet_levels = NULL, color_levels = NULL) {


  df <- tbl(db, "AZONE") %>%
    transmute(
      AZONE, TSTEP,
      workers = TOTALWORKERS,
      laborforce = PERSON15TO21 + PERSON21TO40 + PERSON40TO60 + PERSON60PLUS
    ) %>%

    # join faceting variables
    left_join(
      tbl(db, "ALLZONES") %>%
        select_("AZONE" = "Azone", facet_var)
    )

    # if no levels specified, then keep all
    if(!is.null(facet_levels)){

      df <- df %>%
        mutate_("facet_var" = facet_var) %>%
        filter(facet_var %in% facet_levels)
    }

  df <- df %>%
    # summarize on faceting variable
    group_by_("TSTEP", facet_var) %>%
    summarise(
      workers = sum(workers),
      laborforce = sum(laborforce)
    ) %>%

    # calculate wapr
    collect() %>%
    mutate(
      wapr = workers / laborforce * 100,
      year = as.numeric(TSTEP) + 1990
    ) %>%
    ungroup() %>%
    select(-TSTEP)

  return(df)

}

#' Plot the labor force participation rate.
#'
#' BLS defines the civilian labor force participation rate as the percent of
#' individuals age 16 and over who are either working or who are looking for
#' work. This is not something we ask about in travel surveys generally, so it is
#' not possible to make an apples-to-apples comparison.
#'
#' This function returns the number of workers in each facet region divided by
#' the number of individuals fifteen years and older in that region. The
#' definition of a "worker" in a travel model is different than the BLS
#' definition in that travel modesl do not consider unemployed individuals who
#' are looking for work.
#'
#' We call our definition "workers to adult population ratio," or "WAPR".
#'
#' @param db The scenario sqlite database.
#' @param color_var Field to color by: c("BZONE", "MPO", "COUNTY", "DOT_REGION",
#'   "STATE")
#' @param color_levels A character vector of the variable specifiying
#'   which levels to include.
#' @param facet_var Field to facet by: c("MPO", "COUNTY", "DOT_REGION",
#'   "STATE")
#' @param facet_levels A character vector of the variable specifiying
#'   which levels to include.
#'
#' @return a ggplot2 plot object.
#'
#' @export
plot_wapr <- function(db,
                      color_var = c("BZONE", "MPO", "COUNTY", "DOT_REGION",
                                    "STATE"),
                      color_levels = NULL,
                      facet_var = NULL,
                      facet_levels = NULL) {

  # Simple plot of groups as colors
  if(is.null(facet_var)){

    df <- extract_wapr(db, color_var, color_levels)
    p <- ggplot(df, aes_string(x = "year", y = "wapr", color = color_var)) +
      geom_path()
  } else {

    # get lookup table grouping color variables to facet variables
    zt <- zones_data %>%
      group_by_(color_var, facet_var) %>%
      slice(1) %>% ungroup() %>%
      select_(color_var, facet_var)

    df <- extract_wapr(db, color_var, color_levels) %>%
      left_join(zt)

    p <- ggplot(
      df, aes_string(x = "year", y = "wapr", color = color_var)) +
      geom_path() +
      facet_wrap(as.formula(paste("~", facet_var)))
  }

  p +
    xlab("Year") + ylab("Labor Force Participation Rate (%)") +
    scale_color_discrete(color_var) +
    theme_bw() + theme(axis.text.x = element_text(angle = 30))

}


#' Plot WAPR Volatility in a Region
#'
#' This function calculates the volatility of worker to adult population ratio
#' at an arbitrary geographic level. Volatility is defined as the standard
#' deviation of the percent growth rate of wapr between consecutive periods.
#'
#' @param db Scenario database.
#' @param level Level at which to calculate volatility over time. Smaller
#'   levels, such as BZONE will show higher volatility.
#' @param scope a filtering criteria to limit the scope of the dataframe
#' @param ggmap if TRUE, return a ggmap background
#'
#'
#' @return a ggplot2 object.
#'
#' @export
plot_wapr_volatility <- function(db,
                                 level = c("BZONE", "COUNTY", "MPO",
                                           "ALDREGION", "STATE"),
                                 scope = NULL,
                                 ggmap = FALSE){

  if(!is.null(scope)){

    # determine the list of levels in the scope.
    zt <- zones_data %>%
      filter_(.dots = scope) %>%
      select_(level) %>%
      group_by_(level) %>%
      slice(1)


    df <- extract_wapr(db, level) %>%
      # trim to scope
      inner_join(zt)

  } else {
    df <- extract_wapr(db, level)
  }

  # calculate volatility as the standard deviation of the growth rates.
  df <- df %>%
    group_by_(level) %>%
    arrange(year) %>%
    mutate(
      rate = (lead(wapr) - wapr) / wapr * 100
    ) %>%
    summarise(
      volatility = sd(rate, na.rm = TRUE)
    )


  # plot volatility as the
  dt <- zones %>%
    inner_join(df)

  if(ggmap){
    map <- get_map(
      bbox(as.matrix(dt[, c("long", "lat")])),
      source = "stamen", color = "bw", maptype = "toner"
    )

    p <- ggmap(map, extent = "dev") +
      theme_bw() + theme(axis.text.x = element_text(angle = 30))
  } else {
    p <- ggplot() +
      coord_map("conic", lat0 = 43) +
      theme_bw() + theme(axis.text.x = element_text(angle = 30))
  }

  p + geom_polygon(
    data = dt,
    alpha = 0.5,
    aes_string(x = "long", y = "lat", fill = "volatility", group = "group")
  ) +
    scale_fill_gradient("Volatility in WAPR", low = "white", high = "red")

}

#' Compare workforce participation in two scenarios
#'
#' @param db1 The swim database for the "Reference" scenario.
#' @param db2 The swim database for the "Current" scenario.
#' @param facet_var Field to facet by: either "MPO" or "COUNTY".
#' @param facet_levels A character vector of the facet variable specifiying
#'   which levels to include.
#'
#' @return ggplot2 object
#' @export
compare_wapr <- function(db1, db2,
                         facet_var = c("BZONE", "MPO", "COUNTY", "DOT_REGION",
                                      "STATE"),
                         facet_levels = NULL) {

  ref <- extract_wapr(db1, facet_var, facet_levels) %>%
    rename(ref = wapr) %>%
    select(-workers, -laborforce)
  com <- extract_wapr(db2, facet_var, facet_levels) %>%
    rename(com = wapr) %>%
    select(-workers, -laborforce)

  df <- left_join(ref, com) %>%
    mutate(diff = (com - ref) / ref * 100)


  ggplot(df,
         aes_string(x = "year", y = "diff", color = facet_var)) +
    geom_path() +
    xlab("Year") + ylab("Percent difference (current - reference).") +
    theme_bw() + theme(axis.text.x = element_text(angle = 30))
}


#' Compare WAPR  across multiple scenarios.
#'
#' @param dbset A list of connections to SWIM databases.
#' @param db_names A character vector naming the scenarios.
#' @param facet_var Field to facet by.
#' @param facet_levels A character vector of the facet variable specifiying
#'   which levels to include.
#'
#' @return a ggplot2 object.
#'
#'
#' @export
multiple_wapr <- function(dbset, db_names,
                          facet_var = c("BZONE", "MPO", "COUNTY", "STATE"),
                          facet_levels = NULL ) {

  # get the wapr table for every scenario.
  names(dbset) <- db_names
  df <- rbind_all(
    lapply(seq_along(dbset), function(i)
      extract_wapr(dbset[[i]], facet_var, facet_levels) %>%
        mutate(scenario = names(dbset)[[i]])
    )
  ) %>%
    mutate_("facet_var" = facet_var)

  ggplot(
    df,
    aes_string(x = "year", y = "wapr", color = "scenario")
  ) +
    geom_path() +
    facet_wrap(~ facet_var) +
    xlab("Year") + ylab("Labor force participation") +
    theme_bw() + theme(axis.text.x = element_text(angle = 30))

}
