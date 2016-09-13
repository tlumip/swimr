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
#' @param color_levels A character vector of the industry sectors to include.
#'   Defaults to all.
#'
#' @return A \code{data_frame} with the participation rate in each facet region
#'   in each transport model year.
#'
#' @export
extract_wapr <- function(db,
                         facet_var = c("BZONE", "MPO", "COUNTY",
                                       "DOT_REGION", "STATE"),
                         facet_levels = NULL, color_levels = NULL) {


  df <- dplyr::tbl(db, "AZONE") %>%
    dplyr::transmute(
      AZONE, TSTEP,
      workers = TOTALWORKERS,
      laborforce = PERSON15TO21 + PERSON21TO40 + PERSON40TO60 + PERSON60PLUS
    ) %>%

    # join faceting variables
    dplyr::left_join(
      dplyr::tbl(db, "ALLZONES") %>%
        dplyr::select_("AZONE" = "Azone", facet_var)
    )

    # if no levels specified, then keep all
    if(!is.null(facet_levels)){

      df <- df %>%
        dplyr::mutate_("facet_var" = facet_var) %>%
        dplyr::filter(facet_var %in% facet_levels)
    }

  df <- df %>%
    # dplyr::summarize on faceting variable
    dplyr::group_by_("TSTEP", facet_var) %>%
    dplyr::summarize(
      workers = sum(workers),
      laborforce = sum(laborforce)
    ) %>%

    # calculate wapr
    dplyr::collect(n=Inf) %>%
    dplyr::mutate(
      wapr = workers / laborforce * 100,
      year = as.numeric(TSTEP) + 1990
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-TSTEP)

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
    p <- ggplot2::ggplot(df, ggplot2::aes_string(x = "year", y = "wapr", color = color_var)) +
      ggplot2::geom_path()
  } else {

    # get lookup table grouping color variables to facet variables
    zt <- zones_data %>%
      dplyr::group_by_(color_var, facet_var) %>%
      dplyr::slice(1) %>% dplyr::ungroup() %>%
      dplyr::select_(color_var, facet_var)

    df <- extract_wapr(db, color_var, color_levels) %>%
      dplyr::left_join(zt)

    p <- ggplot2::ggplot(
      df, ggplot2::aes_string(x = "year", y = "wapr", color = color_var)) +
      ggplot2::geom_path() +
      ggplot2::facet_wrap(stats::as.formula(paste("~", facet_var)))
  }

  p +
    ggplot2::xlab("Year") + ggplot2::ylab("Labor Force Participation Rate (%)") +
    ggplot2::scale_color_discrete(color_var) +
    ggplot2::theme_bw() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30))

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
#' @param scope a dplyr::filtering criteria to limit the scope of the dataframe
#'
#'
#' @return a ggplot2 object.
#'
#' @export
plot_wapr_volatility <- function(db,
                                 level = c("BZONE", "COUNTY", "MPO",
                                           "ALDREGION", "STATE"),
                                 scope = NULL){

  if(!is.null(scope)){

    # determine the list of levels in the scope.
    zt <- zones_data %>%
      dplyr::filter_(.dots = scope) %>%
      dplyr::select_(level) %>%
      dplyr::group_by_(level) %>%
      dplyr::slice(1)


    df <- extract_wapr(db, level) %>%
      # trim to scope
      inner_join(zt)

  } else {
    df <- extract_wapr(db, level)
  }

  # calculate volatility as the standard deviation of the growth rates.
  df <- df %>%
    dplyr::group_by_(level) %>%
    dplyr::arrange(year) %>%
    dplyr::mutate(
      rate = (lead(wapr) - wapr) / wapr * 100
    ) %>%
    dplyr::summarize(
      volatility = stats::sd(rate, na.rm = TRUE)
    )


  # plot volatility as the
  dt <- zones %>%
    inner_join(df)

  p <- ggplot2::ggplot() +
    coord_map("conic", lat0 = 43) +
    ggplot2::theme_bw() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30))

  p + geom_polygon(
    data = dt,
    alpha = 0.5,
    ggplot2::aes_string(x = "long", y = "lat", fill = "volatility", group = "group")
  ) +
    ggplot2::scale_fill_gradient("Volatility in WAPR", low = "white", high = "red")

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
    dplyr::rename(ref = wapr) %>%
    dplyr::select(-workers, -laborforce)
  com <- extract_wapr(db2, facet_var, facet_levels) %>%
    dplyr::rename(com = wapr) %>%
    dplyr::select(-workers, -laborforce)

  df <- dplyr::left_join(ref, com) %>%
    dplyr::mutate(diff = (com - ref) / ref * 100)


  ggplot2::ggplot(df,
         ggplot2::aes_string(x = "year", y = "diff", color = facet_var)) +
    ggplot2::geom_path() +
    ggplot2::xlab("Year") + ggplot2::ylab("Percent difference (current - reference).") +
    ggplot2::theme_bw() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30))
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
  df <- bind_rows(
    lapply(seq_along(dbset), function(i)
      extract_wapr(dbset[[i]], facet_var, facet_levels) %>%
        dplyr::mutate(scenario = names(dbset)[[i]])
    )
  ) %>%
    dplyr::mutate_("facet_var" = facet_var)

  ggplot2::ggplot(
    df,
    ggplot2::aes_string(x = "year", y = "wapr", color = "scenario")
  ) +
    ggplot2::geom_path() +
    ggplot2::facet_wrap(~ facet_var) +
    ggplot2::xlab("Year") + ggplot2::ylab("Labor force participation") +
    ggplot2::theme_bw() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30))

}
