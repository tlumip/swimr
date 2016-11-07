#' Extract employment statistics from scenario
#'
#' This is an internal function to pull the employment data from a scenario and
#' return it to either the single scenario or scenario comparison functions.
#'
#' @param db The scenario database.
#' @param facet_var The variable in the zone table to facet by. Defaults to MPO
#' @param facet_levels The levels of the facet variable to keep. Defaults to all
#'   levels other than external stations.
#' @param type_levels The types of employment to show in the plot.
#' @param employment_categories [Optional] data_frame of employment aggregation categories;
#'   if \code{NULL}, uses \code{\link{emp_types}}. Should have columns
#'   \code{ACTIVITY} (full code of activity in AA databases) and \code{emp_type}.
#'
#' @export
extract_employment <- function(db, facet_var = NULL, facet_levels = NULL,
                               type_levels = NULL, employment_categories = NULL){

  # set facet variable; if null then default to MPO
  if(is.null(facet_var)){
    facet_var = "MPO"
  }

  grouping <- dplyr::tbl(db, "BZONE") %>%
    dplyr::select_("BZONE", "facet_var" = facet_var)

  # get levels of facet_var if none given
  if(is.null(facet_levels)){
    facet_levels <- grouping %>% dplyr::group_by(facet_var) %>% dplyr::collect(n=Inf) %>%
      dplyr::slice(1) %>% .$facet_var

    facet_levels <- facet_levels[which(facet_levels != "EXTSTA")]
  }


  employment <- dplyr::tbl(db, "ActivityLocations") %>%
    dplyr::select(BZONE, ACTIVITY, TSTEP, Employment) %>%
    dplyr::filter(Employment > 0) %>%

    # join grouping variable
    dplyr::left_join(grouping, by = "BZONE") %>%
    dplyr::filter(facet_var %in% facet_levels) %>%
    dplyr::group_by(facet_var, ACTIVITY, TSTEP) %>%
    dplyr::summarize(emp = sum(Employment)) %>%
    dplyr::mutate(year = as.numeric(TSTEP) + 1990) %>%
    dplyr::ungroup() %>%
    dplyr::collect(n=Inf)

  # consolidate employment categories
  if(is.null(employment_categories)){ # use included aggregation
    employment <-
      employment %>%
      dplyr::mutate(ACTIVITY =  gsub("_.*", "", ACTIVITY)) %>%
      dplyr::left_join(emp_types, by = "ACTIVITY") %>%
      dplyr::group_by(facet_var, emp_type, year) %>%
      dplyr::summarize(emp = sum(emp))
  } else {  # use custom aggregation
    employment <- employment %>%
      dplyr::left_join(employment_categories, by = "ACTIVITY") %>%
      dplyr::group_by(facet_var, emp_type, year) %>%
      dplyr::summarize(emp = sum(emp))
  }

  if(!is.null(type_levels)){
    employment <- employment %>%
      dplyr::filter(emp_type %in% type_levels)
  }



  return(employment)
}

#' Plot Employment by Sector
#'
#' This function plots the employment by type in an area over time between two
#' scenarios.
#'
#' @inheritParams extract_employment
#'
#' @return A ggplot2 object showing the employment by type and and year.
#'
#' @export
plot_employment <- function(db, facet_var = "MPO",
                            facet_levels = NULL, type_levels = NULL,
                            employment_categories = NULL){

  employment <- extract_employment(db, facet_var, facet_levels, type_levels,
                                   employment_categories)

  # make plot
  ggplot2::ggplot(employment,
         ggplot2::aes(x = year, y = emp,
             group = emp_type, color = emp_type)) +
    ggplot2::geom_path()  +
    ggplot2::facet_wrap( ~ facet_var) +

    ggplot2::scale_y_log10() +
    ggplot2::xlab("Year") + ggplot2::ylab("Employees") +
    ggplot2::scale_color_discrete("Sector") +
    ggplot2::theme_bw() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30))

}


#' Compare labor output between two scenarios
#'
#' This function compares the employment by type in an area over time between two
#' scenarios.
#'
#' @param db1 The reference scenario database.
#' @param db2 The current scenario database.
#' @inheritParams extract_employment
#'
#' @return A ggplot2 object showing the employment by type and and year.
#'
#' @export
compare_employment <- function(db1, db2, facet_var = "MPO", facet_levels = NULL,
                               type_levels = NULL, employment_categories = NULL){

  # get the reference scenario data
  fref <- extract_employment(db1, facet_var, facet_levels, type_levels,
                             employment_categories) %>%
    dplyr::rename(ref = emp)

  # get the comparison scenario
  fcom <- extract_employment(db2, facet_var, facet_levels, type_levels,
                             employment_categories) %>%
    dplyr::rename(com = emp)

  f <- dplyr::left_join(fref, fcom) %>%
    dplyr::mutate(diff = (com - ref) / ref * 100)  # percent difference


  ggplot2::ggplot(f,
         ggplot2::aes(x = year, y = diff, color = emp_type)) +
    ggplot2::geom_path() +
    ggplot2::facet_wrap( ~ facet_var) +
    ggplot2::xlab("Year") +
    ggplot2::ylab("Percent difference (current - reference) in Number of Employees") +
    ggplot2::scale_color_discrete("Sector") +
    ggplot2::theme_bw() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30))
}

#' Compare Employment by sector across multiple scenarios.
#'
#' @param dbset A list of connections to SWIM databases.
#' @param db_names A character vector naming the scenarios.
#' @param facet_var The variable in the zone table to facet by. Defaults to MPO
#' @param facet_levels The levels of the facet variable to keep. Defaults to all
#'   levels other than external stations.
#' @param type_levels The types of employment to show in the plot.
#'
#' @return a ggplot2 object.
#'
#' @export
multiple_employment <- function(dbset, db_names,
                                facet_var = c("BZONE", "MPO", "COUNTY", "STATE"),
                                facet_levels = NULL,
                                type_levels = NULL) {

  # get the employment table for every scenario.
  names(dbset) <- db_names
  df <- bind_rows(
    lapply(seq_along(dbset), function(i)
      extract_employment(dbset[[i]], facet_var, facet_levels, type_levels) %>%
        dplyr::mutate(scenario = names(dbset)[[i]])
    )
  )

  ggplot2::ggplot(
    df,
    ggplot2::aes_string(x = "year", y = "emp", color = "scenario")
  ) +
    ggplot2::geom_path() +
    ggplot2::facet_grid(facet_var ~ emp_type, scale = "free_y") +
    ggplot2::xlab("Year") + ggplot2::ylab("Employment") +
    ggplot2::theme_bw() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30))

}
