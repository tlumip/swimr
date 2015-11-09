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
#'
#' @export
extract_employment <- function(db,
                               facet_var = c("MPO", "COUNTY", "STATE"),
                               facet_levels = NULL,
                               type_levels = NULL){

  # set facet variable; if null then default to MPO
  if(is.null(facet_var)){
    facet_var = "MPO"
  }

  grouping <- tbl(db, "BZONE") %>%
    select_("BZONE", "facet_var" = facet_var)

  # get levels of facet_var if none given
  if(is.null(facet_levels)){
    facet_levels <- grouping %>% group_by(facet_var) %>% collect() %>%
      slice(1) %>% .$facet_var

    facet_levels <- facet_levels[which(facet_levels != "EXTSTA")]
  }

  # get levels of employment type
  if(is.null(type_levels)){
    type_levels <- employment_types$naics1
  }

  employment <- tbl(db, "ActivityLocations") %>%
    select(BZONE, ACTIVITY, TSTEP, Employment) %>%
    filter(Employment > 0) %>%

    # join grouping variable
    left_join(grouping, by = "BZONE") %>%
    filter(facet_var %in% facet_levels) %>%
    group_by(facet_var, ACTIVITY, TSTEP) %>%
    summarise(emp = sum(Employment)) %>%
    ungroup() %>%
    collect() %>%

    # consolidate employment categories
    mutate(
      ACTIVITY =  gsub("_.*", "", ACTIVITY),
      year = as.numeric(TSTEP) + 1990
    ) %>%
    left_join(emp_types, by = "ACTIVITY") %>%
    group_by(facet_var, emp_type, year) %>%
    summarise(emp = sum(emp))



  return(employment)
}

#' Plot Employment by Sector
#'
#' This function plots the employment by type in an area over time between two
#' scenarios.
#'
#' @param db The scenario database.
#' @param facet_var The variable in the zone table to facet by. Defaults to MPO
#' @param facet_levels The levels of the facet variable to keep. Defaults to all
#'   levels other than external stations.
#' @param type_levels The types of employment to show in the plot.
#'
#' @return A ggplot2 object showing the employment by type and and year.
#'
#' @export
plot_employment <- function(db,
                          facet_var = c("MPO", "COUNTY", "STATE"),
                          facet_levels = NULL,
                          type_levels = NULL){

  employment <- extract_employment(db, facet_var, facet_levels, type_levels)

  # make plot
  ggplot(employment,
         aes(x = year, y = emp,
             group = emp_type, color = emp_type)) +
    geom_path()  +
    facet_wrap( ~ facet_var) +

    scale_y_log10() +
    xlab("Year") + ylab("Employees") +
    scale_color_discrete("Sector") +
    theme_bw() + theme(axis.text.x = element_text(angle = 30))

}


#' Compare labor output between two scenarios
#'
#' This function compares the employment by type in an area over time between two
#' scenarios.
#'
#' @param db1 The reference scenario database.
#' @param db2 The current scenario database.
#' @param facet_var The variable in the zone table to facet by. Defaults to MPO
#' @param facet_levels The levels of the facet variable to keep. Defaults to all
#'   levels other than external stations.
#' @param type_levels The types of employment to show in the plot.
#'
#' @return A ggplot2 object showing the employment by type and and year.
#'
#' @export
compare_employment <- function(db1, db2,
                            facet_var = c("MPO", "COUNTY", "STATE"),
                            facet_levels = NULL,
                            type_levels = NULL){

  # get the reference scenario data
  fref <- extract_employment(db1, facet_var, facet_levels, type_levels) %>%
    rename(ref = emp)

  # get the comparison scenario
  fcom <- extract_employment(db2, facet_var, facet_levels, type_levels) %>%
    rename(com = emp)

  f <- left_join(fref, fcom) %>%
    mutate(diff = (com - ref) / ref * 100)  # percent difference


  ggplot(f,
         aes(x = year, y = diff, color = emp_type)) +
    geom_path() +
    facet_wrap( ~ facet_var) +
    xlab("Year") +
    ylab("Percent difference (current - reference) in Number of Employees") +
    scale_color_discrete("Sector") +
    theme_bw() + theme(axis.text.x = element_text(angle = 30))
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
  df <- rbind_all(
    lapply(seq_along(dbset), function(i)
      extract_employment(dbset[[i]], facet_var, facet_levels, type_levels) %>%
        mutate(scenario = names(dbset)[[i]])
    )
  )

  ggplot(
    df,
    aes_string(x = "year", y = "emp", color = "scenario")
  ) +
    geom_path() +
    facet_grid(facet_var ~ emp_type, scales = "free_y") +
    xlab("Year") + ylab("Employment") +
    theme_bw() + theme(axis.text.x = element_text(angle = 30))

}
