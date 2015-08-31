#' Extract employment statistics from scenario
#'
#' This is an internal function to pull the employment data from a scenario and
#' return it to either the single scenario or scenario comparison functions.
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

  # get buysell table and compute summary
  employment <- tbl(db, "BuySellMatrix") %>%
    select(
      BZONE = FROMBZONE,
      year = TSTEP,
      BuySell_A1_Mgmt_Bus:BuySell_Fire_Business_and_Professional_Services,
      BuySell_Food_Services:BuySell_Return_Investment_Receipts,
      BuySell_Teaching_K12
    )  %>%

    mutate(year = as.numeric(year) + 1990) %>%

    group_by(BZONE, year) %>%
    summarise_each(funs(sum), -BZONE, -year) %>%

    # join facet and filter desired levels
    left_join(grouping, by = "BZONE") %>%
    filter(facet_var %in% facet_levels) %>%
    collect() %>%

    gather(sector, output, BuySell_A1_Mgmt_Bus:BuySell_Teaching_K12) %>%

    # consolidate employment types and filter to desired levels
    left_join(employment_types, by = "sector") %>%
    filter(naics1 %in% type_levels) %>%
    group_by(facet_var, year, naics_label) %>%
    summarise(output = sum(output)) %>%
    mutate(year = as.numeric(year) + 1990)

  return(employment)
}

#' Plot Employment by Sector
#'
#' This function plots the value of employment sold by a zone. This is a proxy
#' for the number of workers in each sector, which is not immediately available
#' from the SWIM database.
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
         aes(x = as.numeric(year), y = output,
             group = naics_label, color = naics_label)) +
    geom_path()  +
    facet_wrap( ~ facet_var) +

    scale_y_log10() +
    xlab("Year") + ylab("Output (dollars)") +
    theme_bw()

}


#' Compare Employment between two scenarios
#'
#' This function plots the value of employment sold by a zone. This is a proxy
#' for the number of workers in each sector, which is not immediately available
#' from the SWIM database.
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
    rename(out_ref = output)

  # get the comparison scenario
  fcom <- extract_employment(db2, facet_var, facet_levels, type_levels) %>%
    rename(out_com = output)

  f <- left_join(fref, fcom) %>%
    mutate(diff = (out_com - out_ref) / out_ref * 100)  # percent difference


  ggplot(f,
         aes(x = year, y = diff, color = naics_label)) +
    geom_path() +
    facet_wrap( ~ facet_var) +
    xlab("Year") +
    ylab("Percent difference (current - reference) in Employee Output") +
    theme_bw()
}
