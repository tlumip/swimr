#' Extract floorspace statistics from scenario
#'
#' This is an internal function to pull the floorspace data from a scenario and
#' return it to either the single scenario or scenario comparison functions.
#' @param db The scenario database.
#' @param facet_var The variable in the zone table to facet by. Defaults to MPO
#' @param facet_levels The levels of the facet variable to keep. Defaults to all
#'   levels other than external stations.
#' @param type_levels The types of employment to show in the plot.
#'
#' @export
extract_floorspace <- function(db,
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

  # get levels of floortype
  if(is.null(type_levels)){
    type_levels <- floor_types$floor_type
  }

  # get floorspace table and compute summary
  floorspace <- tbl(db, "FLR_INVENTORY") %>%
    transmute(
      BZONE,
      year = TSTEP + 1990,
      commodity = COMMODITY,
      floor = FLR, built = INCREMENT
    ) %>%

    # join facet and filter desired levels
    left_join(grouping, by = "BZONE") %>%
    filter(facet_var %in% facet_levels) %>%

    # sum within facet, year, and type
    group_by(facet_var, year, commodity) %>%
    summarise_each(funs(sum), floor:built) %>%
    collect() %>%

    # consolidate floortypes and filter to desired levels
    left_join(floor_types, by = "commodity") %>%
    filter(floor_type %in% type_levels) %>%
    group_by(facet_var, year, floor_type) %>%
    summarise_each(funs(sum), floor:built)

  return(floorspace)
}



#' Plot floorspace over time
#'
#' This function plots the available floorspace by type over time facetted by a
#' variable chosen from the zone attributes table.
#'
#' @param db The scenario database.
#' @param facet_var The variable in the zone table to facet by. Defaults to MPO
#' @param facet_levels The levels of the facet variable to keep. Defaults to all
#'   levels other than external stations.
#' @param type_levels The types of floorspace to show in the plot.
#'
#'
#' @return A ggplot2 object showing the floorspace by type and and year.
#'
#' @export
plot_floorspace <- function(db,
                            facet_var = c("MPO", "COUNTY", "STATE"),
                            facet_levels = NULL,
                            type_levels = NULL ){

  floorspace <- extract_floorspace(db, facet_var, facet_levels, type_levels)

  # make plot
  ggplot(floorspace,
         aes(x = as.numeric(year), y = floor,
             group = floor_type, color = floor_type)) +
    geom_path()  +
    facet_wrap( ~ facet_var) +

    scale_y_log10() +
    xlab("Year") + ylab("Floor Space [million sqft]") +
    theme_bw()

}

#' Compare floorspace over time
#'
#' @param db1 The swim database for the "Reference" scenario.
#' @param db2 The swim database for the "Current" scenario.
#' @param facet_var The variable in the zone table to facet by. Defaults to MPO
#' @param facet_levels The levels of the facet variable to keep. Defaults to all
#'   levels other than external stations.
#' @param type_levels The types of floorspace to show in the plot.
#'
#' @export
#'
compare_floorspace <- function(db1, db2,
                               facet_var = c("MPO", "COUNTY", "STATE"),
                               facet_levels = NULL,
                               type_levels = NULL){

  # get the reference scenario data
  fref <- extract_floorspace(db1, facet_var, facet_levels, type_levels) %>%
    rename(floor_ref = floor, built_ref = built)

  # get the comparison scenario
  fcom <- extract_floorspace(db2, facet_var, facet_levels, type_levels) %>%
    rename(floor_com = floor, built_com = built)

  f <- left_join(fref, fcom) %>%
    gather(var, value, floor_ref:built_com) %>%
    separate(var, c("var", "scenario")) %>%
    spread(scenario, value, fill = NA) %>%
    filter(var == "built") %>%
    mutate(diff = (com - ref) / ref * 100)  # percent difference


  ggplot(f,
         aes(x = year, y = diff, color = floor_type)) +
    geom_path() +
    facet_wrap( ~ facet_var) +
    xlab("Year") + ylab("Percent difference (current - reference) in floor area") +
    theme_bw()
}


