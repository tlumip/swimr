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


#' Extract rent price
#'
#' This function extracts the occupancy rate by floortype from a scenario over
#' time.
#' @param db The scenario database.
#' @param facet_var The variable in the zone table to facet by. Defaults to MPO
#' @param facet_levels The levels of the facet variable to keep. Defaults to all
#'   levels other than external stations.
#' @param type_levels The types of employment to show in the plot.
#'
extract_rents <- function(db,
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

  df <- tbl(db, "ExchangeResults") %>%
    transmute(
      BZONE,
      year = TSTEP + 1990,
      commodity = COMMODITY,
      supply = Supply,
      price = Price
    ) %>%

    # join facet and filter desired levels
    filter(commodity %in% floor_types$commodity) %>%
    left_join(grouping, by = "BZONE") %>%
    filter(facet_var %in% facet_levels) %>%


    # filter to floortypes that the user requests and regroup
    collect() %>%
    left_join(floor_types, by = "commodity") %>%
    filter(floor_type %in% type_levels) %>%
    filter(supply > 0) %>% # only count if it's available.

    # summarise within facet and year
    group_by(facet_var, year, floor_type) %>%
    summarise(supply = sum(supply), price = mean(price))

  return(df)
}


#' Plot floorspace statistics over time
#'
#' This function plots the constructed floorspace by type over time facetted by a
#' variable chosen from the zone attributes table.
#'
#' @param db The scenario database.
#' @param facet_var The variable in the zone table to facet by. Defaults to MPO
#' @param facet_levels The levels of the facet variable to keep. Defaults to all
#'   levels other than external stations.
#' @param type_levels The types of floorspace to show in the plot.
#' @param price Print price instead of floorspace.
#'
#'
#' @return A ggplot2 object showing the floorspace by type and and year.
#'
#' @export
plot_floorspace <- function(db,
                            facet_var = c("MPO", "COUNTY", "STATE"),
                            facet_levels = NULL,
                            type_levels = NULL, price = FALSE){

  if(price){
    floorspace <- extract_rents(db, facet_var, facet_levels, type_levels) %>%
      mutate(floor = price)
    ylabel = "Rent [$/sqft]"
  } else {
    floorspace <- extract_floorspace(db, facet_var, facet_levels, type_levels)
    ylabel <- "Floorspace [sqft]"
  }

  # make plot
  ggplot(floorspace,
         aes(x = as.numeric(year), y = floor,
             group = floor_type, color = floor_type)) +
    geom_path()  +
    facet_wrap( ~ facet_var) +

    scale_y_log10() +
    xlab("Year") + ylab(ylabel) +
    theme_bw() + theme(axis.text.x = element_text(angle = 30))

}

#' Compare floorspace over time
#'
#' @param db1 The swim database for the "Reference" scenario.
#' @param db2 The swim database for the "Current" scenario.
#' @param facet_var The variable in the zone table to facet by. Defaults to MPO
#' @param facet_levels The levels of the facet variable to keep. Defaults to all
#'   levels other than external stations.
#' @param type_levels The types of floorspace to show in the plot.
#' @param price Print price instead of floorspace.
#'
#' @export
#'
compare_floorspace <- function(db1, db2,
                               facet_var = c("MPO", "COUNTY", "STATE"),
                               facet_levels = NULL,
                               type_levels = NULL, price = FALSE){

  # get the reference scenario data
  if(price){
    fref <- extract_rents(db1, facet_var, facet_levels, type_levels) %>%
      select(facet_var, year, floor_type, floor_ref = price)

    # get the comparison scenario
    fcom <- extract_rents(db2, facet_var, facet_levels, type_levels) %>%
      select(facet_var, year, floor_type, floor_com = price)

    ylabel <- "Percent difference (current - reference) in rent price"
  } else {
    fref <- extract_floorspace(db1, facet_var, facet_levels, type_levels) %>%
      select(facet_var, year, floor_type, floor_ref = built)

    # get the comparison scenario
    fcom <- extract_floorspace(db2, facet_var, facet_levels, type_levels) %>%
      select(facet_var, year, floor_type, floor_com = built)

    ylabel <- "Percent difference (current - reference) in floor area"
  }

  f <- left_join(fref, fcom) %>%
    gather(var, value, floor_ref:floor_com) %>%
    separate(var, c("var", "scenario")) %>%
    spread(scenario, value, fill = NA) %>%
    mutate(diff = (com - ref) / ref * 100)  # percent difference


  ggplot(f,
         aes(x = year, y = diff, color = floor_type)) +
    geom_path() +
    facet_wrap( ~ facet_var) +
    xlab("Year") + ylab(ylabel) +
    theme_bw() + theme(axis.text.x = element_text(angle = 30))
}

#' Compare floorspace by type across multiple scenarios.
#'
#' @param dbset A list of connections to SWIM databases.
#' @param db_names A character vector naming the scenarios.
#' @param facet_var The variable in the zone table to facet by. Defaults to MPO
#' @param facet_levels The levels of the facet variable to keep. Defaults to all
#'   levels other than external stations.
#' @param type_levels The types of floorspace to show in the plot.
#' @param price Print price instead of floorspace.
#'
#' @return a ggplot2 object.
#'
#' @export
multiple_floorspace <- function(dbset, db_names,
                                facet_var = c("MPO", "COUNTY", "STATE"),
                                facet_levels = NULL,
                                type_levels = NULL, price = FALSE) {

  # get the wapr table for every scenario.
  names(dbset) <- db_names

  if(price){
    df <- rbind_all(
      lapply(seq_along(dbset), function(i)
        extract_rents(dbset[[i]], facet_var, facet_levels, type_levels) %>%
          mutate(scenario = names(dbset)[[i]])
      )
    ) %>%
      mutate_(facet_var = "facet_var") %>%
      mutate(floor = price)

    ylabel <- "Rent price"

  } else {
    df <- rbind_all(
      lapply(seq_along(dbset), function(i)
        extract_floorspace(dbset[[i]], facet_var, facet_levels, type_levels) %>%
          mutate(scenario = names(dbset)[[i]])
      )
    ) %>%
      mutate_(facet_var = "facet_var") %>%
      mutate(floor = built)

    ylabel <- "New floor space"
  }

  ggplot(
    df,
    aes_string(x = "year", y = "floor", color = "scenario")
  ) +
    geom_path() +
    facet_grid(facet_var ~ floor_type, scales = "free_y") +
    xlab("Year") + ylab(ylabel) +
    scale_x_log10() +
    theme_bw() + theme(axis.text.x = element_text(angle = 30))

}


