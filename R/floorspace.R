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
                               type_levels = NULL, index = TRUE){

  # set facet variable; if null then default to MPO
  if(is.null(facet_var)){
    facet_var = "MPO"
  }

  grouping <- tbl(db, "BZONE") %>%
    select_("BZONE", "facet_var" = facet_var)

  # get levels of facet_var if none given
  if(is.null(facet_levels)){
    facet_levels <- grouping %>% group_by(facet_var) %>% collect(n=Inf) %>%
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
    collect(n=Inf) %>%

    # consolidate floortypes and filter to desired levels
    left_join(floor_types, by = "commodity") %>%
    filter(floor_type %in% type_levels) %>%
    group_by(facet_var, year, floor_type) %>%
    summarise_each(funs(sum), floor:built)

  if(index){
    floorspace <- floorspace %>%
      group_by(facet_var, floor_type) %>%
      mutate(floor = calc_index(floor))
  }

  return(floorspace)
}

#' Extract occupancy rate
#'
#' This function extracts the total value of floorspace sold in each region by
#' floor_type.
#' @param db The scenario database.
#' @param facet_var The variable in the zone table to facet by. Defaults to MPO
#' @param facet_levels The levels of the facet variable to keep. Defaults to all
#'   levels other than external stations.
#' @param type_levels The types of employment to show in the plot.
#'
#' @return A data frame with the total dollars sold.
#'
#' @export
extract_volume <- function(db,
                              facet_var = c("MPO", "COUNTY", "STATE"),
                              facet_levels = NULL,
                              type_levels = NULL){

  # get floorspace available
  total <- extract_floorspace(db, facet_var, facet_levels, type_levels)

  # set facet variable; if null then default to MPO
  if(is.null(facet_var)){
    facet_var = "MPO"
  }

  grouping <- tbl(db, "BZONE") %>%
    select_("BZONE", "facet_var" = facet_var)

  # get levels of facet_var if none given
  if(is.null(facet_levels)){
    facet_levels <- grouping %>% group_by(facet_var) %>% collect(n=Inf) %>%
      slice(1) %>% .$facet_var

    facet_levels <- facet_levels[which(facet_levels != "EXTSTA")]
  }

  # get levels of floortype
  if(is.null(type_levels)){
    type_levels <- floor_types$floor_type
  }

  # get floorspace purchased from buy/sell matrix
  df <- tbl(db, "BuySellMatrix") %>%
    # filter to floortypes
    filter(FROMBZONE == TOBZONE) %>%
    mutate(year = as.numeric(TSTEP) + 1990) %>%
    select(BZONE = FROMBZONE, year, matches("FLR")) %>%
    left_join(grouping) %>%
    ungroup() %>%
    group_by(facet_var, year) %>%
    summarise_each(funs(sum)) %>%

    collect(n=Inf) %>%
    gather(commodity, used, -BZONE, -year, -facet_var) %>%
    mutate(commodity = gsub("BuySell_", "", commodity)) %>%
    left_join(floor_types) %>%
    filter(floor_type %in% type_levels) %>%
    group_by(facet_var, year, floor_type) %>%
    summarize(volume = sum(used))


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
#' @export
extract_rents <- function(db,
                          facet_var = c("MPO", "COUNTY", "STATE"),
                          facet_levels = NULL,
                          type_levels = NULL, index = TRUE){

  # set facet variable; if null then default to MPO
  if(is.null(facet_var)){
    facet_var = "MPO"
  }

  grouping <- tbl(db, "BZONE") %>%
    select_("BZONE", "facet_var" = facet_var)

  # get levels of facet_var if none given
  if(is.null(facet_levels)){
    facet_levels <- grouping %>% group_by(facet_var) %>% collect(n=Inf) %>%
      slice(1) %>% .$facet_var

    facet_levels <- facet_levels[which(facet_levels != "EXTSTA")]
  }

  # get levels of floortype
  if(is.null(type_levels)){
    type_levels <- floor_types$floor_type
  }

  supply <- tbl(db, "FLR_INVENTORY") %>%
    transmute(
      BZONE,
      year = TSTEP + 1990,
      commodity = COMMODITY,
      supply = FLR
    )

  demand <- tbl(db, "ExchangeResults") %>%
    transmute(
      BZONE,
      year = TSTEP + 1990,
      commodity = COMMODITY,
      price = Price,
      bought = InternalBought  # quantity consumed from AA
    ) %>%
    # join facet and filter desired levels
    filter(commodity %in% floor_types$commodity)


  df <- left_join(supply, demand, by = c("BZONE", "year", "commodity")) %>%

    left_join(grouping, by = "BZONE") %>%
    filter(facet_var %in% facet_levels) %>%


    # filter to floortypes that the user requests and regroup
    collect(n=Inf) %>%
    left_join(floor_types, by = "commodity") %>%
    filter(floor_type %in% type_levels) %>%

    # summarise within facet and year
    group_by(facet_var, year, floor_type) %>%
    summarise(supply = sum(supply), price = mean(price), bought = sum(bought)) %>%
    mutate(occrate = bought / supply)

  if(index){
    df <- df  %>%
      group_by(facet_var, floor_type) %>%
      mutate(
        price = calc_index(price),
        occrate = calc_index(occrate)
      )
  }

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
    floorspace <- extract_rents(db, facet_var, facet_levels, type_levels, index = TRUE) %>%
      mutate(floor = price)
    ylabel = "Indexed Rent [$/sqft]"
  } else {
    floorspace <- extract_floorspace(db, facet_var, facet_levels, type_levels, index = TRUE)
    ylabel <- "Indexed Floorspace [sqft]"
  }

  # make plot
  ggplot(floorspace,
         aes(x = as.numeric(year), y = floor,
             group = floor_type, color = floor_type)) +
    geom_path()  +
    facet_wrap( ~ facet_var) +
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


  ggplot(f, aes(x = year, y = diff, color = floor_type)) +
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
#' @param variable The variable to plot, one of rents, occupancy rate, or new
#'   floorspace
#'
#' @return a ggplot2 object.
#'
#' @export
multiple_floorspace <- function(dbset, db_names,
                                facet_var = c("MPO", "COUNTY", "STATE"),
                                facet_levels = NULL,
                                type_levels = NULL,
                                variable = c("floorspace", "rent", "occupancy")) {

  # get the wapr table for every scenario.
  names(dbset) <- db_names

  if(variable %in% c("rent", "occupancy")){
    df <- bind_rows(
      lapply(seq_along(dbset), function(i)
        extract_rents(dbset[[i]], facet_var, facet_levels, type_levels) %>%
          mutate(scenario = names(dbset)[[i]])
      )
    ) %>%
      ungroup() %>%
      mutate_(facet_var = "facet_var")

    if(variable == "rent"){
      df <- df %>% mutate(floor = price)
      ylabel <- "Rent price"
    } else if(variable == "occupancy"){
      df <- df %>% mutate(floor = occrate)
      ylabel <- "Occupancy Rate"
    }


  } else {
    df <- bind_rows(
      lapply(seq_along(dbset), function(i)
        extract_floorspace(dbset[[i]], facet_var, facet_levels, type_levels) %>%
          mutate(scenario = names(dbset)[[i]])
      )
    ) %>%
      ungroup() %>%
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

#' Plot occupancy in a scenario over time.
#'
#' @inheritParams extract_rents
#' @return A ggplot2 plot object.
#'
#' @export
plot_occupancy <- function(db,
                           facet_var = c("MPO", "COUNTY", "STATE"),
                           facet_levels = NULL,
                           type_levels = NULL){

  rents <- extract_rents(db, facet_var, facet_levels, type_levels, index = FALSE)

  ggplot(rents,  aes(x = year, y = occrate, color = floor_type) ) +
    geom_path() +
    facet_wrap(~ facet_var) +
    xlab("Year") + ylab("Occupancy Rate") +
    theme_bw() + theme(axis.text.x = element_text(angle = 30))
}

#' Compare occupancy in a scenario over time.
#'
#' @inheritParams compare_floorspace
#' @return A ggplot2 plot object
#'
#' @export
compare_occupancy <- function(db1, db2,
                              facet_var = c("MPO", "COUNTY", "STATE"),
                              facet_levels = NULL,
                              type_levels = NULL){

  fref <- extract_rents(db1, facet_var, facet_levels, type_levels, index = FALSE) %>%
    select(facet_var, year, floor_type, rate_ref = occrate)

  fcom <- extract_rents(db2, facet_var, facet_levels, type_levels, index = FALSE) %>%
    select(facet_var, year, floor_type, rate_com = occrate)

  df <- left_join(fref, fcom) %>%
    mutate(pct_diff = (rate_com - rate_ref) / rate_ref * 100)

  ggplot(df,  aes(x = year, y = pct_diff, color = floor_type) ) +
    geom_path() +
    facet_wrap(~ facet_var) +
    xlab("Year") + ylab("Percent difference in occupancy rate") +
    theme_bw() + theme(axis.text.x = element_text(angle = 30))

}


