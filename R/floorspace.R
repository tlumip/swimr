#' Extract floorspace statistics from scenario
#'
#' This is an internal function to pull the floorspace data from a scenario and
#' return it to either the single scenario or scenario comparison functions.
#' @param db The scenario database.
#' @param facet_var The variable in the zone table to facet by. Defaults to MPO
#' @param facet_levels The levels of the facet variable to keep. Defaults to all
#'   levels other than external stations.
#' @param type_levels The types of employment to show in the plot.
#' @param index Should the function extract indexed or absolute values?
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

  grouping <- dplyr::tbl(db, "BZONE") %>%
    dplyr::select_("BZONE", "facet_var" = facet_var)

  # get levels of facet_var if none given
  if(is.null(facet_levels)){
    facet_levels <- grouping %>% dplyr::group_by(facet_var) %>% dplyr::collect(n=Inf) %>%
      dplyr::slice(1) %>% .$facet_var

    facet_levels <- facet_levels[which(facet_levels != "EXTSTA")]
  }

  # get levels of floortype
  if(is.null(type_levels)){
    type_levels <- floor_types$floor_type
  }

  # get floorspace table and compute summary
  floorspace <- dplyr::tbl(db, "FLR_INVENTORY") %>%
    dplyr::transmute(
      BZONE,
      year = TSTEP + 1990,
      commodity = COMMODITY,
      floor = FLR, built = INCREMENT
    ) %>%

    # join facet and dplyr::filter desired levels
    dplyr::left_join(grouping, by = "BZONE") %>%
    dplyr::filter(facet_var %in% facet_levels) %>%

    # sum within facet, year, and type
    dplyr::group_by(facet_var, year, commodity) %>%
    dplyr::summarize_each(funs(sum), floor:built) %>%
    dplyr::collect(n=Inf) %>%

    # consolidate floortypes and dplyr::filter to desired levels
    dplyr::left_join(floor_types, by = "commodity") %>%
    dplyr::filter(floor_type %in% type_levels) %>%
    dplyr::group_by(facet_var, year, floor_type) %>%
    dplyr::summarize_each(funs(sum), floor:built)

  if(index){
    floorspace <- floorspace %>%
      dplyr::group_by(facet_var, floor_type) %>%
      dplyr::mutate(floor = calc_index(floor))
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

  grouping <- dplyr::tbl(db, "BZONE") %>%
    dplyr::select_("BZONE", "facet_var" = facet_var)

  # get levels of facet_var if none given
  if(is.null(facet_levels)){
    facet_levels <- grouping %>% dplyr::group_by(facet_var) %>% dplyr::collect(n=Inf) %>%
      dplyr::slice(1) %>% .$facet_var

    facet_levels <- facet_levels[which(facet_levels != "EXTSTA")]
  }

  # get levels of floortype
  if(is.null(type_levels)){
    type_levels <- floor_types$floor_type
  }

  # get floorspace purchased from buy/sell matrix
  df <- dplyr::tbl(db, "BuySellMatrix") %>%
    # dplyr::filter to floortypes
    dplyr::filter(FROMBZONE == TOBZONE) %>%
    dplyr::mutate(year = as.numeric(TSTEP) + 1990) %>%
    dplyr::select(BZONE = FROMBZONE, year, matches("FLR")) %>%
    dplyr::left_join(grouping) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(facet_var, year) %>%
    dplyr::summarize_each(funs(sum)) %>%

    dplyr::collect(n=Inf) %>%
    tidyr::gather(commodity, used, -BZONE, -year, -facet_var) %>%
    dplyr::mutate(commodity = gsub("BuySell_", "", commodity)) %>%
    dplyr::left_join(floor_types) %>%
    dplyr::filter(floor_type %in% type_levels) %>%
    dplyr::group_by(facet_var, year, floor_type) %>%
    dplyr::summarize(volume = sum(used))


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
#' @param index Should the function extract indexed or absolute values?
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

  grouping <- dplyr::tbl(db, "BZONE") %>%
    dplyr::select_("BZONE", "facet_var" = facet_var)

  # get levels of facet_var if none given
  if(is.null(facet_levels)){
    facet_levels <- grouping %>% dplyr::group_by(facet_var) %>% dplyr::collect(n=Inf) %>%
      dplyr::slice(1) %>% .$facet_var

    facet_levels <- facet_levels[which(facet_levels != "EXTSTA")]
  }

  # get levels of floortype
  if(is.null(type_levels)){
    type_levels <- floor_types$floor_type
  }

  supply <- dplyr::tbl(db, "FLR_INVENTORY") %>%
    dplyr::transmute(
      BZONE,
      year = TSTEP + 1990,
      commodity = COMMODITY,
      supply = FLR
    )

  demand <- dplyr::tbl(db, "ExchangeResults") %>%
    dplyr::transmute(
      BZONE,
      year = TSTEP + 1990,
      commodity = COMMODITY,
      price = Price,
      bought = InternalBought  # quantity consumed from AA
    ) %>%
    # join facet and dplyr::filter desired levels
    dplyr::filter(commodity %in% floor_types$commodity)


  df <- dplyr::left_join(supply, demand, by = c("BZONE", "year", "commodity")) %>%

    dplyr::left_join(grouping, by = "BZONE") %>%
    dplyr::filter(facet_var %in% facet_levels) %>%


    # dplyr::filter to floortypes that the user requests and regroup
    dplyr::collect(n=Inf) %>%
    dplyr::left_join(floor_types, by = "commodity") %>%
    dplyr::filter(floor_type %in% type_levels) %>%

    # dplyr::summarize within facet and year
    dplyr::group_by(facet_var, year, floor_type) %>%
    dplyr::summarize(supply = sum(supply), price = mean(price), bought = sum(bought)) %>%
    dplyr::mutate(occrate = bought / supply)

  if(index){
    df <- df  %>%
      dplyr::group_by(facet_var, floor_type) %>%
      dplyr::mutate(
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
      dplyr::mutate(floor = price)
    ylabel = "Indexed Rent [$/sqft]"
  } else {
    floorspace <- extract_floorspace(db, facet_var, facet_levels, type_levels, index = TRUE)
    ylabel <- "Indexed Floorspace [sqft]"
  }

  # make plot
  ggplot2::ggplot(floorspace,
         ggplot2::aes(x = as.numeric(year), y = floor,
             group = floor_type, color = floor_type)) +
    ggplot2::geom_path()  +
    ggplot2::facet_wrap( ~ facet_var) +
    ggplot2::xlab("Year") + ggplot2::ylab(ylabel) +
    ggplot2::theme_bw() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30))

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
      dplyr::select(facet_var, year, floor_type, floor_ref = price)

    # get the comparison scenario
    fcom <- extract_rents(db2, facet_var, facet_levels, type_levels) %>%
      dplyr::select(facet_var, year, floor_type, floor_com = price)

    ylabel <- "Percent difference (current - reference) in rent price"
  } else {
    fref <- extract_floorspace(db1, facet_var, facet_levels, type_levels) %>%
      dplyr::select(facet_var, year, floor_type, floor_ref = built)

    # get the comparison scenario
    fcom <- extract_floorspace(db2, facet_var, facet_levels, type_levels) %>%
      dplyr::select(facet_var, year, floor_type, floor_com = built)

    ylabel <- "Percent difference (current - reference) in floor area"
  }

  f <- dplyr::left_join(fref, fcom) %>%
    tidyr::gather(var, value, floor_ref:floor_com) %>%
    tidyr::separate(var, c("var", "scenario")) %>%
    tidyr::spread(scenario, value, fill = NA) %>%
    dplyr::mutate(diff = (com - ref) / ref * 100)  # percent difference


  ggplot2::ggplot(f, ggplot2::aes(x = year, y = diff, color = floor_type)) +
    ggplot2::geom_path() +
    ggplot2::facet_wrap( ~ facet_var) +
    ggplot2::xlab("Year") + ggplot2::ylab(ylabel) +
    ggplot2::theme_bw() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30))
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
          dplyr::mutate(scenario = names(dbset)[[i]])
      )
    ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate_(facet_var = "facet_var")

    if(variable == "rent"){
      df <- df %>% dplyr::mutate(floor = price)
      ylabel <- "Rent price"
    } else if(variable == "occupancy"){
      df <- df %>% dplyr::mutate(floor = occrate)
      ylabel <- "Occupancy Rate"
    }


  } else {
    df <- bind_rows(
      lapply(seq_along(dbset), function(i)
        extract_floorspace(dbset[[i]], facet_var, facet_levels, type_levels) %>%
          dplyr::mutate(scenario = names(dbset)[[i]])
      )
    ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate_(facet_var = "facet_var") %>%
      dplyr::mutate(floor = built)

    ylabel <- "New floor space"
  }

  ggplot2::ggplot(
    df,
    ggplot2::aes_string(x = "year", y = "floor", color = "scenario")
  ) +
    ggplot2::geom_path() +
    ggplot2::facet_grid(facet_var ~ floor_type, scale = "free_y") +
    ggplot2::xlab("Year") + ggplot2::ylab(ylabel) +
    ggplot2::scale_x_log10() +
    ggplot2::theme_bw() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30))

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

  ggplot2::ggplot(rents,  ggplot2::aes(x = year, y = occrate, color = floor_type) ) +
    ggplot2::geom_path() +
    ggplot2::facet_wrap(~ facet_var) +
    ggplot2::xlab("Year") + ggplot2::ylab("Occupancy Rate") +
    ggplot2::theme_bw() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30))
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
    dplyr::select(facet_var, year, floor_type, rate_ref = occrate)

  fcom <- extract_rents(db2, facet_var, facet_levels, type_levels, index = FALSE) %>%
    dplyr::select(facet_var, year, floor_type, rate_com = occrate)

  df <- dplyr::left_join(fref, fcom) %>%
    dplyr::mutate(pct_diff = (rate_com - rate_ref) / rate_ref * 100)

  ggplot2::ggplot(df,  ggplot2::aes(x = year, y = pct_diff, color = floor_type) ) +
    ggplot2::geom_path() +
    ggplot2::facet_wrap(~ facet_var) +
    ggplot2::xlab("Year") + ggplot2::ylab("Percent difference in occupancy rate") +
    ggplot2::theme_bw() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30))

}
