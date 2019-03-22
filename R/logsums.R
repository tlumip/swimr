#' Extract destination choice logsums
#'
#' The logsums are provided at production-side beta zones, by purpose and market
#' segment. This function aggregates market segments by averaging and purposes
#' by summation (sum of all purpose logsums faced by the average user).
#'
#' @param db The scenario sqlite database.
#' @param scope a dplyr::filtering criteria to limit the scope of the dataframe
#' @param purposes a vector of trip purposes to include in the average logsum.
#' @param agg_var The region variable on which to aggregate logsums.
#'
#' @export
#' @return a data_frame
extract_logsums <- function(db, scope = NULL, purposes = NULL,
                            agg_var = "AZONE"){

  if(is.null(purposes)){
    # all purposes: currently no WORK_BASED
    df <- dplyr::tbl(db, "DC_LOGSUM") %>%
      dplyr::filter(PURPOSE != "WORK_BASED")
  } else if(length(purposes) == 1){
    # dplyr::filtering on one category requires different handling:
    # https://github.com/hadley/dplyr/issues/1428
    df <- dplyr::tbl(db, "DC_LOGSUM") %>%
      dplyr::filter(PURPOSE == purposes)
  } else {
    df <- dplyr::tbl(db, "DC_LOGSUM") %>%
      dplyr::filter(PURPOSE %in% purposes)
  }

  zones_data <- extract_zones(db=db)@data

  if(!is.null(scope)){
    # Get a list of zones in the scope
     zt <- zones_data %>%
      dplyr::filter_(.dots = scope)
  } else {
    zt <- zones_data
  }

  df %>%
    # collapse market segments
    dplyr::group_by(BZONE, PURPOSE, TSTEP) %>%
    dplyr::summarize(logsum = mean(AVGLOGSUM)) %>%

    # collapse purposes
    dplyr::group_by(BZONE, TSTEP) %>%
    dplyr::summarize(logsum = sum(logsum)) %>%

    dplyr::collect(n=Inf) %>%
    dplyr::mutate(year = as.numeric(TSTEP) + 1990) %>%

    # trim to scope
    inner_join(zt) %>% dplyr::ungroup() %>%

    # collapse to aggregation level
    dplyr::group_by_(agg_var, "year") %>%
    dplyr::summarize(logsum = mean(logsum)) %>%

    dplyr::select_(agg_var, "year", "logsum") %>%
    dplyr::ungroup()


}

#' Map destination choice logsums from a single scenario
#'
#' @inheritParams extract_logsums
#' @param ggmap If TRUE, then include a ggmap background.
#' @param show_year The year in which to plot the logsums.
#'
#' @export
map_logsums <- function(db, scope = NULL, purposes = NULL, ggmap = FALSE,
                        show_year = 2010){

  df <- extract_logsums(db, scope = scope, purposes = purposes,
                        agg_var = "BZONE") %>%
    dplyr::filter(year == show_year) %>%
    dplyr::mutate(logsum = cut_number(logsum, 5))

  dt <- zones %>%
    inner_join(df, by = "BZONE")

  if(ggmap){
    map <- get_map(
      sp::bbox(as.matrix(dt[, c("long", "lat")])),
      source = "stamen", color = "bw", maptype = "toner"
    )

    p <- ggmap(map, extent = "dev") +
      ggplot2::theme_bw() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30))
  } else {
    p <- ggplot2::ggplot() +
      coord_map("conic", lat0 = 43) +
      ggplot2::theme_bw() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30))
  }

  p + geom_polygon(
    data = dt,
    alpha = 0.3,
    ggplot2::aes_string(x = "long", y = "lat", fill = "logsum", group = "group")
  ) +
    ggplot2::scale_fill_brewer(paste(year, "logsums"), palette = "BrBG")
}



#' Plot average logsums over time for a single scenario
#'
#' @inheritParams extract_logsums
#'
#' @param color_var The variable to color the plot by.
#' @param color_levels The variable to
#'
#' @export
plot_logsums <- function(db,
                         color_var = c("COUNTY", "MPO", "ALDREGION",
                                       "STATE", "DOT_REGION"),
                         color_levels = NULL){

  df <- extract_logsums(db, scope = NULL, purposes = NULL,
                        agg_var = color_var)

  # if no levels given, then  use all
  if(!is.null(color_levels)){
    df <- df %>%
      dplyr::mutate_("color_var" = color_var) %>%
      dplyr::filter(color_var %in% color_levels)
  }

  ggplot2::ggplot(df,
         ggplot2::aes_string(x = "year", y = "logsum", color = color_var)) +
    geom_line() +
    ggplot2::scale_color_discrete(color_var) +
    ggplot2::xlab("Year") + ggplot2::ylab("Average destination choice log sum") +
    ggplot2::theme_bw() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30))

}


#' Compare logsums from two scenarios
#'
#' @inheritParams plot_logsums
#' @param db1 The reference scenario VIZ database.
#' @param db2 The current scenario VIZ database.
#'
#' @export
compare_logsums <- function(db1, db2,
                            color_var = c("COUNTY", "MPO", "ALDREGION",
                                          "STATE", "DOT_REGION"),
                            color_levels = NULL){


  ref <- extract_logsums(db1, scope = NULL, purposes = NULL,
                         agg_var = color_var) %>%
    dplyr::rename(ref = logsum)

  cur <- extract_logsums(db2, scope = NULL, purposes = NULL,
                         agg_var = color_var) %>%
    dplyr::rename(cur = logsum)

  df <- dplyr::left_join(ref, cur) %>%
    dplyr::mutate(pct_diff = (cur - ref) / ref * 100)

  # if no levels given, then  use all
  if(!is.null(color_levels)){
    df <- df %>%
      dplyr::mutate_("color_var" = color_var) %>%
      dplyr::filter(color_var %in% color_levels)
  }

  ggplot2::ggplot(df,
         ggplot2::aes_string(x = "year", y = "pct_diff", color = color_var)) +
    geom_line() +
    ggplot2::scale_color_discrete(color_var) +
    ggplot2::xlab("Year") + ggplot2::ylab("Percent difference between average logsums") +
    ggplot2::theme_bw() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30))

}
