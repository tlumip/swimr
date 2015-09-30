#' Extract destination choice logsums
#'
#' The logsums are provided at production-side beta zones, by purpose and market
#' segment. This function aggregates market segments by averaging and purposes
#' by summation (sum of all purpose logsums faced by the average user).
#'
#' @param db The scenario sqlite database.
#' @param scope a filtering criteria to limit the scope of the dataframe
#' @param purposes a vector of trip purposes to include in the average logsum.
#' @param agg_var The region variable on which to aggregate logsums.
#'
#' @export
#'
#' @return a ggmap object
#' @import dplyr
#'
extract_logsums <- function(db, scope = NULL, purposes = NULL,
                            agg_var = "AZONE"){

  if(is.null(purposes)){
    # all purposes: currently no WORK_BASED
    df <- tbl(db, "DC_LOGSUM") %>%
      filter(PURPOSE != "WORK_BASED")
  } else if(length(purposes) == 1){
    # filtering on one category requires different handling:
    # https://github.com/hadley/dplyr/issues/1428
    df <- tbl(db, "DC_LOGSUM") %>%
      filter(PURPOSE == purposes)
  } else {
    df <- tbl(db, "DC_LOGSUM") %>%
      filter(PURPOSE %in% purposes)
  }

  if(!is.null(scope)){
    # Get a list of zones in the scope
    zt <- zones_data %>%
      filter_(.dots = scope)
  } else {
    zt <- zones_data
  }

  df %>%
    # collapse market segments
    group_by(BZONE, PURPOSE, TSTEP) %>%
    summarise(logsum = mean(AVGLOGSUM)) %>%

    # collapse purposes
    group_by(BZONE, TSTEP) %>%
    summarise(logsum = sum(logsum)) %>%

    collect() %>%
    mutate(year = as.numeric(TSTEP) + 1990) %>%

    # trim to scope
    inner_join(zt) %>% ungroup() %>%

    # collapse to aggregation level
    group_by_(agg_var, "year") %>%
    summarise(logsum = mean(logsum)) %>%

    select_(agg_var, "year", "logsum")


}

#' Map destination choice logsums from a single scenario
#'
#' @inheritParams extract_logsums
#' @param ggmap If TRUE, then include a ggmap background.
#' @param year The year in which to plot the logsums.
#'
#' @import ggmap
#' @import ggplot2
#' @importFrom sp bbox
#' @export
map_logsums <- function(db, scope = NULL, purposes = NULL, ggmap = FALSE,
                         year = 2010){

  df <- extract_logsums(db, scope, purposes) %>%
    filter(year == year) %>%
    mutate(logsum = cut_interval(logsum, 5))

  dt <- zones %>%
    inner_join(df, by = "AZONE")

  if(ggmap){
    map <- get_map(
      sp::bbox(as.matrix(dt[, c("long", "lat")])),
      source = "stamen", color = "bw", maptype = "toner"
    )

    p <- ggmap(map, extent = "dev") +
      theme_bw()
  } else {
    p <- ggplot() +
      coord_map("conic", lat0 = 43) +
      theme_bw()
  }

  p + geom_polygon(
    data = dt,
    alpha = 0.3,
    aes_string(x = "long", y = "lat", fill = "logsum", group = "group")
  ) +
    scale_fill_brewer(paste(year, "logsums"), palette = "BrBG")
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
      mutate_("color_var" = color_var) %>%
      filter(color_var %in% color_levels)
  }

  ggplot(df,
         aes_string(x = "year", y = "logsum", color = color_var)) +
    geom_line() +
    scale_color_discrete(color_var) +
    xlab("Year") + ylab("Average destination choice log sum") +
    theme_bw()

}


#' Compare logsums from two scenarios
#'
#' @inheritParams plot_logsums
#'
#' @export
compare_logsums <- function(db1, db2,
                            color_var = c("COUNTY", "MPO", "ALDREGION",
                                          "STATE", "DOT_REGION"),
                            color_levels = NULL){


  ref <- extract_logsums(db1, scope = NULL, purposes = NULL,
                         agg_var = color_var) %>%
    rename(ref = logsum)

  cur <- extract_logsums(db2, scope = NULL, purposes = NULL,
                         agg_var = color_var) %>%
    rename(cur = logsum)

  df <- left_join(ref, cur) %>%
    mutate(pct_diff = (cur - ref) / ref * 100)

  # if no levels given, then  use all
  if(!is.null(color_levels)){
    df <- df %>%
      mutate_("color_var" = color_var) %>%
      filter(color_var %in% color_levels)
  }

  ggplot(df,
         aes_string(x = "year", y = "pct_diff", color = color_var)) +
    geom_line() +
    scale_color_discrete(color_var) +
    xlab("Year") + ylab("Percent difference between average logsums") +
    theme_bw()

