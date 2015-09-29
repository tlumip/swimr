#' Extract destination choice logsums
#'
#' The logsums are provided at production-side beta zones, by purpose and market
#' segment. This function aggregates market segments by averaging and purposes
#' by summation (sum of all purpose logsums faced by the average user).
#'
#' @param db The scenario sqlite database.
#' @param scope a filtering criteria to limit the scope of the dataframe
#' @param purposes a vector of trip purposes to include in the average logsum.
#'
#' @export
#'
#' @return a ggmap object
#' @import dplyr
#'
extract_logsums <- function(db, scope = NULL, purposes = NULL){

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
    select(AZONE, year, logsum)

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



#
