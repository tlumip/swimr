#' Extract trips from scenario
#'
#' This function looks at the trip matrix for a scenario and dplyr::summarizes trips on
#' the origin end by mode and region of origin.
#'
#' @param db the scenario database.
#' @param facet_var The region to dplyr::summarize by.
#' @param facet_levels Regions to include in summary.
#' @param color_levels Modes to include in summary. Defaults to all modes other
#'   than \code{school}. See consolidated modes in \link{mode_types}.
#' @param index whether to index mode split off the base year.
#'
#' @return A data frame with total trips originating in the region by mode and
#'   year.
#'
#' @export
extract_trips <- function(db,
                          facet_var = NULL,
                          facet_levels = NULL,
                          color_levels = c("auto", "transit",
                                           "non-motorized", "truck"),
                          index = FALSE){

  if(is.null(facet_var)){ facet_var = "MPO" }

  # Get lookup table of zones to grouping variable.
  df <- dplyr::tbl(db, "TRIPMATRIX") %>%
    # determine the origin MPO of the trip
    dplyr::mutate(BZONE = FROMBZONE) %>%
    dplyr::select(-FROMBZONE, -TOBZONE) %>%
    dplyr::left_join(
      dplyr::tbl(db, "BZONE") %>%
        dplyr::select_("BZONE", "facet_var" = facet_var), by = "BZONE") %>%
    dplyr::select(-BZONE)


  # if no levels specified, then keep all
  if(!is.null(facet_levels)){
    df <- df %>% dplyr::filter(facet_var %in% facet_levels)
  }


  df <- df %>%
    dplyr::group_by(facet_var, TSTEP) %>%
    dplyr::summarize_all(funs(sum, .args=list(na.rm=TRUE))) %>%
    dplyr::ungroup() %>% dplyr::collect(n=Inf) %>%

    # combine periods
    tidyr::gather(mode, trips, am_BIKE:pm_WK_TRAN) %>%
    dplyr::mutate(year = as.numeric(TSTEP) + 1990) %>%
    dplyr::select(-TSTEP) %>%
    tidyr::separate(mode, into = c("period", "mode"), sep = "_", extra = "merge") %>%
    dplyr::filter(mode != "SCHOOL_BUS") %>%

    # join consolidated mode information
    dplyr::group_by(facet_var, year, mode) %>%
    dplyr::summarize(trips = sum(trips, na.rm = TRUE)) %>%

    #consolidate modes
    dplyr::left_join(mode_types, by = "mode") %>%
    dplyr::mutate(mode = consolidated_mode) %>%
    dplyr::filter(mode %in% color_levels) %>%
    dplyr::group_by(facet_var, year, mode) %>%
    dplyr::summarize(trips = sum(trips, na.rm = TRUE)) %>%
    dplyr::ungroup()

  if(index){
    df <- df %>%
      dplyr::group_by(facet_var, mode) %>%
      dplyr::mutate(trips = calc_index(trips))
  }

  return(df)

}

#' Plot Trip productions
#'
#' @inheritDotParams extract_trips
#' @param share Plot mode share instead of total trips? Defaults to \code{TRUE}.
#'
#' @return A ggplot2 object.
#'
#' @export
plot_trips <- function(..., share = TRUE){

  df <- extract_trips(...)
  dots <- list(...)

  if(share) {
    df <- df %>%
      dplyr::group_by(facet_var, year) %>%
      dplyr::mutate(share = trips / sum(trips))

    p <- ggplot2::ggplot(df,
         ggplot2::aes(x = as.numeric(year), y = share,
             group = mode, color = mode)) +
       ggplot2::ylab("Mode Split")

  } else {
    p <- ggplot2::ggplot(df,
           ggplot2::aes(x = as.numeric(year), y = trips,
               group = mode, color = mode)) +
      ggplot2::ylab(ifelse(dots$index, "Trips Indexed to Base Year",
                  "Total Trips"))
  }

  p +
    ggplot2::geom_path()  +
    ggplot2::facet_wrap( ~ facet_var) +
    ggplot2::xlab("Year") +
    ggplot2::theme_bw() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30))

}

#' Compare Trips
#'
#' @param db1 The swim database for the "Reference" scenario.
#' @param db2 The swim database for the "Current" scenario.
#' @inheritDotParams extract_trips
#' @param diff_type Character string saying whether to use abolute (`diff`) or
#'   percent (`pct_diff`) differences. Defaults to percent.
#'
#' @return A ggplot2 object.
#'
#' @export
compare_trips <- function(db1, db2, ..., diff_type = "pct_diff"){

  # reference scenario
  fref <- extract_trips(db = db1, ...) %>% dplyr::rename(ref = trips)
  # current scenario
  fcom <- extract_trips(db = db2, ...) %>% dplyr::rename(com = trips)

  df <- dplyr::left_join(fref, fcom, by = c("facet_var", "year", "mode")) %>%
    dplyr::mutate(
      diff = com - ref,
      pct_diff = diff / ref * 100
    )

  # plot of percent difference
  p <- ggplot2::ggplot(df, ggplot2::aes_string(
    x = "year", y = diff_type, color = "mode")) +
    ggplot2::geom_path() +
    ggplot2::facet_wrap(~ facet_var)

  p +
    ggplot2::xlab("Year") +
    ggplot2::ylab(ifelse(diff_type == "pct_diff", "Percent Difference",
                         "Difference in Trips")) +
    ggplot2::theme_bw() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30))

}


#' Plot trips in multiple scenarios.
#'
#' @param dbset A list of connections to SWIM databases.
#' @param db_names A character vector naming the scenarios.
#' @inheritDotParams extract_trips
#'
#' @return A ggplot2 object
#'
#' @export
multiple_trips <- function(dbset, db_names, ...){

  # get the trips table for every scenario.
  names(dbset) <- db_names
  df <- bind_rows(
    lapply(seq_along(dbset), function(i)
      extract_trips(dbset[[i]], facet_var, facet_levels, color_levels) %>%
        dplyr::mutate(scenario = names(dbset)[[i]])
    )
  )

  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = year, y = trips, color = scenario)
  )

  p + ggplot2::geom_path() +
    ggplot2::facet_grid(mode ~ facet_var, scale = "free_y") +
    ggplot2::ylab("Total Trips") + ggplot2::xlab("Year") +
    ggplot2::theme_bw() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30))

}

