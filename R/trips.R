#' Extract trips from scenario
#'
#' This function looks at the trip matrix for a scenario and summarizes trips on
#' the origin end by mode and region of origin.
#'
#' @param db the scenario database.
#' @param facet_var The region to summarize by.
#' @param facet_levels Regions to include in summary.
#' @param color_levels Modes to include in summary. Defaults to all modes other
#'   than \code{school}. See consolidated modes in \link{mode_types}.
#' @param index whether to index mode split off the base year.
#'
#' @return A data frame with total trips originating in the region by mode and
#'   year.
#'
#' @import dplyr
#' @import tidyr
#' @export
extract_trips <- function(db,
                          facet_var = c("MPO", "COUNTY", "STATE"),
                          facet_levels = NULL,
                          color_levels = c("auto", "transit",
                                           "non-motorized", "truck"),
                          index = FALSE){

  # Get lookup table of zones to grouping variable.
  df <- tbl(db, "TRIPMATRIX") %>%
    # determine the origin MPO of the trip
    mutate(BZONE = FROMBZONE) %>%
    select(-FROMBZONE, -TOBZONE) %>%
    left_join(
      tbl(db, "BZONE") %>%
        select_("BZONE", "facet_var" = facet_var), by = "BZONE") %>%
    select(-BZONE)


  # if no levels specified, then keep all
  if(!is.null(facet_levels)){
    df <- df %>% filter(facet_var %in% facet_levels)
  }


  df <- df %>%
    group_by(facet_var, TSTEP) %>%
    summarise_each(funs(sum), -TSTEP, -facet_var) %>%
    ungroup() %>% collect(n=Inf) %>%

    # combine periods
    gather(mode, trips, am_BIKE:pm_WK_TRAN) %>%
    mutate(year = as.numeric(TSTEP) + 1990) %>%
    select(-TSTEP) %>%
    separate(mode, into = c("period", "mode"), sep = "_", extra = "merge") %>%
    filter(mode != "SCHOOL_BUS") %>%

    # join consolidated mode information
    group_by(facet_var, year, mode) %>%
    summarise(trips = sum(trips)) %>%

    #consolidate modes
    left_join(mode_types) %>%
    mutate(mode = consolidated_mode) %>%
    filter(mode %in% color_levels) %>%
    group_by(facet_var, year, mode) %>%
    summarise(trips = sum(trips)) %>%
    ungroup()

  if(index){
    df <- df %>%
      group_by(facet_var, mode) %>%
      mutate(trips = calc_index(trips))
  }

  return(df)

}

#' Plot Trip productions
#'
#' @param db the scenario database.
#' @param facet_var The region to summarize by.
#' @param facet_levels Regions to include in summary.
#' @param color_levels Modes to include in summary. Defaults to all modes other
#'   than \code{school}. See consolidated modes in \link{mode_types}.
#' @param share Plot mode share instead of total trips? Defaults to \code{TRUE}.
#' @param index whether to index mode split off the base year.
#'
#' @return A ggplot2 object.
#'
#' @import dplyr
#' @import tidyr
#' @export
plot_trips <- function(db,
                       facet_var = c("MPO", "COUNTY", "STATE"),
                       facet_levels = NULL,
                       color_levels = c("auto", "transit",
                                        "non-motorized", "truck"),
                       share = TRUE, index = TRUE){

  df <- extract_trips(db, facet_var, facet_levels, color_levels, index)

  if(share) {
    df <- df %>%
      group_by(facet_var, year) %>%
      mutate(share = trips / sum(trips))

    p <- ggplot(df,
         aes(x = as.numeric(year), y = share,
             group = mode, color = mode)) +
       ylab("Mode Split")

  } else {
    p <- ggplot(df,
           aes(x = as.numeric(year), y = trips,
               group = mode, color = mode)) +
      ylab(ifelse(index, "Trips Indexed to Base Year",
                  "Total Trips"))
  }

  p +
    geom_path()  +
    facet_wrap( ~ facet_var) +
    xlab("Year") +
    theme_bw() + theme(axis.text.x = element_text(angle = 30))

}

#' Compare Trips
#'
#' @param db1 The swim database for the "Reference" scenario.
#' @param db2 The swim database for the "Current" scenario.
#' @param facet_var The region to summarize by.
#' @param facet_levels Regions to include in summary.
#' @param color_levels Modes to include in summary. Defaults to all modes other
#'   than \code{school}. See consolidated modes in \link{mode_types}.
#'
#' @return A ggplot2 object.
#'
#' @export
compare_trips <- function(db1, db2,
                          facet_var = c("MPO", "COUNTY", "STATE"),
                          facet_levels = NULL,
                          color_levels = c("auto", "transit",
                                           "non-motorized", "truck")){

  # reference scenario
  fref <- extract_trips(db1, facet_var, facet_levels, color_levels, index = TRUE) %>%
    rename(ref = trips)
  # current scenario
  fcom <- extract_trips(db2, facet_var, facet_levels, color_levels, index = TRUE) %>%
    rename(com = trips)

  df <- left_join(fref, fcom) %>%
    mutate(
      diff = (com - ref) / ref * 100,
      diff = ifelse(is.na(diff), 0, diff)
    )

  # plot of percent difference
  p <- ggplot(df,
              aes(x = year, y = diff, color = mode)) +
    geom_path() +
    facet_wrap(~ facet_var)

  p +
    xlab("Year") +
    ylab("Percent difference in trip productions (current - reference)") +
    theme_bw() + theme(axis.text.x = element_text(angle = 30))

}


#' Plot trips in multiple scenarios.
#'
#' @param dbset A list of connections to SWIM databases.
#' @param db_names A character vector naming the scenarios.
#' @param facet_var The region to summarize by.
#' @param facet_levels Regions to include in summary.
#' @param color_levels Modes to include in summary. Defaults to all modes other
#'   than \code{school}. See consolidated modes in \link{mode_types}.
multiple_trips <- function(dbset, db_names,
                           facet_var = c("MPO", "COUNTY", "STATE"),
                           facet_levels = NULL,
                           color_levels = c("auto", "transit",
                                            "non-motorized", "truck")){

  # get the trips table for every scenario.
  names(dbset) <- db_names
  df <- bind_rows(
    lapply(seq_along(dbset), function(i)
      extract_trips(dbset[[i]], facet_var, facet_levels, color_levels) %>%
        mutate(scenario = names(dbset)[[i]])
    )
  )

  p <- ggplot(
    df,
    aes(x = year, y = trips, color = scenario)
  )

  p + geom_path() +
    facet_grid(mode ~ facet_var, scales = "free_y") +
    ylab("Total Trips") + xlab("Year") +
    theme_bw() + theme(axis.text.x = element_text(angle = 30))

}

