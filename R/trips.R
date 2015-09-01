#' Extract trips from scenario
#'
#' @param db the scenario database.
#' @param facet_var Defaults to MPO
#' @param facet_levels defaults to all
#'
#' @import dplyr
#' @import tidyr
#' @export
extract_trips <- function(db, facet_var = "MPO", facet_levels = NULL){

  df <- tbl(db, "TRIPMATRIX") %>%
    # sum trips on origin
    mutate(BZONE = FROMBZONE) %>%
    select(-FROMBZONE, -TOBZONE) %>%
    left_join(tbl(db, "BZONE") %>% select_("BZONE", "facet_var" = facet_var),
              by = "BZONE")

  # if no levels specified, then keep all
  if(!is.null(facet_levels)){
    df <- df %>% filter(facet_var %in% facet_levels)
  }

  df <- df %>%
    group_by(facet_var, TSTEP) %>%
    summarise_each(funs(sum), am_BIKE:am_WK_TRAN) %>%

    ungroup() %>% collect() %>%
    mutate(year = as.numeric(TSTEP) + 1990) %>%
    select(-TSTEP) %>%

    # combine periods
    gather(mode, trips, -facet_var, -year) %>%
    separate(mode, into = c("period", "mode"), sep = "_", extra = "merge") %>%

    group_by(facet_var, year, mode) %>%
    summarise(trips = sum(trips))

}

#' Plot Trips by MPO
#'
#' @param db the scenario database.
#' @param facet_var Defaults to MPO
#' @param facet_levels defaults to all
#' @param share Plot mode split.
#'
#' @import dplyr
#' @import tidyr
#' @export
plot_trips <- function(db, facet_var = "MPO", facet_levels = NULL,
                       share = TRUE) {

  df <- extract_trips(db, facet_var, facet_levels)

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
      scale_y_log10() + ylab("Trips Produced")
  }

  p +
    geom_path()  +
    facet_wrap( ~ facet_var) +
    xlab("Year") +
    theme_bw()

}

#' Compare Trips
#'
#'
#' @param db1 The swim database for the "Reference" scenario.
#' @param db2 The swim database for the "Current" scenario.
#' @param facet_var Field to facet by: either "MPO" or "COUNTY".
#' @param facet_levels A character vector of the facet variable specifiying
#'   which levels to include.
#'
#' @export
compare_trips <- function(db1, db2, facet_var = "MPO", facet_levels = NULL,
                          share = TRUE){

  # reference scenario
  fref <- extract_trips(db1, facet_var, facet_levels) %>%
    rename(ref = trips)
  # current scenario
  fcom <- extract_trips(db2, facet_var, facet_levels) %>%
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
    ylab("Percent difference in number of trips produced (current - reference)") +
    theme_bw()

}
