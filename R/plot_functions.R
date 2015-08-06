#' Make a yearly summary table of a land use variable
#'
#' @param df A \code{data_frame} of zones, such as is stored in the sqlite
#'   database.
#' @param group The variable to use for grouping the zones (for instance,
#'   "County").
#'
#' @param var The variable to sum (for instaance, "POPULATION")
#'
#' @return A \code{data_frame} with the grouping variable in each row, and the
#' value of the variable at each time stage going across the columns.
#'
#' @export
#'
#' @import dplyr
#' @import tidyr
yearly_summary <- function(df, group, var){
  df %>%
    group_by_(group, "year") %>%
    mutate_("var" = var) %>%
    summarise(var = sum(var)) %>%
    collect() %>%
    spread(year, var, fill = NA)
}


#' Make a plot of population and employment
#'
#' @param db The scenario sqlite database.
#' @param color_var Field to color by: either "MPO" or "COUNTY".
#' @param color_levels A character vector of the color variable specifiying
#'   which levels to include.
#' @param controls Plot against the control totals. Defaults to TRUE, cannot
#'   currently run with FALSE.
#'
#' @return A \code{ggplot2} plot object showing the modeled change in employment
#'   and population over time.
#'
#' @export
#'
#' @import ggplot2
#' @import tidyr
#' @import dplyr
plot_sevar <- function(db, color_var = c("MPO", "COUNTY"),
                       color_levels = NULL, controls = TRUE
                       ){

  # set color variable; if null then default to County
  if(is.null(color_var)){
    color_var = "COUNTY"
  }

  # county plot with controls
  if(color_var == "COUNTY"){

    # Pull se data
    df <- tbl(db, "AZONE") %>%
      select(AZONE, POPULATION, EMPLOYMENT, TOTALHHS, TSTEP) %>%
      # join information for county and state
      left_join(
        tbl(db, "ALLZONES") %>%
          select(AZONE = Azone, county = COUNTY, state = STATE)
      ) %>%

      # summarize to the county level
      group_by(county, state, TSTEP) %>%
      summarise(
        population = sum(POPULATION),
        employment = sum(EMPLOYMENT),
        totalhh = sum(TOTALHHS)
      ) %>%
      mutate(year = as.numeric(TSTEP) + 1990) %>%
      ungroup() %>% collect()

    # if no levels specified, then keep all
    if(!is.null(color_levels)){
      df <- filter(df, county %in% color_levels)
    }

    df <- df %>%
      select(county, year, population, employment) %>%
      gather(var, y, population:employment) %>%
      mutate(data = "SWIM")

    # add county controls
    if(controls){
      ct <- county_controls %>%
        filter(county %in% df$county) %>%
        select(county, year, var, y) %>%
        mutate(data = "Control")

      df <- rbind(df, ct)
    }

  } else {
    # get levels of facet_var if none given
    if(is.null(color_levels)){
      color_levels <- grouping %>% group_by(color_var) %>% collect() %>%
        slice(1) %>% .$color_var

      color_levels <- color_levels[which(color_levels != "EXTSTA")]
    }
  }

  # create plot frame
  p <- ggplot(df) +
    geom_line(aes(x = year, y = y, color = county, lty = data)) +
    scale_linetype_manual("Data", values = c("dashed", "solid"))

  # theme, etc
  p +
    facet_grid(. ~ var, scales = "free_y") +
    xlab("Year") + ylab("Count") +
    theme_bw()
}
