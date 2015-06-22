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
#' @param df A \code{data_frame} consisting of at least the following variables:
#'   county, year, population, employment.
#'
#' @param which_county A character vector of county names whose values should
#'   be included in the plot.
#'
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
plot_sevar <- function(df, which_county, controls = TRUE){
  # prepare data_frame for plotting
  df <- df %>% ungroup() %>% collect() %>%
    filter(county %in% which_county) %>%
    select(county, year, population, employment) %>%
    gather(var, y, population:employment) %>%
    mutate(data = "SWIM")

  # add county controls
  if(controls){
    ct <- county_controls %>%
      filter(county %in% which_county) %>%
      select(county, year, var, y) %>%
      mutate(data = "Control")
    df <- rbind_list(df, ct)
  }

  # create plot frame
  p <- ggplot(df) +
    geom_line(aes(x = year, y = y, color = county, lty = data))

  # theme, etc
  p +
    facet_grid(. ~ var, scales = "free_y") +
    xlab("Year") + ylab("Count") +
    theme_bw()
}
