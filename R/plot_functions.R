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
#' @import dplyr
yearly_summary <- function(df, group, var){
  df %>%
    group_by_(group, "TSTEP") %>%
    mutate_("var" = var) %>%
    summarise(var = sum(var, na.rm = TRUE)) %>%
    mutate(TSTEP = as.numeric(TSTEP) + 1990) %>%
    spread(TSTEP, var, fill = NA)
}


#' Make a plot of population and employment
#'
#' @param df A \code{data_frame} of
#'
#' @import ggplot2
plot_sevar <- function(df, which_county, controls = TRUE){
  # prepare data_frame for plotting
  df <- df %>% ungroup() %>%
    filter(county %in% which_county) %>%
    select(year, population, employment) %>%
    gather(var, y, population:employment) %>%
    mutate(data = "SWIM")

  # add county controls
  if(controls){
    ct <- county_controls %>%
      filter(county %in% which_county) %>%
      select(year, var, y) %>%
      mutate(data = "Control")
    df <- rbind_list(df, ct)
  }

  # create plot frame
  p <- ggplot(df) +
    geom_line(aes(x = year, y = y, color = data, lty = data))

  # theme, etc
  p +
    facet_grid(. ~ var, scales = "free_y") +
    xlab("Year") + ylab("Count") +
    ggtitle(which_county) +
    theme_bw()
}
