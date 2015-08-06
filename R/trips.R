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


  df <- tbl(db, "TRIPMATRIX") %>%
    # sum trips on origin
    mutate(BZONE = FROMBZONE) %>%
    select(-FROMBZONE, -TOBZONE) %>%
    left_join(tbl(db, "BZONE") %>% select_("BZONE", "facet_var" = facet_var),
              by = "BZONE") %>%

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
