#' Plot floorspace over time
#'
#' This function plots the available floorspace by type over time facetted by a
#' variable chosen from the zone attributes table.
#'
#' @param db The scenario database.
#' @param facet_var The variable in the zone table to facet by. Defaults to MPO
#' @param facet_levels The levels of the facet variable to keep. Defaults to all
#'   levels other than external stations.
#'
#'
#' @return A ggplot2 object showing the floorspace by type and and year.
#'
#' @export
plot_floorspace <- function(db,
                            facet_var = c("MPO", "COUNTY", "STATE"),
                            facet_levels = NULL
                            ){


  # set facet variable; if null then default to MPO
  if(is.null(facet_var)){
    facet_var = "MPO"
  }

  grouping <- tbl(db, "BZONE") %>%
    select_("BZONE", "facet_var" = facet_var)

  # get levels of facet_var if none given
  if(is.null(facet_levels)){
    facet_levels <- grouping %>% group_by(facet_var) %>% collect() %>%
      slice(1) %>% .$facet_var

    facet_levels <- facet_levels[which(facet_levels != "EXTSTA")]
  }


  # get floorspace table and compute summary
  floorspace <- tbl(db, "FLR_INVENTORY") %>%
    transmute(
      BZONE,
      year = TSTEP + 1990,
      commodity = COMMODITY,
      floor = FLR, built = INCREMENT
    ) %>%

    # join facet and filter desired levels
    left_join(grouping, by = "BZONE") %>%
    filter(facet_var %in% facet_levels) %>%

    # sum within facet, year, and type
    group_by(facet_var, year, commodity) %>%
    summarise_each(funs(sum), floor:built) %>%
    collect()


  # make plot
  ggplot(floorspace,
         aes(x = as.numeric(year), y = floor,
             group = commodity, color = commodity)) +
    geom_path()  +
    facet_wrap( ~ facet_var) +

    scale_y_log10() +
    theme_bw()

}
