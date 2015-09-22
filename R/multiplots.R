#' Compare Population or Employment across multiple scenarios.
#'
#' @param dbset A list of connections to SWIM databases.
#' @param db_names A character vector naming the scenarios.
#' @param variable One of \code{c("population", "employment")} defining which
#'   socioeconomic variable to include in
#' @param facet_var Field to facet by: either "MPO" or "COUNTY".
#' @param facet_levels A character vector of the facet variable specifiying
#'   which levels to include.
#' @param controls If \code{facet_var = "COUNTY" & variable = "employment"} then
#'   can print OEA forecast.
#'
#' @return a ggplot2 object.
#' @export
#'
multiple_sevar <- function(dbset, db_names,
                           variable = c("population", "employment"),
                           facet_var = c("MPO", "COUNTY"),
                           controls = FALSE,
                           facet_levels = NULL ) {

  # only allow controls if possible
  if(controls){
    if(facet_var != "COUNTY" & variable != "population"){
      stop("Controls only available for count-level population forecasts.")
    }
  }

  # get the population table for every scenario.
  names(dbset) <- db_names
  df <- rbind_all(
    lapply(seq_along(dbset), function(i)
      extract_se(dbset[[i]], facet_var, facet_levels, controls) %>%
        mutate(scenario = names(dbset)[[i]]) %>%
        filter(var == variable)
    )
  ) %>%
    mutate_("facet_var" = tolower(facet_var))


  p <- ggplot(
    df,
    aes(x = year, y = y, color = scenario)
  ) +
    geom_path() +
    facet_wrap(~ facet_var, scales = "free_y") +
    ylab(variable) + xlab("Year") +
    theme_bw()

  return(p)

}
