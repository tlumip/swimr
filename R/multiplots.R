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
#'
#' @export
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
    mutate_("facet_var" = names(.)[1])


  # add control data if desired
  if(controls){
    p <- ggplot(
      data = df %>%
        # only need control once
        filter(data != "Control" | scenario == names(dbset)[1]) %>%
        mutate(scenario = ifelse(data == "Control", "Control", scenario)),
      aes(x = year, y = y, color = scenario, lty = data)
    ) +
      scale_linetype_manual("source",
                            values = c("dotted", rep("solid", length(dbset))))
  } else {
    p <- ggplot(
      data = df %>%
        filter(data != "Control"),
      aes(x = year, y = y, color = scenario)
    )

  }

  p <- p + geom_path() +
    facet_wrap(~ facet_var, scales = "free_y") +
    ylab(variable) + xlab("Year") +
    theme_bw()

  return(p)

}


#' Compare WAPR  across multiple scenarios.
#'
#' @param dbset A list of connections to SWIM databases.
#' @param db_names A character vector naming the scenarios.
#  @param facet_var Field to facet by.
#' @param facet_levels A character vector of the facet variable specifiying
#'   which levels to include.
#' @param controls If \code{facet_var = "COUNTY" & variable = "employment"} then
#'   can print OEA forecast.
#'
#' @return a ggplot2 object.
#'
#' @export
multiple_wapr <- function(dbset, db_names,
                          facet_var = c("BZONE", "MPO", "COUNTY", "STATE"),
                          facet_levels = NULL ) {

  # get the wapr table for every scenario.
  names(dbset) <- db_names
  df <- rbind_all(
    lapply(seq_along(dbset), function(i)
      extract_wapr(dbset[[i]], facet_var, facet_levels) %>%
        mutate(scenario = names(dbset)[[i]])
    )
  ) %>%
    mutate_("facet_var" = facet_var)

  ggplot(
    df,
    aes_string(x = "year", y = "wapr", color = "scenario")
  ) +
    geom_path() +
    facet_wrap(~ facet_var) +
    xlab("Year") + ylab("Labor force participation") +
    theme_bw()

}
