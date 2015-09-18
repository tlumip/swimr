#' Extract labor force participation
#'
#' BLS defines the civilian labor force participation rate as the percent of
#' individuals age 16 and over who are either working or who are looking for
#' work. This is not something we ask about in travel surveys generally, so
#' it is not possible to make an apples-to-apples comparison.
#'
#' This function returns the number of workers in each facet region divided
#' by the number of individuals fifteen years and older in that region. The
#' definition of a "worker" in a travel model is different than the BLS definition
#' in that travel modesl do not consider unemployed individuals who are looking
#' for work.
#'
#' @param db The scenario sqlite database.
#' @param facet_var Field to facet by: "MPO", "COUNTY", or "STATE".
#' @param facet_levels A character vector of the variable specifiying
#'   which levels to include.
#'
#' @return A \code{data_frame} with the participation rate in each facet region
#'   in each transport model year.
extract_lfpr <- function(db,
                         facet_var = c("BZONE", "MPO", "COUNTY",
                                       "DOT_REGION", "STATE"),
                         facet_levels = NULL, color_levels = NULL) {


  df <- tbl(db, "AZONE") %>%
    transmute(
      AZONE, TSTEP,
      workers = TOTALWORKERS,
      laborforce = PERSON15TO21 + PERSON21TO40 + PERSON40TO60 + PERSON60PLUS
    ) %>%

    # join faceting variables
    left_join(
      tbl(db, "ALLZONES") %>%
        select_("AZONE" = "Azone", facet_var)
    )

    # if no levels specified, then keep all
    if(!is.null(facet_levels)){
      df <- df %>% filter(facet_var %in% facet_levels)
    }

  df <- df %>%
    # summarize on faceting variable
    group_by_("TSTEP", facet_var) %>%
    summarise(
      workers = sum(workers),
      laborforce = sum(laborforce)
    ) %>%

    # calculate lfpr
    collect() %>%
    mutate(
      lfpr = workers / laborforce * 100,
      year = as.numeric(TSTEP) + 1990
    ) %>%
    ungroup() %>%
    select(-TSTEP)

  return(df)

}

#' Plot the labor force participation rate.
#'
#' BLS defines the civilian labor force participation rate as the percent of
#' individuals age 16 and over who are either working or who are looking for
#' work. This is not something we ask about in travel surveys generally, so it is
#' not possible to make an apples-to-apples comparison.
#'
#' This function returns the number of workers in each facet region divided by
#' the number of individuals fifteen years and older in that region. The
#' definition of a "worker" in a travel model is different than the BLS
#' definition in that travel modesl do not consider unemployed individuals who
#' are looking for work.
#'
#' @param db The scenario sqlite database.
#' @param facet_var Field to facet by: "MPO", "COUNTY", or "STATE".
#' @param facet_levels A character vector of the variable specifiying
#'   which levels to include.
#'
#' @return a ggplot2 plot object.
plot_lfpr <- function(db, facet_var = c("MPO", "COUNTY", "STATE"),
                     facet_levels = NULL) {

  df <- extract_lfpr(db, facet_var, facet_levels)

  p <- ggplot(df, aes(x = year, y = lfpr, color = facet_var)) +
    geom_path() +
    xlab("Year") + ylab("Labor Force Participation Rate (%)") +
    scale_color_discrete(facet_var) +
    theme_bw()

  return(p)

}



lpfr_volatility <- function(db) {

}
