#' Extract GDP of Labor
#'
#' This function returns the total dollar value of all the labor sold by a
#' collect(n=Inf)ion of zones over time.
#'
#' @param db The scenario sqlite database.
#' @param facet_var Field to facet by: "MPO", "COUNTY", or "STATE".
#' @param facet_levels A character vector of the variable specifiying
#'   which levels to include.
#' @param color_levels A character vector of the industry sectors to include.
#'   Defaults to all.
#'
#' @return A \code{data_frame} with the participation rate in each facet region
#'   in each transport model year.
#'
#' @export
extract_gdp <- function(db,
                         facet_var = c("MPO", "COUNTY", "STATE"),
                         facet_levels = NULL, color_levels = NULL) {

  # set facet variable; if null then default to MPO
  if(is.null(facet_var)){
    facet_var = "MPO"
  }

  grouping <- tbl(db, "BZONE") %>%
    select_("BZONE", "facet_var" = facet_var)

  # get levels of facet_var if none given
  if(is.null(facet_levels)){
    facet_levels <- grouping %>% group_by(facet_var) %>% collect(n=Inf) %>%
      slice(1) %>% .$facet_var

    facet_levels <- facet_levels[which(facet_levels != "EXTSTA")]
  }

  types <- gsub("BuySell_", "", employment_types$sector)

  df <- tbl(db, "ExchangeResults") %>%
    transmute(
      BZONE, year = as.numeric(TSTEP) + 1990,
      commodity = Commodity,
      sold = InternalSold
    )  %>%
    # join faceting variables
    left_join(grouping ) %>%
    filter(facet_var %in% facet_levels) %>%

    # summarize on faceting variable
    filter(sold  > 0) %>%
    group_by(facet_var, year, commodity) %>%
    summarise( sold = sum(sold) ) %>%
    collect(n=Inf) %>%

    # only keep employment types
    # some genius is using different labels in different tables. Fix this.
    mutate(commodity = gsub("-", "_", commodity)) %>%
    filter(commodity %in% types) %>%
    left_join(
      employment_types %>%
        mutate(commodity = gsub("BuySell_", "", employment_types$sector)),
      by = c("commodity")
    ) %>%

    group_by(facet_var, year, naics_label) %>%
    summarise(value = sum(sold))

  return(df)

}

#' Plot the GDP contribution of Labor over time in a scenario
#'
#' @inheritParams extract_gdp
#'
#' @return a ggplot2 plot object.
#'
#' @export
plot_gdp <- function(db,
                     facet_var = c("MPO", "COUNTY", "STATE"),
                     facet_levels = NULL, color_levels = NULL) {

  df <- extract_gdp(db, facet_var, facet_levels, color_levels)

  ggplot(df, aes(x = year, y = value / 1e9, color = naics_label)) +
    geom_path() +
    facet_wrap(~facet_var, scales = "free_y") +
    xlab("Year") + ylab("GDP of Labor [$B]") +
    scale_color_discrete("Sector") +
    theme_bw() + theme(axis.text.x = element_text(angle = 30))
}


#' Compare GDP between two scenarios
#'
#' @param db1 The swim database for the "Reference" scenario.
#' @param db2 The swim database for the "Current" scenario.
#' @inheritParams extract_gdp
#'
#' @return ggplot2 object
#' @export
compare_gdp <- function(db1, db2,
                        facet_var = c("MPO", "COUNTY", "STATE"),
                        facet_levels = NULL, color_levels = NULL) {

  ref <- extract_gdp(db1, facet_var, facet_levels, color_levels) %>%
    rename(ref = value)
  com <- extract_gdp(db2, facet_var, facet_levels, color_levels) %>%
    rename(com = value)

  df <- left_join(ref, com) %>%
    mutate(diff = (com - ref) / ref * 100)


  ggplot(df, aes(x = year, y = diff, color = naics_label)) +
    geom_path() +
    facet_wrap(~ facet_var) +
    xlab("Year") + ylab("GDP of Labor [$B]") +
    scale_color_discrete("Sector") +
    theme_bw() + theme(axis.text.x = element_text(angle = 30))
}


#' Compare GDP across multiple scenarios.
#'
#' @param dbset A list of connections to SWIM databases.
#' @param db_names A character vector naming the scenarios.
#' @param facet_var Field to facet by.
#' @param facet_levels A character vector of the facet variable specifiying
#'   which levels to include.
#'
#' @return a ggplot2 object.
#'
#'
#' @export
multiple_gdp <- function(dbset, db_names,
                         facet_var = c("MPO", "COUNTY", "STATE"),
                         facet_levels = NULL ) {

  # get the wapr table for every scenario.
  names(dbset) <- db_names
  df <- rbind_all(
    lapply(seq_along(dbset), function(i)
      extract_gdp(dbset[[i]], facet_var, facet_levels) %>%
        mutate(scenario = names(dbset)[[i]])
    )
  ) %>%
    group_by(scenario, facet_var, year) %>%
    summarise(value = sum(value))

  ggplot(
    df,
    aes(x = year, y = value, color = scenario)
  ) +
    geom_path() +
    facet_wrap(~ facet_var, scales = "free_y") +
    xlab("Year") + ylab("GDP of Labor") +
    theme_bw() + theme(axis.text.x = element_text(angle = 30))

}
