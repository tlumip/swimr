#' Extract GDP of Labor
#'
#' This function returns the total dollar value of all the labor sold by a
#' dplyr::collect(n=Inf)ion of zones over time.
#'
#' @param db The scenario sqlite database.
#' @param facet_var Field to facet by: "MPO", "COUNTY", or "STATE".
#' @param facet_levels A character vector of the variable specifiying
#'   which levels to include.
#' @param color_levels A character vector of the industry sectors to include.
#'   Defaults to all.
#' @param index_year [Optional] index year that should be used as the starting year for data or plots;
#'
#' @return A \code{data_frame} with the participation rate in each facet region
#'   in each transport model year.
#'
#' @export
extract_gdp <- function(db,
                         facet_var = c("MPO", "COUNTY", "STATE"),
                         facet_levels = NULL, color_levels = NULL, index_year=2000) {

  # set facet variable; if null then default to MPO
  if(is.null(facet_var)){
    facet_var = "MPO"
  }

  grouping <- dplyr::tbl(db, "BZONE") %>%
    dplyr::select_("BZONE", "facet_var" = facet_var)

  # get levels of facet_var if none given
  if(is.null(facet_levels)){
    facet_levels <- grouping %>% group_by(facet_var) %>% dplyr::collect(n=Inf) %>%
      slice(1) %>% .$facet_var

    facet_levels <- facet_levels[which(facet_levels != "EXTSTA")]
  }

  types <- gsub("BuySell_", "", employment_types$sector)

  df <- dplyr::tbl(db, "ExchangeResults") %>%
    dplyr::transmute(
      BZONE, year = as.numeric(TSTEP) + 1990,
      commodity = Commodity,
      sold = InternalSold
    )  %>%
    # join faceting variables
    dplyr::left_join(grouping ) %>%
    dplyr::filter(facet_var %in% facet_levels) %>%
    dplyr::filter(year >= index_year) %>%

    # dplyr::summarize on faceting variable
    dplyr::filter(sold  > 0) %>%
    group_by(facet_var, year, commodity) %>%
    dplyr::summarize( sold = sum(sold) ) %>%
    dplyr::collect(n=Inf) %>%

    # only keep employment types
    # some genius is using different labels in different tables. Fix this.
    dplyr::mutate(commodity = gsub("-", "_", commodity)) %>%
    dplyr::filter(commodity %in% types) %>%
    dplyr::left_join(
      employment_types %>%
        dplyr::mutate(commodity = gsub("BuySell_", "", employment_types$sector)),
      by = c("commodity")
    ) %>%

    group_by(facet_var, year, naics_label) %>%
    dplyr::summarize(value = sum(sold))

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
                     facet_levels = NULL, color_levels = NULL, index_year=2000) {

  df <- extract_gdp(db, facet_var, facet_levels, color_levels, index_year=index_year)

  ggplot2::ggplot(df, ggplot2::aes(x = year, y = value / 1e9, color = naics_label)) +
    ggplot2::geom_path() +
    ggplot2::facet_wrap(~facet_var, scale = "free_y") +
    ggplot2::xlab("Year") + ggplot2::ylab("GDP of Labor [$B]") +
    ggplot2::scale_color_discrete("Sector") +
    ggplot2::theme_bw() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30))
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
                        facet_levels = NULL, color_levels = NULL, index_year=2000) {

  ref <- extract_gdp(db1, facet_var, facet_levels, color_levels, index_year=index_year) %>%
    dplyr::rename(ref = value)
  com <- extract_gdp(db2, facet_var, facet_levels, color_levels, index_year=index_year) %>%
    dplyr::rename(com = value)

  df <- dplyr::left_join(ref, com) %>%
    dplyr::mutate(diff = (com - ref) / ref * 100)


  ggplot2::ggplot(df, ggplot2::aes(x = year, y = diff, color = naics_label)) +
    ggplot2::geom_path() +
    ggplot2::facet_wrap(~ facet_var) +
    ggplot2::xlab("Year") + ggplot2::ylab("GDP of Labor [% difference]") +
    ggplot2::scale_color_discrete("Sector") +
    ggplot2::theme_bw() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30))
}


#' Compare GDP across multiple scenarios.
#'
#' @param dbset A list of connections to SWIM databases.
#' @param db_names A character vector naming the scenarios.
#' @param facet_var Field to facet by.
#' @param facet_levels A character vector of the facet variable specifiying
#'   which levels to include.
#' @param index_year [Optional] index year that should be used as the starting year for data or plots;
#'
#' @return a ggplot2 object.
#'
#'
#' @export
multiple_gdp <- function(dbset, db_names,
                         facet_var = c("MPO", "COUNTY", "STATE"),
                         facet_levels = NULL, index_year=2000 ) {

  # get the wapr table for every scenario.
  names(dbset) <- db_names
  df <- bind_rows(
    lapply(seq_along(dbset), function(i)
      extract_gdp(dbset[[i]], facet_var, facet_levels, index_year=index_year) %>%
        dplyr::mutate(scenario = names(dbset)[[i]])
    )
  ) %>%
    group_by(scenario, facet_var, year) %>%
    dplyr::summarize(value = sum(value))

  ggplot2::ggplot(
    df,
    ggplot2::aes(x = year, y = value, color = scenario)
  ) +
    ggplot2::geom_path() +
    ggplot2::facet_wrap(~ facet_var, scale = "free_y") +
    ggplot2::xlab("Year") + ggplot2::ylab("GDP of Labor") +
    ggplot2::theme_bw() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30))

}
