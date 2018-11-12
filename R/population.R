#' Make a yearly summary table of a land use variable
#'
#' @param df A \code{data_frame} of zones, such as is stored in the sqlite
#'   database.
#' @param group The variable to use for grouping the zones (for instance,
#'   "County").
#'
#' @param var The variable to sum (for instance, "POPULATION")
#'
#' @return A \code{data_frame} with the grouping variable in each row, and the
#' value of the variable at each time stage going across the columns.
#'
#' @export
yearly_summary <- function(df, group, var){
  df %>%
    dplyr::group_by_(group, "year") %>%
    dplyr::mutate_("var" = var) %>%
    dplyr::summarize(var = sum(var)) %>%
    dplyr::collect(n=Inf) %>%
    tidyr::spread(year, var, fill = NA)
}

#' Extract SE data from DB
#'
#'
#' @param db The scenario sqlite database.
#' @param color_var Field to color by: either "MPO" or "COUNTY".
#' @param color_levels A character vector of the color variable specifiying
#'   which levels to include.
#' @param controls Plot against the control totals. Defaults to TRUE, cannot
#'   currently run with FALSE.
#' @param index Whether to show the variables as indexed against the base year.
#'
#' @export
#' @return a data frame
extract_se <- function(db, color_var = NULL,
                       color_levels = NULL, controls = TRUE, index = FALSE){

  if(is.null(color_var)){color_var = "COUNTY"}

  if(controls){ # if controls are asked for, then make sure that they are available
    if(color_var != "COUNTY"){
      warning("Controls only available for county-level population forecasts.")
      controls <- FALSE
    }
  }

  # county plot with controls
  if(color_var == "COUNTY"){
    # Pull se data
    df <- dplyr::tbl(db, "AZONE") %>%
      dplyr::select(AZONE, POPULATION, EMPLOYMENT, TOTALHHS, TSTEP) %>%
      # join information for county and state
      dplyr::left_join(
        dplyr::tbl(db, "ALLZONES") %>%
          dplyr::select(AZONE = Azone, COUNTY = COUNTY, state = STATE)
      ) %>%

      # dplyr::summarize to the county level
      dplyr::group_by(COUNTY, state, TSTEP) %>%
      dplyr::summarize(
        population = sum(POPULATION),
        employment = sum(EMPLOYMENT),
        totalhh = sum(TOTALHHS)
      ) %>%
      dplyr::mutate(year = as.numeric(TSTEP) + 1990) %>%
      dplyr::ungroup() %>% dplyr::collect(n=Inf)

    # if no levels specified, then keep all
    if(!is.null(color_levels)){
      df <- dplyr::filter(df, COUNTY %in% color_levels)
    }

    df <- df %>%
      dplyr::select(color_var = COUNTY, year, population, employment) %>%
      tidyr::gather(var, y, population:employment) %>%
      dplyr::mutate(data = "SWIM")

    # add COUNTY controls
    if(controls){
      ct <- county_controls %>%
        dplyr::rename(color_var = county) %>%
        dplyr::filter(color_var %in% df$color_var) %>%
        dplyr::filter(year > 2005) %>%
        dplyr::select(color_var, year, var, y) %>%
        dplyr::mutate(data = "OEA Forecast")

      df <- rbind(df, ct)
    }


  } else {

    grouping <- dplyr::tbl(db, "BZONE") %>%
      dplyr::select_("BZONE", "color_var" = color_var)

    # get levels of facet_var if none given
    if(is.null(color_levels)){
      color_levels <- grouping %>% dplyr::group_by(color_var) %>% dplyr::collect(n=Inf) %>%
        dplyr::slice(1) %>% .$color_var

      color_levels <- color_levels[which(color_levels != "EXTSTA")]
    }

    # Pull se data
    df <- dplyr::tbl(db, "AZONE") %>%
      dplyr::select(AZONE, POPULATION, EMPLOYMENT, TOTALHHS, TSTEP) %>%
      # join information for county and state
      dplyr::left_join(
        dplyr::tbl(db, "ALLZONES") %>% rename(AZONE = Azone), by = "AZONE") %>%
      dplyr::left_join(grouping, by = "BZONE") %>%
      dplyr::filter(color_var %in% color_levels) %>%

      # dplyr::summarize to the MPO level
      dplyr::group_by(color_var, TSTEP) %>%
      dplyr::summarize(
        population = sum(POPULATION),
        employment = sum(EMPLOYMENT),
        totalhh = sum(TOTALHHS)
      ) %>%
      dplyr::mutate(year = as.numeric(TSTEP) + 1990) %>%
      dplyr::ungroup() %>% dplyr::collect(n=Inf) %>%

      dplyr::select(color_var, year, population, employment) %>%
      tidyr::gather(var, y, population:employment) %>%
      dplyr::mutate(data = "SWIM")
  }

  if(index){
    df <- df %>%
      dplyr::group_by_("color_var", "var", "data") %>%
      dplyr::mutate(y = calc_index(y))
  }

  return(df)

}

#' Make a plot of population and employment
#'
#' @inheritDotParams extract_se
#'
#' @return A \code{ggplot2} plot object showing the modeled change in employment
#'   and population over time.
#'
#' @export
plot_sevar <- function(...){

  df <- extract_se(...)
  dots <- list(...)

  # county plot with controls
  if(dots$color_var == "COUNTY"){

    p <- ggplot2::ggplot(df) +
      ggplot2::geom_line(ggplot2::aes_string(
        x = "year", y = "y", color = "color_var", lty = "data"))

    if(dots$controls){
      p <- p + ggplot2::scale_linetype_manual("Data", values = c("dashed", "solid"))
    }

  } else { # MPO plot without controls
    p <- ggplot2::ggplot(df) +
      geom_line(ggplot2::aes(x = year, y = y, color = color_var))
  }


  # ggplot2::theme, etc
  p +
    ggplot2::scale_color_discrete(dots$color_var) +
    ggplot2::facet_grid(. ~ var, scale = "free_y") +
    ggplot2::xlab("Year") + ggplot2::ylab(ifelse(dots$index, "Index Relative to Base", "Count")) +
    ggplot2::theme_bw() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30))
}


#' Compare population and employment
#'
#' @param db1 The swim database for the "Reference" scenario.
#' @param db2 The swim database for the "Current" scenario.
#' @inheritDotParams extract_se color_var:index
#'
#' @return A \code{ggplot2} plot object showing the modeled change in employment
#'   and population over time.
#'
#' @export
compare_sevar <- function(db1, db2, ...){

  dots <- list(...)

  seref <- extract_se(db = db1, ..., controls = FALSE) %>% dplyr::rename(ref = y)
  secom <- extract_se(db = db2, ..., controls = FALSE) %>% dplyr::rename(com = y)

  df <- dplyr::left_join(seref, secom) %>%
    dplyr::mutate(diff = (com - ref) / ref * 100)

  ggplot2::ggplot(df,
         ggplot2::aes_string(x = "year", y = "diff", color = "var")) +
    ggplot2::geom_path() +
    ggplot2::facet_wrap(~color_var) +
    ggplot2::xlab("Year") + ggplot2::ylab("Percent difference (current - reference).") +
    ggplot2::scale_color_discrete(dots$color_var) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30))
}

#' Plot population with controls and historical data.
#'
#' @param db Swim database.
#' @param counties If not null (default), a character vector of counties to plot.
#'
#' @return a ggplot2 object
#' @export
plot_history <- function(db, counties = NULL) {

  df <- extract_se(db, color_var = "COUNTY", color_levels = counties, controls = TRUE) %>%
    dplyr::filter(var == "population") %>%
    dplyr::select(-var) %>%
    dplyr::bind_rows(., historical_pop %>% dplyr::mutate(data = "Census")) %>%
    dplyr::filter(color_var %in% counties)

  ggplot2::ggplot(df, ggplot2::aes(x = year, y = y, color = color_var, lty = data)) +
    ggplot2::geom_path() +
    ggplot2::xlab("Year") + ggplot2::ylab("Population") +
    ggplot2::scale_linetype_manual("Data", values = c("solid", "dotted", "longdash")) +
    ggplot2::scale_color_discrete("County") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30))
}

#' Determine annualized population growth rates.
#'
#' This function calculates the annualized growth rate between Census years and
#' between SWIM population synthesizer runs.
#'
#' @param db Swim database.
#' @param counties If not null (default), a character vector of counties to plot.
#'
#' @export
pop_rate <- function(db, counties = NULL) {


  # get the population in every year
  df <- extract_se(db, "COUNTY", counties, controls = TRUE) %>%
    dplyr::filter(var == "population") %>%
    dplyr::filter(data == "SWIM") %>%
    dplyr::select(-var) %>%
    rbind_list(., historical_pop %>% dplyr::mutate(data = "Census")) %>%
    dplyr::arrange(year)

  if(!is.null(counties)){
    # if a list of counties is given, dplyr::filter to that
    df <- df %>% dplyr::filter(county %in% counties)
  } else {
    # only show counties in oregon
    df <- df %>%
      dplyr::filter(
        county %in% unique(zones_data %>% dplyr::filter(STATE == "OR") %>% .$COUNTY)
      )
  }

  # function to interpolate between values
  interpolate <- function(x, year){
    zoo::na.approx(x, year)
  }

  #expand to include all years
  df <- dplyr::tbl_df(expand.grid(
    year = min(df$year):max(df$year),
    county = unique(df$county)
  )) %>%
    dplyr::mutate(
      data = ifelse(year < 2010, "Census", "SWIM")
    ) %>%
    dplyr::left_join(df) %>%

    # Interpolate population and calculate annual growth rate
    dplyr::group_by(county) %>%
    dplyr::mutate(
      pop = interpolate(y, year),
      rate = (lead(pop) - pop) / pop * 100
    )

  return(df)

}

#' Plot annualized growth rates
#'
#' This function returns a box plot comparing the annualized population growth
#' rates in a set of counties. This can help identify problems that the model
#' may be having relative to historical trends.
#'
#' @param db the Swim database.
#' @param counties A character vector of counties to display.
#'
#' @export
plot_rates <- function(db, counties) {

  rates <- pop_rate(db, counties)

  ggplot2::ggplot(rates, ggplot2::aes(x = county, y = rate, color = data)) +
    geom_boxplot() +
    geom_point(position = "jitter") +
    ggplot2::xlab("County") + ggplot2::ylab("Annualized Growth Rates") +
    ggplot2::theme_bw() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30))

}

#' Discover outlying counties
#'
#' A major question we face is identifying whether the modeled population growth
#' rate in a region is "reasonable." To date reasonableness has been subjective,
#' but this function provides quantitative methodologies to calculate this.
#'
#' @param db The swim database
#' @param counties A character vector of counties to calculate.
#' @param method The method by which unreasonable counties are identified. See
#'   "Details".
#'
#' @return A data frame with information on each county's conformity with
#'   historical trends.
#'
#' @details If \code{method = "count"} (default), the number of years for which
#'   the annualized population growth rate lies outside of the historical
#'   confidence interval. If \code{method = "pval"}, a data frame of swim scenario
#'   years with with the $p$-value of the annualized growth rate on the
#'   distribution defined by historical data.
#'
#' @export
#' @seealso \code{\link{pop_rate}}
#'
discover_outlying_rates <- function(db, counties = NULL,
                                    method = c("count", "pval")){


  # get annualized population growth rate in each year, and stash in column
  rates <- pop_rate(db, counties) %>%
    dplyr::group_by(county) %>%
    dplyr::mutate(
      census = ifelse(data == "Census", rate, NA),
      swim   = ifelse(data == "SWIM", rate, NA)
    )

  if(method == "pval"){

    get_pval <- function(x, y){
      # get pvalue for each model year given historical rate distribution
      stats::pnorm(y, mean(x, na.rm = TRUE), stats::sd(x, na.rm = TRUE))
    }

    r <- rates %>%
      dplyr::mutate(
        pval = get_pval(census, swim)
      ) %>%
      dplyr::filter(!is.na(y)) %>%
      dplyr::filter(data == "SWIM") %>%
      dplyr::select(county, year, data, pop, pval)

  } else if(method == "count"){

    count_insideci <- function(x, y) {
      y <- y[!is.na(y)]

      # what percent of y falls inside confidence interval of f?
      low <- mean(x, na.rm = TRUE) - 1.96 * stats::sd(x, na.rm = TRUE)
      hi  <- mean(x, na.rm = TRUE) + 1.96 * stats::sd(x, na.rm = TRUE)

      inside <- ifelse(y >= low & y <= hi, TRUE, FALSE)

      return(sum(inside) / length(y))

    }

    r <- rates %>%
      dplyr::group_by(county) %>%
      dplyr::summarize(
        inside_ci = count_insideci(census, swim)
      )
  }

  return(r)

}


#' Compare Population or Employment across multiple scenarios.
#'
#' @param dbset A list of connections to SWIM databases.
#' @param db_names A character vector naming the scenarios.
#' @param variable One of \code{c("population", "employment")} defining which
#'   socioeconomic variable to include in
#' @inheritDotParams extract_se
#'
#' @return a ggplot2 object.
#'
#' @export
multiple_sevar <- function(dbset, db_names,
                           variable = c("population", "employment"), ... ) {
  dots <- list(...)

  # get the population table for every scenario.
  names(dbset) <- db_names
  df <- bind_rows(
    lapply(seq_along(dbset), function(i)
      extract_se(dbset[[i]]) %>%
        dplyr::mutate(scenario = names(dbset)[[i]]) %>%
        dplyr::filter(var == variable)
    )
  ) %>%
    dplyr::mutate_("facet_var" = names(.)[1])


  # add control data if desired
  if(dots$controls){
    p <- ggplot2::ggplot(
      data = df %>%
        # only need control once
        dplyr::filter(data != "Control" | scenario == names(dbset)[1]) %>%
        dplyr::mutate(scenario = ifelse(data == "Control", "Control", scenario)),
      ggplot2::aes(x = year, y = y, color = scenario, lty = data)
    ) +
      ggplot2::scale_linetype_manual("source",
                            values = c("dotted", rep("solid", length(dbset))))
  } else {
    p <- ggplot2::ggplot(
      data = df %>%
        dplyr::filter(data != "Control"),
      ggplot2::aes(x = year, y = y, color = scenario)
    )

  }

  p <- p + ggplot2::geom_path() +
    ggplot2::facet_wrap(~ facet_var, scale = "free_y") +
    ggplot2::ylab(variable) + ggplot2::xlab("Year") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30))


  return(p)

}
