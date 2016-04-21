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
yearly_summary <- function(df, group, var){
  df %>%
    group_by_(group, "year") %>%
    mutate_("var" = var) %>%
    summarise(var = sum(var)) %>%
    collect() %>%
    spread(year, var, fill = NA)
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
extract_se <- function(db, color_var = c("MPO", "COUNTY"),
                       color_levels = NULL, controls = TRUE, index = FALSE){


  # county plot with controls
  if(color_var == "COUNTY"){
    # Pull se data
    df <- tbl(db, "AZONE") %>%
      select(AZONE, POPULATION, EMPLOYMENT, TOTALHHS, TSTEP) %>%
      # join information for county and state
      left_join(
        tbl(db, "ALLZONES") %>%
          select(AZONE = Azone, COUNTY = COUNTY, state = STATE)
      ) %>%

      # summarize to the county level
      group_by(COUNTY, state, TSTEP) %>%
      summarise(
        population = sum(POPULATION),
        employment = sum(EMPLOYMENT),
        totalhh = sum(TOTALHHS)
      ) %>%
      mutate(year = as.numeric(TSTEP) + 1990) %>%
      ungroup() %>% collect()

    # if no levels specified, then keep all
    if(!is.null(color_levels)){
      df <- filter(df, COUNTY %in% color_levels)
    }

    df <- df %>%
      select(COUNTY, year, population, employment) %>%
      gather(var, y, population:employment) %>%
      mutate(data = "SWIM")

    # add COUNTY controls
    if(controls){
      ct <- county_controls %>%
        rename(COUNTY = county) %>%
        filter(COUNTY %in% df$COUNTY) %>%
        filter(year > 2005) %>%
        select(COUNTY, year, var, y) %>%
        mutate(data = "OEA Forecast")

      df <- rbind(df, ct)
    }


  } else {

    grouping <- tbl(db, "BZONE") %>%
      select_("BZONE", "color_var" = color_var)

    # get levels of facet_var if none given
    if(is.null(color_levels)){
      color_levels <- grouping %>% group_by(color_var) %>% collect() %>%
        slice(1) %>% .$color_var

      color_levels <- color_levels[which(color_levels != "EXTSTA")]
    }

    # Pull se data
    df <- tbl(db, "AZONE") %>%
      select(AZONE, POPULATION, EMPLOYMENT, TOTALHHS, TSTEP) %>%
      # join information for county and state
      left_join(
        tbl(db, "ALLZONES") %>%
          select(AZONE = Azone, MPO = MPO, state = STATE)
      ) %>%
      filter(MPO %in% color_levels) %>%

      # summarize to the MPO level
      group_by(MPO, TSTEP) %>%
      summarise(
        population = sum(POPULATION),
        employment = sum(EMPLOYMENT),
        totalhh = sum(TOTALHHS)
      ) %>%
      mutate(year = as.numeric(TSTEP) + 1990) %>%
      ungroup() %>% collect() %>%

      select(MPO, year, population, employment) %>%
      gather(var, y, population:employment) %>%
      mutate(data = "SWIM")
  }

  if(index){
    df <- df %>%
      group_by_(color_var, "var", "data") %>%
      mutate(y = calc_index(y))
  }

  return(df)

}

#' Make a plot of population and employment
#'
#' @param db The scenario sqlite database.
#' @param color_var Field to color by: either "MPO" or "COUNTY".
#' @param color_levels A character vector of the color variable specifiying
#'   which levels to include.
#' @param controls Plot against the control totals. Defaults to TRUE
#' @param index Whether to show the variables as indexed against the base year.
#'
#' @return A \code{ggplot2} plot object showing the modeled change in employment
#'   and population over time.
#'
#' @export
plot_sevar <- function(db, color_var = c("MPO", "COUNTY"),
                       color_levels = NULL, controls = TRUE,
                       index = FALSE ){


  # county plot with controls
  if(color_var == "COUNTY"){

    df <- extract_se(db, color_var, color_levels, controls, index)

    p <- ggplot(df) +
      geom_line(aes_string(x = "year", y = "y", color = color_var, lty = "data"))

    if(controls){
      p <- p + scale_linetype_manual("Data", values = c("dashed", "solid"))
    }

  } else { # MPO plot without controls

    df <- extract_se(db, color_var, color_levels, controls, index)

    p <- ggplot(df) +
      geom_line(aes(x = year, y = y, color = MPO))
  }


  # theme, etc
  p +
    facet_grid(. ~ var, scales = "free_y") +
    xlab("Year") + ylab(ifelse(index, "Index Relative to Base", "Count")) +
    theme_bw() + theme(axis.text.x = element_text(angle = 30))
}


#' Compare population and employment
#'
#' @param db1 The swim database for the "Reference" scenario.
#' @param db2 The swim database for the "Current" scenario.
#' @param facet_var Field to facet by: either "MPO" or "COUNTY".
#' @param facet_levels A character vector of the facet variable specifiying
#'   which levels to include.
#' @param index Whether to show the variables as indexed against the base year.
#'
#' @return A \code{ggplot2} plot object showing the modeled change in employment
#'   and population over time.
#'
#' @export
compare_sevar <- function(db1, db2, facet_var = c("MPO", "COUNTY"),
                          facet_levels = NULL, index = FALSE){


  seref <- extract_se(db1, facet_var, facet_levels, controls = FALSE, index) %>%
    rename(ref = y)
  secom <- extract_se(db2, facet_var, facet_levels, controls = FALSE, index) %>%
    rename(com = y)

  df <- left_join(seref, secom) %>%
    mutate(diff = (com - ref) / ref * 100)

  ggplot(df,
         aes_string(x = "year", y = "diff", color = "var")) +
    geom_path() +
    facet_wrap(as.formula(paste("~", facet_var))) +
    xlab("Year") + ylab("Percent difference (current - reference).") +
    scale_color_discrete("Variable") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 30))
}

#' Plot population with controls and historical data.
#'
#' @param db Swim database.
#' @param counties If not null (default), a character vector of counties to plot.
#'
#' @return a ggplot2 object
#' @export
plot_history <- function(db, counties = NULL) {

  df <- extract_se(db, "COUNTY", counties, controls = TRUE) %>%
    rename(county = COUNTY) %>%
    filter(var == "population") %>%
    select(-var) %>%
    rbind_list(., historical_pop %>% mutate(data = "Census")) %>%
    filter(county %in% counties)

  ggplot(df, aes(x = year, y = y, color = county, lty = data)) +
    geom_path() +
    xlab("Year") + ylab("Population") +
    scale_linetype_manual("Data", values = c("solid", "dotted", "longdash")) +
    scale_color_discrete("County") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 30))
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
#' @import zoo
pop_rate <- function(db, counties = NULL) {


  # get the population in every year
  df <- extract_se(db, "COUNTY", counties, controls = TRUE) %>%
    filter(var == "population") %>%
    filter(data == "SWIM") %>%
    select(-var) %>%
    rbind_list(., historical_pop %>% mutate(data = "Census")) %>%
    arrange(year)

  if(!is.null(counties)){
    # if a list of counties is given, filter to that
    df <- df %>% filter(county %in% counties)
  } else {
    # only show counties in oregon
    df <- df %>%
      filter(
        county %in% unique(zones_data %>% filter(STATE == "OR") %>% .$COUNTY)
      )
  }

  # function to interpolate between values
  interpolate <- function(x, year){
    zoo::na.approx(x, year)
  }

  #expand to include all years
  df <- tbl_df(expand.grid(
    year = min(df$year):max(df$year),
    county = unique(df$county)
  )) %>%
    mutate(
      data = ifelse(year < 2010, "Census", "SWIM")
    ) %>%
    left_join(df) %>%

    # Interpolate population and calculate annual growth rate
    group_by(county) %>%
    mutate(
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

  ggplot(rates, aes(x = county, y = rate, color = data)) +
    geom_boxplot() +
    geom_point(position = "jitter") +
    xlab("County") + ylab("Annualized Growth Rates") +
    theme_bw() + theme(axis.text.x = element_text(angle = 30))

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
    group_by(county) %>%
    mutate(
      census = ifelse(data == "Census", rate, NA),
      swim   = ifelse(data == "SWIM", rate, NA)
    )

  if(method == "pval"){

    get_pval <- function(x, y){
      # get pvalue for each model year given historical rate distribution
      pnorm(y, mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))
    }

    r <- rates %>%
      mutate(
        pval = get_pval(census, swim)
      ) %>%
      filter(!is.na(y)) %>%
      filter(data == "SWIM") %>%
      select(county, year, data, pop, pval)

  } else if(method == "count"){

    count_insideci <- function(x, y) {
      y <- y[!is.na(y)]

      # what percent of y falls inside confidence interval of f?
      low <- mean(x, na.rm = TRUE) - 1.96 * sd(x, na.rm = TRUE)
      hi  <- mean(x, na.rm = TRUE) + 1.96 * sd(x, na.rm = TRUE)

      inside <- ifelse(y >= low & y <= hi, TRUE, FALSE)

      return(sum(inside) / length(y))

    }

    r <- rates %>%
      group_by(county) %>%
      summarise(
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
#' @param facet_var Field to facet by: either "MPO" or "COUNTY".
#' @param facet_levels A character vector of the facet variable specifiying
#'   which levels to include.
#' @param controls If \code{facet_var = "COUNTY" & variable = "employment"}
#'     then can print OEA forecast.
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
    if(facet_var != "COUNTY" | variable != "population"){
      warning("Controls only available for county-level population forecasts.")
      controls <- FALSE
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
    theme_bw() +
    theme(axis.text.x = element_text(angle = 30))


  return(p)

}

