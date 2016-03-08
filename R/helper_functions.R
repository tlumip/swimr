#' Cut percent error into ranges
#'
#' @param x a vector of error measurements
#' @return a factor showing the bin
#'
cut_error <- function(x){
  brks <- c(0.05, 0.1, 0.2, 0.5, 1)
  cut(x, breaks = c(0, brks, Inf))
}

#' Cut divergins percent error into ranges
#'
#' @param x a vector of error measurements
#' @return a factor showing the bin
#'
cut_diverror <- function(x){
  brks <- c(0.05, 0.10,  0.20, 1) * 100
  cut(x, breaks = c(-Inf, rev(-1 * brks), 0, brks, Inf))
}

exaggerate_diff <- function(x){
  log(abs(x) + 1e-5) * sign(x)
}


#' Cut absolute differences into ranges
#'
#' @param x a vector of error measurements
#' @return a factor showing the bin
#'
cut_abserror <- function(x){
  brks <- c(1, 10, 100, 1000)
  cut(x, breaks = c(-Inf, rev(-1 * brks), 0, brks, Inf))
}


#' Cut growth rates into ranges
#'
#' @param x a vector of error measurements
#' @return a factor showing the bin
#'
cut_grt <- function(x){
  brks <- c(0.05, 0.10,  0.20, 1) * 100
  cut(x, breaks = c(-Inf, rev(-1 * brks), 0, brks, Inf))
}


#' Return all levels of a factor other than external stations
#'
#' @param df a tbl_sqlite with a faceting variable named \code{facet_var}
#' @return names of all levels of the factor
get_levels <- function(df){
  a <- df %>%
    collect() %>%
    filter(facet_var != "EXTSTA")

  names(table(a$facet_var))
}


#' Calculate exponential growth rate
#'
#' @param p1 Value in period 1
#' @param p2 Value in period 2
#' @param t1 Time step for period 1 (i.e., 2010)
#' @param t2 Time step for period 2 (i.e., 2040)
#'
#' @details This function calculates an annualized exponential growth rate
#'   implied by measuring a value in two different time periods. Solves the
#'   elementary exponential growth equation, \deqn{p_2 = p_1 e^{r(t_2 - t_1)}}
#'   for \eqn{r}.
#'
#' @export
#' @examples
#' calc_exprate(10, 12, 2010, 2040)
calc_exprate <- function(p1, p2, t1, t2){
  # p2 = p1e^{r(t2 - t1)}

  (log(p2) - log(p1)) / (t2 - t1)
}


#' Exponentially interpolate around two data points
#'
#' @inheritParams calc_exprate
#' @param t0 The timepoint for interpolation (or extrapolation)
#'
#' @details This function first calculates the implied exponential growth rate
#'   between two points, and then predicts where the function would be in the
#'   intermediate year.
#'
#' @seealso calc_exprate
#'
#' @export
#' @examples
#' interpolate_exponential(10, 12, 2010, 2040, 2020)
#' interpolate_exponential(10, 12, 2010, 2040, 2008)
interpolate_exponential <- function(p1, p2, t1, t2, t0){
  # get implied growth rate
  r <- calc_exprate(p1, p2, t1, t2)

  # calculate new value at t0
  p1 * exp(r * (t0 - t1))
}


#' Percent difference between two conditions
#'
#' @param x base value
#' @param y alternate value
#'
#' @export
#'
pct_diff <- function(x, y) {
  (y - x) / x * 100
}
