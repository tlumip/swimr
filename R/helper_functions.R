#' Cut percent error into ranges
#'
#' @param x a vector of error measurements
#' @return a factor showing the bin
#'
cut_error <- function(x){
  brks <- c(0.05, 0.1, 0.2, 0.5, 1)
  cut(x, breaks = c(0, brks, Inf))
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
