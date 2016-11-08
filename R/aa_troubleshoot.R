#' AA Troubleshooter
#'
#' This function prints aggregate statistics from the AA buy/sell matrix. This
#' can help an analyst determine which commodities (if any) may cause problems.
#'
#' @param db A scenario database.
#' @return A \link[DT](datatable) object.
#'
troubleshoot_aa <- function(db){

  a <- tbl(db, "ExchangeResults") %>%
    group_by(TSTEP, Commodity) %>%
    summarise_each(funs(mean), InternalBought, Exports, Price, Imports, InternalSold) %>%
    collect() %>%
    gather(series, value, InternalBought:InternalSold) %>%

  # calculate indexed value and volatility for each data series
    group_by(Commodity, series) %>%
    mutate(
      indexed = round(calc_index(value), 2),
      indexed = ifelse(is.nan(indexed), 0, indexed),
      growth = round(calc_grt(value), 2)
    ) %>%
    group_by(Commodity, series) %>%

    summarise(
      which_max = which.max(abs(indexed))[1],
      maximum_index = max(abs(indexed)) * sign(indexed[which_max]),
      volatile_grt = max(growth) - min(growth),
      volatility = sd(growth)
    )

  DT::datatable(a)

}

