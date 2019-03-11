#' AA Troubleshooter
#'
#' This function prints aggregate statistics from the AA buy/sell matrix. This
#' can help an analyst determine which commodities (if any) may cause problems.
#'
#' @param db A scenario database.
#' @return A [DT](`datatable`) object.
#'
#' @export
troubleshoot_aa <- function(db){

  a <- tbl(db, "ExchangeResults") %>%
    group_by(TSTEP, Commodity) %>%
    summarise(
      demand_weighted_price = sum(Demand * Price, na.rm=TRUE) / sum(Demand, na.rm=TRUE),
      mean_price = mean(price, na.rm=TRUE)
    ) %>%

    collect() %>%
    gather(series, value, demand_weighted_price, mean_price) %>%

  # calculate indexed value and volatility for each data series
    group_by(Commodity, series) %>%
    mutate(
      indexed = round(calc_index(value), 2),
      indexed = ifelse(is.nan(indexed), 0, indexed),
      growth = round(calc_grt(value), 2)
    ) %>%
    group_by(Commodity, series) %>%

    summarise(
      maximum_index = max(indexed),
      which_max = which.max(indexed)[1],
      minimum_index = min(indexed),
      which_min = which.min(indexed)[1],
      volatile_grt = max(growth) - min(growth),
      volatility = sd(growth)
    )

  DT::datatable(a)

}

