#' Extract employment statistics from scenario
#'
#' This is an internal function to pull the employment data from a scenario and
#' return it to either the single scenario or scenario comparison functions.
extract_employment <- function(db,
                               facet_var = c("MPO", "COUNTY", "STATE"),
                               facet_levels = NULL,
                               type_levels = NULL){

  # set facet variable; if null then default to MPO
  if(is.null(facet_var)){
    facet_var = "MPO"
  }

  grouping <- tbl(db, "BZONE") %>%
    select_("BZONE", "facet_var" = facet_var)

  # get levels of facet_var if none given
  if(is.null(facet_levels)){
    facet_levels <- grouping %>% group_by(facet_var) %>% collect() %>%
      slice(1) %>% .$facet_var

    facet_levels <- facet_levels[which(facet_levels != "EXTSTA")]
  }

  # get levels of employment type
  if(is.null(type_levels)){
    type_levels <- employment_types$naics1
  }

  # get buysell table and compute summary
  employment <- tbl(db, "BuySellMatrix") %>%
    select(
      BZONE = FROMBZONE,
      year = TSTEP,
      BuySell_A1_Mgmt_Bus:BuySell_Fire_Business_and_Professional_Services,
      BuySell_Food_Services:BuySell_Return_Investment_Receipts,
      BuySell_Teaching_K12
    )  %>%

    mutate(year = as.numeric(year) + 1990) %>%

    group_by(BZONE, year) %>%
    summarise_each(funs(sum), -BZONE, -year) %>%

    # join facet and filter desired levels
    left_join(grouping, by = "BZONE") %>%
    filter(facet_var %in% facet_levels) %>%
    collect() %>%

    gather(sector, output, BuySell_A1_Mgmt_Bus:BuySell_Teaching_K12) %>%

    # consolidate employment types and filter to desired levels
    left_join(employment_types, by = "sector") %>%
    filter(naics1 %in% type_levels) %>%
    group_by(facet_var, year, naics_label) %>%
    summarise(output = sum(output)) %>%

  return(employment)
}
