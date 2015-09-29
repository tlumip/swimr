#' Extract destination choice logsums
#'
#' @param db The scenario sqlite database.
#' @param scope a filtering criteria to limit the scope of the dataframe
#' @param purposes a vector of trip purposes to include in the average logsum.
#'
#' @export
#'
#' @return a ggmap object
#' @import dplyr
#'
extract_logsums <- function(db, scope = NULL, purposes = NULL){

  if(is.null(purposes)){
    # all purposes: currently no WORK_BASED
    df <- tbl(db, "DC_LOGSUM") %>%
      filter(PURPOSE != "WORK_BASED")
  } else if(length(purposes) == 1){
    # filtering on one category requires different handling:
    # https://github.com/hadley/dplyr/issues/1428
    df <- tbl(db, "DC_LOGSUM") %>%
      filter(PURPOSE == purposes)
  } else {
    df <- tbl(db, "DC_LOGSUM") %>%
      filter(PURPOSE %in% purposes)
  }

  if(!is.null(scope)){
    # Get a list of zones in the scope
    zt <- zones_data %>%
      filter_(.dots = scope)
  } else {
    zt <- zones_data
  }

  df %>%
    group_by(BZONE, TSTEP) %>%
    summarise(AVGLOGSUM = mean(AVGLOGSUM)) %>%
    collect() %>%

    # trim to scope
    inner_join(zt)

}
