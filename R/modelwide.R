#' Extract model-wide composite utilities
#'
#' @param db Scenario database.
#' @param filters list object containing character strings on which to dplyr::filter
#'   parameters.
#'
#' @export
extract_modelwide <- function(db, filters){


  df <- dplyr::tbl(db, "MODELWIDE") %>% collect(n=Inf)

  for(i in filters){
    df <- df %>% dplyr::filter(grepl(i, DATA))
  }

  df %>%
    dplyr::mutate(year = as.numeric(TSTEP) + 1990)

}



#' Plot model wide parameters.
#'
#' @param db Scenario database.
#' @param filters list object containing character strings on which to dplyr::filter
#'   parameters.
#'
#' @export
plot_modelwide <- function(db, filters){

  df <- extract_modelwide(db, filters)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = year, y = VALUE, color = DATA)) +
    ggplot2::geom_path() +
    ggplot2::theme_bw() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30))

}
