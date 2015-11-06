#' Extract model-wide composite utilities
#'
#' @param db Scenario database.
#' @param filters list object containing character strings on which to filter
#'   parameters.
#'
#' @export
extract_modelwide <- function(db, filters){


  df <- tbl(db, "MODELWIDE") %>% collect()

  for(i in filters){
    df <- df %>% filter(grepl(i, DATA))
  }

  df %>%
    mutate(year = as.numeric(TSTEP) + 1990)

}



#' Plot model wide parameters.
#'
#' @param db Scenario database.
#' @param filters list object containing character strings on which to filter
#'   parameters.
#'
#' @export
plot_modelwide <- function(db, filters){

  df <- extract_modelwide(db, filters)

  p <- ggplot(df, aes(x = year, y = VALUE, color = DATA)) +
    geom_path() +
    theme_bw() + theme(axis.text.x = element_text(angle = 30))

}
