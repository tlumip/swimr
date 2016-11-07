#' Add a geographic facet lookup to a database
#'
#' @param db A SWIM scenario database
#' @param df A lookup table with two colums:
#' \enumerate{
#'   \item{A zone field, which should be named either \code{Azone} or
#'   \code{BZONE}, depending on the level of the zone aggregation.}
#'   \item{An aggregation field, which will have some value for all zones. The
#'   name of the data column will be the name of field in the table.}
#' }
#'
#' @return Writes the new column to the \code{ALLZONES} table of the database.
#'
#' @export
#'
add_lookup <- function(db, df){


  tbl <- tbl(db, "ALLZONES") %>%
    collect() %>%
    left_join(df)

  db_drop_table(db$con, "ALLZONES", force = TRUE)
  copy_to(db, tbl, name = "ALLZONES", temporary = FALSE)



}
