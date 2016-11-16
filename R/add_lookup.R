#' Add a geographic facet lookup to a database
#'
#' @param db A SWIM scenario database
#' @param df A lookup table with two colums:
#' \enumerate{
#'   \item{A beta zone field \code{BZONE}, depending on the level of the zone
#'   aggregation.}
#'   \item{An aggregation field, which will have some value for all zones. The
#'   name of the data column will be the name of field in the table.}
#' }
#'
#' @return Writes the new column to the \code{ALLZONES} table of the database.
#'
#' @export
#'
add_lookup <- function(db, df){

  allzones <- tbl(db, "ALLZONES") %>% collect()
  bzones <- tbl(db, "BZONE") %>% collect()

  # check if field already exists
  new_field <- names(df)[2]
  if(new_field %in% names(allzones)){
    message("Field ", new_field, " already exists in allzones")
  } else {
    # add to alzones
    allzones <- allzones %>% left_join(df, by = "BZONE")
    db_drop_table(db$con, "ALLZONES", force = TRUE)
    copy_to(db, allzones, name = "ALLZONES", temporary = FALSE)
  }

  if(new_field %in% names(bzones)){
    message("Field ", new_field, " already exists in bzones")
  } else {
    # add to beta zones
    bzones <- bzones %>% left_join(df, by = "BZONE")
    db_drop_table(db$con, "BZONE", force = TRUE)
    copy_to(db, bzones, name = "BZONE", temporary = FALSE)
  }

}

#' Remove a column from a database table
#'
#' This might be useful if you make a mistake with \link{add_lookup}.
#' As a warning, this is a destructive operation. Use with care.
#'
#' @param db A scenario database.
#' @param table The name of the table in the database.
#' @param column The name of the column to drop.
#'
#'
#'
remove_field <- function(db, table, column){
  db_tbl <- tbl(db, table) %>%
    select_(.dots = interp(~ -x, x= as.name(column)))

  db_drop_table(db$con, table, force = TRUE)
  copy_to(db, db_tbl, name = table, temporary = FALSE)

}
