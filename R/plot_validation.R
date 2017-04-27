#' Plot base-year traffic validation.
#'
#' Create a plot of the base scenario link volumes compared with observed AADT.
#'
#' @param db The scenario database.
#' @param year the year to use in the validation plot.
#' @param trucks Build a truck count validation table? Defaults to FALSE
#' @return A ggplot2 object.
#'
#' @export
plot_countcomparison <- function(db, year = c(2010, 2013, 2015), trucks = FALSE){

  link_vols <- suppressMessages(get_validation_table(db, year, trucks))

  outviz::plot_validation(link_vols, ifelse(trucks, "trucks", "volume"),
                          "aawdt", show_lm = TRUE) +
    ggplot2::theme_bw()
}


#' Plot historical and projected traffic counts
#'
#' @param db The scenario database.
#' @param atr A numeric vector indicating the traffic recorders to plot track.
#' @return A ggplot2 object
#'
#' @export
#'
plot_traffic_count <- function(db, atr = c("01-001", "01-011", "01-012")){



  counts <- dplyr::tbl(db, "COUNTLOCATIONS") %>%
    dplyr::select(
      ANODE = FROMNODENO, BNODE = TONODENO, ATR_NUM,
      `2000` = AAWDT_2000, `2005` = AAWDT_2005, `2010` = AAWDT_2010,
      `2015` = AAWDT_2015
    ) %>%
    dplyr::filter(ATR_NUM %in% atr) %>%
    collect(n=Inf) %>%
    tidyr::gather(year, volume, `2000`:`2015`) %>%
    dplyr::filter(volume > 1) %>%
    dplyr::mutate(data = "ODOT", volume = as.numeric(volume), year = as.numeric(year))

  # get link traffic volumes
  swim <- dplyr::tbl(db, "LINK_DATA") %>%
    dplyr::select(ANODE, BNODE, TSTEP, volume = DAILY_VOL_TOTAL) %>%
    collect(n=Inf) %>%
    dplyr::mutate(year = as.numeric(TSTEP) + 1990, data = "SWIM") %>%
    inner_join(
      counts %>%
        group_by(ANODE, BNODE, ATR_NUM) %>%
        tally() %>%
        dplyr::select(-n),
      by = c("ANODE", "BNODE"))


  d <- bind_rows(counts, swim) %>%
    group_by(data, year, ATR_NUM) %>%
    dplyr::summarize(volume = sum(volume))

  ggplot2::ggplot(
    d,
    ggplot2::aes(x = year, y = volume, color = factor(ATR_NUM), lty = data) ) +
    ggplot2::geom_path() +

    ggplot2::xlab("Year") + ggplot2::ylab("AAWDT") +
    ggplot2::scale_color_discrete("ATR Location") +
    ggplot2::scale_linetype("Source") +
    ggplot2::theme_bw()

}

#' Get comparison of link volumes to ATR count
#'
#'
#' @param db The scenario database.
#' @param year the year to build the comparison table for.
#'
#' @return A ggplot2 object.
#'
#' @export
get_validation_table <- function(db, year = c(2010, 2013), trucks = FALSE){


  # get link traffic volumes
  dplyr::tbl(db, "LINK_DATA") %>%
    dplyr::select(ANODE, BNODE, TSTEP, DAILY_VOL_TOTAL, DAILY_VOL_TRUCK, PLANNO) %>%
    collect(n=Inf) %>%
    dplyr::rename(volume = DAILY_VOL_TOTAL, trucks = DAILY_VOL_TRUCK) %>%
    dplyr::mutate(year = as.numeric(TSTEP) + 1990) %>%
    dplyr::filter_(lazyeval::interp(~ x == year, x = as.name("year"), year = year)) %>%

    # Join ATR ID and calculate two-way volume
    inner_join(
      get_counts(db, year = year, trucks),
      by = c("ANODE", "BNODE", "year")
    ) %>%

    # join facility types
    dplyr::left_join(fac_types, by = "PLANNO")

}



#' Get counts table
#'
#' @param db The scenario database.
#' @param trucks Get truck counts or AAWDT counts?
#'
#' @return A counts dataframe w
#'
get_counts <- function(db, trucks = FALSE, year = c(2010, 2013, 2015)){

  if (trucks) {
    if (year != 2013){
      stop("Truck counts only available in 2013.")
    }
  } else {
    if (!(year %in% c(2010, 2015))){
      stop("Total AAWDT counts only available in 2010 and 2015")
    }
  }


  # get counts data table from database
  if (trucks) {
    dplyr::tbl(db, "COUNTLOCATIONS") %>%
      dplyr::select(
        ANODE = FROMNODENO, BNODE = TONODENO, XCOORD, YCOORD,
        ATR_NUM, MUT, SUT
      ) %>%
      dplyr::filter(MUT != "None") %>%
      collect(n=Inf) %>%
      dplyr::mutate(
        MUT = ifelse(MUT == "None", NA, as.numeric(MUT)),
        SUT = ifelse(SUT == "None", NA, as.numeric(SUT)),
        aawdt = MUT + SUT,
        year = 2013

      )

  } else {
    dplyr::tbl(db, "COUNTLOCATIONS") %>%
      dplyr::select(
        ANODE = FROMNODENO, BNODE = TONODENO, XCOORD, YCOORD, ATR_NUM,
        `2000` = AAWDT_2000, `2005` = AAWDT_2005, `2010` = AAWDT_2010,
        `2015` = AAWDT_2015
      ) %>%
      dplyr::filter(ATR_NUM != "") %>%
      collect(n=Inf) %>%
      tidyr::gather(year, aawdt, `2000`:`2015`) %>%
      dplyr::mutate_at(vars(aawdt, year), funs(as.numeric(.))) %>%
      dplyr::filter_(lazyeval::interp(~ x == year, x = as.name("year"), year = year))

  }


}
