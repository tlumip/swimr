#' Plot base-year traffic validation.
#'
#' Create a plot of the base scenario link volumes compared with observed AADT.
#'
#' @param db The scenario database.
#' @param facet_var Variable to display in facets
#' @return A ggplot2 object.
#' @import outviz
#'
#' @export
plot_countcomparison <- function(db, year = c(2010, 2013)){

  link_vols <- suppressMessages(get_validation_table(db, year))

  outviz::plot_validation(link_vols, "volume", "count", show_lm = TRUE) +
    theme_bw()
}


#' Plot historical and projected traffic counts
#'
#' @param db The scenario database.
#' @param atr A numeric vector indicating the traffic recorders to plot track.
#' @return A ggplot2 object
#'
#' @export
#'
plot_traffic_count <- function(db, atr = c(01001, 01011, 01012)){

  # protect type
  atr <- as.numeric(atr)

  pc <- counts %>%
    filter(site %in% atr) %>%
    select(-`2035`) %>%
    gather(year, volume, `2000`:`2015`) %>%
    mutate(data = "ODOT ATR Counts", year = as.numeric(year))

  # get base year link traffic volumes
  swim <- tbl(db, "LINK_DATA") %>%
    select(ANODE, BNODE, TSTEP, DAILY_VOL_TOTAL, PLANNO) %>%
    collect() %>%
    mutate(year = as.numeric(TSTEP) + 1990) %>%

    # Join ATR ID and calculate two-way volume
    inner_join(countid) %>%
    group_by(site, year) %>%
    filter(site %in% atr) %>%
    summarise(
      volume = sum(DAILY_VOL_TOTAL, na.rm = TRUE),
      data = "SWIM Volume"
    )

  d <- bind_rows(pc, swim)

  ggplot(
    d,
    aes(x = year, y = volume, color = factor(site), lty = data) ) +
    geom_path() +

    xlab("Year") + ylab("AAWDT") +
    scale_color_discrete("ATR Location") +
    scale_linetype("Source") +
    theme_bw()

}

#' Get comparison of link volumes to ATR count
#'
#' Create a plot of the base scenario link volumes compared with observed AADT.
#'
#' @param db The scenario database.
#' @param facet_var Variable to display in facets
#' @return A ggplot2 object.
#' @import outviz
#'
#' @export
get_validation_table <- function(db, year = c(2010, 2013)){

  # get link traffic volumes
  tbl(db, "LINK_DATA") %>%
    select(ANODE, BNODE, TSTEP, DAILY_VOL_TOTAL, PLANNO) %>%
    collect() %>%
    mutate(year = as.numeric(TSTEP) + 1990) %>%
    filter_(lazyeval::interp(~ x == year, x = as.name("year"), year = year)) %>%

    # Join ATR ID and calculate two-way volume
    inner_join(countid) %>%
    group_by(site) %>%
    summarise(
      volume = sum(DAILY_VOL_TOTAL, na.rm = TRUE),
      PLANNO = PLANNO[1]
    ) %>%

    # Join ATR data
    inner_join(counts %>% select_("site", count = as.name(year))) %>%

    # join facility types
    left_join(fac_types, by = "PLANNO")

}



#' Get counts table
#'
#' @param db The scenario database.
#' @param trucks Get truck counts or AAWDT counts?
#'
#' @return A counts dataframe w
#'
#' @importFrom tidyr gather
get_counts <- function(db, trucks = FALSE){

  # get counts data table from database
  if (trucks) {
    tbl(db, "COUNTLOCATIONS") %>%
      select(
        ANODE = FROMNODENO, BNODE = TONODENO, XCOORD, YCOORD,
        ATR_NUM, MUT, SUT
      ) %>%
      filter(MUT != "None") %>%
      collect() %>%
      mutate(
        MUT = ifelse(MUT == "None", NA, as.numeric(MUT)),
        SUT = ifelse(SUT == "None", NA, as.numeric(SUT))
      )

  } else {
    tbl(db, "COUNTLOCATIONS") %>%
      select(
        ANODE = FROMNODENO, BNODE = TONODENO, XCOORD, YCOORD, ATR_NUM,
        `2000` = AAWDT_2000, `2005` = AAWDT_2005, `2010` = AAWDT_2010,
        `2015` = AAWDT_2015
      ) %>%
      filter(ATR_NUM != "") %>%
      collect() %>%
      tidyr::gather(year, aawdt, `2000`:`2015`) %>%
      mutate(aawdt = as.numeric(aawdt))

  }


}
