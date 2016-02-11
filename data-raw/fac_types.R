fac_types <- data_frame(
  PLANNO = c(0:8, 11:18),
  FacType = c(
    "Local",
    "Expressway",
    "Expressway",
    "Arterial",
    "Arterial",
    "Arterial",
    "Arterial",
    "Collector",
    "Collector",
    "Expressway",
    "Expressway",
    "Arterial",
    "Arterial",
    "Arterial",
    "Arterial",
    "Collector",
    "Collector"
  )
) %>%
  mutate(
    FacType = factor(FacType,
                     levels = c("Expressway", "Arterial", "Collector", "Local"))
  )

devtools::use_data(fac_types, overwrite = TRUE)

mode_types <- data_frame(
  mode = c(
    "BIKE", "DA", "DR_TRAN", "SCHOOL_BUS", "SR2", "SR3P",
    "TRK", "WALK", "WK_TRAN"
  ),
  consolidated_mode = c(
    "non-motorized", "auto", "transit", "school", "auto",
    "auto", "truck", "non-motorized", "transit"
  )
)

#devtools::use_data(mode_types)
