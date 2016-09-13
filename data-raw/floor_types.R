floor_types <- data_frame(
  commodity = c(
    "FLR_Accommodation", "FLR_AT", "FLR_Government_Support", "FLR_Heavy_Industry",
    "FLR_Hospital", "FLR_Institutional", "FLR_K12", "FLR_Light_Industry",
    "FLR_MF", "FLR_MH", "FLR_Office", "FLR_Retail", "FLR_RRMH", "FLR_RRSFD",
    "FLR_SFD", "FLR_Warehouse"
  ),
  floor_type = c(
    "Retail", "Multifamily", "Institutional", "Industrial",
    "Institutional", "Institutional", "Institutional",
    "Industrial", "Multifamily", "Multifamily",
    "Commercial", "Retail", "Multifamily", "Singlefamily",
    "Singlefamily", "Industrial"
  )
)

devtools::use_data(floor_types, overwrite = TRUE)
