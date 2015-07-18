# Read base year counts and save

library(readr)

ref_counts <- read_csv("data-raw/reference-aadt-counts.csv",
                       col_types = "cnncc")

devtools::use_data(ref_counts)
