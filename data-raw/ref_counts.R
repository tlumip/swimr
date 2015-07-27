# Read base year counts and save
library(readr)

ref_counts <- read_csv("data-raw/reference-aadt-counts.csv",
                       col_types = "cnncc")

save(ref_counts, file = "data/ref_counts.rda")
