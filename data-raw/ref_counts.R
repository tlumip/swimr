# Read base year counts and save
library(readxl)
library(readr)
library(tidyr)
library(dplyr)

counts <- read_excel("data-raw/ATRHistory.xlsx", sheet = "AAWDT") %>%
  gather(year, aawdt, `2000`:`2035`) %>%
  filter(year < 2015, !is.na(aawdt)) %>%
  select(site = ATR, aawdt, year)


ref_counts <- read_csv("data-raw/reference-aadt-counts.csv",
                       col_types = "cnncc") %>%
  mutate(site = as.numeric(gsub("[^0-9]","",site))) %>%
  select(-AADT, -year)

ref_counts <- left_join(ref_counts, counts, by = "site")

devtools::use_data(ref_counts, overwrite = TRUE)
