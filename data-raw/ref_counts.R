# Read base year counts and save
library(readxl)
library(readr)
library(tidyr)
library(dplyr)


counts <- read_excel("data-raw/ATRHistory.xlsx", sheet = "AAWDT") %>%
  gather(year, aawdt, `2000`:`2035`) %>%
  filter(!is.na(aawdt)) %>%

  # counter supplies two-way volume, but we only have linkid on one side
  select(site = ATR, aawdt, year) %>%
  spread(year, aawdt, fill = NA)

countid <- read_excel("data-raw/link_counts.xlsx") %>%
  select(ANODE, BNODE, site)

devtools::use_data(counts, overwrite = TRUE)
devtools::use_data(countid, overwrite = TRUE)
