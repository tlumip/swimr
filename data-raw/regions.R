# regions.R

# Convert regions.csv into an rda

regions <- read.csv('data-raw/regions.csv', stringsAsFactors = FALSE)

devtools::use_data(regions, overwrite=TRUE)

# end

