# beginning with swimControls.Rdata file from Alex Bettinardi
# OEA forecasts for population and employment by county
emp <- as.data.frame(Emp.CoYr) %>% tbl_df() %>%
  mutate(county = row.names(Emp.CoYr)) %>%
  gather(year, employment, -county)


pop <- as.data.frame(Pop.CoYr) %>% tbl_df() %>%
  mutate(county = row.names(Pop.CoYr)) %>%
  gather(year, population, -county)


county_controls <- pop %>% left_join(emp) %>%
  mutate(year = as.numeric(year)) %>%
  gather(var, y, population:employment)

devtools::use_data(county_controls, overwrite = TRUE)

# historical population for Oregon counties
historical_pop <- read_excel(
  "~/Documents/projects/swim/Oregon population 1930-2010.xlsx"
  ) %>%
  gather(year, y, -County, convert = TRUE) %>%
  rename(county = County)

devtools::use_data(historical_pop, overwrite = TRUE)


