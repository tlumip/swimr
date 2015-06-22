# beginning with swimControls.Rdata file from Alex Bettinardi

emp <- as.data.frame(Emp.CoYr) %>% tbl_df() %>%
  mutate(county = row.names(Emp.CoYr)) %>%
  gather(year, employment, -county)


pop <- as.data.frame(Pop.CoYr) %>% tbl_df() %>%
  mutate(county = row.names(Pop.CoYr)) %>%
  gather(year, population, -county)


county_controls <- pop %>% left_join(emp)

ggplot(county_controls,
       aes(x = as.numeric(year), y = log(employment), color = county)
       ) +
  geom_line()
