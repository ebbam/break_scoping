# IEA Indicators

iea <- read.csv(here("data/raw/IEA_elec_data/IEA_EI_CB_16122022150351306.csv")) %>%
  rename(elec_gwh = Value, year = Time) %>%
  mutate(country = ifelse(Country %in% names(getcountry), unname(getcountry[Country]), Country),
         elec_twh = elec_gwh/1000) %>%
  select(country, Balance, year, elec_twh) %>%
  filter(country %in% sel & year >=1990) %>% 
  pivot_wider(id_cols = c(country, year), names_from = Balance, values_from = elec_twh, names_glue = "{Balance}_{.value}") %>% 
    clean_names %>% 
  mutate(net_imports_twh = imports_elec_twh - exports_elec_twh,
         elec_demand_twh = total_gross_production_elec_twh + net_imports_twh)

iea$total_consumption_calculated_elec_twh[iea$country == "United States" & iea$year == 2021] <- NA
iea$total_consumption_calculated_elec_twh[iea$country == "New Zealand" & iea$year == 2021] <- NA


owid_all <-  read_excel(here("data/temp/owid_all.xlsx")) %>%
    select(-iso_code) %>% 
    select(country, year, electricity_demand, electricity_generation, net_elec_imports) %>% 
    mutate(country = ifelse(country %in% names(getcountry), unname(getcountry[country]), country)) %>%
    filter(country %in% sel & year >=1990)

saveRDS(iea, here("data/temp/iea_elec_data.RDS"))

ggplot() +
  #geom_line(data = iea, aes(x = year, y = net_production_twh) , color = "blue") +
  #geom_line(data = iea, aes(x = year, y = total_net_production_elec_twh) , color = "blue") +
  geom_line(data = owid_all, aes(x = year, y = electricity_generation - electricity_demand), color = "red") +
  geom_line(data = owid_all, aes(x = year, y = -net_elec_imports))+
  #geom_line(data = owid_all, aes(x = year, y = net_elec_imports), color = "blue") +
  #geom_line(data = owid_all, aes(x = year, y = electricity_demand), color = "green") +
  facet_wrap(~country, scales = "free")

owid_all %>% select(country, year, electricity_generation) %>% filter(!complete.cases(.)) %>% View()
  

product_retain <- read_csv(here("data/raw/IEA_elec_data/IEA_EI_CF_by_source.csv")) %>% 
  select(Country, Product, Balance, Time, Value) %>% 
  filter(Country %in% sel & Time >= 1990) %>% 
  group_by(Product, Balance) %>% 
  summarise(across(everything(), list(n_obs = ~ n(), n_miss = ~ sum(is.na(.x))))) %>% View()
  filter(Value_n_obs != Value_n_miss) %>% 
  pull(Product) %>% unique


# read_csv(here("data/raw/IEA_elec_data/IEA_EI_CF_by_source.csv")) %>% 
#   filter(Product %in% product_retain & Balance == "Gross electricity production (GWh)") %>% 
#   pull(Product) %>% unique %>% 
#   data.frame %>% write.xlsx(here("data/raw/IEA_elec_data/energy_types_lookup.xlsx"))

clean_dat <- read_csv(here("data/raw/IEA_elec_data/IEA_EI_CF_by_source.csv")) %>% 
  filter(Product %in% product_retain) %>% 
  select(Country, Product, Balance, Time, Value) %>% 
  filter(Country %in% sel & Time >= 1990 & Balance == "Gross electricity production (GWh)") %>% 
  clean_names

codes <- read.xlsx(here("data/raw/IEA_elec_data/energy_types_lookup.xlsx")) %>% 
  right_join(., clean_dat, by = "product") %>% 
  filter(!grepl("Total", product)) %>%
  rename(year = time) %>% 
  group_by(country, year, iea_doc_code) %>%
  summarise(sum_value = sum(value, na.rm = TRUE)) %>% 
  rename(code = iea_doc_code)


orig <- df %>% 
  select(country, year, 
         #electricity_generation,
         #low_carbon_electricity,                 
         #renewables_electricity,                 
         #fossil_electricity,
         coal_electricity,
         gas_electricity, 
         hydro_electricity, 
         nuclear_electricity,
         oil_electricity, 
         solar_electricity, 
         wind_electricity) %>% 
  filter(country %in% sel[sel != "Luxembourg"] & year >=1990) %>% 
  pivot_longer(!c(country, year), names_to = "code", values_to = "sum_value") %>% 
  mutate(code = gsub("_electricity", "", code))

codes %>% 
  filter(code %in% unique(orig$code) & country != "Luxembourg") %>% 
  mutate(sum_value = sum_value/1000) %>% saveRDS(here("data/out/IEA_elec_data_by_source.RDS"))

# comb <- codes %>% 
#   filter(code %in% unique(orig$code) & country != "Luxembourg") %>% 
#   mutate(sum_value = sum_value/1000) %>% 
#   left_join(., orig, by = c("country", "year", "code"))

# pdf(file = here("output/temp/share_comp.pdf"))
# 
# unique_codes <- comb %>% pull(code) %>% unique
# for(cod in unique_codes){
#   p <- comb %>%
#     filter(code == cod) %>%
#     ggplot(aes(x = year)) +
#     geom_line(aes(y = sum_value.x, color = "iea")) +
#     geom_line(aes(y = sum_value.y, color = "owid")) +
#     facet_wrap(~country, scales = "free") +
#     labs(title = cod)
#   print(p)
# }
# 
# dev.off()
