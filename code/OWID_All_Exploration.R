rm(list=ls())

# load libraries
library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
library(writexl)
library(gridExtra)

owid_all <- read.csv('https://nyc3.digitaloceanspaces.com/owid-public/data/energy/owid-energy-data.csv')

# Most countries start in either 1985 or 2000 - need to check which
# Share of electricity
# Share of energy
# Production by source (twh or pct change)
# Total energy consumption change (pct or twh)
# Electricity consumption by source (total and per capita)
# Electricity demand
# ...

countries <- unique(owid_all$country)
plots <- list()
for (i in countries){
  p <- owid_all %>% 
    select(country, year, grep(pattern = "_share_elec", names(owid_all))) %>%
    filter(., country == i & year >= 1985) %>% 
    pivot_longer(names_to = "indicator", cols = 3:15) %>%
    ggplot(., aes(x = year, y = value, group = indicator, color = indicator)) +
    geom_line() +
    ggtitle(i)
  
  plots[[i]] <- p
}

ggsave(
  filename = "owid_electricity_shares.pdf", 
  plot = marrangeGrob(plots, nrow=4, ncol=3), 
  width = 15, height = 9
)

write_xlsx(owid_all, "owid_all.xlsx")

types <- unique(lapply(names(owid_all), function(x) sub(".*?_", "", x)))

tab <- matrix(nrow = length(names(owid_all[4:128])), ncol = max(owid_all$year) - min(owid_all$year) + 1)
  
rownames(tab) <- names(owid_all[4:128])
colnames(tab) <- min(owid_all$year):max(owid_all$year)
for(z in names(owid_all[4:128])){
  for(y in min(owid_all$year):max(owid_all$year)){
    obs <- owid_all %>%
      filter(year == y) %>% 
      pull(z) %>% is.na %>% ifelse(FALSE, TRUE) %>% sum
    tab[z, as.character(y)] <- obs
  }
}


owid_all[-1]%>% 
  pivot_longer(c(-country, -year)) %>% 
  group_by(variable = name) %>% 
  filter(!is.na(value)) %>% 
  summarise(firstyear = min(year), lastyear = max(year)) %>% View()


  