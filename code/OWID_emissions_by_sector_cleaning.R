# Annual greenhouse gas emissions by sector by country 1990 - 2018

# https://ourworldindata.org/emissions-by-sector#annual-greenhouse-gas-emissions-by-sector
# Greenhouse gas emissions from other fuel combustion, measured in tonnes of 
# carbon dioxide-equivalents.

# Aggregated from CAIT Climate Data Explorer via Climate Watch

rm(list=ls())

# load libraries
library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
library(writexl)

sector <- read.csv("ghg-emissions-by-sector.csv") %>%
  select(-2) %>% 
  setNames(tolower(gsub("[[:punct:]]and", "", 
                        gsub("energy", "", c("country", names(.)[2:13])))))

countries <- unique(sector$country)
plots <- list()
for (i in countries){
  p <- sector %>% 
    filter(., country == i) %>% 
    pivot_longer(names_to = "sector", cols = 3:13) %>% 
    ggplot(., aes(x = year, y = value, group = sector, color = sector)) +
    geom_line() +
    ggtitle(i)
  
  plots[[i]] <- p
}

library(gridExtra)

ggsave(
  filename = "ghg_emissions_by_sector_plots.pdf", 
  plot = marrangeGrob(plots, nrow=4, ncol=3), 
  width = 15, height = 9
)

write_xlsx(sector, "owid_ghg_emissions_by_sector.xlsx")

