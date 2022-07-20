rm(list=ls())

# load libraries
library(data.table)
library(dplyr)
library(tidyr)
library(xlsx)
library(readxl)
library(openxlsx)
library(stringr)
library(gets)
library(getspanel)


dat2 <- read_excel("/Users/ebbamark/Desktop/EU Road Emissions Work/Datasets/v6_EDGAR_CO2/v60_CO2_excl_short-cycle_org_C_1970_2018.xls", skip = 9) %>%
  mutate(Name = ifelse(Name=="Slovakia", "Slovak Republic", Name)) %>%
  pivot_longer(cols = starts_with("Y_"), names_to = "year") %>%
  pivot_wider(id_cols = c("year", "Name"), names_from = "ipcc_code_2006_for_standard_report") %>% 
  setNames(tolower(gsub(" ", "_", (names(.))))) %>% 
  mutate(country=name, year=as.numeric(str_remove(year, "Y_")))

transp.em <- read_excel("/Users/ebbamark/Desktop/EU Road Emissions Work/Datasets/v6_EDGAR_CO2/v60_CO2_excl_short-cycle_org_C_1970_2018.xls", skip = 9) %>%
  mutate(Name = ifelse(Name=="Slovakia", "Slovak Republic", Name)) 

vars <- unique(transp.em$ipcc_code_2006_for_standard_report)

for(i in 1:length(vars)){
  code = vars[i]
  tester <- transp.em %>%
    filter(ipcc_code_2006_for_standard_report==code) %>%
    pivot_longer(cols = starts_with("Y_"), names_to = "year") %>%
    transmute(country=Name, year=as.numeric(str_remove(year, "Y_")),
              tester=as.numeric(value)) %>% 
    filter(!is.na(tester))
  
  dat[,c(2,1,3)] %>% 
    setNames(c("country", "year", "ems")) %>% 
    mutate(year = as.numeric(year), ems = as.numeric(ems)) %>%
    filter(!is.na(ems)) %>% 
    print(all.equal(., tester))
}

tester <- transp.em %>%
  filter(ipcc_code_2006_for_standard_report==code) %>%
  pivot_longer(cols = starts_with("Y_"), names_to = "year") %>%
  transmute(country=Name, year=as.numeric(str_remove(year, "Y_")),
            tester=as.numeric(value))

dat[,c(2,1,7)] %>% 
  setNames(c("country", "year", "transport.emissions")) %>% 
  filter(!is.na(transport.emissions)) %>% 
  mutate(year = as.numeric(year), transport.emissions = as.numeric(transport.emissions)) %>% 
  all.equal(., transp.em)
