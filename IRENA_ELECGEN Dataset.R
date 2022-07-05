# IRENA (2021), Renewable energy statistics 2021, International Renewable 
# Energy Agency (IRENA), Abu Dhabi

# Electricity generation (GWh) is the gross electricity produced by electricity 
# plants, combined heat and power plants (CHP) and distributed generators measured 
# at the output terminals of generation. It includes on-grid and off-grid generation, 
# and it also includes the electricity self-consumed in energy industries; not 
# only the electricity fed into the grid (net electricity production).

rm(list=ls())

# load libraries
library(dplyr)
library(tidyr)
library(xlsx)
library(readxl)

setwd("/Users/ebbamark/Desktop/EU Road Emissions Work/Datasets/")

ongrid <- read_xlsx("IRENA_ELEC_GEN/ELECGEN_IRENA_ONGRID.xlsx", range = cell_rows(2:79922)) %>%
  mutate(country = ifelse(country=="Slovakia", "Slovak Republic", country), gwh = as.numeric(gwh)) %>%
  fill(country, technology, on_off) %>%
  pivot_wider(id_cols = c(country, year), names_from = technology, values_from = gwh) %>%
  setNames(tolower(gsub(" ", "_", (names(.)))))

ongrid$total_gwh <- rowSums(as.matrix(ongrid[,3:20]), na.rm = TRUE)

offgrid <- read_xlsx("IRENA_ELEC_GEN/ELECGEN_IRENA_OFFGRID.xlsx", range = cell_rows(2:79922)) %>%
  mutate(country = ifelse(country=="Slovakia", "Slovak Republic", country), gwh = as.numeric(gwh)) %>%
  fill(country, technology, on_off) %>%
  pivot_wider(id_cols = c(country, year), names_from = technology, values_from = gwh) %>% 
  setNames(tolower(gsub(" ", "_", (names(.)))))

offgrid$total_gwh <- rowSums(as.matrix(offgrid[,3:20]), na.rm = TRUE)

gwh_on <- sum(select(ongrid, 3:20), na.rm = TRUE)
gwh_off <- sum(select(offgrid, 3:20), na.rm = TRUE)
gwh_on/(gwh_on + gwh_off)

writexl::write_xlsx(ongrid, "clean_elecgen_irena_2000_19.xlsx")
writexl::write_xlsx(offgrid, "clean_elecgen_irena_2000_19.xlsx")
