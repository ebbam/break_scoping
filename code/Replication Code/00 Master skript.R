# clean memory
rm(list=ls())

# load libraries
library(data.table)
library(dplyr)
library(tidyr)
library(openxlsx)
library(stringr)
library(gets)
library(getspanel)

# set working directory


# call skripts
# data assembly
source(here("code/Replication Code/01 Dataset_creation.R"))

# analysis
source(here("code/Replication Code/02 Analysis.R"))
