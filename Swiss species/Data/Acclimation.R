library(readr)
library(taxize)
library(dplyr)
library(magrittr)   

## Load the species list in Switzerland 
species_list <- readRDS("Global Therm/species_list_ch.rds")

## Load the GlobThem Data
acclimation <- read_csv("Acclimation ectothermic.csv")


## Select species that are in the species list of Switzerland
AcclimationSwiss <- acclimation[acclimation$Name %in% species_list,]

save(AcclimationSwiss, file = "AcclimationSwiss.RData")


