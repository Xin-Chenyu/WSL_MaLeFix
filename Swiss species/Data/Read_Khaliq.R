library(readr)
library(taxize)
library(dplyr)
library(magrittr)   

## Load the species list in Switzerland 
species_list <- readRDS("species_list_ch.rds")

## Load the GlobThem Data
Khaliq_mammals <- read_csv("Swiss species/Khaliq mammals.csv")


## Select species that are in the species list of Switzerland
Khaliq_mammals_Swiss <- Khaliq_mammals[Khaliq_mammals$Species %in% species_list,]

save(AcclimationSwiss, file = "AcclimationSwiss.RData")