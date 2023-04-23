library(readr)
library(taxize)
library(dplyr)
library(magrittr)   

## Load the species list in Switzerland 
species_list <- readRDS("species_list_ch.rds")

## Load the Comte_data
Comte_data <- read_csv("Comte_Olden_imputed_data.csv")

## Load the fish table
Fish <- read_csv("Swiss species/Fish.csv")

## Select species that are in the species list of Switzerland
Comte_dataSwiss <- Comte_data[Comte_data$Species %in% species_list,]

save(Comte_dataSwiss, file = "Comte_dataSwiss.RData")

## Fill ctmax values of fish table by matching values in Comte_dataSwiss 
indices <- Fish$SPECIES == Comte_dataSwiss$Species

df1$ele[indices] <- df2$ele[indices]

