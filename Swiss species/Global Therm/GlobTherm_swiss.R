library(readr)
library(taxize)

## Load the species list in Switzerland 
species_list <- readRDS("species_list_ch.rds")

## Load the GlobThem Data
GlobTherm <- read_csv("GlobalTherm_upload_02_11_17.csv")

## Create a columns with the combination of genus and species names 
GlobTherm$name <- paste(GlobTherm$Genus,GlobTherm$Species)

GlobThermSwiss <- GlobTherm[GlobTherm$name %in% species_list,]
save(GlobThermSwiss, file = "GlobThermSwiss.RData")

load("GlobThermSwiss.RData")
