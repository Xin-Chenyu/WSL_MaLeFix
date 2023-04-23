library(readr)
library(tidyverse)

## list all the files with daily mean temperature from 2014 - 2018
waterT_files <- list.files(pattern = "*Tagesmittel_2014-01-01_2018-12-31.csv" )


## Create an empty data frame to store the results
waterT_summer <- data.frame(station_n = numeric(), year = numeric(), BAFU_T = numeric())
waterT_winter <- data.frame(station_n = numeric(), year = numeric(), BAFU_T = numeric())

## Create a loop to fill in the summer/winter temperature for each stations 


## Loop for summers
idx = 1
for(i in 1:length(waterT_files)){
  file_name <- waterT_files[i]
  waterT_df <- read_delim(file_name,skip = 8, delim =';',locale = locale(encoding = "ISO-8859-15",decimal_mark = "."))
  for (j in 2014:2018) {
    waterT_summer[idx,1] <- waterT_df$Stationsnummer[1]
    waterT_summer[idx,2] <- j
    waterT_df$Zeitstempel <- as.Date(waterT_df$Zeitstempel)
    waterT_df_summer <- waterT_df %>% filter(Zeitstempel >= paste0(j,"-07-01"), Zeitstempel <= paste0(j,"-09-30"))
    waterT_summer[idx,3] <- mean(waterT_df_summer$Wert)
    waterT_summer$season <- "summer"
    idx = idx+1
  }
}

## Loop for winters
idx = 1
for(i in 1:length(waterT_files)){
  file_name <- waterT_files[i]
  waterT_df <- read_delim(file_name,skip = 8, delim =';',locale = locale(encoding = "ISO-8859-15",decimal_mark = "."))
  for (j in 2014:2018) {
    waterT_winter[idx,1] <- waterT_df$Stationsnummer[1]
    waterT_winter[idx,2] <- j
    waterT_df$Zeitstempel <- as.Date(waterT_df$Zeitstempel)
    waterT_df_winter <- waterT_df %>% filter(Zeitstempel >= paste0(j,"-01-01"), Zeitstempel <= paste0(j,"-03-31"))
    waterT_winter[idx,3] <- mean(waterT_df_winter$Wert)
    waterT_winter$season <- "winter"
    idx = idx+1
  }
}




save(waterT_summer, file = "waterT_summer.Rda")
save(waterT_winter, file = "waterT_winter.Rda")


waterT_BAFU <- rbind(waterT_summer,waterT_winter)
station_select <- c(2016,2018,2029,2030,2044,2085,2113,2135,2308,2392,2457,2606)

waterT_stations <- waterT_BAFU[waterT_BAFU$station_n %in% station_select, ]
waterT_stations <- arrange(waterT_stations,station_n)

write.csv(waterT_stations, file = "waterT_stations.csv")
