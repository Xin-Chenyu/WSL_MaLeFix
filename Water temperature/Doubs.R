library(raster)
library(sf)
library(tidyverse)


# Load the raster image of Doubs river from GEE
Doubs <- raster("Doubs_2018.tif")
Doubs_max <- raster("Doubs_max.tif")
plot(Doubs)

coefs <- coef(reg_BT_01)

Doubs.mean <- Doubs*coefs[2] + coefs[1]
Doubs.max <- Doubs_max*coefs[2] + coefs[1]
plot(Doubs.max)

doubs_mean <- cellStats(Doubs.mean,stat = "mean")
doubs_max <- cellStats(Doubs.max,stat = "max")



Days <- seq(1,30,1)
up <- seq(doubs_mean,doubs_max,length.out = 15)
down <- seq(doubs_max,doubs_mean,length.out = 15)
Temp <- c(up,down)
dou_sim <- data.frame(Days,Temp)

save(dou_sim, file = "dou_sim.Rda")

