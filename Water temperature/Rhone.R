library(raster)
library(sf)
library(tidyverse)


# Load the raster image of Doubs river from GEE
Rhone <- raster("Rhone_2018.tif")
Rhone_max <- raster("Rhone_max.tif")
plot(Rhone)

coefs <- coef(reg_BT_01)

Rhone.mean <- Rhone*coefs[2] + coefs[1]
Rhone.max <- Rhone_max*coefs[2] + coefs[1]
plot(Rhone.mean)
plot(Rhone.max)

Rhone_mean <- cellStats(Rhone.mean,stat = "mean")
Rhone_max <- cellStats(Rhone.max,stat = "max")

Days <- seq(1,30,1)
up <- seq(Rhone_mean,Rhone_max,length.out = 15)
down <- seq(Rhone_max,Rhone_mean,length.out = 15)
Temp <- c(up,down)
rho_sim <- data.frame(Days,Temp)

save(rho_sim, file = "rho_sim.Rda")
