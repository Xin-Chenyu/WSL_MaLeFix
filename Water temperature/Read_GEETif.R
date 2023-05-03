library(raster)
library(sf)
library(tidyverse)
library(ggthemes)
library(ggpubr)


# Load the raster image from GEE
LST_2014 <- raster("LST_year_landsat_2014_01.tif")
BT_2014 <- raster("thermal_Landsat_2014_01.tif")

# Load the raster image from Konrad's example
fcp <- raster("fcp.tif")
plot(fcp)


## Load the slope and elevation information 
dem <- readRDS("alti3d2016_dem_mean2m.rds")
slope <- readRDS("alti3d2016_slope_mean2m.rds")


# Make the GEE images and  have the same projection as Konrad's
LST_2014 <- projectRaster(LST_2014,fcp)
BT_2014 <- projectRaster(BT_2014,fcp)
plot(LST_2014)
plot(BT_2014)

dem <- projectRaster(dem,fcp)
slope <- projectRaster(slope,fcp)


dd <- st_read("hydrometrische_Stationen_2019/ezg_kombiniert.shp")


## Load the shapefiles of water temperature monitoring stations
stations <- st_read("data/hydrometrische_Stationen_2019/lhg_UBST.shp")
st_crs(stations) <- 'EPSG:21781'
station_utm <- st_transform(stations, st_crs(LST_2014))
LST_stations <- raster::extract(LST_2014, station_utm)
BT_stations <- raster::extract(BT_2014,station_utm)
dem_stations <- raster::extract(dem, station_utm)
slope_stations <- raster::extract(slope, station_utm)

waterT_GEE <- data.frame(station_n = station_utm$EDV_NR4,
                         GEE_LST = LST_stations,
                         GEE_BT = BT_stations,
                         dem = dem_stations,
                         slope = slope_stations,
                         X = st_coordinates(station_utm)[,1],
                         Y = st_coordinates(station_utm)[,2],
                         year = rep(2014, 251))



## convent the digital elevation model to a dataframe
dem_df <- as.data.frame(dem, xy = TRUE)
head(dem_df)




ggplot() +
  geom_raster(data = dem_df, aes(x = x, y = y, fill = ch_topo_alti3d2016_pixel_dem_mean2m)) +
  geom_point(data = waterT_GEE, aes(x = X, y = Y, color = GEE_LST)) + 
  scale_color_viridis(option = "H")+
  theme_classic()+ 
  coord_quickmap()

save(waterT_GEE, file = "waterT_GEE.Rda")




merge_temp = na.omit(merge(waterT_GEE,waterT_summer, by = c("station_n","year")))


merge_temp$diff_LST <- merge_temp$GEE_LST - merge_temp$BAFU_T
merge_temp$diff_BT <- merge_temp$GEE_BT - merge_temp$BAFU_T

save(merge_temp, file = "merge_temp.Rda")
write.csv(merge_temp, file = "merge_temp.csv")

merge_temp <- na.omit(read.csv("waterT_stations.csv"))
merge_temp$station_n <- as.character(merge_temp$station_n)

merge_temp %>% 
  filter(season == "summer")%>% 
  ggplot(aes(x = GEE_LST, y = BAFU_T, size = dem)) +
  geom_point() +
  geom_smooth(method='lm', color="red", size=0.5, se=FALSE) +
  theme_classic()

mse(merge_temp$GEE_LST,merge_temp$BAFU_T)

a <- merge_temp %>% 
  #filter(season == "summer")%>% 
  ggplot(aes(x = GEE_BT, y = BAFU_T, color = station_n, shape = season)) +
  geom_point() +
  geom_smooth(method='lm', color="red", size=0.5, se=FALSE) +
  labs(y = "Water temperature from BAFU", 
       x = "Brightness temperature from remote sensing",
       color = "station number")+
  theme(legend.position="top")+
  theme_bw()

b <- merge_temp %>% 
  filter(season == "summer")%>% 
  ggplot(aes(x = GEE_BT, y = BAFU_T, color = station_n)) +
  geom_point() +
  geom_smooth(method='lm', color="red", size=0.5, se=FALSE) +
  labs(y = "Water temperature from BAFU", 
       x = "Brightness temperature from remote sensing",
       color = "station number")+
  theme(legend.position="top")+
  theme_bw()

ggarrange(a, b, 
          labels = c("a", "b"),
          ncol = 1, nrow = 2)


mse(merge_temp$GEE_BT,merge_temp$BAFU_T)

reg_BT_01 <- lm(BAFU_T ~ GEE_BT, data = merge_temp %>% 
                  filter(season == "summer"))
summary(reg_BT_01)

reg_BT <- lm(BAFU_T ~ GEE_BT + dem + slope, data = merge_temp)
summary(reg_BT)

reg_LST_01 <- lm(BAFU_T ~ GEE_LST, data = merge_temp)
summary(reg_LST_01)

reg_LST <- lm(BAFU_T ~ GEE_LST + dem + slope, data = merge_temp)
summary(reg_LST)


plot(dem, main = "Difference between the brightness T & real T")
points(merge_temp$X[merge_temp$diff_BT >= 10], 
       merge_temp$Y[merge_temp$diff_BT >= 10], 
       col = "purple",pch=16, cex = 1)
points(merge_temp$X[merge_temp$diff_BT <= 10], 
       merge_temp$Y[merge_temp$diff_BT <= 10], 
       col = "cyan",pch=16, cex = 1)
legend("topleft", 
       legend = c("larger than 10 Celsius", "less than 10 Celsius"), 
       col = c("purple","orange"), pch = 16, bty = "n")

library(RColorBrewer)
col.temp <- colorRampPalette(c("cyan","green","yellow","red"))
merge_temp$col <- col.temp(500)[as.numeric(cut(merge_temp$diff_LST,breaks = 500))]
merge_temp <- merge_temp[order(merge_temp$diff_LST),]

par(mar=c(0,0,0,0))
clr <- colorRampPalette(brewer.pal(9, 'Blues'))(21)
plot(dem, col=clr, axes=F, box=F, legend=F)


for(i in 1:nrow(merge_temp)){
  points(merge_temp[i, 'X'], merge_temp[i, 'Y'], bg=merge_temp[i, 'col'], 
         cex=1, pch=21, col='black', lwd='.6')
  legend("bottomright", 
         col=c(merge_temp$col[1], merge_temp$col[nrow(merge_temp)/1.33], 
               merge_temp$col[nrow(merge_temp)/1.67], merge_temp$col[nrow(merge_temp)]),
         legend=c(round(merge_temp$diff_LST[1],3), 
                  round(merge_temp$diff_LST[nrow(merge_temp)/1.33],3), 
                  round(merge_temp$diff_LST[nrow(merge_temp)/1.67],3), 
                  round(merge_temp$diff_LST[nrow(merge_temp)],3)), pch=19)
  #legend.gradient(846755.9, 264672, col=c('yellow', 'red'))
}
dev.off()

plot(dem, main = "Difference between the land surface T & real T")
points(merge_temp$X[merge_temp$diff_LST >= 10], 
       merge_temp$Y[merge_temp$diff_LST >= 10], 
       col = "purple",pch=16, cex = 1)
points(merge_temp$X[merge_temp$diff_LST <= 10], 
       merge_temp$Y[merge_temp$diff_LST <= 10], 
       col = "cyan",pch=16, cex = 1)
legend("topleft", 
       legend = c("larger than 10 Celsius", "less than 10 Celsius"), 
       col = c("purple","cyan"), pch = 16, bty = "n")


library(rgdal)
sf::st_write(stations, '.', layer ='stations',driver='ESRI Shapefile')


ch_stations$geometry[ch_stations$id == 2606]


