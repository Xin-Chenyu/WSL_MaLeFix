library(dplyr)
library(fields)
library(raster)
library(rgdal)
library(lubridate)
library(ggplot2)
library(reshape2)
library(viridis)
library(crayon)
library(ggthemes)

source("N:/meteodaten/SampleR_read_write/swisscors2lonlat")
load("N:/meteodaten/shape300.dat")


homep <- "D:/ch_500/2003"
setwd(homep)



# list all files with average temperature 
files_temp <- list.files(path = "D:/ch_500/2003", pattern = "\\.temp$")
# list all files with relative humidity  
files_relh <- list.files(path = "D:/ch_500/2003", pattern = "\\.relh$")
# list all files with maximum temperature 
files_txxx <- list.files(path = "D:/ch_500/2003", pattern = "\\.txxx$")


df_temp <- data.frame(Date = Date(),Tici_temp = double(),Vala_temp = double(), Zurch_temp =double())
df_relh <- data.frame(Date = Date(),Tici_relh = double(),Vala_relh = double(), Zurch_relh =double())
df_txxx <- data.frame(Date = Date(),Tici_txxx = double(),Vala_txxx = double(), Zurch_txxx =double())

# Select one sub-catchment in Canton Ticino, Valais and Zurich 
Ticino_poly <- shape.300[shape.300$ch_500_clb==1,]
Valais_poly <- shape.300[shape.300$ch_500_clb==6,]
Zurich_poly <- shape.300[shape.300$ch_500_clb==4,]


idx=0


for (i in files_temp){
  file_date <- as.numeric(substr(i,8,15))
  # Get files from June to Sept 2003
  if (file_date >= 20030518 & file_date <= 20030930 ){
    idx=idx+1
    df_temp[idx,1] <- ymd(file_date)
    filen  <-  paste("ch_500-",file_date, "24.temp",sep="")
    
    
#####################################################
#######      Read meteorological data        ########
#######         (Code from Konrad)           ########
#####################################################
    
  
  
cols    <- new.env()
rows    <- new.env()
lons    <- new.env()
lats    <- new.env()
lengths <- new.env()
maxlon  <- c()
minlon  <- c()
maxlat  <- c()
minlat  <- c()
    
    
    
to.read   <- file(filen,"rb")
col       <- readBin(to.read, double(),size=4, n = 1, endian = "little")
row       <- readBin(to.read, double(),size=4, n = 1, endian = "little")
xu        <- readBin(to.read, double(),size=4, n = 1, endian = "little")
yu        <- readBin(to.read, double(),size=4, n = 1, endian = "little")
dist      <- readBin(to.read, double(),size=4, n = 1, endian = "little")  
nodata    <- readBin(to.read, double(),size=4, n = 1, endian = "little")
swiss.lon <- seq(xu,xu+(dist*(col-1)),by=dist)
swiss.lat <- seq(yu,yu+(dist*(row-1)),by=dist)
maxlon    <- max(maxlon,swiss.lon)[1]
minlon    <- min(minlon,swiss.lon)[1]
maxlat    <- max(maxlat,swiss.lat)[1]
minlat    <- min(minlat,swiss.lat)[1]
assign( "swisslon", swiss.lon,    envir=lons)
assign( "swisslat", swiss.lat,    envir=lats)
assign( "len",      (row*col)+12, envir=lengths)
assign( "col",      col,          envir=cols)
assign( "row",      row,          envir=rows)
close(to.read)  


leftdow = swisscors2lonlat(chx=minlon,chy=minlat)
rightup = swisscors2lonlat(chx=maxlon,chy=maxlat)

lonlim = c(leftdow[1],rightup[1])
latlim = c(leftdow[2],rightup[2])

alllon  = seq(minlon,maxlon,by=dist)    
alllat  = seq(minlat,maxlat,by=dist)
alldata = array(NA,dim=c(length(alllon),length(alllat)))



to.read   <- file(filen,"rb")
a         <- readBin(to.read, double(),size=4, n = get("len",envir=lengths), endian = "little")
close(to.read)
col       <- get("col",envir=cols)
row       <- get("row",envir=rows)
a         <- a[13:length(a)]
a         <- array(a,dim=c(col,row))
a_corr    <- array(NA,dim=c(col,row))
for (lat in 1:row){
  a_corr[,lat] <- a[,(row+1)-lat]
}
a_corr[a_corr==nodata] <- NA
indx <- which(alllon%in%get("swisslon",envir=lons))
indy <- which(alllat%in%get("swisslat",envir=lats))
alldata[indx,indy][is.na(a_corr)==F] <- a_corr[is.na(a_corr)==F]




dat <- list(x=alllon,y=alllat,z=alldata)
CRS.new <- CRS("+proj=somerc +lat_0=46.95240555555556 +lon_0=7.439583333333333 +k_0=1 +x_0=600000 +y_0=200000 +ellps=bessel +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs")
fcp <- raster(dat)
proj4string(fcp) <- CRS.new



## Calculate the 95 percentile of the max values inside a catchment
ticival <- unlist(extract(fcp,Ticino_poly))
df_temp[idx,2] <- sort(ticival)[0.95*length(ticival)]

valacal <- unlist(extract(fcp,Valais_poly))
df_temp[idx,3] <- sort(valacal)[0.95*length(valacal)]

zurival <- unlist(extract(fcp, Zurich_poly))
df_temp[idx,4] <- sort(zurival)[0.95*length(valacal)]
    

  }
}


idx=0        

for (i in files_relh){
  file_date <- as.numeric(substr(i,8,15))
  # Get files from June to Sept 2003
  if (file_date >= 20030518 & file_date <= 20030930 ){
    idx=idx+1
    df_relh[idx,1] <- ymd(file_date)
    filen  <-  paste("ch_500-",file_date, "24.relh",sep="")
    
    
  
    
    
#####################################################
#######      Read meteorological data        ########
#######         (Code from Konrad)           ########
#####################################################



cols    <- new.env()
rows    <- new.env()
lons    <- new.env()
lats    <- new.env()
lengths <- new.env()
maxlon  <- c()
minlon  <- c()
maxlat  <- c()
minlat  <- c()



to.read   <- file(filen,"rb")
col       <- readBin(to.read, double(),size=4, n = 1, endian = "little")
row       <- readBin(to.read, double(),size=4, n = 1, endian = "little")
xu        <- readBin(to.read, double(),size=4, n = 1, endian = "little")
yu        <- readBin(to.read, double(),size=4, n = 1, endian = "little")
dist      <- readBin(to.read, double(),size=4, n = 1, endian = "little")  
nodata    <- readBin(to.read, double(),size=4, n = 1, endian = "little")
swiss.lon <- seq(xu,xu+(dist*(col-1)),by=dist)
swiss.lat <- seq(yu,yu+(dist*(row-1)),by=dist)
maxlon    <- max(maxlon,swiss.lon)[1]
minlon    <- min(minlon,swiss.lon)[1]
maxlat    <- max(maxlat,swiss.lat)[1]
minlat    <- min(minlat,swiss.lat)[1]
assign( "swisslon", swiss.lon,    envir=lons)
assign( "swisslat", swiss.lat,    envir=lats)
assign( "len",      (row*col)+12, envir=lengths)
assign( "col",      col,          envir=cols)
assign( "row",      row,          envir=rows)
close(to.read)  


leftdow = swisscors2lonlat(chx=minlon,chy=minlat)
rightup = swisscors2lonlat(chx=maxlon,chy=maxlat)

lonlim = c(leftdow[1],rightup[1])
latlim = c(leftdow[2],rightup[2])

alllon  = seq(minlon,maxlon,by=dist)    
alllat  = seq(minlat,maxlat,by=dist)
alldata = array(NA,dim=c(length(alllon),length(alllat)))



to.read   <- file(filen,"rb")
a         <- readBin(to.read, double(),size=4, n = get("len",envir=lengths), endian = "little")
close(to.read)
col       <- get("col",envir=cols)
row       <- get("row",envir=rows)
a         <- a[13:length(a)]
a         <- array(a,dim=c(col,row))
a_corr    <- array(NA,dim=c(col,row))
for (lat in 1:row){
  a_corr[,lat] <- a[,(row+1)-lat]
}
a_corr[a_corr==nodata] <- NA
indx <- which(alllon%in%get("swisslon",envir=lons))
indy <- which(alllat%in%get("swisslat",envir=lats))
alldata[indx,indy][is.na(a_corr)==F] <- a_corr[is.na(a_corr)==F]




dat <- list(x=alllon,y=alllat,z=alldata)
CRS.new <- CRS("+proj=somerc +lat_0=46.95240555555556 +lon_0=7.439583333333333 +k_0=1 +x_0=600000 +y_0=200000 +ellps=bessel +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs")
fcp <- raster(dat)
proj4string(fcp) <- CRS.new



## Calculate the 95 percentile of the max values inside a catchment
ticival <- unlist(extract(fcp,Ticino_poly))
df_relh[idx,2] <- sort(ticival)[0.95*length(ticival)]

valacal <- unlist(extract(fcp,Valais_poly))
df_relh[idx,3] <- sort(valacal)[0.95*length(valacal)]

zurival <- unlist(extract(fcp, Zurich_poly))
df_relh[idx,4] <- sort(zurival)[0.95*length(valacal)]


}
}


idx=0        

for (i in files_txxx){
file_date <- as.numeric(substr(i,8,15))
# Get files from June to Sept 2003
if (file_date >= 20030518 & file_date <= 20030930 ){
idx=idx+1
df_txxx[idx,1] <- ymd(file_date)
filen  <-  paste("ch_500-",file_date, "24.txxx",sep="")





#####################################################
#######      Read meteorological data        ########
#######         (Code from Konrad)           ########
#####################################################



cols    <- new.env()
rows    <- new.env()
lons    <- new.env()
lats    <- new.env()
lengths <- new.env()
maxlon  <- c()
minlon  <- c()
maxlat  <- c()
minlat  <- c()



to.read   <- file(filen,"rb")
col       <- readBin(to.read, double(),size=4, n = 1, endian = "little")
row       <- readBin(to.read, double(),size=4, n = 1, endian = "little")
xu        <- readBin(to.read, double(),size=4, n = 1, endian = "little")
yu        <- readBin(to.read, double(),size=4, n = 1, endian = "little")
dist      <- readBin(to.read, double(),size=4, n = 1, endian = "little")  
nodata    <- readBin(to.read, double(),size=4, n = 1, endian = "little")
swiss.lon <- seq(xu,xu+(dist*(col-1)),by=dist)
swiss.lat <- seq(yu,yu+(dist*(row-1)),by=dist)
maxlon    <- max(maxlon,swiss.lon)[1]
minlon    <- min(minlon,swiss.lon)[1]
maxlat    <- max(maxlat,swiss.lat)[1]
minlat    <- min(minlat,swiss.lat)[1]
assign( "swisslon", swiss.lon,    envir=lons)
assign( "swisslat", swiss.lat,    envir=lats)
assign( "len",      (row*col)+12, envir=lengths)
assign( "col",      col,          envir=cols)
assign( "row",      row,          envir=rows)
close(to.read)  


leftdow = swisscors2lonlat(chx=minlon,chy=minlat)
rightup = swisscors2lonlat(chx=maxlon,chy=maxlat)

lonlim = c(leftdow[1],rightup[1])
latlim = c(leftdow[2],rightup[2])

alllon  = seq(minlon,maxlon,by=dist)    
alllat  = seq(minlat,maxlat,by=dist)
alldata = array(NA,dim=c(length(alllon),length(alllat)))



to.read   <- file(filen,"rb")
a         <- readBin(to.read, double(),size=4, n = get("len",envir=lengths), endian = "little")
close(to.read)
col       <- get("col",envir=cols)
row       <- get("row",envir=rows)
a         <- a[13:length(a)]
a         <- array(a,dim=c(col,row))
a_corr    <- array(NA,dim=c(col,row))
for (lat in 1:row){
  a_corr[,lat] <- a[,(row+1)-lat]
}
a_corr[a_corr==nodata] <- NA
indx <- which(alllon%in%get("swisslon",envir=lons))
indy <- which(alllat%in%get("swisslat",envir=lats))
alldata[indx,indy][is.na(a_corr)==F] <- a_corr[is.na(a_corr)==F]




dat <- list(x=alllon,y=alllat,z=alldata)
CRS.new <- CRS("+proj=somerc +lat_0=46.95240555555556 +lon_0=7.439583333333333 +k_0=1 +x_0=600000 +y_0=200000 +ellps=bessel +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs")
fcp <- raster(dat)
proj4string(fcp) <- CRS.new

## Calculate the 95 percentile of the max values inside a catchment
ticival <- unlist(extract(fcp,Ticino_poly))
df_txxx[idx,2] <- sort(ticival)[0.95*length(ticival)]

valacal <- unlist(extract(fcp,Valais_poly))
df_txxx[idx,3] <- sort(valacal)[0.95*length(valacal)]

zurival <- unlist(extract(fcp, Zurich_poly))
df_txxx[idx,4] <- sort(zurival)[0.95*length(valacal)]

}
}



df_meteo <- cbind(df_temp, df_relh, df_txxx)
df_meteo <- df_meteo[,-c(5,9)]
write.csv(df_meteo, "E:/Internship/Climate variables/df_meteo.csv",row.names = FALSE)

df_meteo <- read.csv("E:/Internship/Climate variables/df_meteo.csv")

df_temp_all <- df_meteo[,c(1:4,8:10)]
df_temp_ggp <- melt(df_temp_all, id.vars="Date")
df_temp_ggp$Date <- as.Date(df_temp_all$Date)
df_temp_ggp$Location <- rep(c(rep("Ticino",nrow(df_temp_all)),
                         rep("Valais",nrow(df_temp_all)),
                         rep("Zurich",nrow(df_temp_all))),2)
df_temp_ggp$Temp <- c(rep("Mean temperature", 3*nrow(df_temp_all)),
                      rep("Maximum temperature", 3*nrow(df_temp_all)))

write.csv(df_temp_ggp, "E:/Internship/Climate variables/df_temp_ggp.csv",row.names = FALSE)


ggplot(df_temp_ggp, aes(Date,value, col=Temp)) + 
  geom_point()+
  geom_line(size = 1.5, alpha = 0.8)+
  facet_grid(Location ~ .)+
  #scale_color_brewer(palette = "Paired")+
  labs(title = "Maximum and mean temperature in summer 2003",
       subtitle = "Using subcatchments in Ticino, Valais and Zurich as examples",
       y = "Temperature", 
       x = "Month")+
  theme(legend.position="top")+
  theme_base()+
  scale_color_discrete(name = NULL)
  
       





