library(fields)

source("N:/meteodaten/SampleR_read_write/swisscors2lonlat")
load("N:/meteodaten/shape300.dat")


homep <- "E:/Internship/Climate variables"
setwd(homep)


filen <- "ch_50019940426.FCP"

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

# Select one sub-catchment in Canton Ticino, Valais and Zurich 
Ticino_poly <- shape.300[shape.300$ch_500_clb==1,]
Valais_poly <- shape.300[shape.300$ch_500_clb==6,]
Zurich_poly <- shape.300[shape.300$ch_500_clb==4,]


## Calculate the average of the values inside a catchment
ticival <- unlist(extract(fcp,Ticino_poly))
Tici_max <-sort(ticival)[0.95*length(ticival)]

valacal <- extract(fcp,Valais_poly)
df_temp[idx,3] <- quantile(unlist(valacal), probs = 0.95)

zurival <- extract(fcp, Zurich_poly)
df_temp[idx,4] <- quantile(unlist(zurival), probs = 0.95) 











  


## Code from Robert
  
  
  library(fields)
  
  source("N:/meteodaten/SampleR_read_write/swisscors2lonlat")
  
  homep <- "E:/Internship/Climate variables"
  setwd(homep)
  
  filen <- "ch_50019940426.FCP"
  
  
  # homep <- "C:/Users/Bogner/Desktop/Projects/tmp"
  #path <- "D:/data"
  #setwd(path)
  
  # Load files 
  # ETP = Potential evapotranspiration
  # ETR = Actual evapotranspiration
  # fileP <- "ch_50019810101.ETP"
  # fileR <- "ch_50019810101.ETR"
  
  # Read first file only to extract 12 Row Header
  to.read   <- file(filen,"rb")
  col       <- readBin(to.read, double(),size=4, n = 1, endian = "little")
  row       <- readBin(to.read, double(),size=4, n = 1, endian = "little")
  xu        <- readBin(to.read, double(),size=4, n = 1, endian = "little")
  yu        <- readBin(to.read, double(),size=4, n = 1, endian = "little")
  dist      <- readBin(to.read, double(),size=4, n = 1, endian = "little")  
  nodata    <- readBin(to.read, double(),size=4, n = 1, endian = "little")
  swiss.east <- seq(xu,xu+(dist*(col-1)),by=dist)
  swiss.nort <- seq(yu,yu+(dist*(row-1)),by=dist)
  close(to.read)  
  
  # Read  file
  to.read   <- file(filen,"rb")
  a         <- readBin(to.read, double(),size=4, n = (row*col)+12, endian = "little")
  close(to.read)
  a         <- a[13:length(a)] # Remove first 12 metadata entries
  a         <- array(a,dim=c(col,row)) # Shape into a matrix
  ETP_t    <- a[,row:1] # # Flips left to right
  ETP_t[ETP_t==nodata] <- NA # Replace no data values with NA
  # Read ETR file
  to.read   <- file(fileR,"rb")
  b         <- readBin(to.read, double(),size=4, n = (row*col)+12, endian = "little")
  close(to.read)
  b         <- b[13:length(b)] # Remove first 12 metadata entries
  b         <- array(b,dim=c(col,row)) # Shape into a matrix
  ETR_t    <- b[,row:1] # # Flips left to right
  ETR_t[ETR_t==nodata] <- NA # Replace no data values with NA
  rm(a,b)
  
  # plot map
  
   
  image.plot(swiss.east,swiss.nort,ETP_t,xlab='Easting (m)',ylab='Northing (m)',
             main='Potential Evapotranspiration')
  x11(width=w,height=h) 
  image.plot(swiss.east,swiss.nort,ETR_t,xlab='Easting (m)',ylab='Northing (m)',
             main='Actual Evapotranspiration')
  x11(width=w,height=h) 
  image.plot(swiss.east,swiss.nort,ETP_t-ETR_t,xlab='Easting (m)',ylab='Northing (m)',
             main='Transpiration Deficit')
  
  
