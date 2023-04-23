library(fields)

source("N:/meteodaten/SampleR_read_write/swisscors2lonlat")


homep <- "E:/Internship/Climate variables"
setwd(homep)


filen <- "ch_500-2003010124.temp"

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
  
  
 
   
  
  
  image.plot(alllon,alllat,alldata)
  
  ########################################
  
  
  library(raster)
  library(rgdal)
  
  dat <- list(x=alllon,y=alllat,z=alldata)
  

  
  CRS.new <- CRS("+proj=somerc +lat_0=46.95240555555556 +lon_0=7.439583333333333 +k_0=1 +x_0=600000 +y_0=200000 +ellps=bessel +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs")
  
  
  
  fcp <- raster(dat)
  
  proj4string(fcp) <- CRS.new
  

  
  load("N:/meteodaten/shape300.dat")
  
#safe default par() values 
par_def <- par(no.readonly = T)  

par(mar = c(2, 2, 2, 2))
  
plot(fcp)
plot(shape.300,add=T)

# Label all the catchments with their IDs
for (j in 1 :299) {

  poly <- shape.300[shape.300$ch_500_clb==j,]
  plot(poly,add=T)
  if(length(poly@polygons)!= 0){
    text(poly,j,cex=0.8)
  }
 
}

# Select one sub-catchment in Canton Ticino, Valais and Zurich 
Ticino_poly <- shape.300[shape.300$ch_500_clb==1,]
Valais_poly <- shape.300[shape.300$ch_500_clb==6,]
Zurich_poly <- shape.300[shape.300$ch_500_clb==4,]


## Calculate the average of the values inside a catchment
ticival <- extract(fcp,Ticino_poly)
ticiave <- mean(unlist(ticival))

valacal <- extract(fcp,Valais_poly)
valave <- mean(unlist(valacal))

zurival <- extract(fcp, Zurich_poly)
zuriave <- mean(unlist(zurival))

#r.stack <- stack(r,r,r)
v <- extract(fcp, poly)

##### Select catchments Thur, Broye, Emme, Verzasca

#Verzasca
VAG <- readOGR("vag200.shp")

#Thur
thur <- readOGR("thurande.shp")

#Emme 
# EZG[EZG$EZG_NAME=="Emme",]
EZG <- readOGR("EZG.shp")
emme <- EZG[EZG$EZG_NAME=="Emme",]

#Broye
BRY <- readOGR("broye.shp")



bound <- readOGR("VEC200_Com_Boundary.shp","VEC200_Com_Boundary")

### Kantons

plot(fcp)
plot(bound[bound$OBJVAL=="Kantonsgrenze",],col=4,add=T)

plot(VAG,add=T,col=6)


plot(emme,add=T,col=4)


plot(thur,add=T,col=3)

plot(BRY,add=T,col=2)


##################
## Average of the values inside a catchment

thurval <- extract(fcp,thur)
thurave <- mean(unlist(thurval))

emmeval <- extract(fcp,emme)
emmeave <- mean(unlist(emmeval))

VAGval <- extract(fcp,VAG)
VAGave <- mean(unlist(VAGval))

broyeval <- extract(fcp,BRY)
broyeave <- mean(unlist(broyeval))

######

par(par_def)





  
