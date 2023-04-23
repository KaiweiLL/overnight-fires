
# Libararies --------------------------------------------------------------


rm(list = ls())
getwd()
setwd("D:\\000_collections\\010_Nighttime Burning")
library(rgdal)
library(raster)
library(sp)   
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(ggsci)
library(lutz)
library(suncalc)
library(gtools)
library(foreach)
library(doParallel)
library(parallel)
library(tcltk)
library(doSNOW)

# FIRE DATA - NBAC & MTBS -------------------------------------------------

################################################################################ fire data & biome
nbac17to20 <- unlist(readOGR('011_Data\\010_FireData\\Canada\\nbac_1986_to_2020_20210810\\nbac2017_to_2020_20210810.shp'))
mtbs17to20 <- unlist(readOGR('011_Data\\010_FireData\\America\\mtbs_perimeter_data\\mtbs_2017_to_2020.shp'))

####NBAC
nbac17to20$Country <- 'CAN'
nbac17to20$SDATE <- nbac17to20$SDATE
nbac17to20$EDATE <- nbac17to20$EDATE
nbac17to20$AFSDATE <- nbac17to20$AFSDATE
nbac17to20$AFEDATE <- nbac17to20$AFEDATE
nbac17to20$firename <-  paste(nbac17to20$YEAR,'_',nbac17to20$AGENCY,'_',nbac17to20$NFIREID,'_',nbac17to20$POLY_HA,sep = '')
nbac17to20$Ig_Date <- NA
nbac17to20$fireCausOrType <- nbac17to20$FIRECAUS
nbac17to20$source <- 'NBAC'
nbac17to20$inci_name <- NA

####MTBS
mtbs17to20$Country <- 'USA'
mtbs17to20$SDATE <- NA
mtbs17to20$EDATE <- NA
mtbs17to20$AFSDATE <- NA
mtbs17to20$AFEDATE <- NA
mtbs17to20$YEAR <- as.integer(format(as.Date(mtbs17to20$Ig_Date, "%Y/%m/%d"),"%Y"))
mtbs17to20$POLY_HA <- as.numeric(mtbs17to20$BurnBndAc)*0.404686
mtbs17to20$firename <-  paste(mtbs17to20$YEAR,'_',mtbs17to20$Incid_Name,'_',mtbs17to20$Event_ID,'_',mtbs17to20$POLY_HA)
mtbs17to20$fireCausOrType <- mtbs17to20$Incid_Type
mtbs17to20$source <- 'MTBS'
mtbs17to20$inci_name <- mtbs17to20$Incid_Name

##aline with coords variables and fire data
raster.brick = brick("011_Data\\012_Variables\\NA_FFMC\\hFFMC2017_3..nc")
raster.brick = rotate(raster.brick)
nbac17to20 = spTransform(nbac17to20, crs(raster.brick))
mtbs17to20 = spTransform(mtbs17to20, crs(raster.brick))

##combine
NAfire <- bind(nbac17to20,mtbs17to20)
#rm(nbac17to20,mtbs17to20)
NAfire <- NAfire[,c('Country','YEAR','SDATE','EDATE','AFSDATE','AFEDATE','Ig_Date','POLY_HA','firename','fireCausOrType','source','inci_name')]

NAfire$long<-coordinates(NAfire)[,1]
NAfire$lat <- coordinates(NAfire)[,2]
NAfire <- NAfire[NAfire$POLY_HA > 200,]

##remove the duplicated ones
coord_poly <- lapply(NAfire@polygons, function(x){lapply(x@Polygons, function(x){coordinates(x)})}) 
NAfire <- NAfire[!duplicated(coord_poly),]

##separate and combine
NAfire2017 <- NAfire[NAfire$YEAR==2017,]
NAfire2017$seq <- 1:length(NAfire2017)
NAfire2018 <- NAfire[NAfire$YEAR==2018,]
NAfire2018$seq <- 1:length(NAfire2018)
NAfire2019 <- NAfire[NAfire$YEAR==2019,]
NAfire2019$seq <- 1:length(NAfire2019)
NAfire2020 <- NAfire[NAfire$YEAR==2020,]
NAfire2020$seq <- 1:length(NAfire2020)

NAfire <- bind(NAfire2017,NAfire2018,NAfire2019,NAfire2020)
NAfire$ID <- 1:length(NAfire)
NAfire$YEAR <- as.numeric(NAfire$YEAR)
length(NAfire)
##start and end date
dfNAfires <- NAfire@data %>% rename(year=YEAR) %>% mutate(year=as.numeric(year))
dfNAfires <- dfNAfires %>% mutate(startdate=pmin(as.numeric(strftime(SDATE, format = "%j")),as.numeric(strftime(AFSDATE, format = "%j")),as.numeric(strftime(Ig_Date, format = "%j")),na.rm = T),
                                  enddate=pmax(as.numeric(strftime(EDATE, format = "%j")),as.numeric(strftime(AFEDATE, format = "%j")),na.rm = T)) %>% 
  mutate(across(startdate:enddate,~replace_na(.x, 999)))


## biome
shp.biome <- readOGR('011_Data\\013_Biome_wwf2017\\fire_biomes_continent_updated2\\fire_biomes_continent_updated2.shp')

# GOES detection ----------------------------------------------------------

colselect <- c(NA,NA,'NULL','NULL',NA,NA,'NULL',NA,'NULL',NA,NA,NA,NA)
GOES17 <- read.csv(file='011_Data\\011_FireDetectionData_GOESR\\2017\\GOES2017_16_alldata.csv', colClasses = colselect, header=T)
GOES18 <- read.csv(file='011_Data\\011_FireDetectionData_GOESR\\2018/GOES2018_1617_alldata.csv', colClasses = colselect, header=T)
GOES19 <- read.csv(file='011_Data\\011_FireDetectionData_GOESR\\2019/GOES2019_1617_alldata.csv', colClasses = colselect, header=T)
GOES20 <- read.csv(file='011_Data\\011_FireDetectionData_GOESR\\2020\\GOES2020_1617_alldata.csv', colClasses = colselect, header=T)

GOES17 <- GOES17 %>% filter(long>=raster.brick@extent@xmin&long<=raster.brick@extent@xmax&lat>=raster.brick@extent@ymin&lat<=raster.brick@extent@ymax)
GOES18 <- GOES18 %>% filter(long>=raster.brick@extent@xmin&long<=raster.brick@extent@xmax&lat>=raster.brick@extent@ymin&lat<=raster.brick@extent@ymax)
GOES19 <- GOES19 %>% filter(long>=raster.brick@extent@xmin&long<=raster.brick@extent@xmax&lat>=raster.brick@extent@ymin&lat<=raster.brick@extent@ymax)
GOES20 <- GOES20 %>% filter(long>=raster.brick@extent@xmin&long<=raster.brick@extent@xmax&lat>=raster.brick@extent@ymin&lat<=raster.brick@extent@ymax)

GOES17_cp <- GOES17
GOES18_cp <- GOES18
GOES19_cp <- GOES19
GOES20_cp <- GOES20

## csv to spatial df
coordinates(GOES17) <- ~ long + lat
coordinates(GOES18) <- ~ long + lat
coordinates(GOES19) <- ~ long + lat
coordinates(GOES20) <- ~ long + lat

##aline with
proj4string(GOES17) <- proj4string(NAfire)#NAfire_year
proj4string(GOES18) <- proj4string(NAfire)#NAfire_year
proj4string(GOES19) <- proj4string(NAfire)#NAfire_year
proj4string(GOES20) <- proj4string(NAfire)#NAfire_year

## Fire weather variables --------------------------------------------------
BUI <- mixedsort(dir('011_Data\\012_Variables\\NA_BUI_daily\\', full.names = T,pattern = '(.nc)$'))
DC <- mixedsort(dir('011_Data\\012_Variables\\NA_DC_daily\\', full.names = T,pattern = '(.nc)$'))
DMC <- mixedsort(dir('011_Data\\012_Variables\\NA_DMC_daily\\', full.names = T,pattern = '(.nc)$'))
FWI <- mixedsort(dir('011_Data\\012_Variables\\NA_FWI_daily\\', full.names = T,pattern = '(.nc)$'))

var.bui.nc.2017 <- brick(BUI[2])
var.bui.nc.2018 <- brick(BUI[4])
var.bui.nc.2019 <- brick(BUI[6])
var.bui.nc.2020 <- brick(BUI[8])

var.dmc.nc.2017 <- brick(DMC[2])
var.dmc.nc.2018 <- brick(DMC[4])
var.dmc.nc.2019 <- brick(DMC[6])
var.dmc.nc.2020 <- brick(DMC[8])

var.dc.nc.2017 <- brick(DC[2])
var.dc.nc.2018 <- brick(DC[4])
var.dc.nc.2019 <- brick(DC[6])
var.dc.nc.2020 <- brick(DC[8])

var.fwi.nc.2017 <- brick(FWI[2])
var.fwi.nc.2018 <- brick(FWI[4])
var.fwi.nc.2019 <- brick(FWI[6])
var.fwi.nc.2020 <- brick(FWI[8])

var.bui.nc.2017 <- rotate(var.bui.nc.2017)
var.bui.nc.2018 <- rotate(var.bui.nc.2018)
var.bui.nc.2019 <- rotate(var.bui.nc.2019)
var.bui.nc.2020 <- rotate(var.bui.nc.2020)

var.dmc.nc.2017 <- rotate(var.dmc.nc.2017)
var.dmc.nc.2018 <- rotate(var.dmc.nc.2018)
var.dmc.nc.2019 <- rotate(var.dmc.nc.2019)
var.dmc.nc.2020 <- rotate(var.dmc.nc.2020)

var.dc.nc.2017 <- rotate(var.dc.nc.2017)
var.dc.nc.2018 <- rotate(var.dc.nc.2018)
var.dc.nc.2019 <- rotate(var.dc.nc.2019)
var.dc.nc.2020 <- rotate(var.dc.nc.2020)

var.fwi.nc.2017 <- rotate(var.fwi.nc.2017)
var.fwi.nc.2018 <- rotate(var.fwi.nc.2018)
var.fwi.nc.2019 <- rotate(var.fwi.nc.2019)
var.fwi.nc.2020 <- rotate(var.fwi.nc.2020)


################################################################################extract function
f<-function(i){
  library(rgdal)
  library(raster)
  library(sp)   
  library(sf)
  library(dplyr)
  library(ggplot2)
  library(reshape2)
  library(ggsci)
  library(lutz)
  library(suncalc)
  library(gtools)
  library(foreach)
  library(doParallel)
  library(parallel)
  library(tcltk)
  library(doSNOW)
  #select polygon
  shp.indi <- NAfire[i,]
  year.indi <- shp.indi$YEAR
  seq.indi <- shp.indi$seq
  startdate.indi <- (dfNAfires %>% filter(year==year.indi,seq==seq.indi))$startdate
  enddate.indi <- (dfNAfires %>% filter(year==year.indi,seq==seq.indi))$enddate
  
  ##GOES starts on 2017-05-24
  if (year.indi==2017&enddate.indi<144) { 
    outputYoN = 0}else{outputYoN = 1}
  if (year.indi==2017&startdate.indi<144&enddate.indi>144) { 
    startdate.indi = 144
  }
  
  
  if (year.indi==2017) {    
    GOES <- GOES17
    GOES_cp <- GOES17_cp}
  if (year.indi==2018) {    
    GOES <- GOES18
    GOES_cp <- GOES18_cp}
  if (year.indi==2019) {    
    GOES <- GOES19
    GOES_cp <- GOES19_cp}
  if (year.indi==2020) {    
    GOES <- GOES20
    GOES_cp <- GOES20_cp}
  
  #GOES over shp
  GOESovershp <- over(GOES, shp.indi)
  #remove all rows na
  GOESovershpna <- GOESovershp[complete.cases(GOESovershp$long),]
  #match the goes fire info
  GOES_rowind = as.numeric(rownames(GOESovershpna))
  GOES_fireinfo = GOES_cp[GOES_rowind,]
  ##summarize
  #keep other columns
  G19info1=GOES_fireinfo %>%
    group_by(day,hr) %>%
    summarise_each(funs(sum)) %>% 
    rename(frp.total=frp)
  #summarize frp and count hotspots number
  G19info2=GOES_fireinfo %>%
    group_by(year,day ,hr) %>%
    summarise(frp.mean=mean(frp),frp.total=sum(frp),spotsnum=n(),afarea.mean=mean(area,na.rm=T),
              aftemp.mean=mean(temp,na.rm=T))
  #join the full df
  GOES_fireinfo <- full_join(G19info1,G19info2,by=c('frp.total','day','hr')) %>% 
    rename(year=year.y) %>% dplyr::select(-year.x,-area,-temp)
  
  rm(GOESovershp,GOESovershpna,GOES_rowind,G19info1,G19info2)
  
  ## find the start day and end day for consectuive active hour if orignial date equals 999
  if (dim(GOES_fireinfo)[1] > 0){
    vals <- expand.grid(year=year.indi,day = seq(as.integer(GOES_fireinfo[1,1]), 
                                                 max(as.integer(GOES_fireinfo$day)), 1),
                        hr = seq(0, 23, 1)) %>% arrange(day,hr)
    GOES_fireinfo=merge(vals,GOES_fireinfo,all = TRUE)
    GOES_fireinfo = GOES_fireinfo %>% mutate(leadfrp = lead(frp.total))
    
    if (startdate.indi<999) {
      startdoy = startdate.indi
      startdate=as.Date(as.integer(startdoy), origin = paste(year.indi-1,'-12-31', sep = ""))
      startmonth <- as.integer(format(startdate,"%m"))
      startday <- as.integer(format(startdate,"%d"))
    }else {
      tworow = rowMeans(GOES_fireinfo %>% dplyr::select(frp.total,leadfrp),na.rm = F)
      startdoy = GOES_fireinfo[which(tworow>0),]$day[1]
      startdate=as.Date(as.integer(startdoy), origin = paste(year.indi-1,'-12-31', sep = ""))
      startmonth <- as.integer(format(startdate,"%m"))
      startday <- as.integer(format(startdate,"%d"))
    }
    if (enddate.indi<999) {
      enddoy = enddate.indi+1
      enddate=as.Date(as.integer(enddoy), origin = paste(year.indi-1,'-12-31', sep = ""))
      endmonth <- as.integer(format(enddate,"%m"))
      endday <- as.integer(format(enddate,"%d"))
    }else {
      tworow = rowMeans(GOES_fireinfo %>% dplyr::select(frp.total,leadfrp),na.rm = F)
      enddoy = max( GOES_fireinfo[which(tworow>0),]$day)+1
      enddate=as.Date(as.integer(enddoy), origin = paste(year.indi-1,'-12-31', sep = ""))
      endmonth <- as.integer(format(enddate,"%m"))
      endday <- as.integer(format(enddate,"%d"))
    }
    
    # remove rows before the fire
    GOES_fireinfo <- GOES_fireinfo %>% filter(day>=as.integer(startdoy),day<=enddoy) %>% 
      dplyr::select(-leadfrp)
    
  }
  
  if (dim(GOES_fireinfo)[1] > 0){   
    # outputtest = data.frame(year.indi,seq.indi)
    
    #expand the df from start date to the last day with 1hour sequence
    startdoy <- as.integer(startdoy)-3
    startdate=as.Date(as.integer(startdoy), origin = paste(year.indi-1,'-12-31', sep = ""))
    startmonth <- as.integer(format(startdate,"%m"))
    startday <- as.integer(format(startdate,"%d"))
    if (startdoy<=0) {
      startdoy=1
      startdate=as.Date(as.integer(startdoy), origin = paste(year.indi-1,'-12-31', sep = ""))
      startmonth <- as.integer(format(startdate,"%m"))
      startday <- as.integer(format(startdate,"%d"))
    }
    if (enddoy>=365) {
      enddoy=365
      enddate=as.Date(as.integer(enddoy), origin = paste(year.indi-1,'-12-31', sep = ""))
      endmonth <- as.integer(format(enddate,"%m"))
      endday <- as.integer(format(enddate,"%d"))
    }
    vals <- expand.grid(year=year.indi,day = seq(startdoy, (as.integer(enddoy)), 1),
                        hr = seq(0, 23, 1))
    GOES_fireinfo=merge(vals,GOES_fireinfo,all = TRUE)
    GOES_fireinfo=GOES_fireinfo[which(GOES_fireinfo$day>=startdoy),]
    GOES_fireinfo=GOES_fireinfo[which(GOES_fireinfo$day<=enddoy),]
    rm(vals)
    
    #which(!is.na(GOES_fireinfo$frp.total), arr.ind=TRUE)
    
    ##extract biome id
    shp.indi.1 <- shp.indi
    pointtoplot <- data.frame(x=coordinates(shp.indi.1)[,1], y=coordinates(shp.indi.1)[,2])
    coordinates(pointtoplot) <- ~ x + y
    proj4string(pointtoplot) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    pointtoplot <- spTransform(pointtoplot, crs(shp.biome))
    #
    #function over from package sp
    testnip <- data.frame(xx=over(shp.biome, pointtoplot))
    testnip <- na.omit(testnip)
    combine <- shp.biome[row.names(shp.biome) %in% row.names(testnip), ]
    biomeid <- combine$gez_code[1]
    
    rm(shp.indi.1,pointtoplot,testnip,combine)
    
    ##load variables
    #to select corresponding nc file
    if (year.indi==2017) {  
      var.bui.nc <- var.bui.nc.2017
      var.dmc.nc <- var.dmc.nc.2017
      var.dc.nc <- var.dc.nc.2017
      var.fwi.nc <- var.fwi.nc.2017
      } else if (year.indi==2018) {
        var.bui.nc <- var.bui.nc.2018
        var.dmc.nc <- var.dmc.nc.2018
        var.dc.nc <- var.dc.nc.2018
        var.fwi.nc <- var.fwi.nc.2018
        } else if (year.indi==2019) {
          var.bui.nc <- var.bui.nc.2019
          var.dmc.nc <- var.dmc.nc.2019
          var.dc.nc <- var.dc.nc.2019
          var.fwi.nc <- var.fwi.nc.2019
          }else if (year.indi==2020) { 
            var.bui.nc <- var.bui.nc.2020
            var.dmc.nc <- var.dmc.nc.2020
            var.dc.nc <- var.dc.nc.2020
            var.fwi.nc <- var.fwi.nc.2020}


    ext.val.bui <- raster::extract(var.bui.nc, shp.indi,weight=T)[[1]]
    nr <- dim(ext.val.bui)[2]-1
    ext.val.bui <- data.frame(ext.val.bui)[,1:nr]
    ext.val.dmc <- data.frame(raster::extract(var.dmc.nc, shp.indi,weight=T)[[1]])[,1:nr]
    ext.val.dc <- data.frame(raster::extract(var.dc.nc, shp.indi,weight=T)[[1]])[,1:nr]
    ext.val.fwi <- data.frame(raster::extract(var.fwi.nc, shp.indi,weight=T)[[1]])[,1:nr]
    
    out.val.bui <- apply(ext.val.bui, 1, 
                         function(x)  x[(startdoy): enddoy])
    out.val.dmc <- apply(ext.val.dmc, 1, 
                         function(x)  x[(startdoy): enddoy])
    out.val.dc <- apply(ext.val.dc, 1, 
                        function(x)  x[(startdoy): enddoy])
    out.val.fwi <- apply(ext.val.fwi, 1, 
                         function(x)  x[(startdoy): enddoy])
    
    out.val.bui <- cbind(rowMeans(out.val.bui,na.rm = T),apply(out.val.bui, 1, median,na.rm = T))
    out.val.dmc <- cbind(rowMeans(out.val.dmc,na.rm = T),apply(out.val.dmc, 1, median,na.rm = T))
    out.val.dc <- cbind(rowMeans(out.val.dc,na.rm = T),apply(out.val.dc, 1, median,na.rm = T))
    out.val.fwi <- cbind(rowMeans(out.val.fwi,na.rm = T),apply(out.val.fwi, 1, median,na.rm = T))
    
    colnames(out.val.bui) <- c('bui.mean','bui.median')
    colnames(out.val.dmc) <- c('dmc.mean','dmc.median')
    colnames(out.val.dc) <- c('dc.mean','dc.median')
    colnames(out.val.fwi) <- c('fwi.mean','fwi.median')
    
    out.val <- cbind(out.val.bui,out.val.dmc,out.val.dc,out.val.fwi)
    out.val <- data.frame(out.val)
    out.val$biome <- biomeid
    
    rm(var.bui.nc,var.dmc.nc,var.dc.nc,var.fwi.nc)
    rm(ext.val.bui,ext.val.dmc,ext.val.dc,ext.val.fwi)
    rm( out.val.bui,out.val.dmc,out.val.dc,out.val.fwi)
    
    ## add timezone, sunrise and sunset
    GOES_fireinfo <- GOES_fireinfo %>% group_by(year,day) %>% 
      summarise(lat=mean(lat),long=mean(long),frp.mean=mean(frp.mean),frp.total=mean(frp.total),spotsnum=mean(spotsnum))
    GOES_fireinfo.var <- cbind.data.frame(GOES_fireinfo,out.val)
    tz <- tz_offset(as.Date(as.integer(GOES_fireinfo.var$day), origin = paste(year.indi-1,'-12-31', sep = "")), tz_lookup_coords(shp.indi$lat, shp.indi$long, method = "accurate"))
    tzoffset <- tz$utc_offset_h
    GOES_fireinfo.var <- cbind(GOES_fireinfo.var,tzoffset)
    ## sunset and sunrise
    tz <- tz_offset(as.Date(as.integer(GOES_fireinfo.var$day), origin = paste(year.indi-1,'-12-31', sep = "")), tz_lookup_coords(shp.indi$lat, shp.indi$long, method = "accurate"))
    sundata <- data.frame(date=as.Date(tz$date_time),lat=shp.indi$lat,lon=shp.indi$long)
    sunrise <- getSunlightTimes(data=sundata,keep="sunrise",tz=tz$tz_name[1])$sunrise
    sunset <- getSunlightTimes(data=sundata,keep="sunset",tz=tz$tz_name[1])$sunset
    
    

    ## all are local time, even shown as UTC time
    GOES_fireinfo.var$sunrise <- as.character(sunrise) 
    GOES_fireinfo.var$sunset <- as.character(sunset)
    #GOES_fireinfo.var$country <- shp.indi$Country
    GOES_fireinfo.var$seq <- shp.indi$seq
    GOES_fireinfo.var$long <- shp.indi$long
    GOES_fireinfo.var$lat <- shp.indi$lat
    
    if (startdate.indi<999) {
      GOES_fireinfo.var$startburnDOY <- startdate.indi
    }else {
      GOES_fireinfo.var$startburnDOY <- startdoy+3
      
    }
    if (enddate.indi<999) {
      GOES_fireinfo.var$endburnDOY <- enddate.indi
      
    }else {
      GOES_fireinfo.var$endburnDOY <- enddoy-1
      
    }
    
    GOES_fireinfo.var$country <- shp.indi$Country
    GOES_fireinfo.var$POLY_HA <- shp.indi@data$POLY_HA
    GOES_fireinfo.var$ID <- shp.indi$ID
    GOES_fireinfo.var$inci_name <- shp.indi$inci_name
    
    
    GOES_fireinfo.var <- GOES_fireinfo.var %>% dplyr::select(year:spotsnum,biome:inci_name,everything())
    
    out.name <- paste('015_OverNightBurning\\015.1_DataProcessing\\SingleFireCSV_daily\\',year.indi,'_',shp.indi$seq,'_',as.integer( shp.indi$ID),'_',shp.indi$Country,'_',
                      round(GOES_fireinfo.var$long[1],2),'_',round(GOES_fireinfo.var$lat[1],2),'_',biomeid,'_',round(GOES_fireinfo.var$POLY_HA[1],2),'.csv',sep = '')
    
    if (outputYoN ==1) {
      write.table(GOES_fireinfo.var,file=out.name,sep = ',', row.names=FALSE) # keeps the rownames
    }    
  }
}
################################################################################parallel running

no_cores<-detectCores()-2
cl <- makeSOCKcluster(no_cores)
registerDoSNOW(cl)
pb <- txtProgressBar(min=1, max=length(NAfire), style=3)#length(NAfire)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress=progress)

start_time <- Sys.time()
NAfire_csvwrite <- foreach(i=1:length(NAfire), .options.snow = opts) %dopar%  f(i)#, .combine = 'rbind'
stopCluster(cl)
end_time <- Sys.time()
end_time - start_time

