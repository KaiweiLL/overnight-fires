
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
rm(nbac17to20,mtbs17to20)
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

################################################################################ nc/tif folder
ffmc <- mixedsort(dir('011_Data\\012_Variables\\NA_FFMC\\', full.names = T,pattern = '(.nc)$'))
temp <- mixedsort(dir('011_Data\\012_Variables\\NA_temp\\', full.names = T,pattern = '(.nc)$'))
rh <- mixedsort(dir('011_Data\\012_Variables\\NA_RH\\', full.names = T,pattern = '(.nc)$'))
prec <- mixedsort(dir('011_Data\\012_Variables\\NA_prec\\', full.names = T,pattern = '(.nc)$'))
winds <- mixedsort(dir('011_Data\\012_Variables\\NA_winds\\', full.names = T,pattern = '(.nc)$'))
vpd <- mixedsort(dir('011_Data\\012_Variables\\NA_VPD\\', full.names = T,pattern = '(.nc)$'))
isi <- mixedsort(dir('011_Data\\012_Variables\\NA_ISI\\', full.names = T,pattern = '(.nc)$'))

# raster::extract variables -------------------------------------------------------


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


  ## no hot spots from GOES in this shapefile
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
    if (year.indi==2017) {  startseq=0 } else if (year.indi==2018) {startseq=12 } else if (year.indi==2019) {
      startseq=24 }else if (year.indi==2020) { startseq=36 }else if (year.indi==2021) {startseq=48}


    if (startmonth+startseq==endmonth+startseq) {
      var.ffmc.nc <- brick(ffmc[startmonth+startseq])
      var.temp.nc <- brick(temp[startmonth+startseq])
      var.rh.nc <- brick(rh[startmonth+startseq])
      var.prec.nc <- brick(prec[startmonth+startseq])
      var.winds.nc <- brick(winds[startmonth+startseq])
      var.vpd.nc <- brick(vpd[startmonth+startseq])
      var.isi.nc <- brick(isi[startmonth+startseq])


      var.ffmc.nc <- rotate(var.ffmc.nc)
      var.temp.nc <- rotate(var.temp.nc)
      var.rh.nc <- rotate(var.rh.nc)
      var.prec.nc <- rotate(var.prec.nc)
      var.winds.nc <- rotate(var.winds.nc)
      var.vpd.nc <- rotate(var.vpd.nc)
      var.isi.nc <- rotate(var.isi.nc)

      #same coordinate system, already did before
      ext.val.ffmc <- raster::extract(var.ffmc.nc, shp.indi,weight=T)[[1]]

      nr <- dim(ext.val.ffmc)[2]-1
      ext.val.ffmc <- data.frame(ext.val.ffmc)[,1:nr]

      ext.val.temp <- data.frame(raster::extract(var.temp.nc, shp.indi,weight=T)[[1]])[,1:nr]
      ext.val.rh <- data.frame(raster::extract(var.rh.nc, shp.indi,weight=T)[[1]])[,1:nr]
      ext.val.prec <- data.frame(raster::extract(var.prec.nc, shp.indi,weight=T)[[1]])[,1:nr]
      ext.val.winds <- data.frame(raster::extract(var.winds.nc, shp.indi,weight=T)[[1]])[,1:nr]
      ext.val.vpd <- data.frame(raster::extract(var.vpd.nc, shp.indi,weight=T)[[1]])[,1:nr]
      ext.val.isi <- data.frame(raster::extract(var.isi.nc, shp.indi,weight=T)[[1]])[,1:nr]


      out.val.ffmc <- apply(ext.val.ffmc, 1,
                            function(x)  x[((startday-1)*24+1): (endday*24)])
      out.val.temp <- apply(ext.val.temp, 1,
                            function(x)  x[((startday-1)*24+1):
                                             (endday*24)])
      out.val.rh <- apply(ext.val.rh, 1,
                          function(x)  x[((startday-1)*24+1):
                                           (endday*24)])
      out.val.prec <- apply(ext.val.prec, 1,
                            function(x)  x[((startday-1)*24+1):
                                             (endday*24)])
      out.val.winds <- apply(ext.val.winds, 1,
                             function(x)  x[((startday-1)*24+1):
                                              (endday*24)])
      out.val.vpd<- apply(ext.val.vpd, 1,
                          function(x)  x[((startday-1)*24+1):
                                           (endday*24)])
      out.val.isi<- apply(ext.val.isi, 1,
                          function(x)  x[((startday-1)*24+1):
                                           (endday*24)])


      out.val.ffmc <- cbind(rowMeans(out.val.ffmc),apply(out.val.ffmc, 1, median))
      out.val.temp <- cbind(rowMeans(out.val.temp),apply(out.val.temp, 1, median))
      out.val.rh <- cbind(rowMeans(out.val.rh),apply(out.val.rh, 1, median))
      out.val.prec <- cbind(rowMeans(out.val.prec),apply(out.val.prec, 1, median))
      out.val.winds <- cbind(rowMeans(out.val.winds),apply(out.val.winds, 1, median))
      out.val.vpd <- cbind(rowMeans(out.val.vpd),apply(out.val.vpd, 1, median))
      out.val.isi <- cbind(rowMeans(out.val.isi),apply(out.val.isi, 1, median))

      colnames(out.val.ffmc) <- c('ffmc.mean','ffmc.median')
      colnames(out.val.temp) <- c('temp.mean','temp.median')
      colnames(out.val.rh) <- c('rh.mean','rh.median')
      colnames(out.val.prec) <- c('prec.mean','prec.median')
      colnames(out.val.winds) <- c('winds.mean','winds.median')
      colnames(out.val.vpd) <- c('vpd.mean','vpd.median')
      colnames(out.val.isi) <- c('isi.mean','isi.median')

      out.val <- cbind(out.val.ffmc,out.val.temp,out.val.rh,out.val.prec,out.val.winds,out.val.vpd,out.val.isi)
      out.val <- data.frame(out.val)
      out.val$biome <- biomeid

      rm(var.ffmc.nc,var.temp.nc,var.rh.nc,var.prec.nc,var.winds.nc,var.vpd.nc,var.isi.nc)
      rm(ext.val.temp,ext.val.rh,ext.val.prec,ext.val.winds,ext.val.vpd,ext.val.isi)
      rm( out.val.ffmc,out.val.temp,out.val.rh,out.val.prec,out.val.winds,out.val.vpd,out.val.isi)

    }else  {
      for (l in seq((startmonth+startseq),(endmonth+startseq),1)) {
        var.ffmc.nc <- brick(ffmc[l])
        var.temp.nc <- brick(temp[l])
        var.rh.nc <- brick(rh[l])
        var.prec.nc <- brick(prec[l])
        var.winds.nc <- brick(winds[l])
        var.vpd.nc <- brick(vpd[l])
        var.isi.nc <- brick(isi[l])

        var.ffmc.nc <- rotate(var.ffmc.nc)
        var.temp.nc <- rotate(var.temp.nc)
        var.rh.nc <- rotate(var.rh.nc)
        var.prec.nc <- rotate(var.prec.nc)
        var.winds.nc <- rotate(var.winds.nc)
        var.vpd.nc <- rotate(var.vpd.nc)
        var.isi.nc <- rotate(var.isi.nc)

        #same coordinate system, already did before
        ext.val.ffmc <- raster::extract(var.ffmc.nc, shp.indi,weight=T)[[1]]
        nr <- dim(ext.val.ffmc)[2]-1
        ext.val.ffmc <- data.frame(ext.val.ffmc)[,1:nr]
        ext.val.temp <- data.frame(raster::extract(var.temp.nc, shp.indi,weight=T)[[1]])[,1:nr]
        ext.val.rh <- data.frame(raster::extract(var.rh.nc, shp.indi,weight=T)[[1]])[,1:nr]
        ext.val.prec <- data.frame(raster::extract(var.prec.nc, shp.indi,weight=T)[[1]])[,1:nr]
        ext.val.winds <- data.frame(raster::extract(var.winds.nc, shp.indi,weight=T)[[1]])[,1:nr]
        ext.val.vpd <- data.frame(raster::extract(var.vpd.nc, shp.indi,weight=T)[[1]])[,1:nr]
        ext.val.isi <- data.frame(raster::extract(var.isi.nc, shp.indi,weight=T)[[1]])[,1:nr]


        if (l==startmonth+startseq) {
          out.val.ffmc <- apply(ext.val.ffmc, 1,
                                function(x)  x[((startday-1)*24+1):
                                                 ncol(ext.val.ffmc)])
          out.val.temp <- apply(ext.val.temp, 1,
                                function(x)  x[((startday-1)*24+1):
                                                 ncol(ext.val.temp)])
          out.val.rh <- apply(ext.val.rh, 1,
                              function(x)  x[((startday-1)*24+1):
                                               ncol(ext.val.rh)])
          out.val.prec <- apply(ext.val.prec, 1,
                                function(x)  x[((startday-1)*24+1):
                                                 ncol(ext.val.prec)])
          out.val.winds <- apply(ext.val.winds, 1,
                                 function(x)  x[((startday-1)*24+1):
                                                  ncol(ext.val.winds)])
          out.val.vpd <- apply(ext.val.vpd, 1,
                               function(x)  x[((startday-1)*24+1):
                                                ncol(ext.val.vpd)])
          out.val.isi <- apply(ext.val.isi, 1,
                               function(x)  x[((startday-1)*24+1):
                                                ncol(ext.val.isi)])


          out.val.ffmc <- cbind(rowMeans(out.val.ffmc),apply(out.val.ffmc, 1, median))
          out.val.temp <- cbind(rowMeans(out.val.temp),apply(out.val.temp, 1, median))
          out.val.rh <- cbind(rowMeans(out.val.rh),apply(out.val.rh, 1, median))
          out.val.prec <- cbind(rowMeans(out.val.prec),apply(out.val.prec, 1, median))
          out.val.winds <- cbind(rowMeans(out.val.winds),apply(out.val.winds, 1, median))
          out.val.vpd <- cbind(rowMeans(out.val.vpd),apply(out.val.vpd, 1, median))
          out.val.isi <- cbind(rowMeans(out.val.isi),apply(out.val.isi, 1, median))

          colnames(out.val.ffmc) <- c('ffmc.mean','ffmc.median')
          colnames(out.val.temp) <- c('temp.mean','temp.median')
          colnames(out.val.rh) <- c('rh.mean','rh.median')
          colnames(out.val.prec) <- c('prec.mean','prec.median')
          colnames(out.val.winds) <- c('winds.mean','winds.median')
          colnames(out.val.vpd) <- c('vpd.mean','vpd.median')
          colnames(out.val.isi) <- c('isi.mean','isi.median')

          out.val <- cbind(out.val.ffmc,out.val.temp,out.val.rh,out.val.prec,out.val.winds,out.val.vpd,out.val.isi)
          out.val <- data.frame(out.val)
          out.val$biome <- biomeid

        }
        else if (l> startmonth+startseq&l<endmonth+startseq) {
          out.val.ffmc.2 <- apply(ext.val.ffmc, 1,
                                  function(x)  x[1:ncol(ext.val.ffmc)])
          out.val.temp.2 <- apply(ext.val.temp, 1,
                                  function(x)  x[1:ncol(ext.val.temp)])
          out.val.rh.2 <- apply(ext.val.rh, 1,
                                function(x)  x[1:ncol(ext.val.rh)])
          out.val.prec.2 <- apply(ext.val.prec, 1,
                                  function(x)  x[1:ncol(ext.val.prec)])
          out.val.winds.2 <- apply(ext.val.winds, 1,
                                   function(x)  x[1:ncol(ext.val.winds)])
          out.val.vpd.2 <- apply(ext.val.vpd, 1,
                                 function(x)  x[1:ncol(ext.val.vpd)])
          out.val.isi.2 <- apply(ext.val.isi, 1,
                                 function(x)  x[1:ncol(ext.val.isi)])


          out.val.ffmc.2 <- cbind(rowMeans(out.val.ffmc.2),apply(out.val.ffmc.2, 1, median))
          out.val.temp.2 <- cbind(rowMeans(out.val.temp.2),apply(out.val.temp.2, 1, median))
          out.val.rh.2 <- cbind(rowMeans(out.val.rh.2),apply(out.val.rh.2, 1, median))
          out.val.prec.2 <- cbind(rowMeans(out.val.prec.2),apply(out.val.prec.2, 1, median))
          out.val.winds.2 <- cbind(rowMeans(out.val.winds.2),apply(out.val.winds.2, 1, median))
          out.val.vpd.2 <- cbind(rowMeans(out.val.vpd.2),apply(out.val.vpd.2, 1, median))
          out.val.isi.2 <- cbind(rowMeans(out.val.isi.2),apply(out.val.isi.2, 1, median))

          colnames(out.val.ffmc.2) <- c('ffmc.mean','ffmc.median')
          colnames(out.val.temp.2) <- c('temp.mean','temp.median')
          colnames(out.val.rh.2) <- c('rh.mean','rh.median')
          colnames(out.val.prec.2) <- c('prec.mean','prec.median')
          colnames(out.val.winds.2) <- c('winds.mean','winds.median')
          colnames(out.val.vpd.2) <- c('vpd.mean','vpd.median')
          colnames(out.val.isi.2) <- c('isi.mean','isi.median')


          out.val.2 <- cbind(out.val.ffmc.2,out.val.temp.2,out.val.rh.2,out.val.prec.2,out.val.winds.2,out.val.vpd.2,out.val.isi.2)
          out.val.2 <- data.frame(out.val.2)

          out.val.2$biome <- biomeid


          out.val <- rbind(out.val,out.val.2)
        }
        else if (l==endmonth+startseq&l>startmonth+startseq) {
          out.val.ffmc.3 <- apply(ext.val.ffmc, 1,
                                  function(x)  x[1:(endday*24)])
          out.val.temp.3 <- apply(ext.val.temp, 1,
                                  function(x)  x[1:(endday*24)])
          out.val.rh.3 <- apply(ext.val.rh, 1,
                                function(x)  x[1:(endday*24)])
          out.val.prec.3 <- apply(ext.val.prec, 1,
                                  function(x)  x[1:(endday*24)])
          out.val.winds.3 <- apply(ext.val.winds, 1,
                                   function(x)  x[1:(endday*24)])
          out.val.vpd.3 <- apply(ext.val.vpd, 1,
                                 function(x)  x[1:(endday*24)])
          out.val.isi.3 <- apply(ext.val.isi, 1,
                                 function(x)  x[1:(endday*24)])


          out.val.ffmc.3 <- cbind(rowMeans(out.val.ffmc.3),apply(out.val.ffmc.3, 1, median))
          out.val.temp.3 <- cbind(rowMeans(out.val.temp.3),apply(out.val.temp.3, 1, median))
          out.val.rh.3 <- cbind(rowMeans(out.val.rh.3),apply(out.val.rh.3, 1, median))
          out.val.prec.3 <- cbind(rowMeans(out.val.prec.3),apply(out.val.prec.3, 1, median))
          out.val.winds.3 <- cbind(rowMeans(out.val.winds.3),apply(out.val.winds.3, 1, median))
          out.val.vpd.3 <- cbind(rowMeans(out.val.vpd.3),apply(out.val.vpd.3, 1, median))
          out.val.isi.3 <- cbind(rowMeans(out.val.isi.3),apply(out.val.isi.3, 1, median))

          colnames(out.val.ffmc.3) <- c('ffmc.mean','ffmc.median')
          colnames(out.val.temp.3) <- c('temp.mean','temp.median')
          colnames(out.val.rh.3) <- c('rh.mean','rh.median')
          colnames(out.val.prec.3) <- c('prec.mean','prec.median')
          colnames(out.val.winds.3) <- c('winds.mean','winds.median')
          colnames(out.val.vpd.3) <- c('vpd.mean','vpd.median')
          colnames(out.val.isi.3) <- c('isi.mean','isi.median')


          out.val.3 <- cbind(out.val.ffmc.3,out.val.temp.3,out.val.rh.3,out.val.prec.3,out.val.winds.3,out.val.vpd.3,out.val.isi.3)
          out.val.3 <- data.frame(out.val.3)
          out.val.3$biome <- biomeid


          out.val <- rbind(out.val,out.val.3)
        }

      }
      rm(var.ffmc.nc,var.temp.nc,var.rh.nc,var.prec.nc,var.winds.nc,var.vpd.nc,var.isi.nc)
      rm(ext.val.temp,ext.val.rh,ext.val.prec,ext.val.winds,ext.val.vpd,ext.val.isi)
      rm( out.val.ffmc,out.val.temp,out.val.rh,out.val.prec,out.val.winds,out.val.vpd,out.val.isi)

    }

    ## add timezone, sunrise and sunset
    GOES_fireinfo.var <- cbind.data.frame(GOES_fireinfo,out.val)
    tz <- tz_offset(as.Date(as.integer(GOES_fireinfo.var$day), origin = paste(year.indi-1,'-12-31', sep = "")), tz_lookup_coords(shp.indi$lat, shp.indi$long, method = "accurate"))
    tzoffset <- tz$utc_offset_h
    GOES_fireinfo.var <- cbind(GOES_fireinfo.var,tzoffset)
    #utc to local time
    GOES_fireinfo.var$hr <- GOES_fireinfo.var$hr+tzoffset
    timeind <- which(GOES_fireinfo.var$hr<0)
    GOES_fireinfo.var$day[timeind] <- GOES_fireinfo.var$day[timeind]-1
    GOES_fireinfo.var$hr[timeind] <- GOES_fireinfo.var$hr[timeind]+24
    GOES_fireinfo.var$date <- as.character(as.POSIXct(paste(as.Date(as.integer(GOES_fireinfo.var$day), origin = paste(year.indi-1,'-12-31', sep = "")),GOES_fireinfo.var$hr),tz=tz$tz_name[1],format="%Y-%m-%d %H"))
    ## sunset and sunrise
    tz <- tz_offset(as.Date(as.integer(GOES_fireinfo.var$day), origin = paste(year.indi-1,'-12-31', sep = "")), tz_lookup_coords(shp.indi$lat, shp.indi$long, method = "accurate"))
    sundata <- data.frame(date=as.Date(tz$date_time),lat=shp.indi$lat,lon=shp.indi$long)
    sunrise <- getSunlightTimes(data=sundata,keep="sunrise",tz=tz$tz_name[1])$sunrise
    sunset <- getSunlightTimes(data=sundata,keep="sunset",tz=tz$tz_name[1])$sunset

    ## all are local time, even shown as UTC time
    GOES_fireinfo.var$sunrise <- as.character(sunrise)
    GOES_fireinfo.var$sunset <- as.character(sunset)

    tz1 <- tz_offset(as.Date(as.integer(GOES_fireinfo.var$day), origin = paste(year.indi-1,'-12-30', sep = "")), tz_lookup_coords(shp.indi$lat, shp.indi$long, method = "accurate"))

    sundata1 <- data.frame(date=as.Date(tz1$date_time),lat=shp.indi$lat,lon=shp.indi$long)
    GOES_fireinfo.var$sunset_lastday <- as.character(getSunlightTimes(data=sundata1,keep="sunset",tz=tz1$tz_name[1])$sunset)
    ##day:0;night:1
    GOES_fireinfo.var <- GOES_fireinfo.var %>% mutate(dorn=ifelse(((date>sunrise&date<sunset)|(date<sunset_lastday)),0,1))

    rm(sundata,sunrise,sunset,tz,tzoffset,timeind)

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

    GOES_fireinfo.var <- GOES_fireinfo.var %>% dplyr::select(year:aftemp.mean,biome:inci_name,everything())

    out.name <- paste('015_OverNightBurning\\015.1_DataProcessing\\SingleFireCSV_hourly\\',year.indi,'_',shp.indi$seq,'_',as.integer( shp.indi$ID),'_',shp.indi$Country,'_',
                      round(GOES_fireinfo.var$long[1],2),'_',round(GOES_fireinfo.var$lat[1],2),'_',biomeid,'_',round(GOES_fireinfo.var$POLY_HA[1],2),'.csv',sep = '')

    if (outputYoN ==1) {
      write.table(GOES_fireinfo.var,file=out.name,sep = ',', row.names=FALSE) # keeps the rownames
    }
    
    }
  
}
################################################################################parallel running
no_cores<-detectCores()-4
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

