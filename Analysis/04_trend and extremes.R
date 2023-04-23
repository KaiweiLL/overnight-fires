
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
library(grid)


Nights_FeatDesi_all <- read.csv('015_OverNightBurning\\015.1_DataProcessing\\FeatureDesign\\FD_All_by2FireHr_gt1000.csv')
# all the rows with na were found in bui:fwi, and var_nightsd, like vpd_nightsd
# if rows with NightLength greater than 1, na was caused by daily variables, bui:fwi
# if rows with NightLength equals 1, na was caused by night sd
Nights_FeatDesi_all <- Nights_FeatDesi_all %>% dplyr::select(-inci_name,
                                                             -AFTempDayMeanFire,
                                                             -AFareaDayMeanFire,
                                                             -AFTempNightMeanFire,
                                                             -AFareaNightMeanFire)
ONBwithna <- 
  Nights_FeatDesi_all %>% 
  filter(NightLength >= 4, ONB_Event_YoN == 1) %>% 
  filter_all(any_vars(is.na(.)))
NonEwithna <- 
  Nights_FeatDesi_all %>% 
  filter(NightLength >= 4, ONB_Event_YoN == 0) %>% 
  filter_all(any_vars(is.na(.)))
which(is.na(ONBwithna[1,]))
which(is.na(NonEwithna[1,]))

new_DF1 <-
  Nights_FeatDesi_all %>% filter(NightLength >= 4, ONB_Event_YoN == 1) %>%
  mutate(
    BUI = ifelse(is.na(BUI), mean(BUI, na.rm = TRUE), BUI),
    DMC = ifelse(is.na(DMC), mean(DMC, na.rm = TRUE), DMC),
    DC = ifelse(is.na(DC), mean(DC, na.rm = TRUE), DC),
    FWI = ifelse(is.na(FWI), mean(FWI, na.rm = TRUE), FWI)
  ) %>% dplyr::select(ONB_FIRE_YoN:emc_cr_daymin2sunrise)
new_DF2 <-
  Nights_FeatDesi_all %>% 
  filter(NightLength >= 4, ONB_Event_YoN == 0) %>% 
  dplyr::select(ONB_FIRE_YoN:emc_cr_daymin2sunrise) %>% 
  drop_na()
FD_NLgte4 <- rbind(new_DF1, new_DF2)
FD_NLgte4 <-
  FD_NLgte4 %>% mutate(Nfirepc = NightFireHr / NightLength,
                       Dfirepc = DayFireHr / DayLength) %>%
  mutate(Ncondition = case_when((Nfirepc == 0) ~ 'Non-NBE',
                                (Nfirepc > 0 & Nfirepc < 1) ~ 'NEE',
                                (Nfirepc == 1) ~ 'OBE'
  ))%>% mutate(ONB_Event_YoN= ifelse(ONB_Event_YoN==1, 'OBEs', 
                                     'Non events'))%>% 
  mutate(biome_4zones = case_when(
    (biome >= 11 & biome <= 16) ~ 'tropical',
    (biome >= 21 & biome <= 25) ~ 'subtropical',
    (biome >= 31 & biome <= 35) ~ 'temperate',
    (biome >= 41 & biome <= 43) ~ 'boreal')) %>% 
  mutate(WorEzone = ifelse((long<(-90)),'W','E')) %>% 
  mutate(biome_4areas = case_when(
    biome_4zones == 'boreal' ~'Boreal',
    (biome == 35 &WorEzone=='W') ~ 'Temperate mountain system',
    (biome == 25 &WorEzone=='W') ~ 'Subtropical mountain system',
    (biome == 34 &WorEzone=='W') ~ 'Temperate desert')) %>%
  mutate(season = case_when((month >= 3 & month <=5) ~ 'spring',
                            (month >= 6 & month <=8) ~'summer',
                            (month >= 9 & month <=11) ~ 'fall',
                            (month==12|month==1|month==2) ~ 'winter')) 

x = FD_NLgte4 %>% filter(ONB_Event_YoN=='OBEs')
y = unique(x$ID)
FD_NLgte4 <- FD_NLgte4 %>% mutate(ONB_FIRE_YoN = ifelse((ID%in%y), 1, 0))
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

NA_obe = unique((FD_NLgte4 %>% filter(ONB_Event_YoN=='OBEs'))$ID)
NAfire_obe = NAfire[NAfire$ID%in%NA_obe,]

# Long Term Fire Weather Daily Extract--------------------------------------------

# firepath <- mixedsort(dir(path='011_Data\\015_LongTermFireWeather\\outputs_na\\',full.names = T,pattern = '(nc)$'))
# head(firepath)
# BUI <- firepath[1:43]
# DSR <- firepath[44:86]
# DC <- firepath[87:129]
# DMC <- firepath[130:172]
# FFMC <- firepath[173:215]
# FWI <- firepath[216:258]
# ISI <- firepath[259:301]
# 
# 
# f<-function(i){
#   library(rgdal)
#   library(raster)
#   library(sp)
#   library(dplyr)
#   library(ggplot2)
#   library(reshape2)
#   library(ggsci)
#   library(lutz)
#   library(suncalc)
#   library(gtools)
#   library(foreach)
#   library(doParallel)
#   library(parallel)
#   library(tcltk)
#   library(doSNOW)
#   #select polygon
# 
#   bui.nc <- brick(BUI[i])
#   dc.nc <- brick(DC[i])
#   dmc.nc <- brick(DMC[i])
#   fwi.nc <- brick(FWI[i])
#   # isi.nc <- brick(ISI[i])
#   # ffmc.nc <- brick(FFMC[i])
# 
# 
#   bui.nc <- rotate(bui.nc)
#   dc.nc <- rotate(dc.nc)
#   dmc.nc <- rotate(dmc.nc)
#   fwi.nc <- rotate(fwi.nc)
#   # isi.nc <- rotate(isi.nc)
#   # ffmc.nc <- rotate(ffmc.nc)
#   
#   ext.val.bui <- raster::extract(bui.nc, NAfire_obe,weight=T,df=T,cellnumbers=T)
#   ext.val.bui = cbind(xyFromCell(bui.nc,ext.val.bui$cell),ext.val.bui) %>% dplyr::select(-c(weight,cell))
#   
#   ID = unique(ext.val.bui$ID)
#   IDseq2 = unique((NAfire_obe@data)$ID)
#   IDlookup = data.frame(ID,IDseq2)
#   ext.val.bui = full_join(ext.val.bui, IDlookup) %>% dplyr::select(-ID) %>% rename(ID=IDseq2) %>% dplyr::select(x,y,ID,everything())
#   
#   ext.val.dc <- raster::extract(dc.nc, NAfire_obe,weight=T,df=T,cellnumbers=T)
#   ext.val.dc = cbind(xyFromCell(dc.nc,ext.val.dc$cell),ext.val.dc) %>% dplyr::select(-c(weight,cell))
#   ext.val.dc = full_join(ext.val.dc, IDlookup) %>% dplyr::select(-ID) %>% rename(ID=IDseq2) %>% dplyr::select(x,y,ID,everything())
#   
#   
#   ext.val.dmc <- raster::extract(dmc.nc, NAfire_obe,weight=T,df=T,cellnumbers=T)
#   ext.val.dmc = cbind(xyFromCell(dmc.nc,ext.val.dmc$cell),ext.val.dmc) %>% dplyr::select(-c(weight,cell))
#   ext.val.dmc = full_join(ext.val.dmc, IDlookup) %>% dplyr::select(-ID) %>% rename(ID=IDseq2) %>% dplyr::select(x,y,ID,everything())
#   
#   ext.val.fwi <- raster::extract(fwi.nc, NAfire_obe,weight=T,df=T,cellnumbers=T)
#   ext.val.fwi = cbind(xyFromCell(fwi.nc,ext.val.fwi$cell),ext.val.fwi) %>% dplyr::select(-c(weight,cell))
#   ext.val.fwi = full_join(ext.val.fwi, IDlookup) %>% dplyr::select(-ID) %>% rename(ID=IDseq2) %>% dplyr::select(x,y,ID,everything())
#   
#   # ext.val.isi <- extract(isi.nc, NAfire,weight=T,df=T,cellnumbers=T)
#   # ext.val.isi = cbind(xyFromCell(isi.nc,ext.val.isi$cell),ext.val.isi) %>% dplyr::select(-c(weight,cell))
#   # 
#   # ext.val.ffmc <- extract(ffmc.nc, NAfire,weight=T,df=T,cellnumbers=T)
#   # ext.val.ffmc = cbind(xyFromCell(ffmc.nc,ext.val.ffmc$cell),ext.val.ffmc) %>% dplyr::select(-c(weight,cell))
# 
#   out.bui.name <- paste('011_Data\\015_LongTermFireWeather\\results\\','LongTerm','_BUI_',i+1978,'.csv',sep = '')
#   write.table(ext.val.bui,file=out.bui.name,sep = ',', row.names=FALSE) # keeps the rownames
# 
#   out.dc.name <- paste('011_Data\\015_LongTermFireWeather\\results\\','LongTerm','_DC_',i+1978,'.csv',sep = '')
#   write.table(ext.val.dc,file=out.dc.name,sep = ',', row.names=FALSE) # keeps the rownames
# 
#   out.dmc.name <- paste('011_Data\\015_LongTermFireWeather\\results\\','LongTerm','_DMC_',i+1978,'.csv',sep = '')
#   write.table(ext.val.dmc,file=out.dmc.name,sep = ',', row.names=FALSE) # keeps the rownames
# 
#   out.fwi.name <- paste('011_Data\\015_LongTermFireWeather\\results\\','LongTerm','_FWI_',i+1978,'.csv',sep = '')
#   write.table(ext.val.fwi,file=out.fwi.name,sep = ',', row.names=FALSE) # keeps the rownames
# 
#   # out.isi.name <- paste('011_Data\\015_LongTermFireWeather\\results\\','LongTerm','_ISI_',i+1978,'.csv',sep = '')
#   # write.table(ext.val.isi,file=out.isi.name,sep = ',', row.names=FALSE) # keeps the rownames
#   # 
#   # out.ffmc.name <- paste('011_Data\\015_LongTermFireWeather\\results\\','LongTerm','_FFMC_',i+1978,'.csv',sep = '')
#   # write.table(ext.val.ffmc,file=out.ffmc.name,sep = ',', row.names=FALSE) # keeps the rownames
# 
# }
# ################################################################################parallel running
# 
# no_cores<-detectCores()-4
# cl <- makeSOCKcluster(no_cores)
# registerDoSNOW(cl)
# pb <- txtProgressBar(min=1, max=42, style=3)#length(NAfire_year)
# progress <- function(n) setTxtProgressBar(pb, n)
# opts <- list(progress=progress)
# 
# start_time <- Sys.time()
# NAfire_csvwrite <- foreach(i=1:42, .options.snow = opts) %dopar%  f(i)#, .combine = 'rbind'
# stopCluster(cl)
# end_time <- Sys.time()
# end_time - start_time

# Daily variable aggregation ----------------------------------------------------------------

firepath <- mixedsort(dir(path='011_Data\\015_LongTermFireWeather\\results\\',full.names = T,pattern = '(csv)$'))
head(firepath)
var <- unique(substr(firepath,51,53))

g <- function(j) {
  
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
  library(grid)
  firepath <- mixedsort(dir(path='011_Data\\015_LongTermFireWeather\\results\\',full.names = T,pattern = '(csv)$'))
  pathind <- grep(pattern = j,firepath)
  f<-function(i){
    library('dplyr')
    Fire <- read.csv(firepath[i])
    yearind <- gregexpr(pattern ='.csv',firepath[i])[[1]]
    year <- as.integer(substr(firepath[i],yearind-4,yearind-1)) 
    if (year==1979) {
      Fire <- Fire %>% mutate(var=j) %>% dplyr::select(var,everything())
      varname <- paste(j,year,4:365,sep = '_')
      varname <- c('var','x','y','ID',varname)
      colnames(Fire) <- varname
      Fire
    }else {
      Fire <- Fire %>% dplyr::select(-c(x,y,ID))
      if (year%%4==0) {
        varname <- paste(j,year,1:366,sep = '_')
        colnames(Fire) <- varname
      }else{
        varname <- paste(j,year,1:365,sep = '_')
        colnames(Fire) <- varname
      }
      Fire
    }
  }
  f_summer<-function(i){
    library('dplyr')
    Fire <- read.csv(firepath[i])
    yearind <- gregexpr(pattern ='.csv',firepath[i])[[1]]
    year <- as.integer(substr(firepath[i],yearind-4,yearind-1)) 
    if (year==1979) {
      Fire <- Fire %>% mutate(var=j) %>% dplyr::select(var,everything())
      summerdays <- 152
      summerdaye <- 243
      varname <- paste(j,year,4:365,sep = '_')
      varname <- c('var','x','y','ID',varname)
      colnames(Fire) <- varname
      FireSummer <- Fire[,c(1:4,summerdays:summerdaye)] %>% mutate(var=j) %>% dplyr::select(var,everything())
    }else {
      Fire <- Fire %>% dplyr::select(-c(x,y,ID))
      if (year%%4==0) {
        summerdays <- 153
        summerdaye <- 244
        varname <- paste(j,year,1:366,sep = '_')
        colnames(Fire) <- varname
      }else{
        summerdays <- 152
        summerdaye <- 243
        varname <- paste(j,year,1:365,sep = '_')
        colnames(Fire) <- varname
      }
      FireSummer <- Fire[,summerdays:summerdaye]
    }
  }
  f_fall<-function(i){
    library('dplyr')
    Fire <- read.csv(firepath[i])
    yearind <- gregexpr(pattern ='.csv',firepath[i])[[1]]
    year <- as.integer(substr(firepath[i],yearind-4,yearind-1)) 
    if (year==1979) {
      Fire <- Fire %>% mutate(var=j) %>% dplyr::select(var,everything())
      falldays <- 244
      falldaye <- 334
      varname <- paste(j,year,4:365,sep = '_')
      varname <- c('var','x','y','ID',varname)
      colnames(Fire) <- varname
      FireFall <- Fire[,c(1:4,falldays:falldaye)]%>% mutate(var=j) %>% dplyr::select(var,everything())
    }else {
      Fire <- Fire %>% dplyr::select(-c(x,y,ID))
      if (year%%4==0) {
        falldays <- 245
        falldaye <- 335
        varname <- paste(j,year,1:366,sep = '_')
        colnames(Fire) <- varname
      }else{
        falldays <- 244
        falldaye <- 334
        varname <- paste(j,year,1:365,sep = '_')
        colnames(Fire) <- varname
      }
      Fire[,falldays:falldaye]
    }
  }
  f_spring<-function(i){
    library('dplyr')
    Fire <- read.csv(firepath[i])
    yearind <- gregexpr(pattern ='.csv',firepath[i])[[1]]
    year <- as.integer(substr(firepath[i],yearind-4,yearind-1)) 
    if (year==1979) {
      Fire <- Fire %>% mutate(var=j) %>% dplyr::select(var,everything())
      springdays <- 60
      springdaye <- 151
      varname <- paste(j,year,4:365,sep = '_')
      varname <- c('var','x','y','ID',varname)
      colnames(Fire) <- varname
      Firespring <- Fire[,c(1:4,springdays:springdaye)] %>% mutate(var=j) %>% dplyr::select(var,everything())
    }else {
      Fire <- Fire %>% dplyr::select(-c(x,y,ID))
      if (year%%4==0) {
        springdays <- 61
        springdaye <- 152
        varname <- paste(j,year,1:366,sep = '_')
        colnames(Fire) <- varname
      }else{
        springdays <- 60
        springdaye <- 151
        varname <- paste(j,year,1:365,sep = '_')
        colnames(Fire) <- varname
      }
      Firespring <- Fire[,springdays:springdaye]
    }
  }
  no_cores<-detectCores()-6
  cl <- makeSOCKcluster(no_cores)
  registerDoSNOW(cl)
  pb <- txtProgressBar(min=1, max=42, style=3)#length(NAfire_year)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress=progress)
  
  start_time <- Sys.time()
  VarFire <- foreach(i=pathind, .options.snow = opts, .combine = 'cbind') %dopar%  f(i)#, .combine = 'rbind'
  VarFire_summer <- foreach(i=pathind, .options.snow = opts, .combine = 'cbind') %dopar%  f_summer(i)#, .combine = 'rbind'
  VarFire_fall <- foreach(i=pathind, .options.snow = opts, .combine = 'cbind') %dopar%  f_fall(i)#, .combine = 'rbind'
  VarFire_spring <- foreach(i=pathind, .options.snow = opts, .combine = 'cbind') %dopar%  f_spring(i)#, .combine = 'rbind'
  
  stopCluster(cl)
  end_time <- Sys.time()
  end_time - start_time
  
  varlist = list(VarFire,VarFire_spring, VarFire_summer,VarFire_fall)
}

no_cores<-detectCores()-14
cl <- makeSOCKcluster(no_cores)
registerDoSNOW(cl)
pb <- txtProgressBar(min=1, max=4, style=3)#length(NAfire_year)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress=progress)

start_time <- Sys.time()
comb <- function(x, ...) {
  lapply(seq_along(x),
         function(i) c(x[[i]], lapply(list(...), function(y) y[[i]])))
}
varlist <- foreach(j=var, .options.snow = opts, .combine='comb', .multicombine=TRUE,
                   .init=list(list(), list(), list(),list())) %dopar% g(j)
stopCluster(cl)
end_time <- Sys.time()
end_time - start_time


# first 21 years VS latter 21 years ---------------------------------------

f.f <- function(i) {
  library(dplyr)
  Firevar <- varlist[[1]]
  vardf = Firevar[[i]]
  varname <- vardf$var[1]
  vardf = right_join(dfNAfires,vardf)
  onb = unique((FD_NLgte4 %>% filter(ONB_Event_YoN=='OBEs'))[c('year','seq','day')])
  vardf_onb = left_join(onb,vardf) %>% mutate(varmatch=paste(varname,year,day,sep='_')) %>% dplyr::select(varmatch,everything())
  varmatch = vardf_onb$varmatch
  m = vardf_onb[,varmatch]
  mm = diag(as.matrix(m))
  xx = as.data.frame(mm)
  vardf_onb$onbvalue <- mm
  
  # df_total = data.frame()
  # for (nr in 1:nrow(vardf_onb)) {
  #   varmatch = (vardf_onb[nr,])$varmatch
  #   m = vardf_onb[nr,varmatch]
  #   df <- data.frame(m)
  #   df_total <- rbind(df_total,df)
  # }
  
  vardf_onb <- vardf_onb %>% dplyr::select(onbvalue,everything())
  vardf_onb <- vardf_onb %>% group_by(year,seq,day) %>% summarise(across(everything(),~ mean(., na.rm = TRUE))) %>% dplyr::select(onbvalue,everything())
  
  vardf_onb_first21_mat = as.matrix((vardf_onb[,c(1,25:ncol(vardf_onb))])[,1:7668])
  vardf_onb_later21_mat = as.matrix((vardf_onb[,c(1,25:ncol(vardf_onb))])[,c(1,7669:15339)])
  
  quanfirst21 = apply(vardf_onb_first21_mat,1,function(x) ecdf(x[2:ncol(vardf_onb_first21_mat)])(x[1]))
  quanlater21 = apply(vardf_onb_later21_mat,1,function(x) ecdf(x[2:ncol(vardf_onb_later21_mat)])(x[1]))
  quanfirst21 = as.data.frame(quanfirst21)
  quanlater21 = as.data.frame(quanlater21)
  
  if (varname == 'DC_') {    varname = 'DC'  }else if(varname =='FFM'){varname='FFMC'}
  colnames(quanfirst21) = paste0('quanfirst21_',varname)
  colnames(quanlater21) = paste0('quanlater21_',varname)
  quan_daily = cbind(quanfirst21,quanlater21)
  quan_daily
}

no_cores<-detectCores()-14
cl <- makeSOCKcluster(no_cores)
registerDoSNOW(cl)
pb <- txtProgressBar(min=1, max=4, style=3)#length(NAfire_year)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress=progress)

start_time <- Sys.time()
quan_daily <- foreach(i=1:4, .options.snow = opts, .combine = 'cbind') %dopar%  f.f(i)#, .combine = 'rbind'
stopCluster(cl)
end_time <- Sys.time()
end_time - start_time

quan_daily_long = quan_daily %>% gather(key = 'variable', value= 'value',quanfirst21_BUI:quanlater21_FWI)

ggplot(data=quan_daily_long)+geom_density(aes(x=value,fill=variable, ..scaled..),alpha=0.6)+ scale_fill_npg()+
  envalysis::theme_publish(base_size = 8, base_family = "", line_size = 0.25,base_rect_size =0.25)

summary(quan_daily_long)
ggplot(data=quan_daily_long)+geom_boxplot(aes(y=value,fill=variable))


quan_daily$id <- rownames(quan_daily)
quanfirst21 = quan_daily %>% dplyr::select(quanfirst21_FWI,quanfirst21_BUI,quanfirst21_DMC,quanfirst21_DC,id)
quanlater21 = quan_daily %>% dplyr::select(quanlater21_FWI,quanlater21_BUI,quanlater21_DMC,quanlater21_DC,id)

quanfirst21$ts <- '1979-1999'
quanlater21$ts <- '2000-2020'

colnames(quanfirst21) <- c('FWI','BUI','DMC','DC','id','ts')
colnames(quanlater21) <- c('FWI','BUI','DMC','DC','id','ts')

summary(quanfirst21)
summary(quanlater21)

xx = rbind(quanfirst21,quanlater21)



xx = xx %>% gather(FWI:DC,key=variable,value=value)
ggplot(data=xx,aes(x=ts,y=value,fill=variable))+geom_boxplot(size=0.5)+
  geom_line(aes(group=id), position = position_dodge(0.2),alpha = 0.05) +
  geom_point(aes(color=variable,group=id),size=1,shape=21, position = position_dodge(0.2),alpha=0.1) +
  theme(legend.position = "none")+facet_wrap(~variable,nrow = 1)+scale_fill_lancet()+scale_color_lancet()+envalysis::theme_publish()+
  ylab('Quantiles')+theme(axis.title.x = element_blank(),legend.position = 'none')


res <- wilcox.test((quanfirst21 %>% dplyr::select(BUI))$BUI, (quanlater21 %>% dplyr::select(BUI))$BUI, paired = TRUE,alternative = 'greater')
res
res <- wilcox.test((quanfirst21 %>% dplyr::select(FWI))$FWI, (quanlater21 %>% dplyr::select(FWI))$FWI, paired = TRUE,alternative = 'greater')
res
res <- wilcox.test((quanfirst21 %>% dplyr::select(DMC))$DMC, (quanlater21 %>% dplyr::select(DMC))$DMC, paired = TRUE,alternative = 'greater')
res
res <- wilcox.test((quanfirst21 %>% dplyr::select(DC))$DC, (quanlater21 %>% dplyr::select(DC))$DC, paired = TRUE,alternative = 'greater')

res


# # Analysis: based on fire perimter  ---------------------------------------
# 
# 
# f.f <- function(i) {
#   library(dplyr)
#   Firevar <- varlist[[1]]
#   vardf = Firevar[[i]]
#   varname <- vardf$var[1]
#   vardf = right_join(dfNAfires,vardf)
#   onb = unique((FD_NLgte4 %>% filter(ONB_Event_YoN=='OBEs'))[c('year','seq','day')])
#   vardf_onb = left_join(onb,vardf) %>% mutate(varmatch=paste(varname,year,day,sep='_')) %>% dplyr::select(varmatch,everything())
#   varmatch = vardf_onb$varmatch
#   m = vardf_onb[,varmatch]
#   mm = diag(as.matrix(m))
#   xx = as.data.frame(mm)
#   vardf_onb$onbvalue <- mm
#   
#   # df_total = data.frame()
#   # for (nr in 1:nrow(vardf_onb)) {
#   #   varmatch = (vardf_onb[nr,])$varmatch
#   #   m = vardf_onb[nr,varmatch]
#   #   df <- data.frame(m)
#   #   df_total <- rbind(df_total,df)
#   # }
#   
#   vardf_onb <- vardf_onb %>% dplyr::select(onbvalue,everything())
#   vardf_onb <- vardf_onb %>% group_by(year,seq,day) %>% summarise(across(everything(),~ mean(., na.rm = TRUE))) %>% dplyr::select(onbvalue,everything())
#   
#   vardf_onb_mat <- as.matrix(vardf_onb[,c(1,25:ncol(vardf_onb))])
#   onb.point.quantile42Y = apply(vardf_onb_mat,1,function(x) ecdf(x[2:ncol(vardf_onb_mat)])(x[1]))
#   onb.point.quantile42Y = as.data.frame(onb.point.quantile42Y)
#   if (varname == 'DC_') {    varname = 'DC'  }else if(varname =='FFM'){varname='FFMC'}
#   colnames(onb.point.quantile42Y) = paste0('Allseason_',varname)
#   onb.point.quantile42Y
# }
# 
# no_cores<-detectCores()-14
# cl <- makeSOCKcluster(no_cores)
# registerDoSNOW(cl)
# pb <- txtProgressBar(min=1, max=4, style=3)#length(NAfire_year)
# progress <- function(n) setTxtProgressBar(pb, n)
# opts <- list(progress=progress)
# 
# start_time <- Sys.time()
# onb.point.quantile42Y.f <- foreach(i=1:4, .options.snow = opts, .combine = 'cbind') %dopar%  f.f(i)#, .combine = 'rbind'
# stopCluster(cl)
# end_time <- Sys.time()
# end_time - start_time
# 
# df.onb.p.q.f = onb.point.quantile42Y.f %>% gather(key = 'variable', value= 'value',Allseason_BUI:Allseason_FWI)
# 
# ggplot(data=df.onb.p.q.f)+geom_density(aes(x=value,fill=variable, ..scaled..),alpha=0.6)+ scale_fill_npg()+
#   envalysis::theme_publish(base_size = 8, base_family = "", line_size = 0.25,base_rect_size =0.25)
# 
# summary(onb.point.quantile42Y.f)
# ggplot(data=df.onb.p.q.f)+geom_boxplot(aes(y=value,fill=variable))
# 
# 
# f_summer.f <- function(i) {
#   library(dplyr)
#   Firevar <- varlist[[3]]
#   vardf = Firevar[[i]]
#   varname <- vardf$var[1]
#   vardf = right_join(dfNAfires,vardf)
#   onb = unique((FD_NLgte4 %>% filter(ONB_Event_YoN=='OBEs'))[c('year','seq','day','season')])
#   vardf_onb = left_join(onb,vardf) %>% mutate(varmatch=paste(varname,year,day,sep='_')) %>% dplyr::select(varmatch,everything()) %>% 
#     filter(season==2)
#   varmatch = vardf_onb$varmatch
#   m = vardf_onb[,varmatch]
#   mm = diag(as.matrix(m))
#   vardf_onb$onbvalue <- mm
#   vardf_onb <- vardf_onb %>% dplyr::select(onbvalue,everything())
#   vardf_onb <- vardf_onb %>% group_by(year,seq,day) %>% summarise(across(everything(),~ mean(., na.rm = TRUE))) %>% dplyr::select(onbvalue,everything())
#   
#   vardf_onb_mat <- as.matrix(vardf_onb[,c(1,26:ncol(vardf_onb))])
#   onb.point.quantile42Y = apply(vardf_onb_mat,1,function(x) ecdf(x[2:ncol(vardf_onb_mat)])(x[1]))
#   onb.point.quantile42Y = as.data.frame(onb.point.quantile42Y)
#   if (varname == 'DC_') {    varname = 'DC'  }else if(varname =='FFM'){varname='FFMC'}
#   colnames(onb.point.quantile42Y) = paste0('Summer_',varname)
#   onb.point.quantile42Y
# }
# f_fall.f <- function(i) {
#   library(dplyr)
#   Firevar <- varlist[[4]]
#   vardf = Firevar[[i]]
#   varname <- vardf$var[1]
#   vardf = right_join(dfNAfires,vardf)
#   onb = unique((FD_NLgte4 %>% filter(ONB_Event_YoN=='OBEs'))[c('year','seq','day','season')])
#   vardf_onb = left_join(onb,vardf) %>% mutate(varmatch=paste(varname,year,day,sep='_')) %>% dplyr::select(varmatch,everything()) %>% 
#     filter(season==3)
#   varmatch = vardf_onb$varmatch
#   m = vardf_onb[,varmatch]
#   mm = diag(as.matrix(m))
#   vardf_onb$onbvalue <- mm
#   vardf_onb <- vardf_onb %>% dplyr::select(onbvalue,everything())
#   vardf_onb <- vardf_onb %>% group_by(year,seq,day) %>% summarise(across(everything(),~ mean(., na.rm = TRUE))) %>% dplyr::select(onbvalue,everything())
#   
#   vardf_onb_mat <- as.matrix(vardf_onb[,c(1,26:ncol(vardf_onb))])
#   onb.point.quantile42Y = apply(vardf_onb_mat,1,function(x) ecdf(x[2:ncol(vardf_onb_mat)])(x[1]))
#   onb.point.quantile42Y = as.data.frame(onb.point.quantile42Y)
#   if (varname == 'DC_') {    varname = 'DC'  }else if(varname =='FFM'){varname='FFMC'}
#   colnames(onb.point.quantile42Y) = paste0('Fall_',varname)
#   onb.point.quantile42Y
# }
# f_spring.f <- function(i) {
#   library(dplyr)
#   Firevar <- varlist[[2]]
#   vardf = Firevar[[i]]
#   varname <- vardf$var[1]
#   vardf = right_join(dfNAfires,vardf)
#   onb = unique((FD_NLgte4 %>% filter(ONB_Event_YoN=='OBEs'))[c('year','seq','day','season')])
#   vardf_onb = left_join(onb,vardf) %>% mutate(varmatch=paste(varname,year,day,sep='_')) %>% dplyr::select(varmatch,everything()) %>% 
#     filter(season==1)
#   varmatch = vardf_onb$varmatch
#   m = vardf_onb[,varmatch]
#   mm = diag(as.matrix(m))
#   vardf_onb$onbvalue <- mm
#   vardf_onb <- vardf_onb %>% dplyr::select(onbvalue,everything())
#   vardf_onb <- vardf_onb %>% group_by(year,seq,day) %>% summarise(across(everything(),~ mean(., na.rm = TRUE))) %>% dplyr::select(onbvalue,everything())
#   
#   vardf_onb_mat <- as.matrix(vardf_onb[,c(1,26:ncol(vardf_onb))])
#   onb.point.quantile42Y = apply(vardf_onb_mat,1,function(x) ecdf(x[2:ncol(vardf_onb_mat)])(x[1]))
#   onb.point.quantile42Y = as.data.frame(onb.point.quantile42Y)
#   if (varname == 'DC_') {    varname = 'DC'  }else if(varname =='FFM'){varname='FFMC'}
#   colnames(onb.point.quantile42Y) = paste0('Spring_',varname)
#   onb.point.quantile42Y
# }
# 
# 
# no_cores<-detectCores()-14
# cl <- makeSOCKcluster(no_cores)
# registerDoSNOW(cl)
# pb <- txtProgressBar(min=1, max=6, style=3)#length(NAfire_year)
# progress <- function(n) setTxtProgressBar(pb, n)
# opts <- list(progress=progress)
# 
# start_time <- Sys.time()
# onb.point.q42Y.summer.f <- foreach(i=1:6, .options.snow = opts, .combine = 'cbind') %dopar%  f_summer.f(i)#, .combine = 'rbind'
# onb.point.q42Y.fall.f <- foreach(i=1:6, .options.snow = opts, .combine = 'cbind') %dopar%  f_fall.f(i)#, .combine = 'rbind'
# onb.point.q42Y.spring.f <- foreach(i=1:6, .options.snow = opts, .combine = 'cbind') %dopar%  f_spring.f(i)#, .combine = 'rbind'
# stopCluster(cl)
# end_time <- Sys.time()
# end_time - start_time
# 
# df.onb.p.q.summer.f = onb.point.q42Y.summer.f %>% gather(key = 'variable', value= 'value',Summer_BUI:Summer_ISI)
# df.onb.p.q.fall.f = onb.point.q42Y.fall.f %>% gather(key = 'variable', value= 'value',Fall_BUI:Fall_ISI)
# df.onb.p.q.spring.f = onb.point.q42Y.spring.f %>% gather(key = 'variable', value= 'value',Spring_BUI:Spring_ISI)
# 
# ggplot(data=df.onb.p.q.summer.f)+geom_density(aes(x=value,fill=variable, ..scaled..),alpha=0.6)+ scale_fill_npg()+
#   envalysis::theme_publish(base_size = 8, base_family = "", line_size = 0.25,base_rect_size =0.25)
# 
# ggplot(data=df.onb.p.q.fall.f)+geom_density(aes(x=value,fill=variable),alpha=0.6)+ scale_fill_npg()+
#   envalysis::theme_publish(base_size = 8, base_family = "", line_size = 0.25,base_rect_size =0.25)
# 
# ggplot(data=df.onb.p.q.spring.f)+geom_density(aes(x=value,fill=variable),alpha=0.6)+ scale_fill_npg()+
#   envalysis::theme_publish(base_size = 8, base_family = "", line_size = 0.25,base_rect_size =0.25)
# 
# 
# summary(onb.point.q42Y.summer.f)
# summary(onb.point.q42Y.fall.f)
# summary(onb.point.q42Y.spring.f)
# st(onb.point.quantile42Y.f,add.median = T)
# 
# st(onb.point.q42Y.spring.f,add.median = T)
# st(onb.point.q42Y.summer.f,add.median = T)
# st(onb.point.q42Y.fall.f,add.median = T)
# 
# 
# df.onb.p.q.f <- df.onb.p.q.f %>% rowwise() %>%  mutate(x=strsplit(variable,'_'),season=x[1],var=x[2])
# df.onb.p.q.spring.f <- df.onb.p.q.spring.f %>% rowwise() %>%  mutate(x=strsplit(variable,'_'),season=x[1],var=x[2])
# df.onb.p.q.summer.f <- df.onb.p.q.summer.f %>% rowwise() %>%  mutate(x=strsplit(variable,'_'),season=x[1],var=x[2])
# df.onb.p.q.fall.f <- df.onb.p.q.fall.f %>% rowwise() %>% mutate(x=strsplit(variable,'_'),season=x[1],var=x[2])
# 
# df.onb.season.var.f <- rbind(df.onb.p.q.f, df.onb.p.q.spring.f,df.onb.p.q.summer.f,df.onb.p.q.fall.f)
# 
# 
# ggplot(data=df.onb.season.var.f)+geom_boxplot(aes(x= var, y=value,fill=season),alpha=0.6)+ scale_fill_aaas()+
#   envalysis::theme_publish(base_size = 8, base_family = "", line_size = 0.25,base_rect_size =0.25)
# 
# 
# 
# # Analysis: annual extremes -----------------------------------------------
# 
# f.annual <- function(i) {
#   library(dplyr)
#   library(foreach)
#   library(doParallel)
#   library(parallel)
#   library(tcltk)
#   library(doSNOW)
#   Firevar <- varlist[[1]]
#   vardf = Firevar[[i]]
#   varname <- vardf$var[1]
#   vardf = right_join(dfNAfires,vardf)
#   onb = unique((FD_NLgte4 %>% filter(ONB_Event_YoN=='OBEs'))[c('year','seq','day')])
#   vardf_onb = left_join(onb,vardf) %>% mutate(varmatch=paste(varname,year,day,sep='_')) %>% dplyr::select(varmatch,everything())
#   varmatch = vardf_onb$varmatch
# 
#   
#   sluni = unique(vardf_onb[,c('ID','day')])
#   f.annual.2 <- function(nr){
#     library(dplyr)
#     sledrow = vardf_onb %>% filter(ID==sluni$ID[nr],day==sluni$day[nr])
#     cont = paste(varname,sledrow$year[1],sep='_')
#     sledrow = sledrow %>% dplyr::select(1:23,contains(cont))
#     varmatch = sledrow$varmatch
#     m = sledrow[,varmatch]
#     mm = diag(as.matrix(m))
#     xx = as.data.frame(mm)
#     sledrow$onbvalue <- mm
#     
#     sledrow <- sledrow %>% dplyr::select(onbvalue,everything())
#     sledrow <- sledrow %>% group_by(year,seq,day) %>% summarise(across(everything(),~ mean(., na.rm = TRUE))) %>% dplyr::select(onbvalue,everything())
#     
#     vardf_onb_mat <- as.matrix(sledrow[,c(1,25:ncol(sledrow))])
#     onb.point.quantile = apply(vardf_onb_mat,1,function(x) ecdf(x[2:ncol(vardf_onb_mat)])(x[1]))
#     onb.point.quantile = as.data.frame(onb.point.quantile)
#     
#     onb.point.quantile
# 
#   }
#   no_cores<-detectCores()-4
#   cl <- makeSOCKcluster(no_cores)
#   registerDoSNOW(cl)
#   pb <- txtProgressBar(min=1, max=nrow(sluni), style=3)#length(NAfire_year)
#   progress <- function(n) setTxtProgressBar(pb, n)
#   opts <- list(progress=progress)
#   
#   start_time <- Sys.time()
#   onb.point.quantile.a.2 <- foreach(nr=1:nrow(sluni), .options.snow = opts, .combine = 'rbind') %dopar%  f.annual.2(nr)#, .combine = 'rbind'
#   stopCluster(cl)
#   end_time <- Sys.time()
#   end_time - start_time
#   
#   if (varname == 'DC_') {    varname = 'DC'  }else if(varname =='FFM'){varname='FFMC'}
#   colnames(onb.point.quantile.a.2) = paste0('Annual_',varname)
#   onb.point.quantile.a.2
# }
# 
# no_cores<-detectCores()-14
# cl <- makeSOCKcluster(no_cores)
# registerDoSNOW(cl)
# pb <- txtProgressBar(min=1, max=4, style=3)#length(NAfire_year)
# progress <- function(n) setTxtProgressBar(pb, n)
# opts <- list(progress=progress)
# 
# start_time <- Sys.time()
# onb.point.quantile.a <- foreach(i=1:4, .options.snow = opts, .combine = 'cbind') %dopar%  f.annual(i)#, .combine = 'rbind'
# stopCluster(cl)
# end_time <- Sys.time()
# end_time - start_time
# 
# df.onb.a.f = onb.point.quantile.a %>% gather(key = 'variable', value= 'value',Annual_BUI:Annual_FWI)
# 
# ggplot(data=df.onb.a.f)+geom_density(aes(x=value,fill=variable, ..scaled..),alpha=0.6)+ scale_fill_npg()+
#   envalysis::theme_publish(base_size = 8, base_family = "", line_size = 0.25,base_rect_size =0.25)
# 
# summary(onb.point.quantile.a)
# ggplot(data=df.onb.a.f)+geom_boxplot(aes(y=value,fill=variable))
# 
# 
# 
# # paired plot -------------------------------------------------------------
# 
# onb.point.quantile42Y.f$id <- rownames(onb.point.quantile42Y.f)
# onb.point.quantile.a$id <- rownames(onb.point.quantile.a)
# onb.point.quantile42Y.f$ts <- 'Historical'
# onb.point.quantile.a$ts <- 'Annual'
# 
# colnames(onb.point.quantile42Y.f) <- c('BUI','DC','DMC','FWI','id','ts')
# colnames(onb.point.quantile.a) <- c('BUI','DC','DMC','FWI','id','ts')
# 
# 
# xx = rbind(onb.point.quantile42Y.f,onb.point.quantile.a)
# xx = xx %>% gather(BUI:FWI,key=variable,value=value)
# ggplot(data=xx,aes(x=ts,y=value,fill=variable))+geom_boxplot(size=0.5)+
#   geom_line(aes(group=id), position = position_dodge(0.2),alpha = 0.05) +
#   geom_point(aes(color=variable,group=id),size=1,shape=21, position = position_dodge(0.2),alpha=0.1) +
#   theme(legend.position = "none")+facet_wrap(~variable,nrow = 1)+scale_fill_lancet()+scale_color_lancet()+envalysis::theme_publish()+
#   ylab('Quantiles')+theme(axis.title.x = element_blank(),legend.position = 'none')
# 
# 
# res <- t.test((onb.point.quantile42Y.f %>% dplyr::select(BUI))$BUI, (onb.point.quantile.a %>% dplyr::select(BUI))$BUI, paired = TRUE)
# res
# res <- t.test((onb.point.quantile42Y.f %>% dplyr::select(FWI))$FWI, (onb.point.quantile.a %>% dplyr::select(FWI))$FWI, paired = TRUE)
# res
# res <- t.test((onb.point.quantile42Y.f %>% dplyr::select(DMC))$DMC, (onb.point.quantile.a %>% dplyr::select(DMC))$DMC, paired = TRUE)
# res
# res <- t.test((onb.point.quantile42Y.f %>% dplyr::select(DC))$DC, (onb.point.quantile.a %>% dplyr::select(DC))$DC, paired = TRUE)
# 
# res
# 
# 
# 
# 

# #Hourly Extract ---------------------------------------------------------

# x=unique((vardf[c('year','seq','x','y','ID')]))
# write.table(x,file='015_OverNightBurning\\015.1_DataProcessing\\Longterm Analysis\\onb_era5point_LatLong.csv',sep = ',', row.names=FALSE) # keeps the rownames

# onb_era5point_LatLong = read.csv('015_OverNightBurning\\015.1_DataProcessing\\Longterm Analysis\\onb_era5point_LatLong.csv')
# vpd <- mixedsort(dir(path='D:\\000_collections\\010_Nighttime Burning\\011_Data\\016_LongTermHourly\\hvpd',full.names = T,pattern = '(nc)$'))
# isi <- mixedsort(dir(path='D:\\000_collections\\010_Nighttime Burning\\011_Data\\016_LongTermHourly\\hisi',full.names = T,pattern = '(nc)$'))
# ffmc <- mixedsort(dir(path='D:\\000_collections\\010_Nighttime Burning\\011_Data\\016_LongTermHourly\\hffmc',full.names = T,pattern = '(nc)$'))
# rh <- mixedsort(dir(path='D:\\000_collections\\010_Nighttime Burning\\011_Data\\016_LongTermHourly\\hrh',full.names = T,pattern = '(nc)$'))
# ht <- mixedsort(dir(path='D:\\000_collections\\010_Nighttime Burning\\011_Data\\016_LongTermHourly\\ht',full.names = T,pattern = '(nc)$'))
# 
# vpd.brick = brick(vpd[1])
# vpd.brick = rotate(vpd.brick)
# onb_era5point_LatLong_cp <- onb_era5point_LatLong
# ## csv to spatial df
# coordinates(onb_era5point_LatLong) <- ~ x + y
# ##aline with
# proj4string(onb_era5point_LatLong) <- proj4string(vpd.brick)#NAfire_year
# year = rep(1979:2020, each=12)
# 
# xx <- raster::extract(vpd.brick, onb_era5point_LatLong,weight=T,df=T,cellnumbers=T)
# xxx = cbind(xyFromCell(vpd.brick,xx$cell),xx) %>% dplyr::select(-c(cells))
# 
# 
# f_vpd <- function(i) {
#   library(raster)
#   library(dplyr)
#   var.brick = brick(vpd[i])
#   var.brick = rotate(var.brick)
#   
#   extp2var <- raster::extract(var.brick, onb_era5point_LatLong,weight=T,df=T,cellnumbers=T)
#   extp2var = cbind(xyFromCell(var.brick,extp2var$cell),extp2var) %>% dplyr::select(-c(ID,cells))
#   extp2var = cbind(onb_era5point_LatLong_cp %>% dplyr::select(ID),extp2var)
#   extp2var <- extp2var %>% group_by(ID) %>% summarise_all(mean, na.rm = TRUE)
#   
#   if (i==1) {extp2var = extp2var}else{extp2var = extp2var %>% dplyr::select(-c(x,y,ID))}
# }
# f_isi <- function(i) {
#   library(raster)
#   library(dplyr)
#   var.brick = brick(isi[i])
#   var.brick = rotate(var.brick)
#   
#   extp2var <- raster::extract(var.brick, onb_era5point_LatLong,weight=T,df=T,cellnumbers=T)
#   extp2var = cbind(xyFromCell(var.brick,extp2var$cell),extp2var) %>% dplyr::select(-c(ID,cells))
#   extp2var = cbind(onb_era5point_LatLong_cp %>% dplyr::select(ID),extp2var)
#   extp2var <- extp2var %>% group_by(ID) %>% summarise_all(mean, na.rm = TRUE)
#   
#   if (i==1) {extp2var = extp2var}else{extp2var = extp2var %>% dplyr::select(-c(x,y,ID))}
# }
# f_ffmc <- function(i) {
#   library(raster)
#   library(dplyr)
#   var.brick = brick(ffmc[i])
#   var.brick = rotate(var.brick)
#   
#   extp2var <- raster::extract(var.brick, onb_era5point_LatLong,weight=T,df=T,cellnumbers=T)
#   extp2var = cbind(xyFromCell(var.brick,extp2var$cell),extp2var) %>% dplyr::select(-c(ID,cells))
#   extp2var = cbind(onb_era5point_LatLong_cp %>% dplyr::select(ID),extp2var)
#   extp2var <- extp2var %>% group_by(ID) %>% summarise_all(mean, na.rm = TRUE)
#   
#   if (i==1) {extp2var = extp2var}else{extp2var = extp2var %>% dplyr::select(-c(x,y,ID))}
# }
# f_rh <- function(i) {
#   library(raster)
#   library(dplyr)
#   var.brick = brick(rh[i])
#   var.brick = rotate(var.brick)
#   
#   extp2var <- raster::extract(var.brick, onb_era5point_LatLong,weight=T,df=T,cellnumbers=T)
#   extp2var = cbind(xyFromCell(var.brick,extp2var$cell),extp2var) %>% dplyr::select(-c(ID,cells))
#   extp2var = cbind(onb_era5point_LatLong_cp %>% dplyr::select(ID),extp2var)
#   extp2var <- extp2var %>% group_by(ID) %>% summarise_all(mean, na.rm = TRUE)
#   
#   if (i==1) {extp2var = extp2var}else{extp2var = extp2var %>% dplyr::select(-c(x,y,ID))}
# }
# f_ht <- function(i) {
#   library(raster)
#   library(dplyr)
#   var.brick = brick(ht[i])
#   var.brick = rotate(var.brick)
#   
#   extp2var <- raster::extract(var.brick, onb_era5point_LatLong,weight=T,df=T,cellnumbers=T)
#   extp2var = cbind(xyFromCell(var.brick,extp2var$cell),extp2var) %>% dplyr::select(-c(ID,cells))
#   extp2var = cbind(onb_era5point_LatLong_cp %>% dplyr::select(ID),extp2var)
#   extp2var <- extp2var %>% group_by(ID) %>% summarise_all(mean, na.rm = TRUE)
#   
#   if (i==1) {extp2var = extp2var}else{extp2var = extp2var %>% dplyr::select(-c(x,y,ID))}
# }
# no_cores<-detectCores()-2
# cl <- makeSOCKcluster(no_cores)
# registerDoSNOW(cl)
# pb <- txtProgressBar(min=1, max=504, style=3)#length(NAfire_year)
# progress <- function(n) setTxtProgressBar(pb, n)
# opts <- list(progress=progress)
# 
# start_time <- Sys.time()
# hvar42Y_vpd <- foreach(i=1:504, .options.snow = opts, .combine = 'cbind') %dopar%  f_vpd(i)#, .combine = 'rbind'
# hvar42Y_isi <- foreach(i=1:504, .options.snow = opts, .combine = 'cbind') %dopar%  f_isi(i)#, .combine = 'rbind'
# hvar42Y_ffmc <- foreach(i=1:504, .options.snow = opts, .combine = 'cbind') %dopar%  f_ffmc(i)#, .combine = 'rbind'
# hvar42Y_rh <- foreach(i=1:504, .options.snow = opts, .combine = 'cbind') %dopar%  f_rh(i)#, .combine = 'rbind'
# hvar42Y_ht <- foreach(i=1:504, .options.snow = opts, .combine = 'cbind') %dopar%  f_ht(i)#, .combine = 'rbind'
# 
# stopCluster(cl)
# end_time <- Sys.time()
# end_time - start_time
# 
# readr::write_rds(hvar42Y_vpd, "D:\\000_collections\\010_Nighttime Burning\\011_Data\\015_LongTermFireWeather\\results_hr\\hvar42Y_vpd.Rds")
# readr::write_rds(hvar42Y_isi, "D:\\000_collections\\010_Nighttime Burning\\011_Data\\015_LongTermFireWeather\\results_hr\\hvar42Y_isi.Rds")
# readr::write_rds(hvar42Y_ffmc, "D:\\000_collections\\010_Nighttime Burning\\011_Data\\015_LongTermFireWeather\\results_hr\\hvar42Y_ffmc.Rds")
# readr::write_rds(hvar42Y_rh, "D:\\000_collections\\010_Nighttime Burning\\011_Data\\015_LongTermFireWeather\\results_hr\\hvar42Y_rh.Rds")
# readr::write_rds(hvar42Y_ht, "D:\\000_collections\\010_Nighttime Burning\\011_Data\\015_LongTermFireWeather\\results_hr\\hvar42Y_ht.Rds")


hvar42Y_vpd = readr::read_rds("D:\\000_collections\\010_Nighttime Burning\\011_Data\\015_LongTermFireWeather\\results_hr\\hvar42Y_vpd.Rds")
hvar42Y_isi = readr::read_rds("D:\\000_collections\\010_Nighttime Burning\\011_Data\\015_LongTermFireWeather\\results_hr\\hvar42Y_isi.Rds")
hvar42Y_ffmc = readr::read_rds("D:\\000_collections\\010_Nighttime Burning\\011_Data\\015_LongTermFireWeather\\results_hr\\hvar42Y_ffmc.Rds")
hvar42Y_rh = readr::read_rds("D:\\000_collections\\010_Nighttime Burning\\011_Data\\015_LongTermFireWeather\\results_hr\\hvar42Y_rh.Rds")
hvar42Y_ht = readr::read_rds("D:\\000_collections\\010_Nighttime Burning\\011_Data\\015_LongTermFireWeather\\results_hr\\hvar42Y_ht.Rds")
onb = unique((FD_NLgte4 %>% filter(ONB_Event_YoN=='OBEs'))[c('year','seq','day','ID')])


FireWeatherLongTerm <- function(i){
  library(dplyr)
  library(suncalc)
  library(lutz)
  library(foreach)
  library(doParallel)
  library(parallel)
  library(tcltk)
  library(doSNOW)
  x <- as.data.frame(seq(as.POSIXct("1979-01-01 00:00:00"), as.POSIXct("2020-12-31 23:00:00"), by="hour")) 
  colnames(x) <- 'date'
  x <- x %>% mutate(year=as.integer(format(date,"%Y")),day=as.integer(data.table::yday(date)),hr=as.integer(format(date,"%H")))
  x$long <- hvar42Y_isi$x[i]
  x$lat <- hvar42Y_isi$y[i]
  ID=hvar42Y_vpd$ID[i]
  tz <- tz_offset(as.Date(x$date, format = "%Y-%m-%d"), tz_lookup_coords(as.numeric(x$lat[i]), as.numeric(x$long[i])))#as.numeric(x$lat[1]), as.numeric(x$long[1])
  tzoffset <- tz$utc_offset_h
  tzname <- tz$tz_name
  x <- cbind(x,tzoffset,tzname)
  #utc to local time
  x$hr <- x$hr+tzoffset
  timeind <- which(x$hr<0)
  x$day[timeind] <- x$day[timeind]-1
  x$hr[timeind] <- x$hr[timeind]+24
  timeind <- which(x$day==0)
  x$year[timeind] <- x$year[timeind]-1
  timeind2 <- which((x$year[timeind]%%4)==0)
  timeind3 <- which((x$year[timeind]%%4)>0)
  x$day[timeind[timeind2]] <- 366
  x$day[timeind[timeind3]] <- 365
  x <- x %>% mutate(date=as.character(date+lubridate::hours(tzoffset)))
  tz <- tz_offset(as.Date(x$date, format = "%Y-%m-%d"), tz_lookup_coords(as.numeric(x$lat[i]), as.numeric(x$long[i])))#
  
  ## sunset and sunrise
  sundata <- data.frame(date=as.Date(tz$date_time),lat=as.numeric(x$lat[i]),lon=as.numeric(x$long[i]))
  sunrise <- getSunlightTimes(data=sundata,keep="sunrise",tz=tz$tz_name[1])$sunrise
  sunset <- getSunlightTimes(data=sundata,keep="sunset",tz=tz$tz_name[1])$sunset
  
  ## all are local time, even shown as UTC time
  x$sunrise <- as.character(sunrise) 
  x$sunset <- as.character(sunset)
  x <- x %>% mutate(naturalDay=ifelse((date<sunrise),day-1,day))
  timeind <- which(x$naturalDay==0)
  timeind2 <- which(((x$year[timeind]-1)%%4)==0)
  timeind3 <- which(((x$year[timeind]-1)%%4)>0)
  x$naturalDay[timeind[timeind2]] <- 366
  x$naturalDay[timeind[timeind3]] <- 365
  
  tz1 <- tz_offset(as.Date(as.Date(x$date)-lubridate::hours(24), format = "%Y-%m-%d"), tz_lookup_coords(as.numeric(x$lat[i]), as.numeric(x$long[i])))
  sundata1 <- data.frame(date=as.Date(tz1$date_time),lat=as.numeric(x$lat[i]),lon=as.numeric(x$long[i]))
  x$sunset_lastday <- as.character(getSunlightTimes(data=sundata1,keep="sunset",tz=tz1$tz_name[1])$sunset) 
  x <- x %>% mutate(dorn=ifelse(((date>sunrise&date<sunset)|(date<sunset_lastday)),0,1),
                    year=ifelse(day==1&naturalDay>360,year-1,year))
  
  x1 <- t(hvar42Y_vpd[i,4:ncol(hvar42Y_vpd)])
  colnames(x1) <- 'vpd'
  x <- cbind(x,x1)
  
  x1 <- t(hvar42Y_isi[i,4:ncol(hvar42Y_isi)])
  colnames(x1) <- 'isi'
  x <- cbind(x,x1)
  
  x1 <- t(hvar42Y_ffmc[i,4:ncol(hvar42Y_ffmc)])
  colnames(x1) <- 'ffmc'
  x <- cbind(x,x1)
  
  x1 <- t(hvar42Y_rh[i,4:ncol(hvar42Y_rh)])
  colnames(x1) <- 'rh'
  x <- cbind(x,x1)
  
  x1 <- t(hvar42Y_ht[i,4:ncol(hvar42Y_ht)])
  colnames(x1) <- 'ht'
  x <- cbind(x,x1)
  
  xx123 <- unique(x[c('year','naturalDay')])
  
  
  FW_results = foreach(j=1:nrow(xx123), .combine = 'rbind') %do% {
    library(dplyr)
    sunrise2sunrise <- x %>% filter(year==xx123$year[j],naturalDay==xx123$naturalDay[j])
    if (nrow(sunrise2sunrise)>=20) {
      FD_info <- sunrise2sunrise %>% summarise(year=year[1],day=day[1],naturalDay=naturalDay[1],ID=ID,long=long[1],lat=lat[1])
      FD_daymax <- sunrise2sunrise %>% filter(dorn==0) %>% summarise(vpd_daymax=max(vpd),isi_daymax=max(isi),rh_daymax=max(rh),temp_daymax=max(ht),ffmc_daymax=max(ffmc))
      # ,rh_daymax=max(rh),temp_daymax=max(temp),ffmc_daymax=max(ffmc),
      # winds_daymax=max(winds),emc_daymax=max(emc),hdw_daymax=max(hdw))
      FD_daymin <- sunrise2sunrise %>% filter(dorn==0) %>% summarise(vpd_daymin=min(vpd),isi_daymin=min(isi),rh_daymin=min(rh),temp_daymin=min(ht),ffmc_daymin=min(ffmc))
      # ,rh_daymin=min(rh),temp_daymin=min(temp),ffmc_daymin=min(ffmc),
      # winds_daymin=min(winds),emc_daymin=min(emc),hdw_daymin=min(hdw))
      FD_daymean <- sunrise2sunrise %>% filter(dorn==0) %>% summarise(vpd_daymean=mean(vpd),isi_daymean=mean(isi),rh_daymean=mean(rh),temp_daymean=mean(ht),ffmc_daymean=mean(ffmc))
      # ,rh_daymean=mean(rh),temp_daymean=mean(temp),ffmc_daymean=mean(ffmc),
      # winds_daymean=mean(winds),emc_daymean=mean(emc),hdw_daymean=mean(hdw))
      
      FD_nightmax <- sunrise2sunrise %>% filter(dorn==1) %>% summarise(vpd_nightmax=max(vpd),isi_nightmax=max(isi),rh_nightmax=max(rh),temp_nightmax=max(ht),ffmc_nightmax=max(ffmc))
      # ,rh_nightmax=max(rh),temp_nightmax=max(temp),ffmc_nightmax=max(ffmc),
      # winds_nightmax=max(winds),emc_nightmax=max(emc),hdw_nightmax=max(hdw))
      FD_nightmin <- sunrise2sunrise %>% filter(dorn==1) %>% summarise(vpd_nightmin=min(vpd),isi_nightmin=min(isi),rh_nightmin=min(rh),temp_nightmin=min(ht),ffmc_nightmin=min(ffmc))
      # ,rh_nightmin=min(rh),temp_nightmin=min(temp),ffmc_nightmin=min(ffmc),
      # winds_nightmin=min(winds),emc_nightmin=min(emc),hdw_nightmin=min(hdw)) 
      FD_nightmean <- sunrise2sunrise %>% filter(dorn==1) %>% summarise(vpd_nightmean=mean(vpd),isi_nightmean=mean(isi),rh_nightmean=mean(rh),temp_nightmean=mean(ht),ffmc_nightmean=mean(ffmc))
      # ,rh_nightmean=mean(rh),temp_nightmean=mean(temp),ffmc_nightmean=mean(ffmc),
      # winds_nightmean=mean(winds),emc_nightmean=mean(emc),hdw_nightmean=mean(hdw))
      FD_basic <- cbind(FD_info,FD_daymax,FD_daymin,FD_daymean,
                        FD_nightmax,FD_nightmin,FD_nightmean)
    }
  }
  
  
  # FW <- function(j){
  #   library(dplyr)
  #   sunrise2sunrise <- x %>% filter(year==xx123$year[j],naturalDay==xx123$naturalDay[j])
  #   if (nrow(sunrise2sunrise)>=20) {
  #     FD_info <- sunrise2sunrise %>% summarise(year=year[1],day=day[1],naturalDay=naturalDay[1],ID=ID,long=long[1],lat=lat[1])
  #     FD_daymax <- sunrise2sunrise %>% filter(dorn==0) %>% summarise(vpd_daymax=max(vpd),isi_daymax=max(isi),rh_daymax=max(rh),temp_daymax=max(ht),ffmc_daymax=max(ffmc))
  #     # ,rh_daymax=max(rh),temp_daymax=max(temp),ffmc_daymax=max(ffmc),
  #     # winds_daymax=max(winds),emc_daymax=max(emc),hdw_daymax=max(hdw))
  #     FD_daymin <- sunrise2sunrise %>% filter(dorn==0) %>% summarise(vpd_daymin=min(vpd),isi_daymin=min(isi),rh_daymin=min(rh),temp_daymin=min(ht),ffmc_daymin=min(ffmc))
  #     # ,rh_daymin=min(rh),temp_daymin=min(temp),ffmc_daymin=min(ffmc),
  #     # winds_daymin=min(winds),emc_daymin=min(emc),hdw_daymin=min(hdw))
  #     FD_daymean <- sunrise2sunrise %>% filter(dorn==0) %>% summarise(vpd_daymean=mean(vpd),isi_daymean=mean(isi),rh_daymean=mean(rh),temp_daymean=mean(ht),ffmc_daymean=mean(ffmc))
  #     # ,rh_daymean=mean(rh),temp_daymean=mean(temp),ffmc_daymean=mean(ffmc),
  #     # winds_daymean=mean(winds),emc_daymean=mean(emc),hdw_daymean=mean(hdw))
  #     
  #     FD_nightmax <- sunrise2sunrise %>% filter(dorn==1) %>% summarise(vpd_nightmax=max(vpd),isi_nightmax=max(isi),rh_nightmax=max(rh),temp_nightmax=max(ht),ffmc_nightmax=max(ffmc))
  #     # ,rh_nightmax=max(rh),temp_nightmax=max(temp),ffmc_nightmax=max(ffmc),
  #     # winds_nightmax=max(winds),emc_nightmax=max(emc),hdw_nightmax=max(hdw))
  #     FD_nightmin <- sunrise2sunrise %>% filter(dorn==1) %>% summarise(vpd_nightmin=min(vpd),isi_nightmin=min(isi),rh_nightmin=min(rh),temp_nightmin=min(ht),ffmc_nightmin=min(ffmc))
  #     # ,rh_nightmin=min(rh),temp_nightmin=min(temp),ffmc_nightmin=min(ffmc),
  #     # winds_nightmin=min(winds),emc_nightmin=min(emc),hdw_nightmin=min(hdw)) 
  #     FD_nightmean <- sunrise2sunrise %>% filter(dorn==1) %>% summarise(vpd_nightmean=mean(vpd),isi_nightmean=mean(isi),rh_nightmean=mean(rh),temp_nightmean=mean(ht),ffmc_nightmean=mean(ffmc))
  #     # ,rh_nightmean=mean(rh),temp_nightmean=mean(temp),ffmc_nightmean=mean(ffmc),
  #     # winds_nightmean=mean(winds),emc_nightmean=mean(emc),hdw_nightmean=mean(hdw))
  #     FD_basic <- cbind(FD_info,FD_daymax,FD_daymin,FD_daymean,
  #                       FD_nightmax,FD_nightmin,FD_nightmean)
  #   }
  # }
  # 
  # no_cores<-detectCores()-18
  # cl <- makeSOCKcluster(no_cores)
  # registerDoSNOW(cl)
  # pb <- txtProgressBar(min=1, max=nrow(xx123), style=3)#length(NAfire_year)
  # progress <- function(n) setTxtProgressBar(pb, n)
  # opts <- list(progress=progress)
  # 
  # start_time <- Sys.time()
  # FW_results <- foreach(j=1:nrow(xx123), .options.snow = opts, .combine = 'rbind') %dopar%  FW(j)#, .combine = 'rbind'
  # stopCluster(cl)
  # end_time <- Sys.time()
  # end_time - start_time
  
  
  ID_this = ID
  OBE_this = onb %>% filter(ID==ID_this)
  OBE_quanfirst21 <- foreach(k=1:nrow(OBE_this), .combine = 'rbind') %do% {
    OBE_day = FW_results %>% filter(year==OBE_this$year[k],day==OBE_this$day[k])
    OBE_quantile = rbind(OBE_day,FW_results[1:7670,])
    OBE_quantile = OBE_quantile[,7:ncol(OBE_quantile)]
    OBE_quantile = as.matrix(t(OBE_quantile))
    quanfirst21 = apply(OBE_quantile,1,function(x) ecdf(x[2:ncol(OBE_quantile)])(x[1]))
    quanfirst21 = as.data.frame(t(quanfirst21))
    quanfirst21$ts = '1979-1999'
    
    OBE_quantile = rbind(OBE_day,FW_results[7671:15340,])
    OBE_quantile = OBE_quantile[,7:ncol(OBE_quantile)]
    OBE_quantile = as.matrix(t(OBE_quantile))
    quanlater21 = apply(OBE_quantile,1,function(x) ecdf(x[2:ncol(OBE_quantile)])(x[1]))
    quanlater21 = as.data.frame(t(quanlater21))
    quanlater21$ts = '2000-2020'
    quan_output = rbind(quanfirst21,quanlater21)
    
  }
  OBE_quanfirst21
}

no_cores<-detectCores()-4
cl <- makeSOCKcluster(no_cores)
registerDoSNOW(cl)
pb <- txtProgressBar(min=1, max=nrow(hvar42Y_isi), style=3)#nrow(hvar42Y_isi)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress=progress)

start_time <- Sys.time()
FW_results <- foreach(i=1:nrow(hvar42Y_isi), .options.snow = opts, .combine = 'rbind') %dopar%  FireWeatherLongTerm(i)#, .combine = 'rbind'
stopCluster(cl)
end_time <- Sys.time()
end_time - start_time


#write.csv(FW_results,'D:\\000_collections\\010_Nighttime Burning\\011_Data\\015_LongTermFireWeather\\results_hr\\FW_results.csv',sep = ',', row.names=FALSE)



# hourly paired plots -------------------------------------------------------------------


FW_results = read.csv('D:\\000_collections\\010_Nighttime Burning\\011_Data\\015_LongTermFireWeather\\results_hr\\FW_results.csv')
FW_results.f21 = FW_results %>% filter(ts=='1979-1999')
FW_results.l21 = FW_results %>% filter(ts=='2000-2020')


FW_results.f21$id <- rownames(FW_results.f21)
FW_results.l21$id <- rownames(FW_results.l21)




## daytime high
FW.d.42 = FW_results.f21 %>% dplyr::select('vpd_daymax','isi_daymax','ffmc_daymax','temp_daymax','rh_daymin','id','ts')
FW.d.a = FW_results.l21 %>% dplyr::select('vpd_daymax','isi_daymax','ffmc_daymax','temp_daymax','rh_daymin','id','ts')
colnames(FW.d.42) <- c('VPD_Dmax','ISI_Dmax','FFMC_Dmax','T_Dmax','RH_Dmin','id','ts')
colnames(FW.d.a) <- c('VPD_Dmax','ISI_Dmax','FFMC_Dmax','T_Dmax','RH_Dmin','id','ts')
summary(FW.d.42)
summary(FW.d.a)
Dmeanf21 = colMeans(FW.d.42 %>% dplyr::select(VPD_Dmax:RH_Dmin),na.rm = T)
Dmeanl21 = colMeans(FW.d.a %>% dplyr::select(VPD_Dmax:RH_Dmin),na.rm = T)
Dmeanf21-Dmeanl21

xx2 = rbind(FW.d.42,FW.d.a)
xx2 = xx2 %>% gather(VPD_Dmax:RH_Dmin,key=variable,value=value)
ggplot(data=xx2,aes(x=ts,y=value,fill=variable))+geom_boxplot(size=0.5)+
  geom_line(aes(group=id), position = position_dodge(0.2),alpha = 0.08) +
  geom_point(aes(color=variable,group=id),size=1,shape=21, position = position_dodge(0.2),alpha=0.1) +
  theme(legend.position = "none")+facet_wrap(~variable,nrow = 1)+
  scale_fill_manual(values = viridis::inferno(11)[c(4,6,8,9,10)])+
  scale_color_manual(values = viridis::inferno(11)[c(4,6,8,9,10)])+
  envalysis::theme_publish()+
  ylab('Quantiles')+theme(axis.title.x = element_blank(),legend.position = 'none')



## nighttime low
FW.n.42 = FW_results.f21 %>% dplyr::select('vpd_nightmin','isi_nightmin','ffmc_nightmin','temp_nightmin','rh_nightmin','id','ts')
FW.n.a = FW_results.l21 %>% dplyr::select('vpd_nightmin','isi_nightmin','ffmc_nightmin','temp_nightmin','rh_nightmin','id','ts')
colnames(FW.n.42) <- c('VPD_Nmin','ISI_Nmin','FFMC_Nmin','T_Nmin','RH_Nmax','id','ts')
colnames(FW.n.a) <- c('VPD_Nmin','ISI_Nmin','FFMC_Nmin','T_Nmin','RH_Nmax','id','ts')
summary(FW.n.42)
summary(FW.n.a)
Nmeanf21 = colMeans(FW.n.42 %>% dplyr::select(VPD_Nmin:RH_Nmax),na.rm = T)
Nmeanl21 = colMeans(FW.n.a %>% dplyr::select(VPD_Nmin:RH_Nmax),na.rm = T)
Nmeanf21-Nmeanl21

xx3 = rbind(FW.n.42,FW.n.a)
xx3 = xx3 %>% gather(VPD_Nmin:RH_Nmax,key=variable,value=value)
ggplot(data=xx3,aes(x=ts,y=value,fill=variable))+geom_boxplot(size=0.5)+
  geom_line(aes(group=id), position = position_dodge(0.2),alpha = 0.08) +
  geom_point(aes(color=variable,group=id),size=1,shape=21, position = position_dodge(0.2),alpha=0.1) +
  theme(legend.position = "none")+facet_wrap(~variable,nrow = 1)+
  scale_fill_manual(values = viridis::inferno(11)[c(4,6,8,9,10)])+
  scale_color_manual(values = viridis::inferno(11)[c(4,6,8,9,10)])+
  envalysis::theme_publish()+
  ylab('Quantiles')+theme(axis.title.x = element_blank(),legend.position = 'none')

res <- wilcox.test((FW.d.42 %>% dplyr::select(VPD_Dmax))$VPD_Dmax, (FW.d.a %>% dplyr::select(VPD_Dmax))$VPD_Dmax, paired = TRUE,alternative = 'greater')
res
res <- wilcox.test((FW.n.42 %>% dplyr::select(VPD_Nmin))$VPD_Nmin, (FW.n.a %>% dplyr::select(VPD_Nmin))$VPD_Nmin, paired = TRUE,alternative = 'greater')
res

res <- wilcox.test((FW.d.42 %>% dplyr::select(ISI_Dmax))$ISI_Dmax, (FW.d.a %>% dplyr::select(ISI_Dmax))$ISI_Dmax, paired = TRUE,alternative = 'greater')
res
res <- wilcox.test((FW.n.42 %>% dplyr::select(ISI_Nmin))$ISI_Nmin, (FW.n.a %>% dplyr::select(ISI_Nmin))$ISI_Nmin, paired = TRUE,alternative = 'greater')
res

res <- wilcox.test((FW.d.42 %>% dplyr::select(FFMC_Dmax))$FFMC_Dmax, (FW.d.a %>% dplyr::select(FFMC_Dmax))$FFMC_Dmax, paired = TRUE,alternative = 'greater')
res
res <- wilcox.test((FW.n.42 %>% dplyr::select(FFMC_Nmin))$FFMC_Nmin, (FW.n.a %>% dplyr::select(FFMC_Nmin))$FFMC_Nmin, paired = TRUE,alternative = 'greater')
res

res <- wilcox.test((FW.d.42 %>% dplyr::select(T_Dmax))$T_Dmax, (FW.d.a %>% dplyr::select(T_Dmax))$T_Dmax, paired = TRUE,alternative = 'greater')
res
res <- wilcox.test((FW.n.42 %>% dplyr::select(T_Nmin))$T_Nmin, (FW.n.a %>% dplyr::select(T_Nmin))$T_Nmin, paired = TRUE,alternative = 'greater')
res

res <- wilcox.test((FW.d.42 %>% dplyr::select(RH_Dmin))$RH_Dmin, (FW.d.a %>% dplyr::select(RH_Dmin))$RH_Dmin, paired = TRUE,alternative = 'less')
res
res <- wilcox.test((FW.n.42 %>% dplyr::select(RH_Nmax))$RH_Nmax, (FW.n.a %>% dplyr::select(RH_Nmax))$RH_Nmax, paired = TRUE,alternative = 'greater')
res

a = (FW.d.42 %>% dplyr::select(RH_Dmin))$RH_Dmin-(FW.d.a %>% dplyr::select(RH_Dmin))$RH_Dmin
(shapiro.test(a))$p.value
qqnorm(a); qqline(a)
ggplot()+geom_density(data=as.data.frame(a),aes(x=a))



plotx <- rbind(xx,xx2,xx3) %>% filter(variable%in%c('FWI','BUI','DMC','FFMC_Nmin',"VPD_Nmin"))

plotx$variable <- factor(plotx$variable,levels = c('FWI','BUI','DMC','FFMC_Nmin',"VPD_Nmin"))

p <- ggplot(data=plotx,aes(x=ts,y=value))+
  geom_line(aes(group=id), position = position_dodge(0.45),alpha = 0.08,color='black') +
  geom_point(aes(group=id),size=1,shape=21, position = position_dodge(0.45),alpha=0.2,color='darkred',fill='darkred') +
  geom_boxplot(aes(x=ts,y=value),fill=NA,size=0.4,color='darkgoldenrod1',outlier.shape = NA)+
  stat_summary(fun.y=mean, geom="point", shape=2, size=1.5, color="darkgoldenrod1") +
  theme(legend.position = "none")+facet_wrap(~variable,nrow = 1,scales = 'free_x')+
  # scale_fill_manual(values = viridis::inferno(12)[c(2,3,6,7,8)])+
  # scale_color_manual(values = viridis::inferno(12)[c(2,3,6,7,8)])+
  envalysis::theme_publish(base_size = 7, base_family = "", line_size = 0.25,base_rect_size =0.25)+
  ylab('Percentile')+
  scale_y_continuous(breaks = c(0.25,0.5,0.75,1),labels = c(25,50,75,100))+
  theme(text = element_text(size = 7),
        axis.title.x=element_blank(),
        axis.text.x = element_text(
          angle = 22.5,
          hjust = 1
        ),
        #axis.text.x = element_text(angle = 45,vjust=0.1),
        legend.position = 'none',
        strip.text.x = element_text(size = 7, color = "black", face = "plain"),
        plot.title = element_text(color="black", size=7, face="bold",vjust = - 1))+
  labs(fill='')
p

# x <- paste0('015_OverNightBurning\\015.5_Plots\\fig.6.666','.pdf')
# ggsave(plot=p,x,width = 15,height = 6,units = 'cm')


plotx <- rbind(xx,xx2,xx3) %>% filter(variable!='FWI'&variable!='BUI'&variable!='DMC'&variable!='FFMC_Nmin'&variable!='VPD_Nmin')
plotx$variable <- factor(plotx$variable,levels = c('FWI','BUI','DMC','DC','ISI_Dmax',"ISI_Nmin",'FFMC_Dmax',"FFMC_Nmin" ,
                                                   'VPD_Dmax',"VPD_Nmin",
                                                   'T_Dmax',
                                                   'T_Nmin',
                                                   'RH_Dmin',
                                                   'RH_Nmax'
))
p <- ggplot(data=plotx,aes(x=ts,y=value))+
  geom_line(aes(group=id), position = position_dodge(0.45),alpha = 0.08,color='black') +
  geom_point(aes(group=id),size=1,shape=21, position = position_dodge(0.45),alpha=0.2,color='darkred',fill='darkred') +
  geom_boxplot(aes(x=ts,y=value),fill=NA,size=0.4, color="darkgoldenrod1",outlier.shape = NA)+
  stat_summary(fun.y=mean, geom="point", shape=2, size=1.5, color="darkgoldenrod1") +
  theme(legend.position = "none")+facet_wrap(~variable,nrow = 2,scales = 'free_x')+
  # scale_fill_manual(values = viridis::inferno(12)[c(4,5,6,7,8,9,9,10,10)])+
  # scale_color_manual(values = viridis::inferno(12)[c(4,5,6,7,8,9,9,10,10)])+
  envalysis::theme_publish(base_size = 7, base_family = "", line_size = 0.25,base_rect_size =0.25)+
  ylab('Percentile')+
  scale_y_continuous(breaks = c(0.25,0.5,0.75,1),labels = c(25,50,75,100))+
  theme(text = element_text(size = 7),
        axis.title.x=element_blank(),
        axis.text.x = element_text(
          angle = 22.5,
          hjust = 1
        ),
        legend.position = 'none',
        strip.text.x = element_text(size = 7, color = "black", face = "plain"),
        plot.title = element_text(color="black", size=7, face="bold",vjust = - 1))+
  labs(fill='')

p
# x <- paste0('015_OverNightBurning\\015.5_Plots\\fig.6.888','.pdf')
# ggsave(plot=p,x,width = 15,height = 12,units = 'cm')

