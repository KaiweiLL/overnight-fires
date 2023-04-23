
# Libararies --------------------------------------------------------------


rm(list = ls())
getwd()
setwd("D:\\000_collections\\010_Nighttime Burning\\015_OverNightBurning")
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

# Combo_Daily -------------------------------------------------------------

# CsvCombDaily <- function(i)  {
#   library(dplyr)
#   library(tidyr)
#   library(foreach)
#   library(doParallel)
#   library(parallel)
#   Fire_indi <- read.csv(firepath[i])
#   year <- Fire_indi$year[1]
#   ##
#   Fire_indi <-  Fire_indi %>%
#     mutate(month=as.Date(day, origin = paste(as.character(year-1) ,'-12-31', sep = ""))) %>%
#     mutate(month=as.numeric(strftime(month, "%m"))) %>%
#     mutate(season = case_when((month >= 3 & month <=5) ~ 'spring',
#                               (month >= 6 & month <=8) ~ 'summer',
#                               (month >= 9 & month <=11) ~ 'fall',
#                               (month==12|month==1|month==2) ~ 'winter')) %>%
#     mutate(BUI=bui.mean,DMC=dmc.mean,DC=dc.mean,FWI=fwi.mean) %>% 
#     dplyr::select(year:day,seq,ID,month,season,BUI:FWI)
# 
#   Fire_indi
# 
# }
# ####start
# firepath <- mixedsort(dir(path='D:\\000_collections\\010_Nighttime Burning\\015_OverNightBurning\\015.1_DataProcessing\\SingleFireCSV_daily\\',full.names = T,pattern = '(.csv)$'))
# head(firepath)
# 
# no_cores<-detectCores()-4
# cl <- makeSOCKcluster(no_cores)
# registerDoSNOW(cl)
# pb <- txtProgressBar(min=1, max=length(firepath), style=3)#length(NAfire_year)
# progress <- function(n) setTxtProgressBar(pb, n)
# opts <- list(progress=progress)
# start_time <- Sys.time()
# Fire_all_daily <- foreach(i=1:length(firepath), .combine = 'rbind', .options.snow = opts) %dopar%  CsvCombDaily(i)#, .combine = 'rbind'
# stopCluster(cl)
# end_time <- Sys.time()
# end_time - start_time
# 
# write.table(Fire_all_daily,file='D:\\000_collections\\010_Nighttime Burning\\015_OverNightBurning\\015.1_DataProcessing\\AllFireCSV_Comb_HrDaily\\Fire_all_daily.csv',sep = ',', row.names=FALSE)
# 

# Feature extraction ------------------------------------------------------

Fire_all_daily <- read.csv('.\\015.1_DataProcessing\\AllFireCSV_Comb_HrDaily\\Fire_all_daily.csv')
firepath <- mixedsort(dir(path='015.1_DataProcessing\\SingleFireCSV_hourly\\',full.names = T,pattern = '(.csv)$'))
head(firepath)

Nights_FeatDesi <- function(i){
  library(dplyr)
  library(foreach)
  library(doParallel)
  library(parallel)
  library(lutz)
  library(suncalc)
  Fire_indi <- read.csv(firepath[i])
  year <- Fire_indi$year[1]
  Fire_indi <- Fire_indi %>% 
    mutate(naturalDay=ifelse((date<sunrise),day-1,day)) %>% 
    mutate(month=as.Date(day, origin = paste(as.character(year-1) ,'-12-31', sep = ""))) %>% 
    mutate(month=as.numeric(strftime(month, "%m"))) %>% 
    mutate(season = case_when((month >= 3 & month <=5) ~ 1,
                              (month >= 6 & month <=8) ~ 2,
                              (month >= 9 & month <=11) ~ 3,
                              (month==12|month==1|month==2) ~ 4)) %>%
    mutate(label = ifelse(is.na(frp.total),0,1)) %>% 
    mutate(vpd=vpd.mean,rh=rh.mean,temp=temp.mean-273.15,ffmc=ffmc.mean,
           winds=winds.mean,prec=prec.mean,isi=isi.mean,emc=emc.mean) %>% 
    dplyr::select(year:inci_name,naturalDay:emc)
  
  fire.night.over <- Fire_indi %>% group_by(naturalDay) %>% 
    filter(dorn==1) %>% summarise(frp.night.mean=mean(frp.mean,na.rm=F))
  
  fire.night.over_1 <- fire.night.over[which(!is.na(fire.night.over$frp.night.mean)),]
  fire.night.over.info <- Fire_indi %>% filter(naturalDay %in% unique(fire.night.over_1$naturalDay))
  ONB_Fire_seq <- unique(fire.night.over.info[c('year','naturalDay')])
  
  
  UniSeqDay <- unique(Fire_indi[c('year','seq','day')])
  startburnDOY <- Fire_indi$startburnDOY[1]
  endburnDOY <- Fire_indi$endburnDOY[1]
  if (startburnDOY==endburnDOY) {endburnDOY=endburnDOY+1}
  startburnDOY
  endburnDOY

  if (length(startburnDOY)>0&length(endburnDOY)>0) {

    #####
    if(nrow(inner_join((UniSeqDay[,1:2])[1,],ONB_Fire_seq))>0){ONB_FIRE_YoN = 1}else{ONB_FIRE_YoN = 0}
    
    Fire_indi_Nights_Feature <- function(j){
      sunrise2sunrise <- Fire_indi %>% filter(naturalDay==j)
      FireDayinfo <- sunrise2sunrise %>% filter(dorn==0,frp.total>0) %>% summarise(DayFireLength=n(),FRPDayHrMean=sum(frp.mean)/DayFireLength)
      FireNightinfo <- sunrise2sunrise %>% filter(dorn==1,frp.total>0) %>% summarise(NightFireLength=n(),FRPNightMean=sum(frp.mean)/NightFireLength)
      DayLength <- sunrise2sunrise %>% filter(dorn==0) %>% summarise(DayLength=n())
      NightLength <- sunrise2sunrise %>% filter(dorn==1) %>% summarise(NightLength=n(),FRPNightMeanFire=sum(frp.mean,na.rm = T)/NightLength)
      Nlasthr_label <- sunrise2sunrise %>% filter(row_number()==n())
      Nlast2hr_label <- sunrise2sunrise %>% filter(row_number()==n()-1)
      
      ###high latitude area has polar day, for i=1751,j=193, nights back but nrow equals 4
      ###some fires are located at the boundries between usa and mexico
      if (NightLength[1]>0) {
        #NightLength[1]>0&nrow(sunrise2sunrise)>22&!is.na(sunrise2sunrise$vpd[1])&sunrise2sunrise$lat[1]<=60
        Noon2Sunset <- sunrise2sunrise %>% filter(dorn==0) %>% filter(between(row_number(),which(hr==12),n())) %>% 
          summarise(Noon2SetLength=n(),FRPNoon2SetMean=sum(frp.mean,na.rm = T)/Noon2SetLength)
        FireNoon2Sunset <- sunrise2sunrise %>% filter(dorn==0) %>% filter(between(row_number(),which(hr==12),n())) %>% filter(frp.total>0) %>% 
          summarise(Noon2SetFireLength=n(),FRPNoon2SetMeanFire=sum(frp.mean)/Noon2SetFireLength)
        if (((FireNightinfo$NightFireLength)[1]/(NightLength)[1])==1) {ONB_Event_YoN=1 }else{ONB_Event_YoN =0}
        
        ##hourly
        FD_frp <- sunrise2sunrise %>% summarise(FRPNightMean=unlist(FireNightinfo[2]),FRPNightMeanFire=unlist(NightLength[2]),
                                                Noon2SetLength=unlist(Noon2Sunset[1]),FRPNoon2SetMean=unlist(Noon2Sunset[2]),
                                                Noon2SetFireLength=unlist(FireNoon2Sunset[1]),FRPNoon2SetMeanFire=unlist(FireNoon2Sunset[2]))
        FD_frp <- FD_frp %>% replace(is.na(.), 0)

        FD_AF_Dchar <- sunrise2sunrise %>% filter(dorn==0) %>% summarise(
          AFTempDayMeanFire = mean(aftemp.mean,na.rm=T),
          AFareaDayMeanFire = mean(afarea.mean,na.rm=T)
        )
        FD_AF_Nchar <- sunrise2sunrise %>% filter(dorn==1) %>% summarise(
          AFTempNightMeanFire = mean(aftemp.mean,na.rm=T),
          AFareaNightMeanFire = mean(afarea.mean,na.rm=T)
        )
        ##daily
        FD_daily <-  Fire_all_daily %>% filter(year==Fire_indi$year[1],seq==Fire_indi$seq[1],day==j) %>% 
          summarise(BUI=BUI,DMC=DMC,DC=DC,FWI=FWI)
        ##hourly
        FD_info <- sunrise2sunrise %>% summarise(ONB_FIRE_YoN=ONB_FIRE_YoN,ONB_Event_YoN=ONB_Event_YoN,year=year[1],day=day[1],naturalDay=naturalDay[1],long=long[1],lat=lat[1],country=country[1],seq=seq[1],ID=ID[1],POLY_HA=POLY_HA[1],inci_name=inci_name[1],
                                                 DayLength=unlist(DayLength[1]),DayFireHr=unlist(FireDayinfo[1]),NightLength=unlist(NightLength[1]),NightFireHr=unlist(FireNightinfo[1]),Nlasthr_label=Nlasthr_label$label,Nlast2hr_label=Nlast2hr_label$label,
                                                 month=month[1],season=season[1],biome=biome[1])
        FD_dayprec <- sunrise2sunrise %>% filter(dorn==0) %>% summarise(prec_dayyon = ifelse(sum(prec)>0,1,0),prec_daylasthr=sum(prec>0),prec_daysum=sum(prec))
        FD_nightprec <- sunrise2sunrise %>% filter(dorn==1) %>% summarise(prec_nightyon = ifelse(sum(prec)>0,1,0),prec_nightlasthr=sum(prec>0),prec_nightsum=sum(prec))
        FD_daymax <- sunrise2sunrise %>% filter(dorn==0) %>% summarise(vpd_daymax=max(vpd),rh_daymax=max(rh),temp_daymax=max(temp),ffmc_daymax=max(ffmc),
                                                                       winds_daymax=max(winds),isi_daymax=max(isi),emc_daymax=max(emc))
        FD_daymin <- sunrise2sunrise %>% filter(dorn==0) %>% summarise(vpd_daymin=min(vpd),rh_daymin=min(rh),temp_daymin=min(temp),ffmc_daymin=min(ffmc),
                                                                       winds_daymin=min(winds),isi_daymin=min(isi),emc_daymin=min(emc))
        FD_daymean <- sunrise2sunrise %>% filter(dorn==0) %>% summarise(vpd_daymean=mean(vpd),rh_daymean=mean(rh),temp_daymean=mean(temp),ffmc_daymean=mean(ffmc),
                                                                        winds_daymean=mean(winds),isi_daymean=mean(isi),emc_daymean=mean(emc))
        FD_dayquant <- sunrise2sunrise %>% filter(dorn==0) %>% summarise(vpd_dayquant25=quantile(vpd,0.25),rh_dayquant25=quantile(rh,0.25),temp_dayquant25=quantile(temp,0.25),ffmc_dayquant25=quantile(ffmc,0.25),
                                                                         winds_dayquant25=quantile(winds,0.25),isi_dayquant25=quantile(isi,0.25),emc_dayquant25=quantile(emc,0.25),
                                                                         vpd_dayquant75=quantile(vpd,0.75),rh_dayquant75=quantile(rh,0.75),temp_dayquant75=quantile(temp,0.75),ffmc_dayquant75=quantile(ffmc,0.75),
                                                                         winds_dayquant75=quantile(winds,0.75),isi_dayquant75=quantile(isi,0.75),emc_dayquant75=quantile(emc,0.75))
        
        FD_nightmax <- sunrise2sunrise %>% filter(dorn==1) %>% summarise(vpd_nightmax=max(vpd),rh_nightmax=max(rh),temp_nightmax=max(temp),ffmc_nightmax=max(ffmc),
                                                                         winds_nightmax=max(winds),isi_nightmax=max(isi),emc_nightmax=max(emc))
        FD_nightmin <- sunrise2sunrise %>% filter(dorn==1) %>% summarise(vpd_nightmin=min(vpd),rh_nightmin=min(rh),temp_nightmin=min(temp),ffmc_nightmin=min(ffmc),
                                                                         winds_nightmin=min(winds),isi_nightmin=min(isi),emc_nightmin=min(emc)) 
        FD_nightmean <- sunrise2sunrise %>% filter(dorn==1) %>% summarise(vpd_nightmean=mean(vpd),rh_nightmean=mean(rh),temp_nightmean=mean(temp),ffmc_nightmean=mean(ffmc),
                                                                          winds_nightmean=mean(winds),isi_nightmean=mean(isi),emc_nightmean=mean(emc))
        FD_nightquant <- sunrise2sunrise %>% filter(dorn==1) %>% summarise(vpd_nightquant25=quantile(vpd,0.25),rh_nightquant25=quantile(rh,0.25),temp_nightquant25=quantile(temp,0.25),ffmc_nightquant25=quantile(ffmc,0.25),
                                                                           winds_nightquant25=quantile(winds,0.25),isi_nightquant25=quantile(isi,0.25),emc_nightquant25=quantile(emc,0.25),
                                                                           vpd_nightquant75=quantile(vpd,0.75),rh_nightquant75=quantile(rh,0.75),temp_nightquant75=quantile(temp,0.75),ffmc_nightquant75=quantile(ffmc,0.75),
                                                                           winds_nightquant75=quantile(winds,0.75),isi_nightquant75=quantile(isi,0.75),emc_nightquant75=quantile(emc,0.75))
        
        FD_sunset <- sunrise2sunrise %>% filter(dorn==0) %>% filter(row_number()==n()) %>% summarise(vpd_sunset=vpd, rh_sunset=rh, temp_sunset=temp,ffmc_sunset=ffmc, 
                                                                                                     winds_sunset=winds, isi_sunset=isi, emc_sunset=emc)
        FD_sunrise <- sunrise2sunrise %>% filter(dorn==1) %>% filter(row_number()==n()) %>% summarise(vpd_sunrise=vpd, rh_sunrise=rh, temp_sunrise=temp,ffmc_sunrise=ffmc, 
                                                                                                      winds_sunrise=winds, isi_sunrise=isi, emc_sunrise=emc)
        # FD_1hrAFsunset <- sunrise2sunrise %>% filter(dorn==1) %>% filter(row_number()==1) %>% summarise(vpd_1hrAFsunset=vpd, rh_1hrAFsunset=rh, temp_1hrAFsunset=temp,ffmc_1hrAFsunset=ffmc, 
        #                                                                                              winds_1hrAFsunset=winds, isi_1hrAFsunset=isi, emc_1hrAFsunset=emc)
        # FD_2hrAFsunset <- sunrise2sunrise %>% filter(dorn==1) %>% filter(row_number()==2) %>% summarise(vpd_2hrAFsunset=vpd, rh_2hrAFsunset=rh, temp_2hrAFsunset=temp,ffmc_2hrAFsunset=ffmc, 
        #                                                                                                 winds_2hrAFsunset=winds, isi_2hrAFsunset=isi, emc_2hrAFsunset=emc)
        # FD_3hrAFsunset <- sunrise2sunrise %>% filter(dorn==1) %>% filter(row_number()==3) %>% summarise(vpd_3hrAFsunset=vpd, rh_3hrAFsunset=rh, temp_3hrAFsunset=temp,ffmc_3hrAFsunset=ffmc, 
        #                                                                                                 winds_3hrAFsunset=winds, isi_3hrAFsunset=isi, emc_3hrAFsunset=emc)
        # FD_basic <- cbind(FD_info,FD_dayprec,FD_nightprec,FD_daily,FD_daymax,FD_daymin,FD_daymean,FD_dayquant,
        #                   FD_nightmax,FD_nightmin,FD_nightmean,FD_nightquant,FD_sunset,FD_sunrise,FD_1hrAFsunset,FD_2hrAFsunset,FD_3hrAFsunset)
        FD_basic <- cbind(FD_info,FD_frp,FD_AF_Dchar,FD_AF_Nchar,FD_dayprec,FD_nightprec,FD_daily,FD_daymax,FD_daymin,FD_daymean,FD_dayquant,
                          FD_nightmax,FD_nightmin,FD_nightmean,FD_nightquant,FD_sunset,FD_sunrise)
        ##feature diff
        FD_diff_Dmax_sunset <- FD_daymax-FD_sunset
        FD_diff_sunset_sunrise <- FD_sunset-FD_sunrise
        FD_diff_Nmax_Nmin <- FD_nightmax-FD_nightmin
        FD_diff_Dmax_Nmax <- FD_daymax-FD_nightmax
        FD_diff_Dmax_Nmin <- FD_daymax-FD_nightmin
        
        colnames(FD_diff_Dmax_sunset) <- c(  "vpd_diff_Dmax_sunset","rh_diff_Dmax_sunset","temp_diff_Dmax_sunset", "ffmc_diff_Dmax_sunset", 
                                             "winds_diff_Dmax_sunset", "isi_diff_Dmax_sunset", "emc_diff_Dmax_sunset")
        colnames(FD_diff_sunset_sunrise) <- c("vpd_diff_sunset_sunrise","rh_diff_sunset_sunrise","temp_diff_sunset_sunrise", "ffmc_diff_sunset_sunrise", 
                                              "winds_diff_sunset_sunrise", "isi_diff_sunset_sunrise", "emc_diff_sunset_sunrise")
        colnames(FD_diff_Nmax_Nmin) <- c(  "vpd_diff_Nmax_Nmin","rh_diff_Nmax_Nmin","temp_diff_Nmax_Nmin", "ffmc_diff_Nmax_Nmin", 
                                           "winds_diff_Nmax_Nmin", "isi_diff_Nmax_Nmin", "emc_diff_Nmax_Nmin")
        colnames(FD_diff_Dmax_Nmax) <- c(  "vpd_diff_Dmax_Nmax","rh_diff_Dmax_Nmax","temp_diff_Dmax_Nmax", "ffmc_diff_Dmax_Nmax", 
                                           "winds_diff_Dmax_Nmax", "isi_diff_Dmax_Nmax", "emc_diff_Dmax_Nmax")
        colnames(FD_diff_Dmax_Nmin) <- c(  "vpd_diff_Dmax_Nmin","rh_diff_Dmax_Nmin","temp_diff_Dmax_Nmin", "ffmc_diff_Dmax_Nmin", 
                                           "winds_diff_Dmax_Nmin", "isi_diff_Dmax_Nmin", "emc_diff_Dmax_Nmin")
        rh_diff_Dmin_Nmax = FD_daymin$rh_daymin-FD_nightmax$rh_nightmax
        emc_diff_Dmin_Nmax= FD_daymin$emc_daymin-FD_nightmax$emc_nightmax
        rh_emc_diff_Dmin_Nmax = cbind(rh_diff_Dmin_Nmax,emc_diff_Dmin_Nmax)
        colnames(rh_emc_diff_Dmin_Nmax) = c('rh_diff_Dmin_Nmax','emc_diff_Dmin_Nmax')

        
        FD_diff <- cbind(FD_diff_Dmax_sunset,FD_diff_sunset_sunrise,FD_diff_Nmax_Nmin,FD_diff_Dmax_Nmax,FD_diff_Dmax_Nmin,rh_emc_diff_Dmin_Nmax)
        
        ##sd
        FD_daysd <- sunrise2sunrise %>% filter(dorn==0) %>% summarise(vpd_daysd=sd(vpd),rh_daysd=sd(rh),temp_daysd=sd(temp),ffmc_daysd=sd(ffmc),
                                                                      winds_daysd=sd(winds),isi_daysd=sd(isi),emc_daysd=sd(emc))
        FD_nightsd <- sunrise2sunrise %>% filter(dorn==1) %>% summarise(vpd_nightsd=sd(vpd),rh_nightsd=sd(rh),temp_nightsd=sd(temp),ffmc_nightsd=sd(ffmc),
                                                                        winds_nightsd=sd(winds),isi_nightsd=sd(isi),emc_nightsd=sd(emc))
        
        FD_sd <- cbind(FD_daysd,FD_nightsd)
        
        ##ChangeRate between MAX and MIN, and max and sunrise
        sunsetIndex <-  as.numeric(sunrise2sunrise %>% filter(dorn==0) %>% summarise(which(row_number()==n()))) 
        daymaxIndex <- sunrise2sunrise %>% filter(dorn==0) %>% summarise(vpd_hr_daymax2sunset = which.max(vpd),rh_hr_daymin2sunset = which.min(rh),temp_hr_daymax2sunset = which.max(temp),ffmc_hr_daymax2sunset = which.max(ffmc),
                                                                         winds_hr_daymax2sunset = which.max(winds),isi_hr_daymax2sunset = which.max(isi),emc_hr_daymin2sunset = which.min(emc))
        FD_hr_daymax2sunset<- sunsetIndex-daymaxIndex
        daymaxIndex <- as.numeric(daymaxIndex)
        FD_hr_sunset2nightmin <- sunrise2sunrise %>% filter(dorn==1) %>% summarise(vpd_hr_sunset2nightmin = which.min(vpd),rh_hr_sunset2nightmax = which.max(rh),temp_hr_sunset2nightmin = which.min(temp),ffmc_hr_sunset2nightmin = which.min(ffmc),
                                                                                   winds_hr_sunset2nightmin = which.min(winds),isi_hr_sunset2nightmin = which.min(isi),emc_hr_sunset2nightmax = which.max(emc))
        nightminIndex <- as.numeric(FD_hr_sunset2nightmin+sunsetIndex)
        sunriseIndex <- as.numeric(sunrise2sunrise %>% summarise(which(row_number()==n()))) 
        
        FD_cr_daymax2nightmin <- sunrise2sunrise %>% summarise(vpd_cr_daymax2nightmin=(vpd[daymaxIndex[1]]-vpd[nightminIndex[1]])/(nightminIndex[1]-daymaxIndex[1]),
                                                               rh_cr_daymin2nightmax=(rh[daymaxIndex[2]]-rh[nightminIndex[2]])/(nightminIndex[2]-daymaxIndex[2]),
                                                               temp_cr_daymax2nightmin=(temp[daymaxIndex[3]]-temp[nightminIndex[3]])/(nightminIndex[3]-daymaxIndex[3]),
                                                               ffmc_cr_daymax2nightmin=(ffmc[daymaxIndex[4]]-ffmc[nightminIndex[4]])/(nightminIndex[4]-daymaxIndex[4]),
                                                               winds_cr_daymax2nightmin=(winds[daymaxIndex[5]]-winds[nightminIndex[5]])/(nightminIndex[5]-daymaxIndex[5]),
                                                               isi_cr_daymax2nightmin=(isi[daymaxIndex[6]]-isi[nightminIndex[6]])/(nightminIndex[6]-daymaxIndex[6]),
                                                               emc_cr_daymin2nightmax=(emc[daymaxIndex[7]]-emc[nightminIndex[7]])/(nightminIndex[7]-daymaxIndex[7])
                                                              )
        FD_cr_daymax2sunrise <- sunrise2sunrise %>% summarise(vpd_cr_daymax2sunrise=(vpd[daymaxIndex[1]]-vpd[sunriseIndex])/(sunriseIndex-daymaxIndex[1]),
                                                              rh_cr_daymin2sunrise=(rh[daymaxIndex[2]]-rh[sunriseIndex])/(sunriseIndex-daymaxIndex[2]),
                                                              temp_cr_daymax2sunrise=(temp[daymaxIndex[3]]-temp[sunriseIndex])/(sunriseIndex-daymaxIndex[3]),
                                                              ffmc_cr_daymax2sunrise=(ffmc[daymaxIndex[4]]-ffmc[sunriseIndex])/(sunriseIndex-daymaxIndex[4]),
                                                              winds_cr_daymax2sunrise=(winds[daymaxIndex[5]]-winds[sunriseIndex])/(sunriseIndex-daymaxIndex[5]),
                                                              isi_cr_daymax2sunrise=(isi[daymaxIndex[6]]-isi[sunriseIndex])/(sunriseIndex-daymaxIndex[6]),
                                                              emc_cr_daymin2sunrise=(emc[daymaxIndex[7]]-emc[sunriseIndex])/(sunriseIndex-daymaxIndex[7])
                                                              )
        FD_cr <- cbind(FD_hr_daymax2sunset,FD_hr_sunset2nightmin,FD_cr_daymax2nightmin,FD_cr_daymax2sunrise)
        
        FD_24hr <- cbind(FD_basic,FD_diff,FD_sd,FD_cr)
        
        FD_all <- cbind(FD_24hr)
        ##########################################################################1day 3day 7day before
        # ########################1day
        # S2S1day <- Fire_indi %>% filter(naturalDay>=j-1&naturalDay<=j-1)
        # FireDayinfo <- S2S1day %>% filter(dorn==0,frp.total>0) %>% summarise(DayFireLength=n(),FRPDayHrMean=sum(frp.mean)/DayFireLength)
        # FireNightinfo <- S2S1day %>% filter(dorn==1,frp.total>0) %>% summarise(NightFireLength=n(),FRPNightHrMean=sum(frp.mean)/NightFireLength)
        # DayLength <- S2S1day %>% filter(dorn==0) %>% summarise(DayLength=n())
        # NightLength <- S2S1day %>% filter(dorn==1) %>% summarise(NightLength=n())
        # if (((FireNightinfo$NightFireLength)[1]/(NightLength)[1])<0.5) {ONB_Event_YoN=0 }else{ONB_Event_YoN = ((FireNightinfo$NightFireLength)[1]/(NightLength)[1])}
        # #daily
        # FD_daily <-  Fire_all_daily %>% filter(year==Fire_indi$year[1],seq==Fire_indi$seq[1],day==j-1) %>% 
        #   summarise(bui_1day=bui.median,dmc_1day=dmc.median,dc_1day=dc.median,fwi_1day=fwi.median)
        # #hourly
        # FD_info <- S2S1day %>% summarise(ONB_Event_1day_YoN=ONB_Event_YoN,Day_1day_FireHr=unlist(FireDayinfo[1]),Night_1day_FireHr=unlist(FireNightinfo[1]))
        # FD_dayprec <- S2S1day %>% filter(dorn==0) %>% summarise(prec_1day_dayyon = ifelse(sum(prec)>0,1,0),prec_1day_daylasthr=sum(prec>0),prec_1day_daysum=sum(prec))
        # FD_nightprec <- S2S1day %>% filter(dorn==1) %>% summarise(prec_1day_nightyon = ifelse(sum(prec)>0,1,0),prec_1day_nightlasthr=sum(prec>0),prec_1day_nightsum=sum(prec))
        # FD_daymax <- S2S1day %>% filter(dorn==0) %>% summarise(vpd_1day_daymax=max(vpd),rh_1day_daymax=max(rh),temp_1day_daymax=max(temp),ffmc_1day_daymax=max(ffmc),
        #                                                                winds_1day_daymax=max(winds),isi_1day_daymax=max(isi),emc_1day_daymax=max(emc),hdw_1day_daymax=max(hdw))
        # FD_daymin <- S2S1day %>% filter(dorn==0) %>% summarise(vpd_1day_daymin=min(vpd),rh_1day_daymin=min(rh),temp_1day_daymin=min(temp),ffmc_1day_daymin=min(ffmc),
        #                                                                winds_1day_daymin=min(winds),isi_1day_daymin=min(isi),emc_1day_daymin=min(emc),hdw_1day_daymin=min(hdw))
        # FD_daymean <- S2S1day %>% filter(dorn==0) %>% summarise(vpd_1day_daymean=mean(vpd),rh_1day_daymean=mean(rh),temp_1day_daymean=mean(temp),ffmc_1day_daymean=mean(ffmc),
        #                                                                 winds_1day_daymean=mean(winds),isi_1day_daymean=mean(isi),emc_1day_daymean=mean(emc),hdw_1day_daymean=mean(hdw))
        # 
        # FD_nightmax <- sunrise2sunrise %>% filter(dorn==1) %>% summarise(vpd_1day_nightmax=max(vpd),rh_1day_nightmax=max(rh),temp_1day_nightmax=max(temp),ffmc_1day_nightmax=max(ffmc),
        #                                                                  winds_1day_nightmax=max(winds),isi_1day_nightmax=max(isi),emc_1day_nightmax=max(emc),hdw_1day_nightmax=max(hdw))
        # FD_nightmin <- sunrise2sunrise %>% filter(dorn==1) %>% summarise(vpd_1day_nightmin=min(vpd),rh_1day_nightmin=min(rh),temp_1day_nightmin=min(temp),ffmc_1day_nightmin=min(ffmc),
        #                                                                  winds_1day_nightmin=min(winds),isi_1day_nightmin=min(isi),emc_1day_nightmin=min(emc),hdw_1day_nightmin=min(hdw)) 
        # FD_nightmean <- sunrise2sunrise %>% filter(dorn==1) %>% summarise(vpd_1day_nightmean=mean(vpd),rh_1day_nightmean=mean(rh),temp_1day_nightmean=mean(temp),ffmc_1day_nightmean=mean(ffmc),
        #                                                                   winds_1day_nightmean=mean(winds),isi_1day_nightmean=mean(isi),emc_1day_nightmean=mean(emc),hdw_1day_nightmean=mean(hdw))
        # #diff
        # FD_diff_Dmax_sunset <- FD_daymax-FD_sunset
        # FD_diff_sunset_sunrise <- FD_sunset-FD_sunrise
        # FD_diff_Nmax_Nmin <- FD_nightmax-FD_nightmin
        # FD_diff_Dmax_Nmax <- FD_daymax-FD_nightmax
        # FD_diff_Dmax_Nmin <- FD_daymax-FD_nightmin
        # 
        # colnames(FD_diff_Dmax_sunset) <- c(  "vpd_diff_1day_Dmax_sunset","rh_diff_1day_Dmax_sunset","temp_diff_1day_Dmax_sunset", "ffmc_diff_1day_Dmax_sunset", 
        #                                      "winds_diff_1day_Dmax_sunset", "isi_diff_1day_Dmax_sunset", "emc_diff_1day_Dmax_sunset","hdw_diff_1day_Dmax_sunset")
        # colnames(FD_diff_sunset_sunrise) <- c("vpd_diff_1day_sunset_sunrise","rh_diff_1day_sunset_sunrise","temp_diff_1day_sunset_sunrise", "ffmc_diff_1day_sunset_sunrise", 
        #                                       "winds_diff_1day_sunset_sunrise", "isi_diff_1day_sunset_sunrise", "emc_diff_1day_sunset_sunrise","hdw_diff_1day_sunset_sunrise")
        # colnames(FD_diff_Nmax_Nmin) <- c(  "vpd_diff_1day_Nmax_Nmin","rh_diff_1day_Nmax_Nmin","temp_diff_1day_Nmax_Nmin", "ffmc_diff_1day_Nmax_Nmin", 
        #                                    "winds_diff_1day_Nmax_Nmin", "isi_diff_1day_Nmax_Nmin", "emc_diff_1day_Nmax_Nmin","hdw_diff_1day_Nmax_Nmin")
        # colnames(FD_diff_Dmax_Nmax) <- c(  "vpd_diff_1day_Dmax_Nmax","rh_diff_1day_Dmax_Nmax","temp_diff_1day_Dmax_Nmax", "ffmc_diff_1day_Dmax_Nmax", 
        #                                    "winds_diff_1day_Dmax_Nmax", "isi_diff_1day_Dmax_Nmax", "emc_diff_1day_Dmax_Nmax","hdw_diff_1day_Dmax_Nmax")
        # colnames(FD_diff_Dmax_Nmin) <- c(  "vpd_diff_1day_Dmax_Nmin","rh_diff_1day_Dmax_Nmin","temp_diff_1day_Dmax_Nmin", "ffmc_diff_1day_Dmax_Nmin", 
        #                                    "winds_diff_1day_Dmax_Nmin", "isi_diff_1day_Dmax_Nmin", "emc_diff_1day_Dmax_Nmin","hdw_diff_1day_Dmax_Nmin")
        # 
        # FD_diff <- cbind(FD_diff_Dmax_sunset,FD_diff_sunset_sunrise,FD_diff_Nmax_Nmin,FD_diff_Dmax_Nmax,FD_diff_Dmax_Nmin)
        # 
        # FD_1day <- cbind(FD_info,FD_daily,FD_dayprec,FD_nightprec,FD_daymax,FD_daymin,FD_daymean,FD_nightmax,FD_nightmin,FD_nightmean,FD_diff)
        # 
        # #######################3day
        # S2S3day <- Fire_indi %>% filter(naturalDay>=j-3&naturalDay<=j-1)
        # 
        # #daily
        # FD_daily <-  Fire_all_daily %>% filter(year==Fire_indi$year[1],seq==Fire_indi$seq[1]) %>% filter(day>=j-3&day<=j-1) %>% 
        #   summarise(bui_3day_mean=mean(bui.median),dmc_3day_mean=mean(dmc.median),dc_3day_mean=mean(dc.median),fwi_3day_mean=mean(fwi.median))
        # #hourly
        # 
        # FD_prec <- S2S3day %>% summarise(prec_3day_dayyon = ifelse(sum(prec)>0,1,0),prec_3day_daylasthr=sum(prec>0),prec_3day_daysum=sum(prec))
        # FD_daymax <- S2S3day %>% filter(dorn==0) %>% summarise(vpd_3day_daymax=max(vpd),rh_3day_daymax=max(rh),temp_3day_daymax=max(temp),ffmc_3day_daymax=max(ffmc),
        #                                                        winds_3day_daymax=max(winds),isi_3day_daymax=max(isi),emc_3day_daymax=max(emc),hdw_3day_daymax=max(hdw))
        # FD_daymin <- S2S3day %>% filter(dorn==0) %>% summarise(vpd_3day_daymin=min(vpd),rh_3day_daymin=min(rh),temp_3day_daymin=min(temp),ffmc_3day_daymin=min(ffmc),
        #                                                        winds_3day_daymin=min(winds),isi_3day_daymin=min(isi),emc_3day_daymin=min(emc),hdw_3day_daymin=min(hdw))
        # FD_daymean <- S2S3day %>% filter(dorn==0) %>% summarise(vpd_3day_daymean=mean(vpd),rh_3day_daymean=mean(rh),temp_3day_daymean=mean(temp),ffmc_3day_daymean=mean(ffmc),
        #                                                         winds_3day_daymean=mean(winds),isi_3day_daymean=mean(isi),emc_3day_daymean=mean(emc),hdw_3day_daymean=mean(hdw))
        # 
        # FD_nightmax <- S2S3day %>% filter(dorn==1) %>% summarise(vpd_3day_nightmax=max(vpd),rh_3day_nightmax=max(rh),temp_3day_nightmax=max(temp),ffmc_3day_nightmax=max(ffmc),
        #                                                                  winds_3day_nightmax=max(winds),isi_3day_nightmax=max(isi),emc_3day_nightmax=max(emc),hdw_3day_nightmax=max(hdw))
        # FD_nightmin <- S2S3day %>% filter(dorn==1) %>% summarise(vpd_3day_nightmin=min(vpd),rh_3day_nightmin=min(rh),temp_3day_nightmin=min(temp),ffmc_3day_nightmin=min(ffmc),
        #                                                                  winds_3day_nightmin=min(winds),isi_3day_nightmin=min(isi),emc_3day_nightmin=min(emc),hdw_3day_nightmin=min(hdw)) 
        # FD_nightmean <- S2S3day %>% filter(dorn==1) %>% summarise(vpd_3day_nightmean=mean(vpd),rh_3day_nightmean=mean(rh),temp_3day_nightmean=mean(temp),ffmc_3day_nightmean=mean(ffmc),
        #                                                                   winds_3day_nightmean=mean(winds),isi_3day_nightmean=mean(isi),emc_3day_nightmean=mean(emc),hdw_3day_nightmean=mean(hdw))
        # #diff
        # FD_diff_Nmax_Nmin <- FD_nightmax-FD_nightmin
        # FD_diff_Dmax_Nmax <- FD_daymax-FD_nightmax
        # FD_diff_Dmax_Nmin <- FD_daymax-FD_nightmin
        # 
        # 
        # colnames(FD_diff_Nmax_Nmin) <- c(  "vpd_diff_3day_Nmax_Nmin","rh_diff_3day_Nmax_Nmin","temp_diff_3day_Nmax_Nmin", "ffmc_diff_3day_Nmax_Nmin", 
        #                                    "winds_diff_3day_Nmax_Nmin", "isi_diff_3day_Nmax_Nmin", "emc_diff_3day_Nmax_Nmin","hdw_diff_3day_Nmax_Nmin")
        # colnames(FD_diff_Dmax_Nmax) <- c(  "vpd_diff_3day_Dmax_Nmax","rh_diff_3day_Dmax_Nmax","temp_diff_3day_Dmax_Nmax", "ffmc_diff_3day_Dmax_Nmax", 
        #                                    "winds_diff_3day_Dmax_Nmax", "isi_diff_3day_Dmax_Nmax", "emc_diff_3day_Dmax_Nmax","hdw_diff_3day_Dmax_Nmax")
        # colnames(FD_diff_Dmax_Nmin) <- c(  "vpd_diff_3day_Dmax_Nmin","rh_diff_3day_Dmax_Nmin","temp_diff_3day_Dmax_Nmin", "ffmc_diff_3day_Dmax_Nmin", 
        #                                    "winds_diff_3day_Dmax_Nmin", "isi_diff_3day_Dmax_Nmin", "emc_diff_3day_Dmax_Nmin","hdw_diff_3day_Dmax_Nmin")
        # 
        # FD_diff <- cbind(FD_diff_Nmax_Nmin,FD_diff_Dmax_Nmax,FD_diff_Dmax_Nmin)
        # 
        # FD_3day <- cbind(FD_daily,FD_prec,FD_daymax,FD_daymin,FD_daymean,FD_nightmax,FD_nightmin,FD_nightmean,FD_diff)
        # 
        # #######################7day
        # S2S7day <- Fire_indi %>% filter(naturalDay>=j-7&naturalDay<=j-1)
        # 
        # #daily
        # FD_daily <-  Fire_all_daily %>% filter(year==Fire_indi$year[1],seq==Fire_indi$seq[1]) %>% filter(day>=j-7&day<=j-1) %>%
        #   summarise(bui_7day_mean=mean(bui.median),dmc_7day_mean=mean(dmc.median),dc_7day_mean=mean(dc.median),fwi_7day_mean=mean(fwi.median))
        # #hourly
        # 
        # FD_prec <- S2S7day %>% summarise(prec_7day_dayyon = ifelse(sum(prec)>0,1,0),prec_7day_daylasthr=sum(prec>0),prec_7day_daysum=sum(prec))
        # FD_daymax <- S2S7day %>% filter(dorn==0) %>% summarise(vpd_7day_daymax=max(vpd),rh_7day_daymax=max(rh),temp_7day_daymax=max(temp),ffmc_7day_daymax=max(ffmc),
        #                                                        winds_7day_daymax=max(winds),isi_7day_daymax=max(isi),emc_7day_daymax=max(emc),hdw_7day_daymax=max(hdw))
        # FD_daymin <- S2S7day %>% filter(dorn==0) %>% summarise(vpd_7day_daymin=min(vpd),rh_7day_daymin=min(rh),temp_7day_daymin=min(temp),ffmc_7day_daymin=min(ffmc),
        #                                                        winds_7day_daymin=min(winds),isi_7day_daymin=min(isi),emc_7day_daymin=min(emc),hdw_7day_daymin=min(hdw))
        # FD_daymean <- S2S7day %>% filter(dorn==0) %>% summarise(vpd_7day_daymean=mean(vpd),rh_7day_daymean=mean(rh),temp_7day_daymean=mean(temp),ffmc_7day_daymean=mean(ffmc),
        #                                                         winds_7day_daymean=mean(winds),isi_7day_daymean=mean(isi),emc_7day_daymean=mean(emc),hdw_7day_daymean=mean(hdw))
        # 
        # FD_nightmax <- S2S7day %>% filter(dorn==1) %>% summarise(vpd_7day_nightmax=max(vpd),rh_7day_nightmax=max(rh),temp_7day_nightmax=max(temp),ffmc_7day_nightmax=max(ffmc),
        #                                                          winds_7day_nightmax=max(winds),isi_7day_nightmax=max(isi),emc_7day_nightmax=max(emc),hdw_7day_nightmax=max(hdw))
        # FD_nightmin <- S2S7day %>% filter(dorn==1) %>% summarise(vpd_7day_nightmin=min(vpd),rh_7day_nightmin=min(rh),temp_7day_nightmin=min(temp),ffmc_7day_nightmin=min(ffmc),
        #                                                          winds_7day_nightmin=min(winds),isi_7day_nightmin=min(isi),emc_7day_nightmin=min(emc),hdw_7day_nightmin=min(hdw)) 
        # FD_nightmean <- S2S7day %>% filter(dorn==1) %>% summarise(vpd_7day_nightmean=mean(vpd),rh_7day_nightmean=mean(rh),temp_7day_nightmean=mean(temp),ffmc_7day_nightmean=mean(ffmc),
        #                                                           winds_7day_nightmean=mean(winds),isi_7day_nightmean=mean(isi),emc_7day_nightmean=mean(emc),hdw_7day_nightmean=mean(hdw))
        # #diff
        # FD_diff_Nmax_Nmin <- FD_nightmax-FD_nightmin
        # FD_diff_Dmax_Nmax <- FD_daymax-FD_nightmax
        # FD_diff_Dmax_Nmin <- FD_daymax-FD_nightmin
        # 
        # 
        # colnames(FD_diff_Nmax_Nmin) <- c(  "vpd_diff_7day_Nmax_Nmin","rh_diff_7day_Nmax_Nmin","temp_diff_7day_Nmax_Nmin", "ffmc_diff_7day_Nmax_Nmin", 
        #                                    "winds_diff_7day_Nmax_Nmin", "isi_diff_7day_Nmax_Nmin", "emc_diff_7day_Nmax_Nmin","hdw_diff_7day_Nmax_Nmin")
        # colnames(FD_diff_Dmax_Nmax) <- c(  "vpd_diff_7day_Dmax_Nmax","rh_diff_7day_Dmax_Nmax","temp_diff_7day_Dmax_Nmax", "ffmc_diff_7day_Dmax_Nmax", 
        #                                    "winds_diff_7day_Dmax_Nmax", "isi_diff_7day_Dmax_Nmax", "emc_diff_7day_Dmax_Nmax","hdw_diff_7day_Dmax_Nmax")
        # colnames(FD_diff_Dmax_Nmin) <- c(  "vpd_diff_7day_Dmax_Nmin","rh_diff_7day_Dmax_Nmin","temp_diff_7day_Dmax_Nmin", "ffmc_diff_7day_Dmax_Nmin", 
        #                                    "winds_diff_7day_Dmax_Nmin", "isi_diff_7day_Dmax_Nmin", "emc_diff_7day_Dmax_Nmin","hdw_diff_7day_Dmax_Nmin")
        # 
        # FD_diff <- cbind(FD_diff_Nmax_Nmin,FD_diff_Dmax_Nmax,FD_diff_Dmax_Nmin)
        # 
        # FD_7day <- cbind(FD_daily,FD_prec,FD_daymax,FD_daymin,FD_daymean,FD_nightmax,FD_nightmin,FD_nightmean,FD_diff)
        
        #FD_all <- cbind(FD_24hr,FD_1day,FD_3day,FD_7day)
      }
    }
    Fire_indi_Feature <- foreach(j=startburnDOY:(endburnDOY), .combine = 'rbind') %do%  Fire_indi_Nights_Feature(j)#, .combine = 'rbind'
    Fire_indi_Feature
  }
  
}

no_cores<-detectCores()-4
cl <- makeSOCKcluster(no_cores)
registerDoSNOW(cl)
pb <- txtProgressBar(min=1, max=length(firepath), style=3)#length(firepath)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress=progress)

start_time <- Sys.time()
Nights_FeatDesi_all <- foreach(i=1:length(firepath), .combine = 'rbind', .options.snow = opts) %dopar%  Nights_FeatDesi(i)#, .combine = 'rbind'
stopCluster(cl)
end_time <- Sys.time()
end_time - start_time
##write.table(Nights_FeatDesi_all,file='015.1_DataProcessing\\FeatureDesign\\FD_All_by2FireHr.csv',sep = ',', row.names=FALSE)

Nights_FeatDesi_all <- read.csv('015.1_DataProcessing\\FeatureDesign\\FD_All_by2FireHr.csv')
x = Nights_FeatDesi_all %>%  filter(POLY_HA>=1000)
##write.table(x,file='015.1_DataProcessing\\FeatureDesign\\FD_All_by2FireHr.csv',sep = ',', row.names=FALSE)
##write.table(x,file='015.1_DataProcessing\\FeatureDesign\\FD_All_by2FireHr_gt1000.csv',sep = ',', row.names=FALSE)
