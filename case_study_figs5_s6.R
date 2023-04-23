rm(list = ls())
getwd()
setwd("D:\\000_collections\\010_Nighttime Burning")
# packages ----------------------------------------------------------------
library(ggplot2)
library(ggsci)
library(gtools)
library(dplyr)
library(tidyr)
library(MKinfer)
library(lutz)
library(suncalc)
library(gridExtra)
library(gtable)
library(cowplot)
library(foreach)
library(doParallel)
library(parallel)
library(raster)
library(sp)
library(sf)
library(tcltk)
library(doSNOW)
library(rgdal)
library(gapminder)
library(gganimate)
library(gifski)
library(ggExtra)
library(caret)
library(reshape2)
Sys.setlocale("LC_TIME", "English") 


# Load data ---------------------------------------------------------------


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
# multiple y-axis plot ----------------------------------------------------

#zhihu:https://zhuanlan.zhihu.com/p/434852115
#github:https://github.com/dxsbiocc/learn/blob/main/R/plot/plot_multi_yaxis.R
library(ggplot2)
library(gtable)
library(grid)


hinvert_title_grob <- function(grob){
  # 交换宽度
  widths <- grob$widths
  grob$widths[1] <- widths[3]
  grob$widths[3] <- widths[1]
  grob$vp[[1]]$layout$widths[1] <- widths[3]
  grob$vp[[1]]$layout$widths[3] <- widths[1]
  
  # 修改对齐
  grob$children[[1]]$hjust <- 1 - grob$children[[1]]$hjust 
  grob$children[[1]]$vjust <- 1 - grob$children[[1]]$vjust 
  grob$children[[1]]$x <- unit(1, "npc") - grob$children[[1]]$x
  grob
}

#左侧添加轴
add_yaxis_left <- function(g1, g2) {
  # 添加轴
  pos <- c(subset(g1$layout, name == "ylab-l", select = t:r))
  index <- which(g2$layout$name == "axis-l")
  yaxis <- g2$grobs[[index]]
  g <- gtable_add_cols(g1, unit(1, "mm"), pos$l - 1)
  g <- gtable_add_cols(g, g2$widths[g2$layout[index, ]$l], pos$l - 1)
  g <- gtable_add_grob(g, yaxis, pos$t, pos$l, pos$b, pos$l, clip = "off")
  # 添加轴标签
  # pos <- c(subset(g1$layout, name == "ylab-l", select = t:r))
  index <- which(g2$layout$name == "ylab-l")
  ylab <- g2$grobs[[index]]
  g <- gtable_add_cols(g, g2$widths[g2$layout[index, ]$l], pos$l - 1)
  g <- gtable_add_grob(g, ylab, pos$t, pos$l, pos$b, pos$l, clip = "off")
  g
}
# 右侧添加轴
add_yaxis_right <- function(g1, g2, pos) {
  # ============ 2. 轴标签 ============ #
  index <- which(g2$layout$name == "ylab-l")
  ylab <- g2$grobs[[index]]
  ylab <- hinvert_title_grob(ylab)
  # 添加轴标签
  g <- gtable_add_cols(g1, g2$widths[g2$layout[index, ]$l], pos$r)
  g <- gtable_add_grob(g, ylab, pos$t, pos$r + 1, pos$b, pos$r + 1, clip = "off", name = "ylab-r")
  # ============ 3. 轴设置 ============ #
  index <- which(g2$layout$name == "axis-l")
  yaxis <- g2$grobs[[index]]
  # 将 Y 轴线移动到最左边
  yaxis$children[[1]]$x <- unit.c(unit(0, "npc"), unit(0, "npc"))
  # 交换刻度线和刻度标签
  ticks <- yaxis$children[[2]]
  ticks$widths <- rev(ticks$widths)
  ticks$grobs <- rev(ticks$grobs)
  # 移动刻度线
  ticks$grobs[[1]]$x <- ticks$grobs[[1]]$x - unit(1, "npc") + unit(3, "pt")
  # 刻度标签位置转换和对齐
  ticks$grobs[[2]] <- hinvert_title_grob(ticks$grobs[[2]])
  yaxis$children[[2]] <- ticks
  # 添加轴，unit(3, "mm") 增加轴间距
  g <- gtable_add_cols(g, g2$widths[g2$layout[index, ]$l] + unit(1, "mm"), pos$r)
  g <- gtable_add_grob(g, yaxis, pos$t, pos$r + 1, pos$b, pos$r + 1, clip = "off", name = "axis-r")
  g
}

add_yaxis <- function(g1, g2, offset = 0) {
  # ============ 1. 主绘图区 ============ #
  # 获取主绘图区域
  pos <- c(subset(g1$layout, name == "panel", select = t:r))
  # 添加图形
  g1 <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], 
                        pos$t, pos$l, pos$b * ((offset - 2) * 0.00001 + 1), pos$l)
  if (offset > 3 && offset %% 2 == 0) {
    g1 <- add_yaxis_left(g1, g2)
  } else {
    g1 <- add_yaxis_right(g1, g2, pos)
  }
  g1
}

# 接受可变参数，可添加多个 Y 轴
plot_multi_yaxis <- function(..., right_label_reverse = TRUE) {
  args <- list(...)
  my_theme <- theme(panel.grid = element_blank(), panel.background = element_rect(fill = NA))
  len <- length(args)
  args[[1]] <- args[[1]] + my_theme
  g <- ggplotGrob(args[[1]])
  for (i in len:2) { 
    if (i < 4 || i %% 2 && right_label_reverse) {
      # 为轴标签添加旋转
      args[[i]] <- args[[i]] + 
        theme(axis.title.y = element_text(angle = 270))
    }
    args[[i]] <- args[[i]] + my_theme
    # 获取 gtable 对象
    g2 <- ggplotGrob(args[[i]])
    g <- add_yaxis(g, g2, offset = i)
  }
  # 绘制图形
  grid.newpage()
  grid.draw(g)
}

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

shp.biome <-  st_read('D:\\000_collections\\010_Nighttime Burning\\011_Data\\013_Biome_wwf2017\\fire_biomes_continent_updated2\\fire_biomes_USCA.shp')
shp.biome <- shp.biome %>% filter(gez_name !='Water',gez_name!="Tropical dry forest")
st_crs(shp.biome)

# Select fires ------------------------------------------------------------

ONB_SingleEvent <- FD_NLgte4 %>% filter(ONB_Event_YoN=='OBEs') %>% group_by(year,seq) %>% filter(n()==1)
ONB_multiEvent <- FD_NLgte4 %>% filter(ONB_Event_YoN=='OBEs') %>% group_by(year,seq) %>% filter(n()>1)

ONB_stat <- FD_NLgte4 %>% filter(ONB_Event_YoN=='OBEs') %>% group_by(year,seq,ID,biome,country,lat,long,POLY_HA) %>% mutate(daydiff=max(day)-min(day)) %>% 
  summarise(counts=n(),daydiff=mean(daydiff)) %>% mutate(daydiff.avg=case_when((counts==1) ~ 0,(counts>=1) ~ ((daydiff+1)/counts)))
ONB_stat1 <- ONB_stat %>% group_by(counts) %>% summarise(ncounts=n(),events=sum(counts),daysum=sum(daydiff)+ncounts) %>% mutate(daydiff.avg=daysum/events)

ONBselected <- ONB_stat %>% filter(counts>=1)


caseplot <- function(i) {
  library(sf)
  library(dplyr)
  library(sp)
  library(raster)
  library(suncalc)
  library(lutz)
  library(foreach)
  library(doParallel)
  library(parallel)
  library(tcltk)
  library(doSNOW)
  library(ggplot2)
  library(tidyr)
  library(cowplot)
  library(magick)
  library(viridis)
  yearused=ONBselected[i,]$year
  seqused=ONBselected[i,]$seq
  grepused=paste0(yearused,'_',seqused,'_')
  if (yearused==2017) {    
    NAfireused <- NAfire2017
    GOESused <- GOES17
    GOEScpused <- GOES17_cp}
  if (yearused==2018) {    
    NAfireused <- NAfire2018
    GOESused <- GOES18
    GOEScpused <- GOES18_cp}
  if (yearused==2019) {    
    NAfireused <- NAfire2019
    GOESused <- GOES19
    GOEScpused <- GOES19_cp}
  if (yearused==2020) {    
    NAfireused <- NAfire2020
    GOESused <- GOES20
    GOEScpused <- GOES20_cp}
  Fireindi <- NAfireused[NAfireused$seq ==seqused,]
  Fireindi_sf <- as(Fireindi, "sf")
  GOESovershp <- over(GOESused, Fireindi)
  #remove all rows na
  GOESovershpna <- GOESovershp[complete.cases(GOESovershp$firename),]
  #match the goes fire info
  GOES_rowind = as.numeric(rownames(GOESovershpna))
  GOES_fireinfo = GOEScpused[GOES_rowind,]
  
  # 
  #   xx = st_buffer( Fireindi_sf, 1000)
  #   xx <- as(xx, "Spatial")
  #   xxovershp <- over(GOESused, xx)
  #   #remove all rows na
  #   xxovershpna <- xxovershp[complete.cases(xxovershp$firename),]
  #   #match the xx fire info
  #   xx_rowind = as.numeric(rownames(xxovershpna))
  #   xx_fireinfo = GOEScpused[xx_rowind,]
  #   
  #   xx <- as(xx, "sf")
  
  # 
  # 
  # ggplot()+  
  #   geom_sf(data = Fireindi_sf,color='grey', lwd = 0.5,alpha=1,fill='blue')+
  #   geom_sf(data = xx,color='grey', lwd = 0.5,alpha=0.5,fill='red')
  
  
  
  tz <- tz_offset(as.Date(as.integer(GOES_fireinfo$day), origin = paste(yearused-1,'-12-31', sep = "")), tz_lookup_coords(Fireindi$lat, Fireindi$long, method = "accurate"))
  tzoffset <- tz$utc_offset_h
  GOES_fireinfo <- cbind(GOES_fireinfo,tzoffset)
  #utc to local time
  GOES_fireinfo$hr <- GOES_fireinfo$hr+tzoffset
  timeind <- which(GOES_fireinfo$hr<0)
  GOES_fireinfo$day[timeind] <- GOES_fireinfo$day[timeind]-1
  GOES_fireinfo$hr[timeind] <- GOES_fireinfo$hr[timeind]+24
  GOES_fireinfo$date <- as.character(as.POSIXct(paste(as.Date(as.integer(GOES_fireinfo$day), origin = paste(yearused-1,'-12-31', sep = "")),GOES_fireinfo$hr),tz=tz$tz_name[1],format="%Y-%m-%d %H"))
  
  
  tz <- tz_offset(as.Date(as.integer(GOES_fireinfo$day), 
                          origin = paste(yearused-1,'-12-31', sep = "")), 
                  tz_lookup_coords(GOES_fireinfo$lat[1], GOES_fireinfo$long[1], method = "accurate"))
  sundata <- data.frame(date=as.Date(tz$date_time),lat=GOES_fireinfo$lat,lon=GOES_fireinfo$long)
  sunrise <- getSunlightTimes(data=sundata,keep="sunrise",tz=tz$tz_name[1])$sunrise
  sunset <- getSunlightTimes(data=sundata,keep="sunset",tz=tz$tz_name[1])$sunset
  GOES_fireinfo$sunrise <- as.character(sunrise) 
  GOES_fireinfo$sunset <- as.character(sunset)
  
  
  tz1 <- tz_offset(as.Date(as.integer(GOES_fireinfo$day), origin = paste(yearused-1,'-12-30', sep = ""))
                   ,tz_lookup_coords(GOES_fireinfo$lat[1], GOES_fireinfo$long[1], method = "accurate"))
  sundata1 <- data.frame(date=as.Date(tz1$date_time),lat=GOES_fireinfo$lat,lon=GOES_fireinfo$long)
  GOES_fireinfo$sunset_lastday <- as.character(getSunlightTimes(data=sundata1,keep="sunset",tz=tz1$tz_name[1])$sunset) 
  
  GOES_fireinfo <- GOES_fireinfo %>% mutate(dorn=ifelse(((date>sunrise&date<sunset)|(date<sunset_lastday)),'daytime','nighttime'))
  
  onbday <- FD_NLgte4 %>% filter(year==yearused,seq==seqused,ONB_Event_YoN=='OBEs')
  
  GOES_BA_Temp <- GOES_fireinfo %>% group_by(year,day,hr)%>%
    summarise(AF_MeanArea=mean(area,na.rm=T),AF_MeanTemp=mean(temp,na.rm=T),AF_MeanFRP=mean(frp,na.rm=T),AF_detections =n(),
              date=date[1],sunrise=sunrise[1],sunset=sunset[1],dorn=dorn[1])%>% mutate(naturalDay=ifelse((date<sunrise),day-1,day)) %>% 
    filter((naturalDay>=min(onbday$naturalDay)-3)&(naturalDay<=max(onbday$naturalDay)+3))%>% 
    gather(AF_MeanArea:AF_detections,key=variable,value=value)
  test <- GOES_BA_Temp
  tdat1 <- test %>% filter(dorn=='nighttime')
  tdat1$timelabel <- 'Nighttime'
  tdat2 <- GOES_BA_Temp %>% filter(dorn=='nighttime',naturalDay%in%onbday$naturalDay)
  tdat2$timelabel <- 'OBE'
  tdat3 <- test %>% filter(dorn=='daytime')
  tdat3$timelabel <- 'Daytime'
  GOES_BA_Temp <- rbind(tdat3,tdat1,tdat2)
  GOES_BA_Temp[GOES_BA_Temp == "AF_detections"] <- 'GOES-R active fire detections'
  
  
  library(sf)
  sf_goes_day <- st_as_sf(GOES_fireinfo %>% filter( dorn=='daytime'), coords = c("long", "lat"), 
                          crs = 4326 , agr = "constant")
  sf_goes_night <- st_as_sf(GOES_fireinfo %>% filter(dorn=='nighttime'), coords = c("long", "lat"), 
                            crs = 4326 , agr = "constant")
  
  
  sf_goes_onb <- st_as_sf(GOES_fireinfo %>% mutate(naturalDay=ifelse((date<sunrise),day-1,day)) %>% 
                            filter(dorn=='nighttime',naturalDay%in%onbday$naturalDay), coords = c("long", "lat"), 
                          crs = 4326 , agr = "constant")
  data = GOES_fireinfo %>% mutate(naturalDay=ifelse((date<sunrise),day-1,day)) %>% 
    filter(dorn=='nighttime',naturalDay%in%onbday$naturalDay)
  
  sf_goes_day_jittered <- st_jitter(sf_goes_day, amount = 0.01)
  sf_goes_night_jittered <- st_jitter(sf_goes_night, amount = 0.01)
  sf_goes_onb_jittered <- st_jitter(sf_goes_onb, amount = 0.01)
  
  g1 <- ggplot()+  
    geom_sf(data = Fireindi_sf,color='grey', lwd = 0.5,alpha=1,fill=NA)+
    geom_sf(data = sf_goes_onb_jittered,color='red',size=0.1)+
    geom_sf(data = sf_goes_day_jittered,color='blue',size=0.1)+
    #geom_sf(data = sf_goes_night_jittered,color='green',alpha=0.4,size=0.1)+
    coord_sf(crs = 4326,expand = F)+
    theme_minimal()+ theme(legend.position = "none")+#,axis.text.x = element_text(angle = 90)
    ggspatial::annotation_scale(location = 'br',pad_x=unit(0.01, "cm"), pad_y=unit(0.01, "cm"))
  
  g1
  
  firepointsf <- st_as_sf(Fireindi@data,coords = c('long','lat')) %>% st_set_crs(4326)
  
  g2<-ggplot()+  
    geom_sf(data = shp.biome, fill=NA,lwd = 0.02)+
    geom_sf(data=firepointsf,color='red',shape=8,size=3,stroke=1.5)+
    coord_sf(crs = 3979,expand = F)+
    theme_void()+ theme(legend.position = "none",panel.background = element_rect( colour = "black", 
                                                                                  size = 0.5))
  g2
  
  firepath <- mixedsort(dir(path='015_OverNightBurning\\015.1_DataProcessing\\SingleFireCSV_hourly\\',full.names = T,pattern = '(.csv)$'))
  firepath2 <- mixedsort(dir(path='015_OverNightBurning\\015.1_DataProcessing\\SingleFireCSV_daily\\',full.names = T,pattern = '(.csv)$'))
  
  xxx = grep(grepused,firepath2)
  
  
  bchourly <- read.csv(firepath[xxx])
  bcdaily <- read.csv(firepath2[xxx])
  
  tz1 <- tz_offset(as.Date(as.integer(bchourly$day), origin = paste(yearused-1,'-12-30', sep = ""))
                   ,tz_lookup_coords(bchourly$lat[1], bchourly$long[1], method = "accurate"))
  sundata1 <- data.frame(date=as.Date(tz1$date_time),lat=bchourly$lat,lon=bchourly$long)
  bchourly$sunset_lastday <- as.character(getSunlightTimes(data=sundata1,keep="sunset",tz=tz1$tz_name[1])$sunset) 
  
  bchourly <- bchourly %>% mutate(dorn=ifelse(((date>sunrise&date<sunset)|(date<sunset_lastday)),'daytime','nighttime'))%>% mutate(naturalDay=ifelse((date<sunrise),day-1,day))
  
  
  bchourlyp <- bchourly %>% filter((naturalDay>=min(onbday$naturalDay)-3)&(naturalDay<=max(onbday$naturalDay)+3)) %>% dplyr::select(contains(
    c('year','day','hr','long','lat','seq','date','sunrise','biome','dorn','sunset','frp.total','frp.mean','spotsnum',
      'temp.mean','rh.mean','winds.mean','prec.mean','ffmc.mean','isi.mean','vpd.mean'))) 
  bchourly_cp = bchourlyp
  
  bchourlyp <- bchourlyp %>% mutate(
    T=(temp.mean-273.15),RH=rh.mean,WS=winds.mean,Precip=prec.mean,FFMC=ffmc.mean,ISI=isi.mean,VPD=vpd.mean
    #temp.median-273.15)/2,RH=rh.median/5,WS=winds.median,Precip=prec.median,FFMC=ffmc.median/5,ISI=isi.median,VPD=vpd.median,EMC=emc.median
  ) %>% 
    gather(T:VPD,key=variable,value=value)
  bcdailyp <- bcdaily %>% filter((day>=min(onbday$day)-3)&(day<=max(onbday$day)+3)) %>% mutate( date=as.Date(as.integer(day), origin = paste(year-1,'-12-31', sep = "")),format="%Y-%m-%d %H") %>% 
    dplyr::select(contains(
      c('year','date','day','hr','long','lat','seq','fwi.mean','bui.mean','dmc.mean','dc.mean'))) %>% mutate(
        FWI=fwi.mean,BUI=bui.mean,DMC=dmc.mean,DC=dc.mean
      )%>% 
    gather(FWI:DC,key=variable,value=value)
  
  
  test <- bchourlyp
  tdat1 <- test %>% filter(dorn=='nighttime')
  tdat1$timelabel <- 'Nighttime'
  tdat2 <- bchourlyp %>% filter(dorn=='nighttime',naturalDay%in%onbday$naturalDay)
  tdat2$timelabel <- 'OBE'
  tdat3 <- test %>% filter(dorn=='daytime')
  tdat3$timelabel <- 'Daytime'
  testdat <- rbind(tdat3,tdat1,tdat2)
  
  test$date <- as.POSIXct(as.character(test$date),format="%Y-%m-%d %H")
  bchourly_cp$date <- as.POSIXct(as.character(bchourly_cp$date),format="%Y-%m-%d %H")
  bchourly_cp$sunrise <- as.POSIXct(as.character(bchourly_cp$sunrise),format="%Y-%m-%d %H:%M:%S")
  bchourly_cp$sunset <- as.POSIXct(as.character(bchourly_cp$sunset),format="%Y-%m-%d %H:%M:%S")
  bcdailyp = bcdailyp %>% mutate(date=as.character(date+lubridate::hours(14)))
  bcdailyp$date <- as.POSIXct(as.character(bcdailyp$date),format="%Y-%m-%d %H:%M:%S")
  
  Maxfrp.mean <- max(testdat$frp.mean,na.rm=T)
  MaxVar <- max(bchourlyp$value)
  Maxspotsnum <- max(bchourlyp$spotsnum,na.rm=T)
  Maxdaily <- max(bcdailyp$value)
  coeff <- Maxfrp.mean/MaxVar
  coeff2 <- Maxfrp.mean/Maxspotsnum
  coeff3 <- Maxdaily/MaxVar
  
  
  df_rect_daytime <- bchourly_cp %>% group_by(naturalDay) %>% filter(dorn=='daytime') %>% 
    summarise(start=date[1],end=date[n()])
  df_rect_nighttime <- bchourly_cp %>% group_by(naturalDay) %>% filter(dorn=='nighttime') %>% 
    summarise(start=date[1],end=date[n()])
  
  g3 <-ggplot() +
    geom_rect(data=df_rect_daytime,aes(xmin=start,xmax=end,ymin=- Inf,ymax = Inf), fill = "lightskyblue1", alpha = 0.4)+
    geom_rect(data=df_rect_nighttime,aes(xmin=start,xmax=end,ymin=- Inf,ymax = Inf), fill = "black", alpha = 0.4)+
    geom_line(data=bchourly_cp,aes(x=date,y=spotsnum))+
    geom_point(data=testdat,aes(x=as.POSIXct(date) ,y=frp.mean/coeff2,fill=timelabel),shape=21,size=2, alpha = 0.9,stroke=0.1)+  
    scale_y_continuous(name = "GOES detections",limits = c(0,Maxspotsnum),
                       sec.axis = sec_axis( trans=~.*coeff2, name="Mean FRP(MW)"))+ 
    scale_x_datetime()+scale_fill_manual(name = "FRP", values = c('blue','black','red'))+
    envalysis::theme_publish(base_size = 7, base_family = "", line_size = 0.25,base_rect_size =0.2)+
    theme(axis.title.x=element_blank())
  
  
  
  
  g4 <-ggplot() + 
    geom_rect(data=df_rect_daytime,aes(xmin=start,xmax=end,ymin=- Inf,ymax = Inf), fill = "lightskyblue1", alpha = 0.4)+
    geom_rect(data=df_rect_nighttime,aes(xmin=start,xmax=end,ymin=- Inf,ymax = Inf), fill = "black", alpha = 0.4)+
    geom_line(data=test, aes(x = date, y = value, color = variable),size=0.5) +
    geom_line(data=bcdailyp,aes(x = date, y = value/coeff3, color = variable),size=0.5,linetype=2)+
    scale_y_continuous(name = "Hourly Variable",limits = c(0,MaxVar),
                       sec.axis = sec_axis( trans=~.*coeff3, name="Daily Variable"))+  
    scale_x_datetime()+#date_breaks = "1 day"
    theme_bw()+
    #scale_fill_manual(name = "FRP", values = c('grey','black','red'))+
    #scale_color_nejm()+
    envalysis::theme_publish(base_size = 7, base_family = "", line_size = 0.25,base_rect_size =0.25)+
    theme(axis.title.x=element_blank())
  #theme(axis.text.x = element_text(angle = 45, vjust = 0, hjust=0))
  
  
  testxx = rbind(bcdailyp %>% dplyr::select(date,variable,value),test%>% dplyr::select(date,variable,value))
  testxx_fake =   bcdailyp <- bcdaily %>% filter((day>=min(onbday$day)-3)&(day<=max(onbday$day)+3)) %>% mutate( date=as.Date(as.integer(day), origin = paste(year-1,'-12-31', sep = "")),format="%Y-%m-%d %H") %>% 
    dplyr::select(contains( c('date','fwi.mean'))) %>% mutate(AFWI=fwi.mean)%>% 
    gather(AFWI,key=variable,value=value) %>% dplyr::select(date,variable,value) %>% filter(variable=='AFWI')
  testxx = rbind(testxx,testxx_fake)
  testxx$variable <- factor(testxx$variable,levels = c('AFWI','FWI','BUI','DMC','DC','ISI','FFMC','VPD','T','RH','WS','Precip'))
  
  
  g4.1 <-ggplot() + 
    geom_rect(data=df_rect_daytime,aes(xmin=start,xmax=end,ymin=- Inf,ymax = Inf), fill = 'gray96')+#lightskyblue1
    geom_rect(data=df_rect_nighttime,aes(xmin=start,xmax=end,ymin=- Inf,ymax = Inf), fill = 'gray90')+
    # geom_line(data=test, aes(x = as.POSIXct(date), y = value, color = variable),size=0.2) +
    # geom_line(data=bcdailyp,aes(x = as.POSIXct(date), y = value, color = variable),size=0.2)+
    geom_line(data=testxx,aes(x = as.POSIXct(date), y = value, color = variable),size=0.2)+
    facet_wrap(~variable, scale="free_y",nrow=6)+
    scale_color_manual(values = c(inferno(12)[seq(1,11,1)],'deepskyblue'))+
    scale_x_datetime()+#date_breaks = "1 day"
    theme_bw()+
    #scale_fill_manual(name = "FRP", values = c('grey','black','red'))+
    #scale_color_nejm()+
    theme_classic()+
    theme(axis.title.x=element_blank(),legend.position = 'none',axis.title.y=element_blank(),text = element_text(size = 7),
          strip.background = element_blank())
  #theme(axis.text.x = element_text(angle = 45, vjust = 0, hjust=0))
  g4.1
  
  aa = test %>% filter(day==143|day==144) %>% filter(variable=='Precip')
  sum(aa$prec.mean)
  
  
  # xg4.1 <- paste0('015_OverNightBurning\\015.5_Plots\\fig.cs\\fig.creek.fw','.pdf')
  # ggsave(plot=g4.1,xg4.1,width = 4.5,height = 5.4,units = 'in')
  
  
  
  g3.1 = ggplot() +
    geom_rect(data=df_rect_daytime,aes(xmin=start,xmax=end,ymin=- Inf,ymax = Inf), fill = 'gray96')+#lightskyblue1
    geom_rect(data=df_rect_nighttime,aes(xmin=start,xmax=end,ymin=- Inf,ymax = Inf), fill = 'gray90')+
    geom_point(data=GOES_BA_Temp,aes(x=as.POSIXct(date) ,y=value,color=timelabel),size=0.1, alpha = 0.9)+ #,stroke=0.1
    #geom_line(data=GOES_BA_Temp,aes(x=as.POSIXct(date) ,y=value),size=0.1)+
    facet_wrap(~variable,scales = 'free_y',nrow=2)+#, strip.position = "left"
    scale_x_datetime()+scale_color_manual(name = "Burning Status", values = c('blue','green','red'))+
    theme_classic()+
    theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
          legend.position = 'none',text = element_text(size = 7),strip.background = element_blank())
  
  # strip.background = element_blank(),
  # strip.placement = "outside"
  g3.1
  
  # xg3.1 <- paste0('015_OverNightBurning\\015.5_Plots\\fig.cs\\fig.creek.af','.pdf')
  # ggsave(plot=g3.1,xg3.1,width = 4.6,height = 1.9,units = 'in')
  
  
  
  
  g2<-ggplot()+  
    geom_sf(data = shp.biome, fill=NA,lwd = 0.02)+
    geom_sf(data=firepointsf,color='red',shape=8,size=3,stroke=1)+
    coord_sf(crs = 3979,expand = F)+
    theme_void()+ theme(legend.position = "none",panel.background = element_rect( colour = "black", 
                                                                                  size = 0.5))
  g2
  # xg2 <- paste0('015_OverNightBurning\\015.5_Plots\\fig.cs\\fig.creek.location','.pdf')
  # ggsave(plot=g2,xg2,width = 1,height =1,units = 'in')
  
  
  OBEfilter = GOES_BA_Temp %>% filter(timelabel=='OBE')
  dayfilter = GOES_BA_Temp %>% filter(timelabel=='Daytime')
  night_nonevent_filter =   GOES_BA_Temp %>% filter(timelabel=='Nighttime')
  night_nonevent_date = setdiff(night_nonevent_filter$date,OBEfilter$date)
  night_nonevent_filter = night_nonevent_filter %>% filter(date%in%night_nonevent_date)
  
  
  ggplot()+geom_point(data=OBEfilter,aes(x=as.POSIXct(date),y=variable))+facet_wrap(~variable)
  ggplot()+geom_point(data=night_nonevent_filter,aes(x=as.POSIXct(date),y=variable))+facet_wrap(~variable)
  
  
  sf_obe_interval = sf_goes_onb %>% filter(date%in%(OBEfilter$date)) %>% mutate(timelabel = 'Nighttime AF detections during OBEs')
  sf_day_interval = sf_goes_day %>% filter(date%in%(dayfilter$date)) %>% mutate(timelabel = 'Daytime AF detections')
  sf_night.nonevents_interval = sf_goes_night %>% filter(date%in%(night_nonevent_filter$date)) %>% mutate(timelabel = 'Nighttime AF detections during non-events')
  
  
  df.obe.day = rbind(sf_obe_interval %>% dplyr::select(-naturalDay),sf_day_interval)
  df.obe.day_jittered <- st_jitter(df.obe.day, amount = 0.01)
  df.obe.day_jittered$timelabel <- factor(df.obe.day_jittered$timelabel,
                                          levels = c('Daytime AF detections',
                                                     'Nighttime AF detections during OBEs'))
  sf_night.nonevents_interval_jittered <- st_jitter(sf_night.nonevents_interval, amount = 0.01)
  
  
  
  g1.1 <- ggplot()+  
    geom_sf(data = Fireindi_sf,color='grey', lwd = 0.5,alpha=1,fill=NA)+
    geom_sf(data = df.obe.day_jittered ,aes(color=timelabel) ,alpha=0.1,size=0.01)+
    geom_sf(data = sf_night.nonevents_interval_jittered ,color='green',alpha=0.2,size=0.01)+
    
    # geom_sf(data = sf_goes_onb %>% filter(date%in%(OBEfilter$date)),color='red',alpha=0.1,size=0.001)+
    # geom_sf(data = sf_goes_night %>% filter(date%in%(night_nonevent_filter$date)),color='green',alpha=0.1,size=0.001)+
    coord_sf(crs = 4326,expand = F)+
    scale_color_manual(name = "Burning Status", values = c('blue','red'))+
    theme_minimal()+ theme(legend.position = "none",text = element_text(size = 7))+#legend.position = "none"
    ggspatial::annotation_scale(location = 'br',pad_x=unit(0.01, "cm"), pad_y=unit(0.01, "cm"))
  
  g1.1
  
  
  # xg1.1 <- paste0('015_OverNightBurning\\015.5_Plots\\fig.cs\\fig.creek.goes','.pdf')
  # ggsave(plot=g1.1,xg1.1,width = 3.2,height =4.3,units = 'in')
  # 
  
  
  
  
  
  library(viridis)
  colors <- colorRamps::primary.colors(20)
  
  
  gfwi <-ggplot() + 
    geom_rect(data=df_rect_daytime,aes(xmin=start,xmax=end,ymin=- Inf,ymax = Inf), fill = "gray", alpha = 0.1)+#lightskyblue1
    geom_rect(data=df_rect_nighttime,aes(xmin=start,xmax=end,ymin=- Inf,ymax = Inf), fill = "black", alpha = 0.1)+
    geom_line(data=bcdailyp%>% filter(variable=='FWI'),aes(x = as.POSIXct(date), y = value), color = colors[15],size=0.5)+
    scale_x_datetime()+
    theme_bw()+
    ylab('FWI')+
    envalysis::theme_publish()+
    theme(axis.title.x=element_blank(),
          axis.text.y = element_text(color = colors[15]), 
          axis.ticks.y = element_line(color = colors[15]), 
          axis.title.y = element_text(color = colors[15]), 
          axis.line.y = element_line(color = colors[15]))
  gbui <-ggplot() + 
    # geom_rect(data=df_rect_daytime,aes(xmin=start,xmax=end,ymin=- Inf,ymax = Inf), fill = "lightskyblue1", alpha = 0.4)+
    # geom_rect(data=df_rect_nighttime,aes(xmin=start,xmax=end,ymin=- Inf,ymax = Inf), fill = "black", alpha = 0.4)+
    geom_line(data=bcdailyp%>% filter(variable=='BUI'),aes(x = as.POSIXct(date), y = value), color = colors[2],size=0.5)+
    scale_x_datetime()+
    theme_bw()+
    ylab('BUI')+
    envalysis::theme_publish()+
    theme(axis.title.x=element_blank(),
          axis.text.y = element_text(color = colors[2]), 
          axis.ticks.y = element_line(color = colors[2]), 
          axis.title.y = element_text(color = colors[2]), 
          axis.line.y = element_line(color = colors[2]))
  gdmc <-ggplot() + 
    # geom_rect(data=df_rect_daytime,aes(xmin=start,xmax=end,ymin=- Inf,ymax = Inf), fill = "lightskyblue1", alpha = 0.4)+
    # geom_rect(data=df_rect_nighttime,aes(xmin=start,xmax=end,ymin=- Inf,ymax = Inf), fill = "black", alpha = 0.4)+
    geom_line(data=bcdailyp%>% filter(variable=='DMC'),aes(x = as.POSIXct(date), y = value), color = colors[3],size=0.5)+
    scale_x_datetime()+
    theme_bw()+
    ylab('DMC')+
    envalysis::theme_publish()+
    theme(axis.title.x=element_blank(),
          axis.text.y = element_text(color = colors[3]), 
          axis.ticks.y = element_line(color = colors[3]), 
          axis.title.y = element_text(color = colors[3]), 
          axis.line.y = element_line(color = colors[3]))
  gdc <-ggplot() + 
    # geom_rect(data=df_rect_daytime,aes(xmin=start,xmax=end,ymin=- Inf,ymax = Inf), fill = "lightskyblue1", alpha = 0.4)+
    # geom_rect(data=df_rect_nighttime,aes(xmin=start,xmax=end,ymin=- Inf,ymax = Inf), fill = "black", alpha = 0.4)+
    geom_line(data=bcdailyp%>% filter(variable=='DC'),aes(x = as.POSIXct(date), y = value), color = colors[4],size=0.5)+
    scale_x_datetime()+
    theme_bw()+
    ylab('DMC')+
    envalysis::theme_publish()+
    theme(axis.title.x=element_blank(),
          axis.text.y = element_text(color = colors[4]), 
          axis.ticks.y = element_line(color = colors[4]), 
          axis.title.y = element_text(color = colors[4]), 
          axis.line.y = element_line(color = colors[4]))
  
  
  gffmc <-ggplot() + 
    # geom_rect(data=df_rect_daytime,aes(xmin=start,xmax=end,ymin=- Inf,ymax = Inf), fill = "gray", alpha = 0.1)+#lightskyblue1
    # geom_rect(data=df_rect_nighttime,aes(xmin=start,xmax=end,ymin=- Inf,ymax = Inf), fill = "black", alpha = 0.1)+
    geom_line(data=test%>% filter(variable=='FFMC'),aes(x = as.POSIXct(date), y = value), color = colors[5],size=0.5)+
    scale_x_datetime()+
    ylab('FFMC')+
    envalysis::theme_publish()+
    theme(axis.title.x=element_blank(),
          axis.text.y = element_text(color = colors[5]), 
          axis.ticks.y = element_line(color = colors[5]), 
          axis.title.y = element_text(color = colors[5]), 
          axis.line.y = element_line(color = colors[5]))
  gisi <-ggplot() + 
    geom_rect(data=df_rect_daytime,aes(xmin=start,xmax=end,ymin=- Inf,ymax = Inf), fill = "gray", alpha = 0.1)+#lightskyblue1
    geom_rect(data=df_rect_nighttime,aes(xmin=start,xmax=end,ymin=- Inf,ymax = Inf), fill = "black", alpha = 0.1)+
    geom_line(data=test%>% filter(variable=='ISI'),aes(x = as.POSIXct(date), y = value), color = colors[16],size=0.5)+
    scale_x_datetime()+
    theme_bw()+
    ylab('ISI')+
    envalysis::theme_publish()+
    theme(axis.title.x=element_blank(),
          axis.text.y = element_text(color = colors[16]), 
          axis.ticks.y = element_line(color = colors[16]), 
          axis.title.y = element_text(color = colors[16]), 
          axis.line.y = element_line(color = colors[16]))
  gvpd <-ggplot() + 
    geom_rect(data=df_rect_daytime,aes(xmin=start,xmax=end,ymin=- Inf,ymax = Inf), fill = "gray", alpha = 0.1)+#lightskyblue1
    geom_rect(data=df_rect_nighttime,aes(xmin=start,xmax=end,ymin=- Inf,ymax = Inf), fill = "black", alpha = 0.1)+
    geom_line(data=test%>% filter(variable=='VPD'),aes(x = as.POSIXct(date), y = value), color = colors[7],size=0.5)+
    scale_x_datetime()+
    theme_bw()+
    ylab('VPD')+
    envalysis::theme_publish()+
    theme(axis.title.x=element_blank(),
          axis.text.y = element_text(color = colors[7]), 
          axis.ticks.y = element_line(color = colors[7]), 
          axis.title.y = element_text(color = colors[7]), 
          axis.line.y = element_line(color = colors[7]))
  gt <-ggplot() + 
    # geom_rect(data=df_rect_daytime,aes(xmin=start,xmax=end,ymin=- Inf,ymax = Inf), fill = "lightskyblue1", alpha = 0.4)+
    # geom_rect(data=df_rect_nighttime,aes(xmin=start,xmax=end,ymin=- Inf,ymax = Inf), fill = "black", alpha = 0.4)+
    geom_line(data=test%>% filter(variable=='T'),aes(x = as.POSIXct(date), y = value), color = colors[8],size=0.5)+
    scale_x_datetime()+
    theme_bw()+
    ylab('T')+
    envalysis::theme_publish()+
    theme(axis.title.x=element_blank(),
          axis.text.y = element_text(color = colors[8]), 
          axis.ticks.y = element_line(color = colors[8]), 
          axis.title.y = element_text(color = colors[8]), 
          axis.line.y = element_line(color = colors[8]))
  grh <-ggplot() + 
    # geom_rect(data=df_rect_daytime,aes(xmin=start,xmax=end,ymin=- Inf,ymax = Inf), fill = "lightskyblue1", alpha = 0.4)+
    # geom_rect(data=df_rect_nighttime,aes(xmin=start,xmax=end,ymin=- Inf,ymax = Inf), fill = "black", alpha = 0.4)+
    geom_line(data=test%>% filter(variable=='RH'),aes(x = as.POSIXct(date), y = value), color = colors[9],size=0.5)+
    scale_x_datetime()+
    theme_bw()+
    ylab('RH')+
    envalysis::theme_publish()+
    theme(axis.title.x=element_blank(),
          axis.text.y = element_text(color = colors[9]), 
          axis.ticks.y = element_line(color = colors[9]), 
          axis.title.y = element_text(color = colors[9]), 
          axis.line.y = element_line(color = colors[9]))
  gws <-ggplot() + 
    # geom_rect(data=df_rect_daytime,aes(xmin=start,xmax=end,ymin=- Inf,ymax = Inf), fill = "lightskyblue1", alpha = 0.4)+
    # geom_rect(data=df_rect_nighttime,aes(xmin=start,xmax=end,ymin=- Inf,ymax = Inf), fill = "black", alpha = 0.4)+
    geom_line(data=test%>% filter(variable=='WS'),aes(x = as.POSIXct(date), y = value), color = colors[10],size=0.5)+
    scale_x_datetime()+
    theme_bw()+
    ylab('WS')+
    envalysis::theme_publish()+
    theme(axis.title.x=element_blank(),
          axis.text.y = element_text(color = colors[10]), 
          axis.ticks.y = element_line(color = colors[10]), 
          axis.title.y = element_text(color = colors[10]), 
          axis.line.y = element_line(color = colors[10]))
  gprecip <-ggplot() + 
    # geom_rect(data=df_rect_daytime,aes(xmin=start,xmax=end,ymin=- Inf,ymax = Inf), fill = "lightskyblue1", alpha = 0.4)+
    # geom_rect(data=df_rect_nighttime,aes(xmin=start,xmax=end,ymin=- Inf,ymax = Inf), fill = "black", alpha = 0.4)+
    geom_line(data=test%>% filter(variable=='Precip'),aes(x = as.POSIXct(date), y = value), color = colors[10],size=0.5)+
    scale_x_datetime()+
    theme_bw()+
    ylab('Precip')+
    envalysis::theme_publish()+
    theme(axis.title.x=element_blank(),
          axis.text.y = element_text(color = colors[10]), 
          axis.ticks.y = element_line(color = colors[10]), 
          axis.title.y = element_text(color = colors[10]), 
          axis.line.y = element_line(color = colors[10]))
  
  pm1 = plot_multi_yaxis(gfwi, gbui,gdmc,gdc,gffmc,gprecip)
  pm2 = plot_multi_yaxis(gvpd,gt,grh)
  pm3 = plot_multi_yaxis(gisi,gws)
  plot_grid(g3.1, pm1,pm2,pm3,  ncol = 1,align = "hv")
  
  
  
  
  g5 = plot_grid(g3.1,g3.1,  ncol = 2, align = "hv",rel_widths = c(1,1))
  g5.1 = plot_grid(g3.1, g4.1,  ncol = 1,align = "hv",rel_heights  = c(3,8))
  
  g5.1
  
  biome <- bchourly_cp$biome[1]
  if (biome==41) {biomename <- 'Boreal coniferous forest' }  else if (biome==43) 
  {biomename <- 'Boreal mountain system'}  else if (biome==42)
  {biomename <- 'Boreal tundra woodland'}  else if (biome==50) 
  {biomename <- 'Polar'}  else if (biome==24) 
  {biomename <- 'Subtropical desert'}  else if (biome==22) 
  {biomename <- 'Subtropical dry forest'}  else if (biome==21) 
  {biomename <- 'Subtropical humid forest'}  else if (biome==25)
  {biomename <- 'Subtropical mountain system'}  else if (biome==23) 
  {biomename <- 'Subtropical steppe'}  else if (biome==32) 
  {biomename <- 'Temperate continental forest'}  else if (biome==34) 
  {biomename <- 'Temperate desert'}  else if (biome==35)
  {biomename <- 'Temperate mountain system'}  else if (biome==31) 
  {biomename <- 'Temperate oceanic forest'}  else if (biome==33) 
  {biomename <- 'Temperate steppe'}  else if (biome==13) 
  {biomename <- 'Tropical dry forest'}  else if (biome==12) 
  {biomename <- 'Tropical moist forest'}  else if (biome==16) 
  {biomename <- 'Tropical mountain system'}  else if (biome==11) 
  {biomename <- 'Tropical rainforest'}  else if (biome==41) 
  {biomename <- 'Tropical rainforest'}  else if (biome==14) 
  {biomename <- 'Tropical shrubland'}
  
  ONBnum = nrow(onbday)
  g <-plot_grid(g1, g5.1,  ncol = 2, rel_widths = c(1, 1)) +
    draw_plot(g2, .40, .70, .15, .15)
  title <- ggdraw() + 
    draw_label(
      paste0(i,"_",biomename,'(N=',ONBnum,')',Fireindi@data$firename),
      fontface = 'bold',
      x = 0,
      hjust = 0
    ) +
    theme(
      # add margin on the left of the drawing canvas,
      # so title is aligned with left edge of first plot
      plot.margin = margin(0, 0, 0, 7)
    )
  p<-plot_grid(title,g5.1,rel_heights = c(0.05, 1),ncol = 1)
  p
  x <- paste0('015_OverNightBurning\\015.1_DataProcessing\\CaseStudy\\',i,"_",biomename,'(N=',ONBnum,')',Fireindi@data$firename,'.pdf')
  
  ggsave(plot=p,x,width = 25,height =28,units = 'cm')
  
  # ggsave(plot=g,x,width = 30,height =15,units = 'cm')
  
}

no_cores<-detectCores()-1
cl <- makeSOCKcluster(no_cores)
registerDoSNOW(cl)
pb <- txtProgressBar(min=1, max=nrow(ONBselected), style=3)#length(NAfire_year)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress=progress)

start_time <- Sys.time()
foreach(i=1:nrow(ONBselected), .options.snow = opts) %dopar%  caseplot(i)#, .combine = 'rbind'
stopCluster(cl)
end_time <- Sys.time()
end_time - start_time



# Creek Fire --------------------------------------------------------------


ONBselected <- ONB_stat %>% filter(counts>=10)
i=12
library(sf)
library(dplyr)
library(sp)
library(raster)
library(suncalc)
library(lutz)
library(foreach)
library(doParallel)
library(parallel)
library(tcltk)
library(doSNOW)
library(ggplot2)
library(tidyr)
library(cowplot)
library(magick)
library(viridis)
yearused=ONBselected[i,]$year
seqused=ONBselected[i,]$seq
grepused=paste0(yearused,'_',seqused,'_')
if (yearused==2017) {    
  NAfireused <- NAfire2017
  GOESused <- GOES17
  GOEScpused <- GOES17_cp}
if (yearused==2018) {    
  NAfireused <- NAfire2018
  GOESused <- GOES18
  GOEScpused <- GOES18_cp}
if (yearused==2019) {    
  NAfireused <- NAfire2019
  GOESused <- GOES19
  GOEScpused <- GOES19_cp}
if (yearused==2020) {    
  NAfireused <- NAfire2020
  GOESused <- GOES20
  GOEScpused <- GOES20_cp}
Fireindi <- NAfireused[NAfireused$seq ==seqused,]
Fireindi_sf <- as(Fireindi, "sf")
GOESovershp <- over(GOESused, Fireindi)
#remove all rows na
GOESovershpna <- GOESovershp[complete.cases(GOESovershp$firename),]
#match the goes fire info
GOES_rowind = as.numeric(rownames(GOESovershpna))
GOES_fireinfo = GOEScpused[GOES_rowind,]



tz <- tz_offset(as.Date(as.integer(GOES_fireinfo$day), origin = paste(yearused-1,'-12-31', sep = "")), tz_lookup_coords(Fireindi$lat, Fireindi$long, method = "accurate"))
tzoffset <- tz$utc_offset_h
GOES_fireinfo <- cbind(GOES_fireinfo,tzoffset)
#utc to local time
GOES_fireinfo$hr <- GOES_fireinfo$hr+tzoffset
timeind <- which(GOES_fireinfo$hr<0)
GOES_fireinfo$day[timeind] <- GOES_fireinfo$day[timeind]-1
GOES_fireinfo$hr[timeind] <- GOES_fireinfo$hr[timeind]+24
GOES_fireinfo$date <- as.character(as.POSIXct(paste(as.Date(as.integer(GOES_fireinfo$day), origin = paste(yearused-1,'-12-31', sep = "")),GOES_fireinfo$hr),tz=tz$tz_name[1],format="%Y-%m-%d %H"))


tz <- tz_offset(as.Date(as.integer(GOES_fireinfo$day), 
                        origin = paste(yearused-1,'-12-31', sep = "")), 
                tz_lookup_coords(GOES_fireinfo$lat[1], GOES_fireinfo$long[1], method = "accurate"))
sundata <- data.frame(date=as.Date(tz$date_time),lat=GOES_fireinfo$lat,lon=GOES_fireinfo$long)
sunrise <- getSunlightTimes(data=sundata,keep="sunrise",tz=tz$tz_name[1])$sunrise
sunset <- getSunlightTimes(data=sundata,keep="sunset",tz=tz$tz_name[1])$sunset
GOES_fireinfo$sunrise <- as.character(sunrise) 
GOES_fireinfo$sunset <- as.character(sunset)


tz1 <- tz_offset(as.Date(as.integer(GOES_fireinfo$day), origin = paste(yearused-1,'-12-30', sep = ""))
                 ,tz_lookup_coords(GOES_fireinfo$lat[1], GOES_fireinfo$long[1], method = "accurate"))
sundata1 <- data.frame(date=as.Date(tz1$date_time),lat=GOES_fireinfo$lat,lon=GOES_fireinfo$long)
GOES_fireinfo$sunset_lastday <- as.character(getSunlightTimes(data=sundata1,keep="sunset",tz=tz1$tz_name[1])$sunset) 

GOES_fireinfo <- GOES_fireinfo %>% mutate(dorn=ifelse(((date>sunrise&date<sunset)|(date<sunset_lastday)),'daytime','nighttime'))

onbday <- FD_NLgte4 %>% filter(year==yearused,seq==seqused,ONB_Event_YoN=='OBEs')

GOES_BA_Temp <- GOES_fireinfo %>% group_by(year,day,hr)%>%
  summarise(AF_MeanArea=mean(area,na.rm=T),AF_MeanTemp=mean(temp,na.rm=T),AF_MeanFRP=mean(frp,na.rm=T),AF_detections =n(),
            date=date[1],sunrise=sunrise[1],sunset=sunset[1],dorn=dorn[1])%>% mutate(naturalDay=ifelse((date<sunrise),day-1,day)) %>% 
  filter((naturalDay>=min(onbday$naturalDay)-3)&(naturalDay<=max(onbday$naturalDay)+3))%>% 
  gather(AF_MeanArea:AF_detections,key=variable,value=value)
test <- GOES_BA_Temp
tdat1 <- test %>% filter(dorn=='nighttime')
tdat1$timelabel <- 'Nighttime'
tdat2 <- GOES_BA_Temp %>% filter(dorn=='nighttime',naturalDay%in%onbday$naturalDay)
tdat2$timelabel <- 'OBE'
tdat3 <- test %>% filter(dorn=='daytime')
tdat3$timelabel <- 'Daytime'
GOES_BA_Temp <- rbind(tdat3,tdat1,tdat2)
GOES_BA_Temp[GOES_BA_Temp == "AF_detections"] <- 'GOES-R active fire detections'


library(sf)
sf_goes_day <- st_as_sf(GOES_fireinfo %>% filter( dorn=='daytime'), coords = c("long", "lat"), 
                        crs = 4326 , agr = "constant")
sf_goes_night <- st_as_sf(GOES_fireinfo %>% filter(dorn=='nighttime'), coords = c("long", "lat"), 
                          crs = 4326 , agr = "constant")


sf_goes_onb <- st_as_sf(GOES_fireinfo %>% mutate(naturalDay=ifelse((date<sunrise),day-1,day)) %>% 
                          filter(dorn=='nighttime',naturalDay%in%onbday$naturalDay), coords = c("long", "lat"), 
                        crs = 4326 , agr = "constant")
data = GOES_fireinfo %>% mutate(naturalDay=ifelse((date<sunrise),day-1,day)) %>% 
  filter(dorn=='nighttime',naturalDay%in%onbday$naturalDay)

sf_goes_day_jittered <- st_jitter(sf_goes_day, amount = 0.01)
sf_goes_night_jittered <- st_jitter(sf_goes_night, amount = 0.01)
sf_goes_onb_jittered <- st_jitter(sf_goes_onb, amount = 0.01)

firepointsf <- st_as_sf(Fireindi@data,coords = c('long','lat')) %>% st_set_crs(4326)


firepath <- mixedsort(dir(path='015_OverNightBurning\\015.1_DataProcessing\\SingleFireCSV_hourly\\',full.names = T,pattern = '(.csv)$'))
firepath2 <- mixedsort(dir(path='015_OverNightBurning\\015.1_DataProcessing\\SingleFireCSV_daily\\',full.names = T,pattern = '(.csv)$'))

xxx = grep(grepused,firepath2)


bchourly <- read.csv(firepath[xxx])
bcdaily <- read.csv(firepath2[xxx])

tz1 <- tz_offset(as.Date(as.integer(bchourly$day), origin = paste(yearused-1,'-12-30', sep = ""))
                 ,tz_lookup_coords(bchourly$lat[1], bchourly$long[1], method = "accurate"))
sundata1 <- data.frame(date=as.Date(tz1$date_time),lat=bchourly$lat,lon=bchourly$long)
bchourly$sunset_lastday <- as.character(getSunlightTimes(data=sundata1,keep="sunset",tz=tz1$tz_name[1])$sunset) 

bchourly <- bchourly %>% mutate(dorn=ifelse(((date>sunrise&date<sunset)|(date<sunset_lastday)),'daytime','nighttime'))%>% mutate(naturalDay=ifelse((date<sunrise),day-1,day))


bchourlyp <- bchourly %>% filter((naturalDay>=min(onbday$naturalDay)-3)&(naturalDay<=max(onbday$naturalDay)+3)) %>% dplyr::select(contains(
  c('year','day','hr','long','lat','seq','date','sunrise','biome','dorn','sunset','frp.total','frp.mean','spotsnum',
    'temp.mean','rh.mean','winds.mean','prec.mean','ffmc.mean','isi.mean','vpd.mean'))) 
bchourly_cp = bchourlyp

bchourlyp <- bchourlyp %>% mutate(
  T=(temp.mean-273.15),RH=rh.mean,WS=winds.mean,Precip=prec.mean,FFMC=ffmc.mean,ISI=isi.mean,VPD=vpd.mean
  #temp.median-273.15)/2,RH=rh.median/5,WS=winds.median,Precip=prec.median,FFMC=ffmc.median/5,ISI=isi.median,VPD=vpd.median,EMC=emc.median
) %>% 
  gather(T:VPD,key=variable,value=value)
bcdailyp <- bcdaily %>% filter((day>=min(onbday$day)-3)&(day<=max(onbday$day)+3)) %>% mutate( date=as.Date(as.integer(day), origin = paste(year-1,'-12-31', sep = "")),format="%Y-%m-%d %H") %>% 
  dplyr::select(contains(
    c('year','date','day','hr','long','lat','seq','fwi.mean','bui.mean','dmc.mean','dc.mean'))) %>% mutate(
      FWI=fwi.mean,BUI=bui.mean,DMC=dmc.mean,DC=dc.mean
    )%>% 
  gather(FWI:DC,key=variable,value=value)


test <- bchourlyp
tdat1 <- test %>% filter(dorn=='nighttime')
tdat1$timelabel <- 'Nighttime'
tdat2 <- bchourlyp %>% filter(dorn=='nighttime',naturalDay%in%onbday$naturalDay)
tdat2$timelabel <- 'OBE'
tdat3 <- test %>% filter(dorn=='daytime')
tdat3$timelabel <- 'Daytime'
testdat <- rbind(tdat3,tdat1,tdat2)

test$date <- as.POSIXct(as.character(test$date),format="%Y-%m-%d %H")
bchourly_cp$date <- as.POSIXct(as.character(bchourly_cp$date),format="%Y-%m-%d %H")
bchourly_cp$sunrise <- as.POSIXct(as.character(bchourly_cp$sunrise),format="%Y-%m-%d %H:%M:%S")
bchourly_cp$sunset <- as.POSIXct(as.character(bchourly_cp$sunset),format="%Y-%m-%d %H:%M:%S")
bcdailyp = bcdailyp %>% mutate(date=as.character(date+lubridate::hours(14)))
bcdailyp$date <- as.POSIXct(as.character(bcdailyp$date),format="%Y-%m-%d %H:%M:%S")

Maxfrp.mean <- max(testdat$frp.mean,na.rm=T)
MaxVar <- max(bchourlyp$value)
Maxspotsnum <- max(bchourlyp$spotsnum,na.rm=T)
Maxdaily <- max(bcdailyp$value)
coeff <- Maxfrp.mean/MaxVar
coeff2 <- Maxfrp.mean/Maxspotsnum
coeff3 <- Maxdaily/MaxVar


df_rect_daytime <- bchourly_cp %>% group_by(naturalDay) %>% filter(dorn=='daytime') %>% 
  summarise(start=date[1],end=date[n()])
df_rect_nighttime <- bchourly_cp %>% group_by(naturalDay) %>% filter(dorn=='nighttime') %>% 
  summarise(start=date[1],end=date[n()])

g3 <-ggplot() +
  geom_rect(data=df_rect_daytime,aes(xmin=start,xmax=end,ymin=- Inf,ymax = Inf), fill = "lightskyblue1", alpha = 0.4)+
  geom_rect(data=df_rect_nighttime,aes(xmin=start,xmax=end,ymin=- Inf,ymax = Inf), fill = "black", alpha = 0.4)+
  geom_line(data=bchourly_cp,aes(x=date,y=spotsnum))+
  geom_point(data=testdat,aes(x=as.POSIXct(date) ,y=frp.mean/coeff2,fill=timelabel),shape=21,size=2, alpha = 0.9,stroke=0.1)+  
  scale_y_continuous(name = "GOES detections",limits = c(0,Maxspotsnum),
                     sec.axis = sec_axis( trans=~.*coeff2, name="Mean FRP(MW)"))+ 
  scale_x_datetime()+scale_fill_manual(name = "FRP", values = c('blue','black','red'))+
  envalysis::theme_publish(base_size = 7, base_family = "", line_size = 0.25,base_rect_size =0.2)+
  theme(axis.title.x=element_blank())




g4 <-ggplot() + 
  geom_rect(data=df_rect_daytime,aes(xmin=start,xmax=end,ymin=- Inf,ymax = Inf), fill = "lightskyblue1", alpha = 0.4)+
  geom_rect(data=df_rect_nighttime,aes(xmin=start,xmax=end,ymin=- Inf,ymax = Inf), fill = "black", alpha = 0.4)+
  geom_line(data=test, aes(x = date, y = value, color = variable),size=0.5) +
  geom_line(data=bcdailyp,aes(x = date, y = value/coeff3, color = variable),size=0.5,linetype=2)+
  scale_y_continuous(name = "Hourly Variable",limits = c(0,MaxVar),
                     sec.axis = sec_axis( trans=~.*coeff3, name="Daily Variable"))+  
  scale_x_datetime()+#date_breaks = "1 day"
  theme_bw()+
  #scale_fill_manual(name = "FRP", values = c('grey','black','red'))+
  #scale_color_nejm()+
  envalysis::theme_publish(base_size = 7, base_family = "", line_size = 0.25,base_rect_size =0.25)+
  theme(axis.title.x=element_blank())
#theme(axis.text.x = element_text(angle = 45, vjust = 0, hjust=0))


testxx = rbind(bcdailyp %>% dplyr::select(date,variable,value),test%>% dplyr::select(date,variable,value))
testxx_fake =   bcdailyp <- bcdaily %>% filter((day>=min(onbday$day)-3)&(day<=max(onbday$day)+3)) %>% mutate( date=as.Date(as.integer(day), origin = paste(year-1,'-12-31', sep = "")),format="%Y-%m-%d %H") %>% 
  dplyr::select(contains( c('date','fwi.mean'))) %>% mutate(AFWI=fwi.mean)%>% 
  gather(AFWI,key=variable,value=value) %>% dplyr::select(date,variable,value) %>% filter(variable=='AFWI')
testxx = rbind(testxx,testxx_fake)
testxx$variable <- factor(testxx$variable,levels = c('AFWI','FWI','BUI','DMC','DC','ISI','FFMC','VPD','T','RH','WS','Precip'))


g4.1 <-ggplot() + 
  geom_rect(data=df_rect_daytime,aes(xmin=start,xmax=end,ymin=- Inf,ymax = Inf), fill = 'white')+#lightskyblue1
  geom_rect(data=df_rect_nighttime,aes(xmin=start,xmax=end,ymin=- Inf,ymax = Inf), fill = 'white')+
  geom_rect(data=df_rect_nighttime[c(16,17),],aes(xmin=start,xmax=end,ymin=- Inf,ymax = Inf), fill = 'lightblue')+
  geom_rect(data=df_rect_nighttime[c(37,38),],aes(xmin=start,xmax=end,ymin=- Inf,ymax = Inf), fill = 'lightblue')+
  # geom_line(data=test, aes(x = as.POSIXct(date), y = value, color = variable),size=0.2) +
  # geom_line(data=bcdailyp,aes(x = as.POSIXct(date), y = value, color = variable),size=0.2)+
  geom_line(data=testxx,aes(x = as.POSIXct(date), y = value),size=0.2)+
  facet_wrap(~variable, scale="free_y",nrow=6)+
  #scale_color_manual(values = c(inferno(12)[seq(1,11,1)],'deepskyblue'))+
  scale_x_datetime()+#date_breaks = "1 day"
  theme_bw()+
  #scale_fill_manual(name = "FRP", values = c('grey','black','red'))+
  #scale_color_nejm()+
  theme_classic()+
  theme(axis.title.x=element_blank(),legend.position = 'none',axis.title.y=element_blank(),text = element_text(size = 7),
        strip.background = element_blank())
#theme(axis.text.x = element_text(angle = 45, vjust = 0, hjust=0))
g4.1

aa = test %>% filter(day==143|day==144) %>% filter(variable=='Precip')
sum(aa$prec.mean)

# 
# xg4.1 <- paste0('015_OverNightBurning\\015.5_Plots\\fig.cs\\fig.creek.fw.2','.pdf')
# ggsave(plot=g4.1,xg4.1,width = 18,height = 15,units = 'cm')



g3.1 = ggplot() +
  geom_rect(data=df_rect_nighttime[c(16,17),],aes(xmin=start,xmax=end,ymin=- Inf,ymax = Inf), fill = 'lightblue')+
  geom_rect(data=df_rect_nighttime[c(37,38),],aes(xmin=start,xmax=end,ymin=- Inf,ymax = Inf), fill = 'lightblue')+
  geom_point(data=GOES_BA_Temp,aes(x=as.POSIXct(date) ,y=value,color=timelabel),size=0.2, alpha = 0.9)+ #,stroke=0.1
  #geom_line(data=GOES_BA_Temp,aes(x=as.POSIXct(date) ,y=value),size=0.1)+
  facet_wrap(~variable,scales = 'free_y',nrow=2)+#, strip.position = "left"
  scale_x_datetime()+scale_color_manual(name = "Burning Status", values = c('gray','orange','red'))+
  theme_classic()+
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
        legend.position = 'none',text = element_text(size = 7),strip.background = element_blank())

# strip.background = element_blank(),
# strip.placement = "outside"
g3.1

# xg3.1 <- paste0('015_OverNightBurning\\015.5_Plots\\fig.cs\\fig.creek.af.2','.pdf')
# ggsave(plot=g3.1,xg3.1,width = 17.22,height = 5.5,units = 'cm')




g2<-ggplot()+  
  geom_sf(data = shp.biome, fill=NA,lwd = 0.02)+
  geom_sf(data=firepointsf,color='red',shape=8,size=3,stroke=1)+
  coord_sf(crs = 3979,expand = F)+
  theme_void()+ theme(legend.position = "none",panel.background = element_rect( colour = "black", 
                                                                                size = 0.5))
g2
# xg2 <- paste0('015_OverNightBurning\\015.5_Plots\\fig.cs\\fig.creek.location.1','.pdf')
# ggsave(plot=g2,xg2,width = 1,height =1,units = 'cm')


OBEfilter = GOES_BA_Temp %>% filter(timelabel=='OBE')
dayfilter = GOES_BA_Temp %>% filter(timelabel=='Daytime')
night_nonevent_filter =   GOES_BA_Temp %>% filter(timelabel=='Nighttime')
night_nonevent_date = setdiff(night_nonevent_filter$date,OBEfilter$date)
night_nonevent_filter = night_nonevent_filter %>% filter(date%in%night_nonevent_date)


ggplot()+geom_point(data=OBEfilter,aes(x=as.POSIXct(date),y=variable))+facet_wrap(~variable)
ggplot()+geom_point(data=night_nonevent_filter,aes(x=as.POSIXct(date),y=variable))+facet_wrap(~variable)

sf_obe_interval = sf_goes_onb %>% filter(date%in%(OBEfilter$date)) %>% mutate(timelabel = 'Nighttime AF detections during OBEs')
sf_day_interval = sf_goes_day %>% filter(date%in%(dayfilter$date)) %>% mutate(timelabel = 'Daytime AF detections')
sf_night.nonevents_interval = sf_goes_night %>% filter(date%in%(night_nonevent_filter$date)) %>% mutate(timelabel = 'Nighttime AF detections during non-events')


df.obe.day = rbind(sf_obe_interval %>% dplyr::select(-naturalDay),sf_day_interval)
df.obe.day_jittered <- st_jitter(df.obe.day, amount = 0.01)
df.obe.day_jittered$timelabel <- factor(df.obe.day_jittered$timelabel,
                                        levels = c('Daytime AF detections',
                                                   'Nighttime AF detections during OBEs'))
sf_night.nonevents_interval_jittered <- st_jitter(sf_night.nonevents_interval, amount = 0.01)



g1.1 <- ggplot()+  
  geom_sf(data = Fireindi_sf,color='grey', lwd = 0.25,alpha=1,fill=NA)+
  geom_sf(data = df.obe.day_jittered ,aes(color=timelabel) ,alpha=0.1,size=0.001)+
  geom_sf(data = sf_night.nonevents_interval_jittered ,color='green',alpha=0.2,size=0.001)+
  # 
  # geom_sf(data = sf_goes_onb %>% filter(date%in%(OBEfilter$date)),color='red',alpha=0.1,size=0.001)+
  # geom_sf(data = sf_goes_night %>% filter(date%in%(night_nonevent_filter$date)),color='green',alpha=0.1,size=0.001)+
  coord_sf(crs = 4326,expand = F)+
  scale_color_manual(name = "Burning Status", values = c('blue','red'))+
  theme_minimal()+ theme(legend.position = "none",text = element_text(size = 5),
                         axis.text.x = element_text(
                           angle = 22.5,
                           hjust = 1
                         ))+#legend.position = "none"
  ggspatial::annotation_scale(location = 'br',pad_x=unit(0.01, "cm"), pad_y=unit(0.01, "cm"))

g1.1


# xg1.1 <- paste0('015_OverNightBurning\\015.5_Plots\\fig.cs\\fig.creek.goes.1','.pdf')
# ggsave(plot=g1.1,xg1.1,width = 7.5,height =5,units = 'cm')


# August complex fire -----------------------------------------------------

ONBselected <- ONB_stat %>% filter(counts>=10)
i=17
library(sf)
library(dplyr)
library(sp)
library(raster)
library(suncalc)
library(lutz)
library(foreach)
library(doParallel)
library(parallel)
library(tcltk)
library(doSNOW)
library(ggplot2)
library(tidyr)
library(cowplot)
library(magick)
library(viridis)
yearused=ONBselected[i,]$year
seqused=ONBselected[i,]$seq
grepused=paste0(yearused,'_',seqused,'_')
if (yearused==2017) {    
  NAfireused <- NAfire2017
  GOESused <- GOES17
  GOEScpused <- GOES17_cp}
if (yearused==2018) {    
  NAfireused <- NAfire2018
  GOESused <- GOES18
  GOEScpused <- GOES18_cp}
if (yearused==2019) {    
  NAfireused <- NAfire2019
  GOESused <- GOES19
  GOEScpused <- GOES19_cp}
if (yearused==2020) {    
  NAfireused <- NAfire2020
  GOESused <- GOES20
  GOEScpused <- GOES20_cp}
Fireindi <- NAfireused[NAfireused$seq ==seqused,]
Fireindi_sf <- as(Fireindi, "sf")
GOESovershp <- over(GOESused, Fireindi)
#remove all rows na
GOESovershpna <- GOESovershp[complete.cases(GOESovershp$firename),]
#match the goes fire info
GOES_rowind = as.numeric(rownames(GOESovershpna))
GOES_fireinfo = GOEScpused[GOES_rowind,]



tz <- tz_offset(as.Date(as.integer(GOES_fireinfo$day), origin = paste(yearused-1,'-12-31', sep = "")), tz_lookup_coords(Fireindi$lat, Fireindi$long, method = "accurate"))
tzoffset <- tz$utc_offset_h
GOES_fireinfo <- cbind(GOES_fireinfo,tzoffset)
#utc to local time
GOES_fireinfo$hr <- GOES_fireinfo$hr+tzoffset
timeind <- which(GOES_fireinfo$hr<0)
GOES_fireinfo$day[timeind] <- GOES_fireinfo$day[timeind]-1
GOES_fireinfo$hr[timeind] <- GOES_fireinfo$hr[timeind]+24
GOES_fireinfo$date <- as.character(as.POSIXct(paste(as.Date(as.integer(GOES_fireinfo$day), origin = paste(yearused-1,'-12-31', sep = "")),GOES_fireinfo$hr),tz=tz$tz_name[1],format="%Y-%m-%d %H"))


tz <- tz_offset(as.Date(as.integer(GOES_fireinfo$day), 
                        origin = paste(yearused-1,'-12-31', sep = "")), 
                tz_lookup_coords(GOES_fireinfo$lat[1], GOES_fireinfo$long[1], method = "accurate"))
sundata <- data.frame(date=as.Date(tz$date_time),lat=GOES_fireinfo$lat,lon=GOES_fireinfo$long)
sunrise <- getSunlightTimes(data=sundata,keep="sunrise",tz=tz$tz_name[1])$sunrise
sunset <- getSunlightTimes(data=sundata,keep="sunset",tz=tz$tz_name[1])$sunset
GOES_fireinfo$sunrise <- as.character(sunrise) 
GOES_fireinfo$sunset <- as.character(sunset)


tz1 <- tz_offset(as.Date(as.integer(GOES_fireinfo$day), origin = paste(yearused-1,'-12-30', sep = ""))
                 ,tz_lookup_coords(GOES_fireinfo$lat[1], GOES_fireinfo$long[1], method = "accurate"))
sundata1 <- data.frame(date=as.Date(tz1$date_time),lat=GOES_fireinfo$lat,lon=GOES_fireinfo$long)
GOES_fireinfo$sunset_lastday <- as.character(getSunlightTimes(data=sundata1,keep="sunset",tz=tz1$tz_name[1])$sunset) 

GOES_fireinfo <- GOES_fireinfo %>% mutate(dorn=ifelse(((date>sunrise&date<sunset)|(date<sunset_lastday)),'daytime','nighttime'))

onbday <- FD_NLgte4 %>% filter(year==yearused,seq==seqused,ONB_Event_YoN=='OBEs')

GOES_BA_Temp <- GOES_fireinfo %>% group_by(year,day,hr)%>%
  summarise(AF_MeanArea=mean(area,na.rm=T),AF_MeanTemp=mean(temp,na.rm=T),AF_MeanFRP=mean(frp,na.rm=T),AF_detections =n(),
            date=date[1],sunrise=sunrise[1],sunset=sunset[1],dorn=dorn[1])%>% mutate(naturalDay=ifelse((date<sunrise),day-1,day)) %>% 
  filter((naturalDay>=min(onbday$naturalDay)-3)&(naturalDay<=max(onbday$naturalDay)+3))%>% 
  gather(AF_MeanArea:AF_detections,key=variable,value=value)
test <- GOES_BA_Temp
tdat1 <- test %>% filter(dorn=='nighttime')
tdat1$timelabel <- 'Nighttime'
tdat2 <- GOES_BA_Temp %>% filter(dorn=='nighttime',naturalDay%in%onbday$naturalDay)
tdat2$timelabel <- 'OBE'
tdat3 <- test %>% filter(dorn=='daytime')
tdat3$timelabel <- 'Daytime'
GOES_BA_Temp <- rbind(tdat3,tdat1,tdat2)
GOES_BA_Temp[GOES_BA_Temp == "AF_detections"] <- 'GOES-R active fire detections'


library(sf)
sf_goes_day <- st_as_sf(GOES_fireinfo %>% filter( dorn=='daytime'), coords = c("long", "lat"), 
                        crs = 4326 , agr = "constant")
sf_goes_night <- st_as_sf(GOES_fireinfo %>% filter(dorn=='nighttime'), coords = c("long", "lat"), 
                          crs = 4326 , agr = "constant")


sf_goes_onb <- st_as_sf(GOES_fireinfo %>% mutate(naturalDay=ifelse((date<sunrise),day-1,day)) %>% 
                          filter(dorn=='nighttime',naturalDay%in%onbday$naturalDay), coords = c("long", "lat"), 
                        crs = 4326 , agr = "constant")
data = GOES_fireinfo %>% mutate(naturalDay=ifelse((date<sunrise),day-1,day)) %>% 
  filter(dorn=='nighttime',naturalDay%in%onbday$naturalDay)

sf_goes_day_jittered <- st_jitter(sf_goes_day, amount = 0.01)
sf_goes_night_jittered <- st_jitter(sf_goes_night, amount = 0.01)
sf_goes_onb_jittered <- st_jitter(sf_goes_onb, amount = 0.01)

firepointsf <- st_as_sf(Fireindi@data,coords = c('long','lat')) %>% st_set_crs(4326)


firepath <- mixedsort(dir(path='015_OverNightBurning\\015.1_DataProcessing\\SingleFireCSV_hourly\\',full.names = T,pattern = '(.csv)$'))
firepath2 <- mixedsort(dir(path='015_OverNightBurning\\015.1_DataProcessing\\SingleFireCSV_daily\\',full.names = T,pattern = '(.csv)$'))

xxx = grep(grepused,firepath2)


bchourly <- read.csv(firepath[xxx])
bcdaily <- read.csv(firepath2[xxx])

tz1 <- tz_offset(as.Date(as.integer(bchourly$day), origin = paste(yearused-1,'-12-30', sep = ""))
                 ,tz_lookup_coords(bchourly$lat[1], bchourly$long[1], method = "accurate"))
sundata1 <- data.frame(date=as.Date(tz1$date_time),lat=bchourly$lat,lon=bchourly$long)
bchourly$sunset_lastday <- as.character(getSunlightTimes(data=sundata1,keep="sunset",tz=tz1$tz_name[1])$sunset) 

bchourly <- bchourly %>% mutate(dorn=ifelse(((date>sunrise&date<sunset)|(date<sunset_lastday)),'daytime','nighttime'))%>% mutate(naturalDay=ifelse((date<sunrise),day-1,day))


bchourlyp <- bchourly %>% filter((naturalDay>=min(onbday$naturalDay)-3)&(naturalDay<=max(onbday$naturalDay)+3)) %>% dplyr::select(contains(
  c('year','day','hr','long','lat','seq','date','sunrise','biome','dorn','sunset','frp.total','frp.mean','spotsnum',
    'temp.mean','rh.mean','winds.mean','prec.mean','ffmc.mean','isi.mean','vpd.mean'))) 
bchourly_cp = bchourlyp

bchourlyp <- bchourlyp %>% mutate(
  T=(temp.mean-273.15),RH=rh.mean,WS=winds.mean,Precip=prec.mean,FFMC=ffmc.mean,ISI=isi.mean,VPD=vpd.mean
  #temp.median-273.15)/2,RH=rh.median/5,WS=winds.median,Precip=prec.median,FFMC=ffmc.median/5,ISI=isi.median,VPD=vpd.median,EMC=emc.median
) %>% 
  gather(T:VPD,key=variable,value=value)
bcdailyp <- bcdaily %>% filter((day>=min(onbday$day)-3)&(day<=max(onbday$day)+3)) %>% mutate( date=as.Date(as.integer(day), origin = paste(year-1,'-12-31', sep = "")),format="%Y-%m-%d %H") %>% 
  dplyr::select(contains(
    c('year','date','day','hr','long','lat','seq','fwi.mean','bui.mean','dmc.mean','dc.mean'))) %>% mutate(
      FWI=fwi.mean,BUI=bui.mean,DMC=dmc.mean,DC=dc.mean
    )%>% 
  gather(FWI:DC,key=variable,value=value)


test <- bchourlyp
tdat1 <- test %>% filter(dorn=='nighttime')
tdat1$timelabel <- 'Nighttime'
tdat2 <- bchourlyp %>% filter(dorn=='nighttime',naturalDay%in%onbday$naturalDay)
tdat2$timelabel <- 'OBE'
tdat3 <- test %>% filter(dorn=='daytime')
tdat3$timelabel <- 'Daytime'
testdat <- rbind(tdat3,tdat1,tdat2)

test$date <- as.POSIXct(as.character(test$date),format="%Y-%m-%d %H")
bchourly_cp$date <- as.POSIXct(as.character(bchourly_cp$date),format="%Y-%m-%d %H")
bchourly_cp$sunrise <- as.POSIXct(as.character(bchourly_cp$sunrise),format="%Y-%m-%d %H:%M:%S")
bchourly_cp$sunset <- as.POSIXct(as.character(bchourly_cp$sunset),format="%Y-%m-%d %H:%M:%S")
bcdailyp = bcdailyp %>% mutate(date=as.character(date+lubridate::hours(14)))
bcdailyp$date <- as.POSIXct(as.character(bcdailyp$date),format="%Y-%m-%d %H:%M:%S")

Maxfrp.mean <- max(testdat$frp.mean,na.rm=T)
MaxVar <- max(bchourlyp$value)
Maxspotsnum <- max(bchourlyp$spotsnum,na.rm=T)
Maxdaily <- max(bcdailyp$value)
coeff <- Maxfrp.mean/MaxVar
coeff2 <- Maxfrp.mean/Maxspotsnum
coeff3 <- Maxdaily/MaxVar


df_rect_daytime <- bchourly_cp %>% group_by(naturalDay) %>% filter(dorn=='daytime') %>% 
  summarise(start=date[1],end=date[n()])
df_rect_nighttime <- bchourly_cp %>% group_by(naturalDay) %>% filter(dorn=='nighttime') %>% 
  summarise(start=date[1],end=date[n()])

g3 <-ggplot() +
  geom_rect(data=df_rect_daytime,aes(xmin=start,xmax=end,ymin=- Inf,ymax = Inf), fill = "lightskyblue1", alpha = 0.4)+
  geom_rect(data=df_rect_nighttime,aes(xmin=start,xmax=end,ymin=- Inf,ymax = Inf), fill = "black", alpha = 0.4)+
  geom_line(data=bchourly_cp,aes(x=date,y=spotsnum))+
  geom_point(data=testdat,aes(x=as.POSIXct(date) ,y=frp.mean/coeff2,fill=timelabel),shape=21,size=2, alpha = 0.9,stroke=0.1)+  
  scale_y_continuous(name = "GOES detections",limits = c(0,Maxspotsnum),
                     sec.axis = sec_axis( trans=~.*coeff2, name="Mean FRP(MW)"))+ 
  scale_x_datetime()+scale_fill_manual(name = "FRP", values = c('blue','black','red'))+
  envalysis::theme_publish(base_size = 7, base_family = "", line_size = 0.25,base_rect_size =0.2)+
  theme(axis.title.x=element_blank())




g4 <-ggplot() + 
  geom_rect(data=df_rect_daytime,aes(xmin=start,xmax=end,ymin=- Inf,ymax = Inf), fill = "lightskyblue1", alpha = 0.4)+
  geom_rect(data=df_rect_nighttime,aes(xmin=start,xmax=end,ymin=- Inf,ymax = Inf), fill = "black", alpha = 0.4)+
  geom_line(data=test, aes(x = date, y = value, color = variable),size=0.5) +
  geom_line(data=bcdailyp,aes(x = date, y = value/coeff3, color = variable),size=0.5,linetype=2)+
  scale_y_continuous(name = "Hourly Variable",limits = c(0,MaxVar),
                     sec.axis = sec_axis( trans=~.*coeff3, name="Daily Variable"))+  
  scale_x_datetime()+#date_breaks = "1 day"
  theme_bw()+
  #scale_fill_manual(name = "FRP", values = c('grey','black','red'))+
  #scale_color_nejm()+
  envalysis::theme_publish(base_size = 7, base_family = "", line_size = 0.25,base_rect_size =0.25)+
  theme(axis.title.x=element_blank())
#theme(axis.text.x = element_text(angle = 45, vjust = 0, hjust=0))


testxx = rbind(bcdailyp %>% dplyr::select(date,variable,value),test%>% dplyr::select(date,variable,value))
testxx_fake =   bcdailyp <- bcdaily %>% filter((day>=min(onbday$day)-3)&(day<=max(onbday$day)+3)) %>% mutate( date=as.Date(as.integer(day), origin = paste(year-1,'-12-31', sep = "")),format="%Y-%m-%d %H") %>% 
  dplyr::select(contains( c('date','fwi.mean'))) %>% mutate(AFWI=fwi.mean)%>% 
  gather(AFWI,key=variable,value=value) %>% dplyr::select(date,variable,value) %>% filter(variable=='AFWI')
testxx = rbind(testxx,testxx_fake)
testxx$variable <- factor(testxx$variable,levels = c('AFWI','FWI','BUI','DMC','DC','ISI','FFMC','VPD','T','RH','WS','Precip'))


g4.1 <-ggplot() + 
  geom_rect(data=df_rect_daytime,aes(xmin=start,xmax=end,ymin=- Inf,ymax = Inf), fill = 'gray96')+#lightskyblue1
  geom_rect(data=df_rect_nighttime,aes(xmin=start,xmax=end,ymin=- Inf,ymax = Inf), fill = 'gray90')+
  # geom_line(data=test, aes(x = as.POSIXct(date), y = value, color = variable),size=0.2) +
  # geom_line(data=bcdailyp,aes(x = as.POSIXct(date), y = value, color = variable),size=0.2)+
  geom_line(data=testxx,aes(x = as.POSIXct(date), y = value, color = variable),size=0.2)+
  facet_wrap(~variable, scale="free_y",nrow=6)+
  scale_color_manual(values = c(inferno(12)[seq(1,11,1)],'deepskyblue'))+
  scale_x_datetime()+#date_breaks = "1 day"
  theme_bw()+
  #scale_fill_manual(name = "FRP", values = c('grey','black','red'))+
  #scale_color_nejm()+
  theme_classic()+
  theme(axis.title.x=element_blank(),legend.position = 'none',axis.title.y=element_blank(),text = element_text(size = 7),
        strip.background = element_blank())
#theme(axis.text.x = element_text(angle = 45, vjust = 0, hjust=0))
g4.1

aa = test %>% filter(day==143|day==144) %>% filter(variable=='Precip')
sum(aa$prec.mean)


# xg4.1 <- paste0('015_OverNightBurning\\015.5_Plots\\fig.cs\\fig.august.fw.1','.pdf')
# ggsave(plot=g4.1,xg4.1,width = 15,height = 15,units = 'cm')


g3.1 = ggplot() +
  geom_rect(data=df_rect_daytime,aes(xmin=start,xmax=end,ymin=- Inf,ymax = Inf), fill = 'gray96')+#lightskyblue1
  geom_rect(data=df_rect_nighttime,aes(xmin=start,xmax=end,ymin=- Inf,ymax = Inf), fill = 'gray90')+
  geom_point(data=GOES_BA_Temp,aes(x=as.POSIXct(date) ,y=value,color=timelabel),size=0.1, alpha = 0.9)+ #,stroke=0.1
  #geom_line(data=GOES_BA_Temp,aes(x=as.POSIXct(date) ,y=value),size=0.1)+
  facet_wrap(~variable,scales = 'free_y',nrow=2)+#, strip.position = "left"
  scale_x_datetime()+scale_color_manual(name = "Burning Status", values = c('blue','green','red'))+
  theme_classic()+
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
        legend.position = 'none',text = element_text(size = 7),strip.background = element_blank())

# strip.background = element_blank(),
# strip.placement = "outside"
g3.1

# xg3.1 <- paste0('015_OverNightBurning\\015.5_Plots\\fig.cs\\fig.august.af.1','.pdf')
# ggsave(plot=g3.1,xg3.1,width = 15.15,height = 5.5,units = 'cm')



g2<-ggplot()+  
  geom_sf(data = shp.biome, fill=NA,lwd = 0.02)+
  geom_sf(data=firepointsf,color='red',shape=8,size=3,stroke=1)+
  coord_sf(crs = 3979,expand = F)+
  theme_void()+ theme(legend.position = "none",panel.background = element_rect( colour = "black", 
                                                                                size = 0.5))
g2
# xg2 <- paste0('015_OverNightBurning\\015.5_Plots\\fig.cs\\fig.august.location','.pdf')
# ggsave(plot=g2,xg2,width = 1,height =1,units = 'in')


OBEfilter = GOES_BA_Temp %>% filter(timelabel=='OBE')
dayfilter = GOES_BA_Temp %>% filter(timelabel=='Daytime')
night_nonevent_filter =   GOES_BA_Temp %>% filter(timelabel=='Nighttime')
night_nonevent_date = setdiff(night_nonevent_filter$date,OBEfilter$date)
night_nonevent_filter = night_nonevent_filter %>% filter(date%in%night_nonevent_date)


ggplot()+geom_point(data=OBEfilter,aes(x=as.POSIXct(date),y=variable))+facet_wrap(~variable)
ggplot()+geom_point(data=night_nonevent_filter,aes(x=as.POSIXct(date),y=variable))+facet_wrap(~variable)

sf_obe_interval = sf_goes_onb %>% filter(date%in%(OBEfilter$date)) %>% mutate(timelabel = 'Nighttime AF detections during OBEs')
sf_day_interval = sf_goes_day %>% filter(date%in%(dayfilter$date)) %>% mutate(timelabel = 'Daytime AF detections')
sf_night.nonevents_interval = sf_goes_night %>% filter(date%in%(night_nonevent_filter$date)) %>% mutate(timelabel = 'Nighttime AF detections during non-events')


df.obe.day = rbind(sf_obe_interval %>% dplyr::select(-naturalDay),sf_day_interval)
df.obe.day_jittered <- st_jitter(df.obe.day, amount = 0.01)
df.obe.day_jittered$timelabel <- factor(df.obe.day_jittered$timelabel,
                                        levels = c('Daytime AF detections',
                                                   'Nighttime AF detections during OBEs'))
sf_night.nonevents_interval_jittered <- st_jitter(sf_night.nonevents_interval, amount = 0.01)



g1.1 <- ggplot()+  
  geom_sf(data = Fireindi_sf,color='grey', lwd = 0.5,alpha=1,fill=NA)+
  geom_sf(data = df.obe.day_jittered ,aes(color=timelabel) ,alpha=0.1,size=0.01)+
  geom_sf(data = sf_night.nonevents_interval_jittered ,color='green',alpha=0.2,size=0.01)+
  
  # geom_sf(data = sf_goes_onb %>% filter(date%in%(OBEfilter$date)),color='red',alpha=0.1,size=0.001)+
  # geom_sf(data = sf_goes_night %>% filter(date%in%(night_nonevent_filter$date)),color='green',alpha=0.1,size=0.001)+
  coord_sf(crs = 4326,expand = F)+
  scale_color_manual(name = "Burning Status", values = c('blue','red'))+
  theme_minimal()+ theme(legend.position = "none",text = element_text(family='Times',size = 6))+#legend.position = "none"
  ggspatial::annotation_scale(location = 'br',pad_x=unit(0.01, "cm"), pad_y=unit(0.01, "cm"))

g1.1


# xg1.1 <- paste0('015_OverNightBurning\\015.5_Plots\\fig.cs\\fig.august.goes','.pdf')
# ggsave(plot=g1.1,xg1.1,width = 3.2,height =4.3,units = 'in')



# 2019 McMillan Complex Fire ----------------------------------------------


ONBselected <- ONB_stat %>% filter(counts>=5)
i=24
library(sf)
library(dplyr)
library(sp)
library(raster)
library(suncalc)
library(lutz)
library(foreach)
library(doParallel)
library(parallel)
library(tcltk)
library(doSNOW)
library(ggplot2)
library(tidyr)
library(cowplot)
library(magick)
library(viridis)
yearused=ONBselected[i,]$year
seqused=ONBselected[i,]$seq
grepused=paste0(yearused,'_',seqused,'_')
if (yearused==2017) {    
  NAfireused <- NAfire2017
  GOESused <- GOES17
  GOEScpused <- GOES17_cp}
if (yearused==2018) {    
  NAfireused <- NAfire2018
  GOESused <- GOES18
  GOEScpused <- GOES18_cp}
if (yearused==2019) {    
  NAfireused <- NAfire2019
  GOESused <- GOES19
  GOEScpused <- GOES19_cp}
if (yearused==2020) {    
  NAfireused <- NAfire2020
  GOESused <- GOES20
  GOEScpused <- GOES20_cp}
Fireindi <- NAfireused[NAfireused$seq ==seqused,]
Fireindi_sf <- as(Fireindi, "sf")
GOESovershp <- over(GOESused, Fireindi)
#remove all rows na
GOESovershpna <- GOESovershp[complete.cases(GOESovershp$firename),]
#match the goes fire info
GOES_rowind = as.numeric(rownames(GOESovershpna))
GOES_fireinfo = GOEScpused[GOES_rowind,]



tz <- tz_offset(as.Date(as.integer(GOES_fireinfo$day), origin = paste(yearused-1,'-12-31', sep = "")), tz_lookup_coords(Fireindi$lat, Fireindi$long, method = "accurate"))
tzoffset <- tz$utc_offset_h
GOES_fireinfo <- cbind(GOES_fireinfo,tzoffset)
#utc to local time
GOES_fireinfo$hr <- GOES_fireinfo$hr+tzoffset
timeind <- which(GOES_fireinfo$hr<0)
GOES_fireinfo$day[timeind] <- GOES_fireinfo$day[timeind]-1
GOES_fireinfo$hr[timeind] <- GOES_fireinfo$hr[timeind]+24
GOES_fireinfo$date <- as.character(as.POSIXct(paste(as.Date(as.integer(GOES_fireinfo$day), origin = paste(yearused-1,'-12-31', sep = "")),GOES_fireinfo$hr),tz=tz$tz_name[1],format="%Y-%m-%d %H"))


tz <- tz_offset(as.Date(as.integer(GOES_fireinfo$day), 
                        origin = paste(yearused-1,'-12-31', sep = "")), 
                tz_lookup_coords(GOES_fireinfo$lat[1], GOES_fireinfo$long[1], method = "accurate"))
sundata <- data.frame(date=as.Date(tz$date_time),lat=GOES_fireinfo$lat,lon=GOES_fireinfo$long)
sunrise <- getSunlightTimes(data=sundata,keep="sunrise",tz=tz$tz_name[1])$sunrise
sunset <- getSunlightTimes(data=sundata,keep="sunset",tz=tz$tz_name[1])$sunset
GOES_fireinfo$sunrise <- as.character(sunrise) 
GOES_fireinfo$sunset <- as.character(sunset)


tz1 <- tz_offset(as.Date(as.integer(GOES_fireinfo$day), origin = paste(yearused-1,'-12-30', sep = ""))
                 ,tz_lookup_coords(GOES_fireinfo$lat[1], GOES_fireinfo$long[1], method = "accurate"))
sundata1 <- data.frame(date=as.Date(tz1$date_time),lat=GOES_fireinfo$lat,lon=GOES_fireinfo$long)
GOES_fireinfo$sunset_lastday <- as.character(getSunlightTimes(data=sundata1,keep="sunset",tz=tz1$tz_name[1])$sunset) 

GOES_fireinfo <- GOES_fireinfo %>% mutate(dorn=ifelse(((date>sunrise&date<sunset)|(date<sunset_lastday)),'daytime','nighttime'))

onbday <- FD_NLgte4 %>% filter(year==yearused,seq==seqused,ONB_Event_YoN=='OBEs')

GOES_BA_Temp <- GOES_fireinfo %>% group_by(year,day,hr)%>%
  summarise(AF_MeanArea=mean(area,na.rm=T),AF_MeanTemp=mean(temp,na.rm=T),AF_MeanFRP=mean(frp,na.rm=T),AF_detections =n(),
            date=date[1],sunrise=sunrise[1],sunset=sunset[1],dorn=dorn[1])%>% mutate(naturalDay=ifelse((date<sunrise),day-1,day)) %>% 
  filter((naturalDay>=min(onbday$naturalDay)-3)&(naturalDay<=max(onbday$naturalDay)+3))%>% 
  gather(AF_MeanArea:AF_detections,key=variable,value=value)
test <- GOES_BA_Temp
tdat1 <- test %>% filter(dorn=='nighttime')
tdat1$timelabel <- 'Nighttime'
tdat2 <- GOES_BA_Temp %>% filter(dorn=='nighttime',naturalDay%in%onbday$naturalDay)
tdat2$timelabel <- 'OBE'
tdat3 <- test %>% filter(dorn=='daytime')
tdat3$timelabel <- 'Daytime'
GOES_BA_Temp <- rbind(tdat3,tdat1,tdat2)
GOES_BA_Temp[GOES_BA_Temp == "AF_detections"] <- 'GOES-R active fire detections'


library(sf)
sf_goes_day <- st_as_sf(GOES_fireinfo %>% filter( dorn=='daytime'), coords = c("long", "lat"), 
                        crs = 4326 , agr = "constant")
sf_goes_night <- st_as_sf(GOES_fireinfo %>% filter(dorn=='nighttime'), coords = c("long", "lat"), 
                          crs = 4326 , agr = "constant")


sf_goes_onb <- st_as_sf(GOES_fireinfo %>% mutate(naturalDay=ifelse((date<sunrise),day-1,day)) %>% 
                          filter(dorn=='nighttime',naturalDay%in%onbday$naturalDay), coords = c("long", "lat"), 
                        crs = 4326 , agr = "constant")
data = GOES_fireinfo %>% mutate(naturalDay=ifelse((date<sunrise),day-1,day)) %>% 
  filter(dorn=='nighttime',naturalDay%in%onbday$naturalDay)

sf_goes_day_jittered <- st_jitter(sf_goes_day, amount = 0.01)
sf_goes_night_jittered <- st_jitter(sf_goes_night, amount = 0.01)
sf_goes_onb_jittered <- st_jitter(sf_goes_onb, amount = 0.01)

firepointsf <- st_as_sf(Fireindi@data,coords = c('long','lat')) %>% st_set_crs(4326)


firepath <- mixedsort(dir(path='015_OverNightBurning\\015.1_DataProcessing\\SingleFireCSV_hourly\\',full.names = T,pattern = '(.csv)$'))
firepath2 <- mixedsort(dir(path='015_OverNightBurning\\015.1_DataProcessing\\SingleFireCSV_daily\\',full.names = T,pattern = '(.csv)$'))

xxx = grep(grepused,firepath2)


bchourly <- read.csv(firepath[xxx])
bcdaily <- read.csv(firepath2[xxx])

tz1 <- tz_offset(as.Date(as.integer(bchourly$day), origin = paste(yearused-1,'-12-30', sep = ""))
                 ,tz_lookup_coords(bchourly$lat[1], bchourly$long[1], method = "accurate"))
sundata1 <- data.frame(date=as.Date(tz1$date_time),lat=bchourly$lat,lon=bchourly$long)
bchourly$sunset_lastday <- as.character(getSunlightTimes(data=sundata1,keep="sunset",tz=tz1$tz_name[1])$sunset) 

bchourly <- bchourly %>% mutate(dorn=ifelse(((date>sunrise&date<sunset)|(date<sunset_lastday)),'daytime','nighttime'))%>% mutate(naturalDay=ifelse((date<sunrise),day-1,day))


bchourlyp <- bchourly %>% filter((naturalDay>=min(onbday$naturalDay)-3)&(naturalDay<=max(onbday$naturalDay)+3)) %>% dplyr::select(contains(
  c('year','day','hr','long','lat','seq','date','sunrise','biome','dorn','sunset','frp.total','frp.mean','spotsnum',
    'temp.mean','rh.mean','winds.mean','prec.mean','ffmc.mean','isi.mean','vpd.mean'))) 
bchourly_cp = bchourlyp

bchourlyp <- bchourlyp %>% mutate(
  T=(temp.mean-273.15),RH=rh.mean,WS=winds.mean,Precip=prec.mean,FFMC=ffmc.mean,ISI=isi.mean,VPD=vpd.mean
  #temp.median-273.15)/2,RH=rh.median/5,WS=winds.median,Precip=prec.median,FFMC=ffmc.median/5,ISI=isi.median,VPD=vpd.median,EMC=emc.median
) %>% 
  gather(T:VPD,key=variable,value=value)
bcdailyp <- bcdaily %>% filter((day>=min(onbday$day)-3)&(day<=max(onbday$day)+3)) %>% mutate( date=as.Date(as.integer(day), origin = paste(year-1,'-12-31', sep = "")),format="%Y-%m-%d %H") %>% 
  dplyr::select(contains(
    c('year','date','day','hr','long','lat','seq','fwi.mean','bui.mean','dmc.mean','dc.mean'))) %>% mutate(
      FWI=fwi.mean,BUI=bui.mean,DMC=dmc.mean,DC=dc.mean
    )%>% 
  gather(FWI:DC,key=variable,value=value)


test <- bchourlyp
tdat1 <- test %>% filter(dorn=='nighttime')
tdat1$timelabel <- 'Nighttime'
tdat2 <- bchourlyp %>% filter(dorn=='nighttime',naturalDay%in%onbday$naturalDay)
tdat2$timelabel <- 'OBE'
tdat3 <- test %>% filter(dorn=='daytime')
tdat3$timelabel <- 'Daytime'
testdat <- rbind(tdat3,tdat1,tdat2)

test$date <- as.POSIXct(as.character(test$date),format="%Y-%m-%d %H")
bchourly_cp$date <- as.POSIXct(as.character(bchourly_cp$date),format="%Y-%m-%d %H")
bchourly_cp$sunrise <- as.POSIXct(as.character(bchourly_cp$sunrise),format="%Y-%m-%d %H:%M:%S")
bchourly_cp$sunset <- as.POSIXct(as.character(bchourly_cp$sunset),format="%Y-%m-%d %H:%M:%S")
bcdailyp = bcdailyp %>% mutate(date=as.character(date+lubridate::hours(14)))
bcdailyp$date <- as.POSIXct(as.character(bcdailyp$date),format="%Y-%m-%d %H:%M:%S")

Maxfrp.mean <- max(testdat$frp.mean,na.rm=T)
MaxVar <- max(bchourlyp$value)
Maxspotsnum <- max(bchourlyp$spotsnum,na.rm=T)
Maxdaily <- max(bcdailyp$value)
coeff <- Maxfrp.mean/MaxVar
coeff2 <- Maxfrp.mean/Maxspotsnum
coeff3 <- Maxdaily/MaxVar


df_rect_daytime <- bchourly_cp %>% group_by(naturalDay) %>% filter(dorn=='daytime') %>% 
  summarise(start=date[1],end=date[n()])
df_rect_nighttime <- bchourly_cp %>% group_by(naturalDay) %>% filter(dorn=='nighttime') %>% 
  summarise(start=date[1],end=date[n()])

g3 <-ggplot() +
  geom_rect(data=df_rect_daytime,aes(xmin=start,xmax=end,ymin=- Inf,ymax = Inf), fill = "lightskyblue1", alpha = 0.4)+
  geom_rect(data=df_rect_nighttime,aes(xmin=start,xmax=end,ymin=- Inf,ymax = Inf), fill = "black", alpha = 0.4)+
  geom_line(data=bchourly_cp,aes(x=date,y=spotsnum))+
  geom_point(data=testdat,aes(x=as.POSIXct(date) ,y=frp.mean/coeff2,fill=timelabel),shape=21,size=2, alpha = 0.9,stroke=0.1)+  
  scale_y_continuous(name = "GOES detections",limits = c(0,Maxspotsnum),
                     sec.axis = sec_axis( trans=~.*coeff2, name="Mean FRP(MW)"))+ 
  scale_x_datetime()+scale_fill_manual(name = "FRP", values = c('blue','black','red'))+
  envalysis::theme_publish(base_size = 7, base_family = "", line_size = 0.25,base_rect_size =0.2)+
  theme(axis.title.x=element_blank())




g4 <-ggplot() + 
  geom_rect(data=df_rect_daytime,aes(xmin=start,xmax=end,ymin=- Inf,ymax = Inf), fill = "lightskyblue1", alpha = 0.4)+
  geom_rect(data=df_rect_nighttime,aes(xmin=start,xmax=end,ymin=- Inf,ymax = Inf), fill = "black", alpha = 0.4)+
  geom_line(data=test, aes(x = date, y = value, color = variable),size=0.5) +
  geom_line(data=bcdailyp,aes(x = date, y = value/coeff3, color = variable),size=0.5,linetype=2)+
  scale_y_continuous(name = "Hourly Variable",limits = c(0,MaxVar),
                     sec.axis = sec_axis( trans=~.*coeff3, name="Daily Variable"))+  
  scale_x_datetime()+#date_breaks = "1 day"
  theme_bw()+
  #scale_fill_manual(name = "FRP", values = c('grey','black','red'))+
  #scale_color_nejm()+
  envalysis::theme_publish(base_size = 7, base_family = "", line_size = 0.25,base_rect_size =0.25)+
  theme(axis.title.x=element_blank())
#theme(axis.text.x = element_text(angle = 45, vjust = 0, hjust=0))


testxx = rbind(bcdailyp %>% dplyr::select(date,variable,value),test%>% dplyr::select(date,variable,value))
testxx_fake =   bcdailyp <- bcdaily %>% filter((day>=min(onbday$day)-3)&(day<=max(onbday$day)+3)) %>% mutate( date=as.Date(as.integer(day), origin = paste(year-1,'-12-31', sep = "")),format="%Y-%m-%d %H") %>% 
  dplyr::select(contains( c('date','fwi.mean'))) %>% mutate(AFWI=fwi.mean)%>% 
  gather(AFWI,key=variable,value=value) %>% dplyr::select(date,variable,value) %>% filter(variable=='AFWI')
testxx = rbind(testxx,testxx_fake)
testxx$variable <- factor(testxx$variable,levels = c('AFWI','FWI','BUI','DMC','DC','ISI','FFMC','VPD','T','RH','WS','Precip'))


g4.1 <-ggplot() + 
  # geom_rect(data=df_rect_daytime,aes(xmin=start,xmax=end,ymin=- Inf,ymax = Inf), fill = 'gray96')+#lightskyblue1
  # geom_rect(data=df_rect_nighttime,aes(xmin=start,xmax=end,ymin=- Inf,ymax = Inf), fill = 'gray90')+
  # geom_line(data=test, aes(x = as.POSIXct(date), y = value, color = variable),size=0.2) +
  # geom_line(data=bcdailyp,aes(x = as.POSIXct(date), y = value, color = variable),size=0.2)+
  geom_rect(data=df_rect_nighttime[c(4,5,6,7),],aes(xmin=start,xmax=end,ymin=- Inf,ymax = Inf), fill = 'lightblue')+
  geom_rect(data=df_rect_nighttime[c(12,13,14,15),],aes(xmin=start,xmax=end,ymin=- Inf,ymax = Inf), fill = 'lightblue')+
  geom_line(data=testxx,aes(x = as.POSIXct(date), y = value),size=0.2)+
  facet_wrap(~variable, scale="free_y",nrow=6)+
  #scale_color_manual(values = c(inferno(12)[seq(1,11,1)],'deepskyblue'))+
  scale_x_datetime()+#date_breaks = "1 day"
  theme_bw()+
  #scale_fill_manual(name = "FRP", values = c('grey','black','red'))+
  #scale_color_nejm()+
  theme_classic()+
  theme(axis.title.x=element_blank(),legend.position = 'none',axis.title.y=element_blank(),text = element_text(size = 7),
        strip.background = element_blank())
#theme(axis.text.x = element_text(angle = 45, vjust = 0, hjust=0))
g4.1

aa = test %>% filter(day==143|day==144) %>% filter(variable=='Precip')
sum(aa$prec.mean)

# xg4.1 <- paste0('015_OverNightBurning\\015.5_Plots\\fig.cs\\fig.McMillan.fw.2','.pdf')
# ggsave(plot=g4.1,xg4.1,width = 18,height = 15,units = 'cm')




g3.1 = ggplot() +
  # geom_rect(data=df_rect_daytime,aes(xmin=start,xmax=end,ymin=- Inf,ymax = Inf), fill = 'gray96')+#lightskyblue1
  # geom_rect(data=df_rect_nighttime,aes(xmin=start,xmax=end,ymin=- Inf,ymax = Inf), fill = 'gray90')+
  geom_rect(data=df_rect_nighttime[c(4,5,6,7),],aes(xmin=start,xmax=end,ymin=- Inf,ymax = Inf), fill = 'lightblue')+
  geom_rect(data=df_rect_nighttime[c(12,13,14,15),],aes(xmin=start,xmax=end,ymin=- Inf,ymax = Inf), fill = 'lightblue')+
  geom_point(data=GOES_BA_Temp,aes(x=as.POSIXct(date) ,y=value,color=timelabel),size=0.1, alpha = 0.9)+ #,stroke=0.1
  #geom_line(data=GOES_BA_Temp,aes(x=as.POSIXct(date) ,y=value),size=0.1)+
  facet_wrap(~variable,scales = 'free_y',nrow=2)+#, strip.position = "left"
  scale_x_datetime()+scale_color_manual(name = "Burning Status", values = c('gray','orange','red'))+
  theme_classic()+
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
        legend.position = 'none',text = element_text(size = 7),strip.background = element_blank())

# strip.background = element_blank(),
# strip.placement = "outside"
g3.1


# xg3.1 <- paste0('015_OverNightBurning\\015.5_Plots\\fig.cs\\fig.McMillan.af.2','.pdf')
# ggsave(plot=g3.1,xg3.1,width = 15.85,height = 5.5,units = 'cm')



g2<-ggplot()+  
  geom_sf(data = shp.biome, fill=NA,lwd = 0.02)+
  geom_sf(data=firepointsf,color='red',shape=8,size=3,stroke=1)+
  coord_sf(crs = 3979,expand = F)+
  theme_void()+ theme(legend.position = "none",panel.background = element_rect( colour = "black", 
                                                                                size = 0.5))
g2
# xg2 <- paste0('015_OverNightBurning\\015.5_Plots\\fig.cs\\fig.McMillan.location','.pdf')
# ggsave(plot=g2,xg2,width = 1,height =1,units = 'in')


OBEfilter = GOES_BA_Temp %>% filter(timelabel=='OBE')
dayfilter = GOES_BA_Temp %>% filter(timelabel=='Daytime')
night_nonevent_filter =   GOES_BA_Temp %>% filter(timelabel=='Nighttime')
night_nonevent_date = setdiff(night_nonevent_filter$date,OBEfilter$date)
night_nonevent_filter = night_nonevent_filter %>% filter(date%in%night_nonevent_date)


ggplot()+geom_point(data=OBEfilter,aes(x=as.POSIXct(date),y=variable))+facet_wrap(~variable)
ggplot()+geom_point(data=night_nonevent_filter,aes(x=as.POSIXct(date),y=variable))+facet_wrap(~variable)

sf_obe_interval = sf_goes_onb %>% filter(date%in%(OBEfilter$date)) %>% mutate(timelabel = 'Nighttime AF detections during OBEs')
sf_day_interval = sf_goes_day %>% filter(date%in%(dayfilter$date)) %>% mutate(timelabel = 'Daytime AF detections')
sf_night.nonevents_interval = sf_goes_night %>% filter(date%in%(night_nonevent_filter$date)) %>% mutate(timelabel = 'Nighttime AF detections during non-events')


df.obe.day = rbind(sf_obe_interval %>% dplyr::select(-naturalDay),sf_day_interval)
df.obe.day_jittered <- st_jitter(df.obe.day, amount = 0.012)
df.obe.day_jittered$timelabel <- factor(df.obe.day_jittered$timelabel,
                                        levels = c('Daytime AF detections',
                                                   'Nighttime AF detections during OBEs'))
sf_night.nonevents_interval_jittered <- st_jitter(sf_night.nonevents_interval, amount = 0.012)



g1.1 <- ggplot()+  
  geom_sf(data = Fireindi_sf,color='grey', lwd = 0.5,alpha=1,fill=NA)+
  geom_sf(data = df.obe.day_jittered ,aes(color=timelabel) ,alpha=0.1,size=0.01)+
  geom_sf(data = sf_night.nonevents_interval_jittered ,color='green',alpha=0.2,size=0.01)+
  
  # geom_sf(data = sf_goes_onb %>% filter(date%in%(OBEfilter$date)),color='red',alpha=0.1,size=0.001)+
  # geom_sf(data = sf_goes_night %>% filter(date%in%(night_nonevent_filter$date)),color='green',alpha=0.1,size=0.001)+
  coord_sf(crs = 4326,expand = F)+
  scale_color_manual(name = "Burning Status", values = c('blue','red'))+
  theme_minimal()+ theme(legend.position = "none",text = element_text(size = 7))+#legend.position = "none"
  ggspatial::annotation_scale(location = 'br',pad_x=unit(0.01, "cm"), pad_y=unit(0.01, "cm"))

g1.1


# xg1.1 <- paste0('015_OverNightBurning\\015.5_Plots\\fig.cs\\fig.McMillan.goes','.pdf')
# ggsave(plot=g1.1,xg1.1,width = 3.2,height =4.3,units = 'in')

