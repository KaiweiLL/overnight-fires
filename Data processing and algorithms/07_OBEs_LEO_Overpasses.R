rm(list = ls())
getwd()
setwd("D:\\")

# packages ----------------------------------------------------------------

Sys.setlocale("LC_TIME", "English") 
windowsFonts(Times=windowsFont("Arial"))
world <- ne_countries(scale = "medium", returnclass = "sf")


# load feature design data and omit and replace NA--------------------

Nights_FeatDesi_all <- read.csv('015.1_DataProcessing\\FeatureDesign\\FD_All_by2FireHr_gt1000.csv')
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



# load NBAC and MTBS ------------------------------------------------------

################################################################################ fire data & biome
nbac17to20 <- unlist(readOGR('D:\\000_collections\\010_Nighttime Burning\\011_Data\\010_FireData\\Canada\\nbac_1986_to_2020_20210810\\nbac2017_to_2020_20210810.shp'))
mtbs17to20 <- unlist(readOGR('D:\\000_collections\\010_Nighttime Burning\\011_Data\\010_FireData\\America\\mtbs_perimeter_data\\mtbs_2017_to_2020.shp'))

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
raster.brick = brick("D:\\000_collections\\010_Nighttime Burning\\011_Data\\012_Variables\\NA_FFMC\\hFFMC2017_3..nc")
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
NAfire = st_as_sf(NAfire) %>% st_set_crs(4326)


# Load LEO AF data and overpasses -----------------------------------------

MODIS61 = read.csv("015.7 NatureReviewRound1\\LEO_FIRMS_AF data\\DL_FIRE_M-C61_357191/fire_archive_M-C61_357191.csv")
VNPP = read.csv("015.7 NatureReviewRound1\\LEO_FIRMS_AF data\\DL_FIRE_SV-C2_357193/fire_archive_SV-C2_357193.csv")
VNOAA20 =  read.csv("015.7 NatureReviewRound1\\LEO_FIRMS_AF data\\DL_FIRE_J1V-C2_357192/fire_nrt_J1V-C2_357192.csv")

LEO_AF = rbind(MODIS61 %>% dplyr::select(-type),VNPP %>% dplyr::select(-type),VNOAA20) %>% mutate(
  year=as.integer(format(as.Date(acq_date, "%Y-%m-%d"),"%Y")),
  month=as.integer(format(as.Date(acq_date, "%Y-%m-%d"),"%m")),
  day=as.integer(format(as.Date(acq_date, "%Y-%m-%d"),"%d")),
  yday=lubridate::yday(ymd(acq_date)),
  hr=floor(acq_time/100),
  min=acq_time %% 100
)
coordinates(LEO_AF) <- ~ longitude + latitude
LEO_AF = st_as_sf(LEO_AF) %>% st_set_crs(4326)


library(purrr)
folder_path <- dir(path="015.7 NatureReviewRound1\\LEO_Overpasses\\output_OBE days\\",full.names = T,recursive = TRUE,pattern = '(.gpkg)$') 
LEO_OP <- map_df(folder_path, st_read)
LEO_OP$StartDateTime <- with_tz(LEO_OP$StartDateTime, tzone = "UTC")


# LEOs on OBEs, AF and Overpasses -----------------------------------------

OBEsinfo = FD_NLgte4 %>% filter(ONB_Event_YoN=='OBEs')%>% group_by(year,day,ID,lat,long) %>% summarise(count=n()) %>% 
  dplyr::select(-count) %>% rename(yday=day)
OBEsinfo_cp=OBEsinfo
coordinates(OBEsinfo) <- ~ long + lat
OBEsinfo = st_as_sf(OBEsinfo) %>% st_set_crs(4326)

final_result <- data.frame()

for (i in 1:nrow(OBEsinfo)) {
  fireindi <- NAfire %>% filter(ID == OBEsinfo$ID[i])
  leoindi <- LEO_AF %>% filter(year == OBEsinfo$year[i] & (yday == OBEsinfo$yday[i] | yday == OBEsinfo$yday[i] + 1))
  fireindi = st_make_valid(fireindi)
  LEOAF_within_fire <- st_intersection(leoindi, fireindi) %>% dplyr::select(-(Country:Ig_Date), -(firename:source), -lat, -long)

  OBEindi <- OBEsinfo[i, ]
  leoopindi <- LEO_OP %>% filter(year == OBEsinfo$year[i] & (yday == OBEsinfo$yday[i] | yday == OBEsinfo$yday[i] + 1))
  leoopindi = st_make_valid(leoopindi)
  firecenter_within_LEOOP <- st_intersection(leoopindi, OBEindi) %>% dplyr::select(-path, -c(ArchiveSet:yday.1)) %>% mutate(year = as.integer(year))

  LEOAF_within_fire$scantime <- with(LEOAF_within_fire, force_tz(make_datetime(year, month, day, hr, min), tzone = "UTC"))
  firecenter_within_LEOOP <- firecenter_within_LEOOP %>% mutate(EndDateTime = StartDateTime + 300)
  LEOAF_within_fire$satellite[LEOAF_within_fire$satellite == "N"] <- "Vnpp"

  # fire_granule = leoopindi %>% filter(GranuleID %in% firecenter_within_LEOOP$GranuleID)
  # ggplot(world) +
  #   geom_sf() +
  #   geom_sf(data=(fire_granule %>% filter(satellite=='Aqua')),aes(color=satellite),alpha=0.2) +
  #   #geom_sf(data=points, color="red", size=3) +
  #   coord_sf(crs="+proj=laea +x_0=0 +y_0=0 +lon_0=-74 +lat_0=40")
  
  AFjoinOP <- right_join(st_drop_geometry(LEOAF_within_fire), st_drop_geometry(firecenter_within_LEOOP), by = c("satellite", "year", "yday", "ID")) %>%
    mutate(count = ifelse(scantime <= EndDateTime & scantime >= StartDateTime, 1, 0)) %>%
    group_by(satellite, year, yday, StartDateTime, ID) %>%
    summarise(countbyOP = sum(count,na.rm=T)) %>%
    mutate(lat = OBEsinfo_cp[i, ]$lat, long = OBEsinfo_cp[i, ]$long,
           OBE_id = i,OBE_burnday = OBEsinfo_cp[i, ]$yday)

  final_result <- rbind(final_result, AFjoinOP) # 将每次迭代的结果使用 rbind() 函数连接在一起
  print(i)
}

print(final_result)
# write.csv(final_result,'015.7 NatureReviewRound1\\LEO_Overpasses\\output_OBE days\\AFandOPbyOBEs_new.csv')

# sunrise, sunset, dayornight ---------------------------------------------

final_result = read.csv('015.7 NatureReviewRound1\\LEO_Overpasses\\output_OBE days\\AFandOPbyOBEs_new.csv')
final_result <- final_result %>% filter(satellite!="1") %>% mutate(date = as.Date(StartDateTime),
                                                                      StartDateTime = as_datetime(StartDateTime,tz='UTC'))
final_result <- final_result %>% 
  rowwise() %>%
  mutate(sun_times = suncalc::getSunlightTimes(date, lat = lat, lon = long),
         sun_timeslastday = suncalc::getSunlightTimes(date-1, lat = lat, lon = long)) %>%
  ungroup() %>%
  mutate(sunrise = sun_times$sunrise,
         sunset = sun_times$sunset,
         sunsetlastday = sun_timeslastday$sunset) %>% 
  dplyr::select(-sun_times,-sun_timeslastday)
  
final_result = final_result  %>% 
  mutate(dayornight=
         ifelse(((StartDateTime>sunrise&StartDateTime<sunset)|(StartDateTime<sunsetlastday)),'D','N')) %>% 
  mutate(inoroutOBEhr = case_when((yday == OBE_burnday & StartDateTime>sunset) ~ 'in',
                            (yday != OBE_burnday & StartDateTime<sunrise & StartDateTime>sunsetlastday) ~ 'in')) %>% 
  mutate(AFinOBEhr=case_when((inoroutOBEhr=='in'& countbyOP>0)~'AFin',
                             (inoroutOBEhr=='in'& countbyOP==0)~'AFmiss'))

OBE_AFOPbyLEO = final_result %>% 
  group_by(year,ID,OBE_id,satellite,OBE_burnday,lat,long) %>% 
  summarise(numOP=sum(inoroutOBEhr=='in',na.rm = T),
            numAFinOP=sum(AFinOBEhr=='AFin',na.rm=T),
            numAFoutOP=sum(AFinOBEhr=='AFmiss',na.rm=T)) %>% 
  mutate(numofAFmiss=numOP-numAFinOP)
OBE_AFOPbyMODIS = final_result %>% 
  filter(satellite!='Vnpp')%>% 
  group_by(year,ID,OBE_id,OBE_burnday,lat,long) %>% 
  summarise(numOP=sum(inoroutOBEhr=='in',na.rm = T),
            numAFinOP=sum(AFinOBEhr=='AFin',na.rm=T),
            numAFoutOP=sum(AFinOBEhr=='AFmiss',na.rm=T)) %>% 
  mutate(numofAFmiss=numOP-numAFinOP)

# double check with No-Overpass and AF miss in Overpasses -----------------

zero_OP = OBE_AFOPbyLEO %>% filter(numOP==0) %>% mutate(yday=OBE_burnday,year=as.character(year))
zero_OP_1 <- zero_OP[!duplicated(zero_OP[, c("satellite", "yday",'year')]), ]

unique(zero_OP_1$OBE_id)
zerocheck = semi_join( LEO_OP,zero_OP_1,by = c("year", "yday",'satellite')) %>% 
  ungroup() %>%
  group_by(year,yday,satellite) %>% summarise(count=n())

zero_lowlat = zero_OP %>% filter(lat<=50)
## note: zero op by 1 high latitudes with short nighttime window to let satellites pass;
## by 2 mid lat (other all cases except in 1) with incomplete granules of Aqua in granules txt files, please see https://forum.earthdata.nasa.gov/viewtopic.php?f=7&t=2111

# check the miss aqua in Overpasses-----------------------------------------------------
aquain = final_result %>% filter(satellite=='Aqua')
aquainOBEid = unique(aquain$OBE_id)
OBEsinfo$OBEid = 1:nrow(OBEsinfo)
`%nin%` = Negate(`%in%`)
OBEsinfo %>% filter(OBEid %nin% aquainOBEid)
## note: miss 2 Overpasses for Aqua due to missing data in 2020, same reason as before

# single LEO and MODIS statistics -----------------------------------------

LEOstat = OBE_AFOPbyLEO %>% group_by(satellite,numOP,numAFinOP) %>% summarise(count=n()) %>% 
  pivot_wider(names_from = satellite, values_from = count)

MODISstat = OBE_AFOPbyMODIS %>% group_by(numOP,numAFinOP) %>% summarise(count=n())

# cases and plots ---------------------------------------------------------

fire_granule = leoopindi %>% filter(GranuleID %in% firecenter_within_LEOOP$GranuleID)
ggplot(world) +
  geom_sf() +
  geom_sf(data=fire_granule %>% filter(satellite=='Aqua'),aes(color=satellite),alpha=0.2) +
  #geom_sf(data=points, color="red", size=3) +
  coord_sf(crs="+proj=laea +x_0=0 +y_0=0 +lon_0=-74 +lat_0=40")
# LEO ---------------------------------------------------------------------


aqua_folder <- dir(path="015.7 NatureReviewRound1\\LEO_Overpasses\\geoMeta61\\AQUA\\",full.names = T,recursive = TRUE,pattern = '(.txt)$')

aqua = aqua_folder %>%
  tibble(satellite = "Aqua",
         path = .,
         year = substr(basename(path), start = 7, stop = 10),
         month = substr(basename(path), start = 12, stop = 13),
         day = substr(basename(path), start = 15, stop = 16),
         yday = lubridate::yday(ymd(substr(basename(path), start = 7, stop = 16)))) %>%
  dplyr::mutate(geoMeta = purrr::map(path, .f = function(this_path) {
    read_delim(this_path,
               delim = ",",
               skip = 2,
               col_types = cols(
                 #GranuleID = col_character(),
                 StartDateTime = col_datetime(format = ""),
                 ArchiveSet = col_double(),
                 OrbitNumber = col_double(),
                 DayNightFlag = col_character(),
                 EastBoundingCoord = col_double(),
                 NorthBoundingCoord = col_double(),
                 SouthBoundingCoord = col_double(),
                 WestBoundingCoord = col_double(),
                 GRingLongitude1 = col_double(),
                 GRingLongitude2 = col_double(),
                 GRingLongitude3 = col_double(),
                 GRingLongitude4 = col_double(),
                 GRingLatitude1 = col_double(),
                 GRingLatitude2 = col_double(),
                 GRingLatitude3 = col_double(),
                 GRingLatitude4 = col_double()
               ))%>%
      dplyr::rename(GranuleID = `# GranuleID`)

  }))


terra_folder <- dir(path="015.7 NatureReviewRound1\\LEO_Overpasses\\geoMeta61\\TERRA\\",full.names = T,recursive = TRUE,pattern = '(.txt)$')

terra = terra_folder %>%
  tibble(satellite = "Terra",
path = .,
year = substr(basename(path), start = 7, stop = 10),
month = substr(basename(path), start = 12, stop = 13),
day = substr(basename(path), start = 15, stop = 16),
yday = lubridate::yday(ymd(substr(basename(path), start = 7, stop = 16)))) %>%
  dplyr::mutate(geoMeta = purrr::map(path, .f = function(this_path) {
    read_delim(this_path,
               delim = ",",
               skip = 2,
               col_types = cols(
                 #GranuleID = col_character(),
                 StartDateTime = col_datetime(format = ""),
                 ArchiveSet = col_double(),
                 OrbitNumber = col_double(),
                 DayNightFlag = col_character(),
                 EastBoundingCoord = col_double(),
                 NorthBoundingCoord = col_double(),
                 SouthBoundingCoord = col_double(),
                 WestBoundingCoord = col_double(),
                 GRingLongitude1 = col_double(),
                 GRingLongitude2 = col_double(),
                 GRingLongitude3 = col_double(),
                 GRingLongitude4 = col_double(),
                 GRingLatitude1 = col_double(),
                 GRingLatitude2 = col_double(),
                 GRingLatitude3 = col_double(),
                 GRingLatitude4 = col_double()
               ))%>%
      dplyr::rename(GranuleID = `# GranuleID`)
  }))

vnpp_folder <- dir(path="015.7 NatureReviewRound1\\LEO_Overpasses\\geoMeatVIIRS\\NPP_5110\\",full.names = T,recursive = TRUE,pattern = '(.txt)$')

vnpp = vnpp_folder %>%
  tibble(satellite = "Vnpp",
         path = .,
         year = substr(basename(path), start = 10, stop = 13),
         month = substr(basename(path), start = 15, stop = 16),
         day = substr(basename(path), start = 18, stop = 19),
         yday = lubridate::yday(ymd(substr(basename(path), start = 10, stop = 19)))) %>%
  dplyr::mutate(geoMeta = purrr::map(path, .f = function(this_path) {
    read_delim(this_path,
               delim = ",",
               skip = 2,
               col_types = cols(
                 #GranuleID = col_character(),
                 StartDateTime = col_datetime(format = ""),
                 ArchiveSet = col_double(),
                 OrbitNumber = col_double(),
                 DayNightFlag = col_character(),
                 EastBoundingCoord = col_double(),
                 NorthBoundingCoord = col_double(),
                 SouthBoundingCoord = col_double(),
                 WestBoundingCoord = col_double(),
                 GRingLongitude1 = col_double(),
                 GRingLongitude2 = col_double(),
                 GRingLongitude3 = col_double(),
                 GRingLongitude4 = col_double(),
                 GRingLatitude1 = col_double(),
                 GRingLatitude2 = col_double(),
                 GRingLatitude3 = col_double(),
                 GRingLatitude4 = col_double()
               ))%>%
      dplyr::rename(GranuleID = `# GranuleID`)
  }))

geoMeta_df <- rbind(aqua,terra,vnpp)

OBE_yryday = FD_NLgte4 %>% filter(ONB_Event_YoN=='OBEs')%>% group_by(year,day) %>% summarise(count=n()) %>%
  dplyr::select(-count) %>% rename(yday=day) %>% mutate(year=as.character(year))
new_row <- OBE_yryday
new_row$yday <- new_row$yday + 1
OBE_yryday <- rbind(OBE_yryday, new_row) %>% distinct()
rownames(OBE_yryday) <- NULL

geoMeta_list <-
  geoMeta_df %>% right_join(OBE_yryday) %>%
  dplyr::group_by(satellite,year,month,day) %>%
  dplyr::group_split()
geoMeta_num = geoMeta_df %>% right_join(OBE_yryday) %>%
  dplyr::group_by(satellite,year,month,day)
glb_ext = st_polygon(
  list(
    cbind(
      c(-179.999, 179.999, 179.999, -179.999, -179.999),
      c(90, 90, -90, -90, 90))
  )
) %>%
  st_sfc(crs = 4326) %>%
  st_sf()
plot(glb_ext)

# reconstruct granules ----------------------------------------------------

Reconstruction_final <- function(i, overwrite = FALSE) {
  library(sf)
  library(rnaturalearth)
  library(tidyverse)
  library(geosphere)
  library(ggplot2)
  library(foreach)
  library(doParallel)
  library(parallel)
  library(tcltk)
  library(doSNOW)
  geoMeta_month=geoMeta_list[[i]]
  # geoMeta_month = geoMeta_list[[3]]
  this_year <- unique(geoMeta_month$year)
  this_month <- unique(geoMeta_month$month)
  this_day <- unique(geoMeta_month$day)
  this_satellite <- unique(geoMeta_month$satellite)
  if(this_satellite=='Vnpp'){
    this_file <- paste0(this_satellite,"_", this_year, "_",this_month,"_",this_day,"_6-minute-footprints.gpkg")
  }else{
    this_file <- paste0(this_satellite,"_", this_year, "_",this_month,"_",this_day,"_5-minute-footprints.gpkg")
  }

  this_path <- file.path("015.7 NatureReviewRound1\\LEO_Overpasses\\output_OBE days\\", this_satellite,this_file)

  if(!file.exists(this_path) | overwrite) {

    #geoMeta_month = geoMeta_list[[1]]

    geoMeta <- tidyr::unnest(geoMeta_month, cols = geoMeta)

    footprints <-
      pmap(geoMeta, .f = Reconstruction_granule)

    footprints <- do.call(what = "rbind", args = footprints)

    sf::st_write(obj = footprints, dsn = this_path, delete_dsn = TRUE)
  }

}

Reconstruction_granule <- function(satellite, path, year, month, day, yday, GranuleID, StartDateTime, ArchiveSet, OrbitNumber, DayNightFlag, EastBoundingCoord, NorthBoundingCoord, SouthBoundingCoord, WestBoundingCoord, GRingLongitude1, GRingLongitude2, GRingLongitude3, GRingLongitude4, GRingLatitude1, GRingLatitude2, GRingLatitude3, GRingLatitude4) {
  # i=576
  # k=75
  # EastBoundingCoord = geoMeta_list[[i]][[7]][[1]]$EastBoundingCoord[k]
  # NorthBoundingCoord = geoMeta_list[[i]][[7]][[1]]$NorthBoundingCoord[k]
  # SouthBoundingCoord = geoMeta_list[[i]][[7]][[1]]$SouthBoundingCoord[k]
  # WestBoundingCoord = geoMeta_list[[i]][[7]][[1]]$WestBoundingCoord[k]
  # GRingLongitude1 = geoMeta_list[[i]][[7]][[1]]$GRingLongitude1[k]
  # GRingLongitude2 = geoMeta_list[[i]][[7]][[1]]$GRingLongitude2[k]
  # GRingLongitude3 = geoMeta_list[[i]][[7]][[1]]$GRingLongitude3[k]
  # GRingLongitude4 = geoMeta_list[[i]][[7]][[1]]$GRingLongitude4[k]
  # GRingLatitude1 = geoMeta_list[[i]][[7]][[1]]$GRingLatitude1[k]
  # GRingLatitude2 = geoMeta_list[[i]][[7]][[1]]$GRingLatitude2[k]
  # GRingLatitude3 = geoMeta_list[[i]][[7]][[1]]$GRingLatitude3[k]
  # GRingLatitude4 = geoMeta_list[[i]][[7]][[1]]$GRingLatitude4[k]

  library(sf)
  library(rnaturalearth)
  library(tidyverse)
  library(geosphere)
  library(ggplot2)
  library(foreach)
  library(doParallel)
  library(parallel)
  library(tcltk)
  library(doSNOW)
  point1 <- c(GRingLongitude1, GRingLatitude1)
  point2 <- c(GRingLongitude2, GRingLatitude2)
  point3 <- c(GRingLongitude3, GRingLatitude3)
  point4 <- c(GRingLongitude4, GRingLatitude4)


  points <- st_as_sf(data.frame(lon = c(point1[1], point2[1], point3[1], point4[1]),
                                lat = c(point1[2], point2[2], point3[2], point4[2])),
                     coords = c("lon", "lat"), crs = 4326)
  init <- points %>% as("Spatial")

  dest <- bind_rows(points[-1,], points[1,]) %>% as("Spatial")
  lines <- gcIntermediate(
    init,
    dest,
    sp = TRUE,
    addStartEnd = TRUE,
    n = 100,
    breakAtDateLine = TRUE
  )
  if (any(abs(GRingLongitude1)>175|abs(GRingLongitude2)>175|abs(GRingLongitude3)>175|abs(GRingLongitude4)>175)) {
    lines <- gcIntermediate(
      init,
      dest,
      sp = TRUE,
      addStartEnd = TRUE,
      n = 1000,
      breakAtDateLine = TRUE
    )
  }
  if(any(abs(GRingLongitude1)>179.9|abs(GRingLongitude2)>179.9|abs(GRingLongitude3)>179.9|abs(GRingLongitude4)>179.9)) {
    if (abs(GRingLongitude1)>179.9) {GRingLongitude1=ifelse(GRingLongitude1<0,-180,180)}
    if (abs(GRingLongitude2)>179.9) {GRingLongitude2=ifelse(GRingLongitude2<0,-180,180)}
    if (abs(GRingLongitude3)>179.9) {GRingLongitude3=ifelse(GRingLongitude3<0,-180,180)}
    if (abs(GRingLongitude4)>179.9) {GRingLongitude4=ifelse(GRingLongitude4<0,-180,180)}
    point1 <- c(GRingLongitude1, GRingLatitude1)
    point2 <- c(GRingLongitude2, GRingLatitude2)
    point3 <- c(GRingLongitude3, GRingLatitude3)
    point4 <- c(GRingLongitude4, GRingLatitude4)


    points <- st_as_sf(data.frame(lon = c(point1[1], point2[1], point3[1], point4[1]),
                                  lat = c(point1[2], point2[2], point3[2], point4[2])),
                       coords = c("lon", "lat"), crs = 4326)
    init <- points %>% as("Spatial")

    dest <- bind_rows(points[-1,], points[1,]) %>% as("Spatial")
    lines <- gcIntermediate(
      init,
      dest,
      sp = TRUE,
      addStartEnd = TRUE,
      n = 100,
      breakAtDateLine = TRUE
    )
  }

  if(WestBoundingCoord == -180 | EastBoundingCoord == 180) {
    lines_seg <- st_union(st_as_sf(lines)) %>%
      st_sfc(crs = 4326)
    if (lines@bbox[1,1]==-180&lines@bbox[1,2]==180) {
      split_globe <- lwgeom::st_split(x = glb_ext, y = lines_seg)
      polys <- st_collection_extract(x = split_globe, type = "POLYGON")
      granule = polys[which.min(st_area(polys)),]
    }else{
      glb_ext_sp = st_polygon(list(cbind(c(lines@bbox[1,1]+0.001, lines@bbox[1,2]-0.001,lines@bbox[1,2]-0.001, lines@bbox[1,1]+0.001, lines@bbox[1,1]+0.001),
                                         c(90, 90, -90, -90, 90)))) %>% st_sfc(crs = 4326) %>% st_sf()
      split_globe <- lwgeom::st_split(x = glb_ext_sp, y = lines_seg)
      polys <- st_collection_extract(x = split_globe, type = "POLYGON")
      granule = polys[which.min(st_area(polys)),]
    }

  }else{
    if(WestBoundingCoord < EastBoundingCoord){
      granule <- st_union(st_as_sf(lines)) %>% st_cast("POLYGON")%>%
        st_transform(crs = 4326) %>%
        st_sf()
    }else{
      lines_seg = sf::st_linestring(x = matrix(rbind(c(0, 90), c(0, -90)), ncol = 2)) %>%
        sf::st_sfc(crs = 4326)
      split_lines <- lwgeom::st_split(x = st_as_sf(lines), y = lines_seg)
      lines_split <- st_collection_extract(x = split_lines, type = "LINESTRING")%>%
        dplyr::mutate(centroid_lon = st_coordinates(st_centroid(.))[, 1])

      east_poly <- tryCatch(
        {
          lines_split %>%
            dplyr::filter(centroid_lon < 0) %>%
            dplyr::summarize() %>% st_cast("POLYGON")%>%
            st_transform(crs = 4326) %>% sf::st_geometry() %>%
            st_sf()
        },
        error = function(err) {
          message("Error occurred in the first block: ", conditionMessage(err))
          NULL
        }
      )
      west_poly <- tryCatch(
        {
          lines_split %>%
            dplyr::filter(centroid_lon >= 0)  %>%
            dplyr::summarize() %>% st_cast("POLYGON")%>%
            st_transform(crs = 4326) %>%
            sf::st_geometry()%>%
            st_sf()
        },
        error = function(err) {
          message("Error occurred in the second block: ", conditionMessage(err))
          NULL
        }
      )

      granule <- tryCatch(
        {
          if (!is.null(east_poly) && !is.null(west_poly)) {
            st_union(st_as_sf(lines)) %>% st_cast("POLYGON")%>%
              st_transform(crs = 4326) %>%
              st_sf()
          } else if (!is.null(east_poly)) {
            east_poly
          } else if (!is.null(west_poly)) {
            west_poly
          } else {
            NULL
          }
        },
        error = function(err) {
          message("Error occurred in the footprint block: ", conditionMessage(err))
          NULL
        }
      )


    }

  }
  obj <-
    tibble(satellite, path, year, month, day, yday, GranuleID,StartDateTime, ArchiveSet, OrbitNumber, DayNightFlag, EastBoundingCoord, NorthBoundingCoord, SouthBoundingCoord, WestBoundingCoord, GRingLongitude1, GRingLongitude2, GRingLongitude3, GRingLongitude4, GRingLatitude1, GRingLatitude2, GRingLatitude3, GRingLatitude4, geometry = granule$geometry) %>%
    st_sf() %>%
    st_cast("MULTIPOLYGON")
  return(obj)
}



# Parallel run ------------------------------------------------------------

#dir.create("Output/", recursive = TRUE)
log_error <- function(task_id, error_msg) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  error_log <- paste("Error at task", task_id, "-", error_msg)
  write(error_log, file = "015.7 NatureReviewRound1\\LEO_Overpasses\\output_OBE days\\error_log_daily.txt", append = TRUE)
  cat(timestamp, error_log, "\n")
}
no_cores<-detectCores()-4
cl <- makeSOCKcluster(no_cores)
registerDoSNOW(cl)
pb <- txtProgressBar(min=1, max=length(geoMeta_list), style=3)#length(nbac86to16)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress=progress)

start_time <- Sys.time()
granule_write <- foreach(i=1:length(geoMeta_list), .options.snow = opts) %dopar%  {
  library(sf)
  library(rnaturalearth)
  library(tidyverse)
  library(geosphere)
  library(ggplot2)
  library(foreach)
  library(doParallel)
  library(parallel)
  library(tcltk)
  library(doSNOW)
  tryCatch(
    Reconstruction_final(i),
    error = function(e) {
      log_error((geoMeta_list[[i]])$path, conditionMessage(e))
      NULL
    }
  )}
stopCluster(cl)
end_time <- Sys.time()
end_time - start_time
