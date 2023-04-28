rm(list = ls())
getwd()
setwd("D:\\")

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
windowsFonts(Times = windowsFont("Times New Roman"))

# load data
Nights_FeatDesi_all <-
  read.csv(
    'FeatureDesign\\FD_All_by2FireHr_gt1000.csv'
  )
# all the rows with na were found in bui:fwi, and var_nightsd, like vpd_nightsd
# if rows with NightLength greater than 1, na was caused by daily variables, bui:fwi
# if rows with NightLength equals 1, na was caused by night sd
Nights_FeatDesi_all <-
  Nights_FeatDesi_all %>% dplyr::select(
    -inci_name,-AFTempDayMeanFire,-AFareaDayMeanFire,-AFTempNightMeanFire,-AFareaNightMeanFire
  )
ONBwithna <-
  Nights_FeatDesi_all %>%
  filter(NightLength >= 4, ONB_Event_YoN == 1) %>%
  filter_all(any_vars(is.na(.)))
NonEwithna <-
  Nights_FeatDesi_all %>%
  filter(NightLength >= 4, ONB_Event_YoN == 0) %>%
  filter_all(any_vars(is.na(.)))
which(is.na(ONBwithna[1, ]))
which(is.na(NonEwithna[1, ]))

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
  )) %>% mutate(ONB_Event_YoN = ifelse(ONB_Event_YoN == 1, 'OBEs',
                                       'Non events')) %>%
  mutate(biome_4zones = case_when(
    (biome >= 11 & biome <= 16) ~ 'tropical',
    (biome >= 21 & biome <= 25) ~ 'subtropical',
    (biome >= 31 & biome <= 35) ~ 'temperate',
    (biome >= 41 & biome <= 43) ~ 'boreal'
  )) %>%
  mutate(WorEzone = ifelse((long < (-90)), 'W', 'E')) %>%
  mutate(
    biome_4areas = case_when(
      biome_4zones == 'boreal' ~ 'Boreal',
      (biome == 35 & WorEzone == 'W') ~ 'Temperate mountain system',
      (biome == 25 & WorEzone == 'W') ~ 'Subtropical mountain system',
      (biome == 34 & WorEzone == 'W') ~ 'Temperate desert'
    )
  ) %>%
  mutate(season = case_when((month >= 3 & month <= 5) ~ 'spring',
                            (month >= 6 & month <= 8) ~ 'summer',
                            (month >= 9 & month <= 11) ~ 'fall',
                            (month == 12 |
                               month == 1 | month == 2) ~ 'winter'
  ))

x = FD_NLgte4 %>% filter(ONB_Event_YoN == 'OBEs')
y = unique(x$ID)
FD_NLgte4 <-
  FD_NLgte4 %>% mutate(ONB_FIRE_YoN = ifelse((ID %in% y), 1, 0))

# Extent of OBEs + Figure 1----------------------------------------------------------

################################################################################ fire data & biome
nbac17to20 <-
  unlist(
    readOGR(
      'D:\\nbac2017_to_2020_20210810.shp'
    )
  )
mtbs17to20 <-
  unlist(
    readOGR(
      'D:\\mtbs_2017_to_2020.shp'
    )
  )

####NBAC
nbac17to20$Country <- 'CAN'
nbac17to20$SDATE <- nbac17to20$SDATE
nbac17to20$EDATE <- nbac17to20$EDATE
nbac17to20$AFSDATE <- nbac17to20$AFSDATE
nbac17to20$AFEDATE <- nbac17to20$AFEDATE
nbac17to20$firename <-
  paste(
    nbac17to20$YEAR,
    '_',
    nbac17to20$AGENCY,
    '_',
    nbac17to20$NFIREID,
    '_',
    nbac17to20$POLY_HA,
    sep = ''
  )
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
mtbs17to20$YEAR <-
  as.integer(format(as.Date(mtbs17to20$Ig_Date, "%Y/%m/%d"), "%Y"))
mtbs17to20$POLY_HA <- as.numeric(mtbs17to20$BurnBndAc) * 0.404686
mtbs17to20$firename <-
  paste(
    mtbs17to20$YEAR,
    '_',
    mtbs17to20$Incid_Name,
    '_',
    mtbs17to20$Event_ID,
    '_',
    mtbs17to20$POLY_HA
  )
mtbs17to20$fireCausOrType <- mtbs17to20$Incid_Type
mtbs17to20$source <- 'MTBS'
mtbs17to20$inci_name <- mtbs17to20$Incid_Name

##aline with coords variables and fire data
raster.brick = brick(
  "D:\\hFFMC2017_3..nc"
)
raster.brick = rotate(raster.brick)
nbac17to20 = spTransform(nbac17to20, crs(raster.brick))
mtbs17to20 = spTransform(mtbs17to20, crs(raster.brick))

##combine
NAfire <- bind(nbac17to20, mtbs17to20)
rm(nbac17to20, mtbs17to20)
NAfire <-
  NAfire[, c(
    'Country',
    'YEAR',
    'SDATE',
    'EDATE',
    'AFSDATE',
    'AFEDATE',
    'Ig_Date',
    'POLY_HA',
    'firename',
    'fireCausOrType',
    'source',
    'inci_name'
  )]

NAfire$long <- coordinates(NAfire)[, 1]
NAfire$lat <- coordinates(NAfire)[, 2]
NAfire <- NAfire[NAfire$POLY_HA > 200, ]

##remove the duplicated ones
coord_poly <-
  lapply(NAfire@polygons, function(x) {
    lapply(x@Polygons, function(x) {
      coordinates(x)
    })
  })
NAfire <- NAfire[!duplicated(coord_poly), ]

##separate and combine
NAfire2017 <- NAfire[NAfire$YEAR == 2017, ]
NAfire2017$seq <- 1:length(NAfire2017)
NAfire2018 <- NAfire[NAfire$YEAR == 2018, ]
NAfire2018$seq <- 1:length(NAfire2018)
NAfire2019 <- NAfire[NAfire$YEAR == 2019, ]
NAfire2019$seq <- 1:length(NAfire2019)
NAfire2020 <- NAfire[NAfire$YEAR == 2020, ]
NAfire2020$seq <- 1:length(NAfire2020)

NAfire <- bind(NAfire2017, NAfire2018, NAfire2019, NAfire2020)
NAfire$ID <- 1:length(NAfire)
NAfire$YEAR <- as.numeric(NAfire$YEAR)
length(NAfire)
##start and end date
dfNAfires <-
  NAfire@data %>% rename(year = YEAR) %>% mutate(year = as.numeric(year))
dfNAfires <-
  dfNAfires %>% mutate(
    startdate = pmin(
      as.numeric(strftime(SDATE, format = "%j")),
      as.numeric(strftime(AFSDATE, format = "%j")),
      as.numeric(strftime(Ig_Date, format = "%j")),
      na.rm = T
    ),
    enddate = pmax(as.numeric(strftime(EDATE, format = "%j")), as.numeric(strftime(AFEDATE, format = "%j")), na.rm = T)
  ) %>%
  mutate(across(startdate:enddate,  ~ replace_na(.x, 999)))

Fire1000 = dfNAfires %>% filter(POLY_HA >= 1000)
CAN = Fire1000 %>% filter(Country == 'CAN')
CANstmiss = CAN %>% filter(startdate == 999)
CANendmiss = CAN %>% filter(enddate == 999)
USA = Fire1000 %>% filter(Country == 'USA')
USAstmiss = USA %>% filter(startdate == 999)
USAendmiss = USA %>% filter(enddate == 999)
OBfires = FD_NLgte4 %>% group_by(year, ID, seq) %>%
  summarise(year = year[1], ID = ID[1])
OBFinfo = left_join(OBfires, Fire1000)

usedCAN = OBFinfo %>% filter(Country == 'CAN')
usedCANstmiss = usedCAN %>% filter(startdate == 999)
usedCANendmiss = usedCAN %>% filter(enddate == 999)
usedUSA = OBFinfo %>% filter(Country == 'USA')
usedUSAstmiss = usedUSA %>% filter(startdate == 999)
usedUSAendmiss = usedUSA %>% filter(enddate == 999)


ONBEvents <- FD_NLgte4 %>% filter(ONB_Event_YoN == 'OBEs') %>%
  group_by(year, seq) %>%
  mutate(counts = n()) %>%
  group_by(year, seq, season) %>%
  summarise(
    year = year[1],
    seq = seq[1],
    season = season[1],
    counts = counts[1],
    month = month[1],
    biome = biome[1],
    long = long[1],
    lat = lat[1],
    ID = ID[1]
  )

OF.shp <- NAfire[(NAfire$ID) %in% (ONBEvents$ID), ]
OF.df <- OF.shp@data %>% dplyr::select(lat, long)

IDAF = setdiff(FD_NLgte4$ID, ONBEvents$ID)
AF.shp <- NAfire[(NAfire$ID) %in% (IDAF), ]
AF.df <- AF.shp@data %>% dplyr::select(lat, long)

OF.sf <- st_as_sf(OF.df,
                  coords = c("long", "lat"),
                  crs = 4326 ,
                  agr = "constant")
AF.sf <- st_as_sf(AF.df,
                  coords = c("long", "lat"),
                  crs = 4326 ,
                  agr = "constant")
# without biome bg
shp.biome <-
  st_read(
    'D:\\fire_biomes_USCA.shp'
  )
shp.biome <-
  shp.biome %>% filter(gez_name != 'Water', gez_name != "Tropical dry forest") %>%
  mutate(
    gez_name = replace(gez_name, gez_name == 'Boreal coniferous forest', 'Boreal'),
    gez_name = replace(gez_name, gez_name == "Boreal mountain system", 'Boreal'),
    gez_name = replace(gez_name, gez_name == "Boreal tundra woodland", 'Boreal')
  )

st_crs(shp.biome)
shp.us <-
  st_read(
    'D:\\cb_2018_us_state_20m_CONUS_AK.shp'
  )
shp.ca <-
  st_read(
    'D:\\lpr_000b16a_e.shp'
  )
st_crs(shp.ca)



OBE = FD_NLgte4 %>% filter(ONB_Event_YoN == 'OBEs') %>%
  group_by(year, seq) %>%
  mutate(counts = n()) %>%
  group_by(year, seq, season) %>%
  summarise(
    year = year[1],
    seq = seq[1],
    season = season[1],
    counts = counts[1],
    month = month[1],
    biome = biome[1],
    long = long[1],
    lat = lat[1],
    ID = ID[1]
  ) %>% group_by(year, seq) %>% summarise(
    year = year[1],
    seq = seq[1],
    countss = n(),
    month = month[1],
    biome = biome[1],
    long = long[1],
    lat = lat[1],
    ID = ID[1]
  )
OBE_cp = FD_NLgte4 %>% filter(ONB_Event_YoN == 'OBEs') %>%
  group_by(year, seq) %>%
  mutate(counts = n()) %>%
  group_by(year, seq, season) %>%
  summarise(
    year = year[1],
    seq = seq[1],
    season = season[1],
    counts = counts[1],
    month = month[1],
    biome = biome[1],
    long = long[1],
    lat = lat[1],
    ID = ID[1]
  )


OBEsingleseason = OBE %>% filter(countss == 1) %>% left_join(OBE_cp, by = c("year", "seq",  "month", "biome", "long", "lat", "ID"))
OBEmultiseason1 = OBE %>% filter(countss > 1)
OBEmultiseason = OBE_cp %>% filter(ID %in% (OBEmultiseason1$ID))


OBEsingleseason_Sf <-
  st_as_sf(
    OBEsingleseason,
    coords = c("long", "lat"),
    crs = 4326 ,
    agr = "constant"
  ) %>%
  mutate(season = case_when((month >= 3 & month <= 5) ~ 'spring',
                            (month >= 6 & month <= 8) ~ 'summer',
                            (month >= 9 & month <= 11) ~ 'fall',
                            (month == 12 |
                               month == 1 | month == 2) ~ 'winter'
  ))
OBEsingleseason_Sf$season <-
  factor(OBEsingleseason_Sf$season,
         levels = c('spring', 'summer', 'fall', 'winter'))

OBEmultiseason_Sf <-
  st_as_sf(
    OBEmultiseason,
    coords = c("long", "lat"),
    crs = 4326 ,
    agr = "constant"
  ) %>%
  mutate(season = case_when((month >= 3 & month <= 5) ~ 'spring',
                            (month >= 6 & month <= 8) ~ 'summer',
                            (month >= 9 & month <= 11) ~ 'fall',
                            (month == 12 |
                               month == 1 | month == 2) ~ 'winter'
  ))
OBEmultiseason_Sf$season <-
  factor(OBEmultiseason_Sf$season,
         levels = c('spring', 'summer', 'fall', 'winter'))

# plot
library(sf)
library("viridis")
nc_points_jittered <- st_jitter(ONBEvents_Sf, amount = 0.5)
nc_mulp_jittered <- st_jitter(OBEmultiseason_Sf, amount = 0.5)



shp.biome$gez_name <-
  factor(
    shp.biome$gez_name,
    levels = c(
      'Boreal',
      "Temperate continental forest",
      "Temperate desert",
      "Temperate mountain system",
      "Temperate oceanic forest",
      "Temperate steppe",
      "Subtropical desert",
      "Subtropical dry forest" ,
      "Subtropical humid forest",
      "Subtropical mountain system",
      "Subtropical steppe",
      "Tropical moist forest",
      "Polar"
    )
  )

windowsFonts(Times = windowsFont("Times New Roman"))


## add fire cases
#creek fire
ONB_SingleEvent <-
  FD_NLgte4 %>% filter(ONB_Event_YoN == 'OBEs') %>% group_by(year, seq) %>% filter(n() ==
                                                                                     1)
ONB_multiEvent <-
  FD_NLgte4 %>% filter(ONB_Event_YoN == 'OBEs') %>% group_by(year, seq) %>% filter(n() >
                                                                                     1)

ONB_stat <-
  FD_NLgte4 %>% filter(ONB_Event_YoN == 'OBEs') %>% group_by(year, seq, ID, biome, country, lat, long, POLY_HA) %>% mutate(daydiff =
                                                                                                                             max(day) - min(day)) %>%
  summarise(counts = n(), daydiff = mean(daydiff)) %>% mutate(daydiff.avg =
                                                                case_when((counts == 1) ~ 0, (counts >= 1) ~ ((daydiff + 1) / counts)))
ONB_stat1 <-
  ONB_stat %>% group_by(counts) %>% summarise(
    ncounts = n(),
    events = sum(counts),
    daysum = sum(daydiff) + ncounts
  ) %>% mutate(daydiff.avg = daysum / events)
ONBselected <- ONB_stat %>% filter(counts >= 10)
i = 12
yearused = ONBselected[i, ]$year
seqused = ONBselected[i, ]$seq
Fireindi <- NAfire2020[NAfire2020$seq == seqused, ]
Fireindi_sf <- as(Fireindi, "sf")
creek_sf <-
  st_as_sf(Fireindi@data, coords = c('long', 'lat')) %>% st_set_crs(4326)

#august
i = 17
yearused = ONBselected[i, ]$year
seqused = ONBselected[i, ]$seq
Fireindi <- NAfire2020[NAfire2020$seq == seqused, ]
Fireindi_sf <- as(Fireindi, "sf")
august_sf <-
  st_as_sf(Fireindi@data, coords = c('long', 'lat')) %>% st_set_crs(4326)

#mcmillan
ONBselected <- ONB_stat %>% filter(counts >= 5)
i = 24
yearused = ONBselected[i, ]$year
seqused = ONBselected[i, ]$seq
Fireindi <- NAfire2019[NAfire2019$seq == seqused, ]
Fireindi_sf <- as(Fireindi, "sf")
mcmillant_sf <-
  st_as_sf(Fireindi@data, coords = c('long', 'lat')) %>% st_set_crs(4326)


p = ggplot() +
  
  geom_sf(
    data = shp.biome,
    aes(fill = gez_name),
    colour = NA,
    alpha = 0.7
  ) + #,aes(fill=gez_name)
  geom_sf(
    data = shp.ca,
    fill = NA,
    alpha = 0.2,
    size = 0.25,
    color = 'gray70'
  ) +
  geom_sf(
    data = shp.us,
    fill = NA,
    alpha = 0.2,
    size = 0.25,
    color = 'gray70'
  ) +
  #geom_sf(data = ONBEvents_Sf, aes(color=as.factor(season),size=counts),alpha = 0.9)+
  geom_sf(
    data = AF.sf,
    shape = 17,
    color = 'gray',
    size = 1
  ) +
  geom_sf(data = nc_mulp_jittered,
          aes(color = season, size = counts),
          alpha = 0.8) +
  geom_sf(data = OBEsingleseason_Sf,
          aes(color = season, size = counts),
          alpha = 0.8) +
  # geom_sf(data = nc_points_jittered, aes(color=as.factor(season),size=counts),alpha = 0.8) +
  geom_sf(
    data = OF.sf,
    shape = 17,
    color = 'black',
    size = 0.5
  ) +
  geom_sf(
    data = creek_sf,
    shape = 17,
    color = 'darkgreen',
    size = 2,
    alpha = 0.8
  ) +
  geom_sf(
    data = august_sf,
    shape = 17,
    color = 'darkgreen',
    size = 2,
    alpha = 0.8
  ) +
  geom_sf(
    data = mcmillant_sf,
    shape = 17,
    color = 'darkgreen',
    size = 2,
    alpha = 0.8
  ) +
  coord_sf(crs = 3979, expand = F) + #crs = st_crs(4326)
  #geom_point(data = ONBEvents,aes(x = long, y = lat,color=as.factor(year)),size=1.5,alpha = 1) +
  scale_shape(solid = FALSE) +
  #geom_hex(data = ONBEvents_latlong,aes(x = long, y = lat),alpha = 0.5) +
  theme(
    legend.box = "vertical",
    legend.position = 'right',
    legend.direction = "vertical"
  ) +
  scale_color_manual(values = c('orange', 'yellow', 'red', 'blue')) + #values=c('blue','red','green4','purple')
  #scale_color_viridis(discrete = TRUE,'inferno')+
  #scale_fill_manual(values=viridis(19))+
  scale_fill_manual(
    values = c(
      'slateblue3',
      'lightgoldenrod4',
      'lightgoldenrod3',
      'lightgoldenrod2',
      'lightgoldenrod1',
      'moccasin',
      "olivedrab4",
      "olivedrab3",
      "olivedrab2",
      "olivedrab1",
      'lightgreen',
      'palevioletred4',
      'gray95'
    )
  ) +
  guides(
    colour = guide_legend(
      order = 2,
      title = "Season of OBE",
      nrow = 2
    ),
    size = guide_legend(
      order = 3,
      title = "Number of OBE",
      nrow = 2
    ),
    fill = guide_legend(order = 1, title = "Biome")
  ) +
  scale_x_continuous(breaks = seq(-180,-50, by = 20)) +
  scale_y_continuous(breaks = seq(25, 90, by = 5)) +
  theme_classic() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    strip.text.x = element_text(size = 7, color = "black", face = "plain"),
    plot.title = element_text(
      color = "black",
      size = 7,
      face = "bold",
      vjust = -1
    ),
    text = element_text(size = 7),
    #family='Times',
    legend.key.size = unit(0.15, 'inches')
  )
p

# x <- paste0('015.5_Plots\\plot01_OBEs Distribution777','.pdf')
# ggsave(plot=p,x,width = 180,height =180,units = 'mm')



# statistics --------------------------------------------------------------


# distribution statistics
OBF <-
  FD_NLgte4 %>% filter(ONB_Event_YoN == 'OBEs') %>% group_by(year, seq) %>% mutate(counts =
                                                                                     n()) %>%
  summarise(
    year = year[1],
    seq = seq[1],
    season = season[1],
    counts = counts[1],
    month = month[1],
    biome = biome[1],
    long = long[1],
    lat = lat[1]
  ) %>%
  mutate(season = case_when((month >= 3 & month <= 5) ~ 'spring',
                            (month >= 6 & month <= 8) ~ 'summer',
                            (month >= 9 & month <= 11) ~ 'fall',
                            (month == 12 |
                               month == 1 | month == 2) ~ 'winter'
  )) %>%
  mutate(
    biome = case_when(
      (biome == 12) ~ 'Tropical moist forest',
      (biome == 21) ~ 'Subtropical humid forest',
      (biome == 22) ~ 'Subtropical dry forest',
      (biome == 23) ~ 'Subtropical steppe',
      (biome == 24) ~ 'Subtropical desert',
      (biome == 25) ~ 'Subtropical mountain system',
      (biome == 31) ~ 'Temperate oceanic forest',
      (biome == 32) ~ 'Temperate continental forest',
      (biome == 33) ~ 'Temperate steppe',
      (biome == 34) ~ 'Temperate desert',
      (biome == 35) ~ 'Temperate mountain system',
      (biome == 41) ~ 'Boreal',
      (biome == 42) ~ 'Boreal',
      (biome == 43) ~ 'Boreal',
      (biome == 50) ~ 'Polar'
      
    )
  )
FirewithOBE <- OBF %>% group_by(biome) %>% summarise(nfwobe = n())

OBE <-
  FD_NLgte4 %>% filter(ONB_Event_YoN == 'OBEs') %>% group_by(year, seq) %>% mutate(counts =
                                                                                     n()) %>%
  mutate(season = case_when((month >= 3 & month <= 5) ~ 'spring',
                            (month >= 6 & month <= 8) ~ 'summer',
                            (month >= 9 & month <= 11) ~ 'fall',
                            (month == 12 |
                               month == 1 | month == 2) ~ 'winter'
  )) %>%
  mutate(
    biome = case_when(
      (biome == 12) ~ 'Tropical moist forest',
      (biome == 21) ~ 'Subtropical humid forest',
      (biome == 22) ~ 'Subtropical dry forest',
      (biome == 23) ~ 'Subtropical steppe',
      (biome == 24) ~ 'Subtropical desert',
      (biome == 25) ~ 'Subtropical mountain system',
      (biome == 31) ~ 'Temperate oceanic forest',
      (biome == 32) ~ 'Temperate continental forest',
      (biome == 33) ~ 'Temperate steppe',
      (biome == 34) ~ 'Temperate desert',
      (biome == 35) ~ 'Temperate mountain system',
      (biome == 41) ~ 'Boreal',
      (biome == 42) ~ 'Boreal',
      (biome == 43) ~ 'Boreal',
      (biome == 50) ~ 'Polar'
      
    )
  )
Totalobe <- OBE %>% group_by(biome) %>% summarise(ntobe = n())
OBEbyseason <-
  OBE %>% group_by(biome, season) %>% summarise(nobebys = n())


ONB_SingleEvent <-
  FD_NLgte4 %>% filter(ONB_Event_YoN == 'OBEs') %>% group_by(year, seq) %>% filter(n() ==
                                                                                     1) %>%   mutate(
                                                                                       biome = case_when(
                                                                                         (biome == 12) ~ 'Tropical moist forest',
                                                                                         (biome == 21) ~ 'Subtropical humid forest',
                                                                                         (biome == 22) ~ 'Subtropical dry forest',
                                                                                         (biome == 23) ~ 'Subtropical steppe',
                                                                                         (biome == 24) ~ 'Subtropical desert',
                                                                                         (biome == 25) ~ 'Subtropical mountain system',
                                                                                         (biome == 31) ~ 'Temperate oceanic forest',
                                                                                         (biome == 32) ~ 'Temperate continental forest',
                                                                                         (biome == 33) ~ 'Temperate steppe',
                                                                                         (biome == 34) ~ 'Temperate desert',
                                                                                         (biome == 35) ~ 'Temperate mountain system',
                                                                                         (biome == 41) ~ 'Boreal',
                                                                                         (biome == 42) ~ 'Boreal',
                                                                                         (biome == 43) ~ 'Boreal',
                                                                                         (biome == 50) ~ 'Polar'
                                                                                         
                                                                                       )
                                                                                     )
ONB_multiEvent <-
  FD_NLgte4 %>% filter(ONB_Event_YoN == 'OBEs') %>% group_by(year, seq) %>% filter(n() >
                                                                                     1) %>%   mutate(
                                                                                       biome = case_when(
                                                                                         (biome == 12) ~ 'Tropical moist forest',
                                                                                         (biome == 21) ~ 'Subtropical humid forest',
                                                                                         (biome == 22) ~ 'Subtropical dry forest',
                                                                                         (biome == 23) ~ 'Subtropical steppe',
                                                                                         (biome == 24) ~ 'Subtropical desert',
                                                                                         (biome == 25) ~ 'Subtropical mountain system',
                                                                                         (biome == 31) ~ 'Temperate oceanic forest',
                                                                                         (biome == 32) ~ 'Temperate continental forest',
                                                                                         (biome == 33) ~ 'Temperate steppe',
                                                                                         (biome == 34) ~ 'Temperate desert',
                                                                                         (biome == 35) ~ 'Temperate mountain system',
                                                                                         (biome == 41) ~ 'Boreal',
                                                                                         (biome == 42) ~ 'Boreal',
                                                                                         (biome == 43) ~ 'Boreal',
                                                                                         (biome == 50) ~ 'Polar'
                                                                                         
                                                                                       )
                                                                                     )

OBEsingle <-
  ONB_SingleEvent %>% group_by(biome) %>% summarise(nobesingle = n())
OBEmulti <-
  ONB_multiEvent %>% group_by(biome) %>% summarise(nobemulti = n())
