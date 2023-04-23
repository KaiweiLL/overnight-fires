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


#1 distribution statistics
OBF <- FD_NLgte4 %>% filter(ONB_Event_YoN=='OBEs') %>% group_by(year,seq) %>% mutate(counts=n()) %>% 
  summarise(year=year[1],seq=seq[1],season=season[1],counts=counts[1],month=month[1],biome=biome[1],long=long[1],lat=lat[1])%>% 
  mutate(season = case_when((month >= 3 & month <=5) ~ 'spring',
                            (month >= 6 & month <=8) ~'summer',
                            (month >= 9 & month <=11) ~ 'fall',
                            (month==12|month==1|month==2) ~ 'winter')) %>% 
  mutate(biome = case_when(
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
    (biome == 41) ~ 'Boreal coniferous forest',
    (biome == 42) ~ 'Boreal tundra woodland',
    (biome == 43) ~ 'Boreal mountain system',
    (biome == 50) ~ 'Polar'
    
  ))

OBE <- FD_NLgte4 %>% filter(ONB_Event_YoN=='OBEs') %>% group_by(year,seq) %>% mutate(counts=n()) %>% 
  mutate(season = case_when((month >= 3 & month <=5) ~ 'spring',
                            (month >= 6 & month <=8) ~'summer',
                            (month >= 9 & month <=11) ~ 'fall',
                            (month==12|month==1|month==2) ~ 'winter')) %>% 
  mutate(biome = case_when(
    (biome == 12) ~ 'Other',
    (biome == 23) ~ 'Other',
    (biome == 24) ~ 'Other',
    (biome == 25) ~ 'Subtropical\nmountain system',
    (biome == 32) ~ 'Other',
    (biome == 33) ~ 'Other',
    (biome == 34) ~ 'Temperate\ndesert',
    (biome == 35) ~ 'Temperate\nmountain system',
    (biome == 41) ~ 'Boreal',
    (biome == 42) ~ 'Boreal',
    (biome == 43) ~ 'Boreal'
  ))


OBF$season <- factor(OBF$season, levels = c('spring','summer','fall','winter'))
OBE$season <- factor(OBE$season, levels = c('spring','summer','fall','winter'))
OBE$biome <- factor(OBE$biome, levels = c('Boreal',
                                          "Temperate\ndesert","Temperate\nmountain system",
                                          "Subtropical\nmountain system","Other"))
