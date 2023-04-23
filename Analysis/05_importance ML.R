rm(list = ls())
getwd()
setwd("D:\\000_collections\\010_Nighttime Burning\\015_OverNightBurning")
# packages ----------------------------------------------------------------
library(ggplot2)
library(ggsci)
library(gtools)
library(dplyr)
library(tidyr)
library(MKinfer)
library(lutz)
library(suncalc)
library(pROC)
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
windowsFonts(Times=windowsFont("Times New Roman"))



# Load data ---------------------------------------------------------------


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

fn <- function(x) scales::rescale(x, to=c(0,1))
Feature_scaled_4areas <- function(Feature_df,area_name,season_num){
  F_scaled <- FD_NLgte4 %>% filter(biome_4areas==area_name,season==season_num) %>% 
    dplyr::select(contains(
      c(
        'biome_4areas',
        'Ncondition',
        'FRPNoon2SetMeanFire',
        'ONB_Event_YoN',
        'BUI',
        'DMC',
        'DC',
        'FWI',
        'vpd_daymax',
        'ffmc_daymax',
        'isi_daymax',
        'vpd_nightmin',
        'ffmc_nightmin',
        'isi_nightmin',
        'temp_daymax',
        'rh_daymin',
        'temp_nightmin',
        'rh_nightmax'
      )
    )) %>% dplyr::select(ONB_Event_YoN:rh_nightmax) %>% #summarise(data.frame(lapply(.,fn)))%>% 
    mutate(ONB_Event_YoN =ifelse(ONB_Event_YoN =='OBEs', 'L1', 'L0' ))
  
  F_scaled
}
Feature_scaled_4areas_daily <- function(Feature_df,area_name,season_num){
  F_scaled <- FD_NLgte4 %>% filter(biome_4areas==area_name,season==season_num) %>% 
    dplyr::select(contains(
      c(
        'biome_4areas',
        'Ncondition',
        'FRPNoon2SetMeanFire',
        'ONB_Event_YoN',
        'FWI',
        'BUI',
        'DMC',
        'DC'
      )
    )) %>% dplyr::select(ONB_Event_YoN:DC) %>% #summarise(data.frame(lapply(.,fn)))%>% 
    mutate(ONB_Event_YoN =ifelse(ONB_Event_YoN =='OBEs', 'L1', 'L0' ))
  
  F_scaled
}

Feature_scaled_daytime_4areas <- function(Feature_df,area_name,season_num){
  F_scaled <- FD_NLgte4 %>% filter(biome_4areas==area_name,season==season_num) %>% 
    dplyr::select(contains(
      c(
        'biome_4areas',
        'Ncondition',
        'FRPNoon2SetMeanFire',
        'ONB_Event_YoN',
        'BUI',
        'DMC',
        'DC',
        'FWI',
        'vpd_daymax',
        'ffmc_daymax',
        'isi_daymax',
        'rh_daymin',
        'temp_daymax'
        
      )
    )) %>% dplyr::select(ONB_Event_YoN:temp_daymax) %>% #summarise(data.frame(lapply(.,fn)))%>% 
    mutate(ONB_Event_YoN =ifelse(ONB_Event_YoN =='OBEs', 'L1', 'L0' ))
  
  F_scaled
}
Feature_scaled_nighttime_4areas <- function(Feature_df,area_name,season_num){
  F_scaled <- FD_NLgte4 %>% filter(biome_4areas==area_name,season==season_num) %>% 
    dplyr::select(contains(
      c(
        'biome_4areas',
        'Ncondition',
        'FRPNoon2SetMeanFire',
        'ONB_Event_YoN',
        'vpd_nightmin',
        'ffmc_nightmin',
        'isi_nightmin',
        'rh_nightmax',
        'temp_nightmin'
        
      )
    )) %>% dplyr::select(ONB_Event_YoN:temp_nightmin) %>% #summarise(data.frame(lapply(.,fn)))%>% 
    mutate(ONB_Event_YoN =ifelse(ONB_Event_YoN =='OBEs', 'L1', 'L0' ))
  
  F_scaled
}
Feature_scaled_dayFL_4areas <- function(Feature_df,area_name,season_num){
  F_scaled <- FD_NLgte4 %>% filter(biome_4areas==area_name,season==season_num) %>% 
    dplyr::select(contains(
      c(
        'biome_4areas',
        'Ncondition',
        'FRPNoon2SetMeanFire',
        'ONB_Event_YoN',
        'FWI',
        'vpd_daymax',
        'ffmc_daymax',
        'isi_daymax',
        'rh_daymin',
        'temp_daymax'
        
      )
    )) %>% dplyr::select(ONB_Event_YoN:temp_daymax) %>% #summarise(data.frame(lapply(.,fn)))%>% 
    mutate(ONB_Event_YoN =ifelse(ONB_Event_YoN =='OBEs', 'L1', 'L0' ))
  
  F_scaled
}
Feature_scaled_4areas_NEE <- function(Feature_df,area_name,season_num){
  F_scaled <- FD_NLgte4 %>% filter(biome_4areas==area_name,season==season_num) %>% 
    dplyr::select(contains(
      c(
        'biome_4areas',
        
        'FRPNoon2SetMeanFire',
        'ONB_Event_YoN',
        'Ncondition',
        'BUI',
        'DMC',
        'DC',
        'FWI',
        'vpd_daymax',
        'ffmc_daymax',
        'isi_daymax',
        'vpd_nightmin',
        'ffmc_nightmin',
        'isi_nightmin',
        'temp_daymax',
        'rh_daymin',
        'temp_nightmin',
        'rh_nightmax'
      )
    )) %>% dplyr::select(Ncondition:rh_nightmax) %>% #summarise(data.frame(lapply(.,fn)))%>% 
    filter(Ncondition==c('OBE','NEE')) %>% 
    mutate(Ncondition =ifelse(Ncondition =='OBE', 'L1', 'L0' ))
  
  F_scaled
}
ctrl_rf <-
  trainControl	(
    method = 'repeatedcv',
    number = 5,
    repeats = 50,
    summaryFunction = twoClassSummary,
    sampling = 'down',
    classProbs = TRUE,
    savePredictions = 'all'
  )
ctrl_lr <-
  trainControl(
    method = 'repeatedcv',
    number = 5,
    repeats = 50,
    classProbs = TRUE,
    summaryFunction = twoClassSummary,
    sampling = 'down',
    savePredictions = 'all'
  )

# RF for new areas ---------------------------------------------------

repGrid <- expand.grid(.mtry=c(1))
# boreal - spring and summer
# set.seed(1000)
# # boreal_spring Num of ONB = 36
# rf_newboreal_spring <-
#   train(
#     ONB_Event_YoN ~ .,
#     data = Feature_scaled_4areas(FD_NLgte4,'Boreal','spring'),
#     method = 'rf',
#     family = 'binomial',
#     metric = 'ROC',
#     trControl = ctrl_rf,tuneGrid = repGrid
#     
#   )#glmStepAIC

set.seed(1000)
# boreal_summer Num of ONB = 101
rf_newboreal_summer <-
  train(
    ONB_Event_YoN ~ .,
    data = Feature_scaled_4areas(FD_NLgte4,'Boreal','summer'),
    method = 'rf',
    family = 'binomial',
    metric = 'ROC',
    trControl = ctrl_rf,tuneGrid = repGrid
  )#glmStepAIC
confusionMatrix(rf_newboreal_summer)

# temperate mountain system - summer and fall
set.seed(1000)
# temperate mountain system summer Num of ONB = 279
rf_Tmountain_summer <-
  train(
    ONB_Event_YoN ~ .,
    data = Feature_scaled_4areas(FD_NLgte4,'Temperate mountain system','summer'),
    method = 'rf',
    family = 'binomial',
    metric = 'ROC',
    trControl = ctrl_rf,tuneGrid = repGrid
    
  )#glmStepAIC
set.seed(1000)
# temperate mountain system fall Num of ONB = 238
rf_Tmountain_fall <-
  train(
    ONB_Event_YoN ~ .,
    data = Feature_scaled_4areas(FD_NLgte4,'Temperate mountain system','fall'),
    method = 'rf',
    family = 'binomial',
    metric = 'ROC',
    trControl = ctrl_rf,tuneGrid = repGrid
    
  )#glmStepAIC

# # temperate desert - summer
# set.seed(1000)
# # temperate desert summer Num of ONB = 54
# rf_Tdesert_summer <-
#   train(
#     ONB_Event_YoN ~ .,
#     data = Feature_scaled_4areas(FD_NLgte4,'Temperate desert','summer'),
#     method = 'rf',
#     family = 'binomial',
#     metric = 'ROC',
#     trControl = ctrl_rf,tuneGrid = repGrid
#     
#   )#glmStepAIC


# subtropical mountain system - summer and fall
set.seed(1000)
# subtropical mountain system summer Num of ONB = 169
rf_Submountain_summer <-
  train(
    ONB_Event_YoN ~ .,
    data = Feature_scaled_4areas(FD_NLgte4,'Subtropical mountain system','summer'),
    method = 'rf',
    family = 'binomial',
    metric = 'ROC',
    trControl = ctrl_rf,tuneGrid = repGrid
    
  )#glmStepAIC
set.seed(1000)
# subtropical mountain system fall Num of ONB = 165
rf_Submountain_fall <-
  train(
    ONB_Event_YoN ~ .,
    data = Feature_scaled_4areas(FD_NLgte4,'Subtropical mountain system','fall'),
    method = 'rf',
    family = 'binomial',
    metric = 'ROC',
    trControl = ctrl_rf,tuneGrid = repGrid
    
  )#glmStepAIC

# plot(rf_Tdesert_summer$finalModel)
# text(rf_newboreal_summer$finalModel)

# importance
# imp_rf_newboreal_spring = as.data.frame(rf_newboreal_spring[["finalModel"]][["importance"]])/sum(rf_newboreal_spring[["finalModel"]][["importance"]])
imp_rf_newboreal_summer = as.data.frame(rf_newboreal_summer[["finalModel"]][["importance"]])/sum(rf_newboreal_summer[["finalModel"]][["importance"]])
imp_rf_Tmountain_summer = as.data.frame(rf_Tmountain_summer[["finalModel"]][["importance"]])/sum(rf_Tmountain_summer[["finalModel"]][["importance"]])
imp_rf_Tmountain_fall = as.data.frame(rf_Tmountain_fall[["finalModel"]][["importance"]])/sum(rf_Tmountain_fall[["finalModel"]][["importance"]])
# imp_rf_Tdesert_summer = as.data.frame(rf_Tdesert_summer[["finalModel"]][["importance"]])/sum(rf_Tdesert_summer[["finalModel"]][["importance"]])
imp_rf_Submountain_summer = as.data.frame(rf_Submountain_summer[["finalModel"]][["importance"]])/sum(rf_Submountain_summer[["finalModel"]][["importance"]])
imp_rf_Submountain_fall = as.data.frame(rf_Submountain_fall[["finalModel"]][["importance"]])/sum(rf_Submountain_fall[["finalModel"]][["importance"]])


# names(imp_rf_newboreal_spring) <- 'BorealspringNeq36'
names(imp_rf_newboreal_summer)<-'BorealsummerNeq101'
names(imp_rf_Tmountain_summer)<-'TemperatemountainsystemsummerNeq271'
names(imp_rf_Tmountain_fall)<-'TemperatemountainsystemfallNeq216'
# names(imp_rf_Tdesert_summer)<-'TemperatedesertsummerNeq64'
names(imp_rf_Submountain_summer)<-'SubtropicalmountainsystemsummerNeq156'
names(imp_rf_Submountain_fall)<-'SubtropicalmountainsystemfallNeq165'


imp = cbind(
  # imp_rf_newboreal_spring,
  imp_rf_newboreal_summer,
  imp_rf_Tmountain_summer,
  imp_rf_Tmountain_fall,
  # imp_rf_Tdesert_summer,
  imp_rf_Submountain_summer,
  imp_rf_Submountain_fall
)
imp$variable <- rownames(imp)

imp <- imp %>% gather(BorealsummerNeq101:SubtropicalmountainsystemfallNeq165,key= zone2season,value=value)  %>% mutate(
  Variable_Class = case_when(variable %in% c('isi_daymax','ffmc_daymax','vpd_daymax','emc_daymin','prec_daysum','temp_daymax','rh_daymin','winds_daymean')~'Fast-responding (Daytime)',
                             variable %in% c('FWI','BUI','DMC','DC')~'Slow-responding (Daily noon)',
                             variable %in% c('isi_nightmin','ffmc_nightmin','vpd_nightmin','emc_nightmax','prec_nightsum','temp_nightmin','rh_nightmax','winds_nightmean')~'Fast-responding (Nighttime)'))


# result_rf_newboreal_spring = rf_newboreal_spring$results %>% filter(mtry==rf_newboreal_spring[["finalModel"]][["mtry"]])
result_rf_newboreal_summer = rf_newboreal_summer$results %>% filter(mtry==rf_newboreal_summer[["finalModel"]][["mtry"]])
result_rf_Tmountain_summer = rf_Tmountain_summer$results %>% filter(mtry==rf_Tmountain_summer[["finalModel"]][["mtry"]])
result_rf_Tmountain_fall = rf_Tmountain_fall$results %>% filter(mtry==rf_Tmountain_fall[["finalModel"]][["mtry"]])
# result_rf_Tdesert_summer = rf_Tdesert_summer$results %>% filter(mtry==rf_Tdesert_summer[["finalModel"]][["mtry"]])
result_rf_Submountain_summer = rf_Submountain_summer$results %>% filter(mtry==rf_Submountain_summer[["finalModel"]][["mtry"]])
result_rf_Submountain_fall = rf_Submountain_fall$results %>% filter(mtry==rf_Submountain_fall[["finalModel"]][["mtry"]])

labels <-
  c(
    # paste(
    #   'Boreal spring\n#n=36',
    #   ' AUC=',
    #   round(result_rf_newboreal_spring$ROC, 2),
    #   # ' mtry=',
    #   # round(result_rf_newboreal_spring$mtry, 2),
    #   # ' sens=',
    #   # round(result_rf_newboreal_spring$Sens, 2),
    #   # ' spec=',
    #   # round(result_rf_newboreal_spring$Spec, 2),
    #   sep = ''
    # ),
    paste(
      'Boreal summer\n',
      ' AUC=',
      round(result_rf_newboreal_summer$ROC, 2),
      # ' mtry=',
      # round(result_rf_newboreal_summer$mtry, 2),
      # ' sens=',
      # round(result_rf_newboreal_summer$Sens, 2),
      # ' spec=',
      # round(result_rf_newboreal_summer$Spec, 2),
      sep = ''
    ),
    paste(
      'Temperate mountain\nsystem summer ',
      ' AUC=',
      round(result_rf_Tmountain_summer$ROC, 2),
      # ' mtry=',
      # round(result_rf_Tmountain_summer$mtry, 2),
      # ' sens=',
      # round(result_rf_Tmountain_summer$Sens, 2),
      # ' spec=',
      # round(result_rf_Tmountain_summer$Spec, 2),
      sep = ''
    ),
    paste(
      'Temperate mountain\nsystem fall ',
      ' AUC=',
      round(result_rf_Tmountain_fall$ROC, 2),
      # ' mtry=',
      # round(result_rf_Tmountain_fall$mtry, 2),
      # ' sens=',
      # round(result_rf_Tmountain_fall$Sens, 2),
      # ' spec=',
      # round(result_rf_Tmountain_fall$Spec, 2),
      sep = ''
    ),
    paste(
      'Subtropical mountain\nsystem summer ',
      ' AUC=',
      round(result_rf_Submountain_summer$ROC, 2),
      # ' mtry=',
      # round(result_rf_Submountain_summer$mtry, 2),
      # ' sens=',
      # round(result_rf_Submountain_summer$Sens, 2),
      # ' spec=',
      # round(result_rf_Submountain_summer$Spec, 2),
      sep = ''
    ),
    paste(
      'Subtropical mountain\nsystem fall ',
      ' AUC=',
      round(result_rf_Submountain_fall$ROC, 2),
      # ' mtry=',
      # round(result_rf_Submountain_fall$mtry, 2),
      # ' sens=',
      # round(result_rf_Submountain_fall$Sens, 2),
      # ' spec=',
      # round(result_rf_Submountain_fall$Spec, 2),
      sep = ''
    )
    # paste(
    #   'Temperate desert summer\n#n=64',
    #   ' AUC=',
    #   round(result_rf_Tdesert_summer$ROC, 2),
    #   # ' mtry=',
    #   # round(result_rf_Tdesert_summer$mtry, 2),
    #   # ' sens=',
    #   # round(result_rf_Tdesert_summer$Sens, 2),
    #   # ' spec=',
    #   # round(result_rf_Tdesert_summer$Spec, 2),
    #   sep = ''
    # )
  )

imp$zone2season <- factor(imp$zone2season, levels = c('BorealsummerNeq101','TemperatemountainsystemsummerNeq271',
                                                      'TemperatemountainsystemfallNeq216','SubtropicalmountainsystemsummerNeq156',
                                                      'SubtropicalmountainsystemfallNeq165'),
                          labels = labels)
# imp$variable <- factor(imp$variable,levels = c('FWI','BUI','DMC','DC','isi_daymax','ffmc_daymax','vpd_daymax','emc_daymin','prec_daysum','temp_daymax','rh_daymin','winds_daymean',
#                                                "isi_nightmin", "ffmc_nightmin" ,"vpd_nightmin",'emc_nightmax','prec_nightsum','temp_nightmin','rh_nightmax','winds_nightmean'
# ))
#imp$variable <- factor(imp$variable, levels=rev(levels(imp$variable)))
# ggplot()+geom_bar(data=imp,aes(x=value,y=variable,fill=Variable_Class),stat='identity')+
#   facet_wrap(.~zone2season)+ envalysis::theme_publish(base_size = 8, base_family = "", line_size = 0.25,base_rect_size =0.25)+ 
#   scale_fill_manual(values=c('grey','black'))+xlab('Mean Decrease Gini Importance (Normalized)')+
#   ylab('Fire Weather Variables')+labs(title='Importance of fire weather variables by areas and seasons (RF)')+ theme(
#     plot.title = element_text(color="red", size=10, face="bold.italic")
#   )
imp[imp == "isi_daymax"] <- "ISI Dmax"
imp[imp == "isi_nightmin"] <- "ISI Nmin"
imp["variable"][imp["variable"] == "ffmc_daymax"] <- 'FFMC Dmax'
imp["variable"][imp["variable"] == "ffmc_nightmin"] <- "FFMC Nmin"
imp["variable"][imp["variable"] == "vpd_daymax"] <- "VPD Dmax"
imp["variable"][imp["variable"] == "vpd_nightmin"] <- "VPD Nmin"
imp["variable"][imp["variable"] == "temp_daymax"] <- "T Dmax"
imp["variable"][imp["variable"] == "temp_nightmin"] <- "T Nmin"
imp["variable"][imp["variable"] == "rh_daymin"] <- "RH Dmin"
imp["variable"][imp["variable"] == "rh_nightmax"] <- "RH Nmax"



library(tidytext)
windowsFonts(Times=windowsFont("Times New Roman"))

p = ggplot()+geom_bar(data=imp,aes(x=value,y=reorder_within(variable,value,zone2season),fill=Variable_Class),stat='identity',width=0.8)+
  facet_wrap(.~zone2season,scales = 'free_y',ncol=3)+ #envalysis::theme_publish(base_size = 10, base_family = "", line_size = 0.25,base_rect_size =0.25)+ 
  scale_fill_manual(values=c('grey','black','darkred'))+xlab('Normalized Gini importance')+
  ylab('Fire Weather traits')+#labs(title='Importance of fire weather variables by areas and seasons (RF)')+ 
  theme(text = element_text(size = 7),axis.title.y=element_blank(),
        strip.text.x = element_text(size = 7, color = "black", face = "plain"),
        plot.title = element_text(color="black", size=7, face="bold",vjust = - 1),legend.title=element_blank())+
  theme(strip.background =element_rect(fill="grey96"),
        panel.background = element_rect(fill = 'grey96'),
        legend.position = c(0.85,0.1))+
  scale_y_reordered()
p
# x <- paste0('015.5_Plots\\fig.3.importance1','.pdf')
# ggsave(plot=p,x,width = 15,height =10,units = 'cm')

