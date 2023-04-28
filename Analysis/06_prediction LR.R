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
# Prediction_final version ------------------------------------------------

# Single ------------------------------------------------------------------

Feature_scaled_4areas_lr <- function(Feature_df,area_name,season_num,fw){
  F_scaled <- FD_NLgte4 %>% filter(biome_4areas==area_name,season==season_num) %>% 
    dplyr::select(contains(
      c(
        'biome_4areas',
        'Ncondition',
        'FRPNoon2SetMeanFire',
        'ONB_Event_YoN',
        fw
      )
    )) %>% dplyr::select(ONB_Event_YoN:fw) %>% #summarise(data.frame(lapply(.,fn)))%>% 
    mutate(ONB_Event_YoN =ifelse(ONB_Event_YoN =='OBEs', 'L1', 'L0' ))
  
  F_scaled
}


collectionfw = c(        'BUI',
                         'DMC',
                         'DC',
                         'FWI'
)

f_lr_boreal = function(i) {
  library(caret)
  library(foreach)
  library(doParallel)
  library(parallel)
  library(tcltk)
  library(dplyr)
  set.seed(1000)
  fw=collectionfw[i]
  lr_newboreal_summer <-
    train(
      ONB_Event_YoN ~ .,
      data = Feature_scaled_4areas_lr(FD_NLgte4,'Boreal','summer',fw),
      method = 'glm',
      family = 'binomial',
      metric = 'ROC',
      trControl = ctrl_lr
    )#glmStepAIC
  lr_newboreal_summer
  roc=round(lr_newboreal_summer$results[['ROC']], 2)
  x = confusionMatrix(lr_newboreal_summer)
  recall = round((x[["table"]][2,2])/(x[["table"]][1,2]+x[["table"]][2,2]),3)
  precision = round((x[["table"]][2,2])/(x[["table"]][2,1]+x[["table"]][2,2]),3)
  f1 = 2*as.numeric(recall)*as.numeric(precision)/(as.numeric(recall)+as.numeric(precision))
  op = c(fw,roc,recall,precision,f1)
}



no_cores<-detectCores()-6
cl <- makeSOCKcluster(no_cores)
registerDoSNOW(cl)
pb <- txtProgressBar(min=1, max=4, style=3)#length(NAfire_year)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress=progress)

start_time <- Sys.time()
lr_newboreal_summer <- foreach(i=1:4, .options.snow = opts, .combine = 'rbind') %dopar%  f_lr_boreal(i)#, .combine = 'rbind'
lr_newboreal_summer = as.data.frame(lr_newboreal_summer)
colnames(lr_newboreal_summer) = c('variable','AUC','Recall','Precision','F1')
lr_newboreal_summer$st='Boreal_summer'
stopCluster(cl)
end_time <- Sys.time()
end_time - start_time


f_lr_tmountain_summer = function(i) {
  library(caret)
  library(foreach)
  library(doParallel)
  library(parallel)
  library(tcltk)
  library(dplyr)
  set.seed(1000)
  fw=collectionfw[i]
  lr_Tmountain_summer <-
    train(
      ONB_Event_YoN ~ .,
      data = Feature_scaled_4areas_lr(FD_NLgte4,'Temperate mountain system','summer',fw),
      method = 'glm',
      family = 'binomial',
      metric = 'ROC',
      trControl = ctrl_lr
    )#glmStepAIC
  lr_Tmountain_summer
  roc=round(lr_Tmountain_summer$results[['ROC']], 2)
  x = confusionMatrix(lr_Tmountain_summer)
  recall = round((x[["table"]][2,2])/(x[["table"]][1,2]+x[["table"]][2,2]),3)
  precision = round((x[["table"]][2,2])/(x[["table"]][2,1]+x[["table"]][2,2]),3)
  f1 = 2*as.numeric(recall)*as.numeric(precision)/(as.numeric(recall)+as.numeric(precision))
  op = c(fw,roc,recall,precision,f1)
}



no_cores<-detectCores()-6
cl <- makeSOCKcluster(no_cores)
registerDoSNOW(cl)
pb <- txtProgressBar(min=1, max=4, style=3)#length(NAfire_year)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress=progress)

start_time <- Sys.time()
lr_Tmountain_summer <- foreach(i=1:4, .options.snow = opts, .combine = 'rbind') %dopar%  f_lr_tmountain_summer(i)#, .combine = 'rbind'
lr_Tmountain_summer = as.data.frame(lr_Tmountain_summer)
colnames(lr_Tmountain_summer) = c('variable','AUC','Recall','Precision','F1')
lr_Tmountain_summer$st='Tmountain_summer'

stopCluster(cl)
end_time <- Sys.time()
end_time - start_time


f_lr_tmountain_fall = function(i) {
  library(caret)
  library(foreach)
  library(doParallel)
  library(parallel)
  library(tcltk)
  library(dplyr)
  set.seed(1000)
  fw=collectionfw[i]
  lr_Tmountain_fallr <-
    train(
      ONB_Event_YoN ~ .,
      data = Feature_scaled_4areas_lr(FD_NLgte4,'Temperate mountain system','fall',fw),
      method = 'glm',
      family = 'binomial',
      metric = 'ROC',
      trControl = ctrl_lr
    )#glmStepAIC
  lr_Tmountain_fallr
  roc=round(lr_Tmountain_fallr$results[['ROC']], 2)
  x = confusionMatrix(lr_Tmountain_fallr)
  recall = round((x[["table"]][2,2])/(x[["table"]][1,2]+x[["table"]][2,2]),3)
  precision = round((x[["table"]][2,2])/(x[["table"]][2,1]+x[["table"]][2,2]),3)
  f1 = 2*as.numeric(recall)*as.numeric(precision)/(as.numeric(recall)+as.numeric(precision))
  op = c(fw,roc,recall,precision,f1)
}


no_cores<-detectCores()-6
cl <- makeSOCKcluster(no_cores)
registerDoSNOW(cl)
pb <- txtProgressBar(min=1, max=4, style=3)#length(NAfire_year)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress=progress)

start_time <- Sys.time()
lr_Tmountain_fall <- foreach(i=1:4, .options.snow = opts, .combine = 'rbind') %dopar%  f_lr_tmountain_fall(i)#, .combine = 'rbind'
lr_Tmountain_fall = as.data.frame(lr_Tmountain_fall)
colnames(lr_Tmountain_fall) = c('variable','AUC','Recall','Precision','F1')
lr_Tmountain_fall$st='Tmountain_fall'

stopCluster(cl)
end_time <- Sys.time()
end_time - start_time


f_lr_smountain_summer = function(i) {
  library(caret)
  library(foreach)
  library(doParallel)
  library(parallel)
  library(tcltk)
  library(dplyr)
  set.seed(1000)
  fw=collectionfw[i]
  lr_Smountain_summer <-
    train(
      ONB_Event_YoN ~ .,
      data = Feature_scaled_4areas_lr(FD_NLgte4,'Subtropical mountain system','summer',fw),
      method = 'glm',
      family = 'binomial',
      metric = 'ROC',
      trControl = ctrl_lr
    )#glmStepAIC
  lr_Smountain_summer
  roc=round(lr_Smountain_summer$results[['ROC']], 2)
  x = confusionMatrix(lr_Smountain_summer)
  recall = round((x[["table"]][2,2])/(x[["table"]][1,2]+x[["table"]][2,2]),3)
  precision = round((x[["table"]][2,2])/(x[["table"]][2,1]+x[["table"]][2,2]),3)
  f1 = 2*as.numeric(recall)*as.numeric(precision)/(as.numeric(recall)+as.numeric(precision))
  op = c(fw,roc,recall,precision,f1)
}

no_cores<-detectCores()-6
cl <- makeSOCKcluster(no_cores)
registerDoSNOW(cl)
pb <- txtProgressBar(min=1, max=4, style=3)#length(NAfire_year)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress=progress)

start_time <- Sys.time()
lr_Smountain_summer <- foreach(i=1:4, .options.snow = opts, .combine = 'rbind') %dopar%  f_lr_smountain_summer(i)#, .combine = 'rbind'
lr_Smountain_summer = as.data.frame(lr_Smountain_summer)
colnames(lr_Smountain_summer) = c('variable','AUC','Recall','Precision','F1')
lr_Smountain_summer$st='Smountain_summer'

stopCluster(cl)
end_time <- Sys.time()
end_time - start_time


f_lr_smountain_fall = function(i) {
  library(caret)
  library(foreach)
  library(doParallel)
  library(parallel)
  library(tcltk)
  library(dplyr)
  set.seed(1000)
  fw=collectionfw[i]
  lr_Smountain_fall <-
    train(
      ONB_Event_YoN ~ .,
      data = Feature_scaled_4areas_lr(FD_NLgte4,'Subtropical mountain system','fall',fw),
      method = 'glm',
      family = 'binomial',
      metric = 'ROC',
      trControl = ctrl_lr
    )#glmStepAIC
  lr_Smountain_fall
  roc=round(lr_Smountain_fall$results[['ROC']], 2)
  x = confusionMatrix(lr_Smountain_fall)
  recall = round((x[["table"]][2,2])/(x[["table"]][1,2]+x[["table"]][2,2]),3)
  precision = round((x[["table"]][2,2])/(x[["table"]][2,1]+x[["table"]][2,2]),3)
  f1 = 2*as.numeric(recall)*as.numeric(precision)/(as.numeric(recall)+as.numeric(precision))
  op = c(fw,roc,recall,precision,f1)
}


no_cores<-detectCores()-6
cl <- makeSOCKcluster(no_cores)
registerDoSNOW(cl)
pb <- txtProgressBar(min=1, max=4, style=3)#length(NAfire_year)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress=progress)

start_time <- Sys.time()
lr_Smountain_fall <- foreach(i=1:4, .options.snow = opts, .combine = 'rbind') %dopar%  f_lr_smountain_fall(i)#, .combine = 'rbind'
lr_Smountain_fall = as.data.frame(lr_Smountain_fall)
colnames(lr_Smountain_fall) = c('variable','AUC','Recall','Precision','F1')
lr_Smountain_fall$st='Smountain_fall'
stopCluster(cl)
end_time <- Sys.time()
end_time - start_time


lr_output = rbind(lr_newboreal_summer,lr_Tmountain_summer,lr_Tmountain_fall,lr_Smountain_summer,lr_Smountain_fall)
lr_output = lr_output[!duplicated(as.list(lr_output))]
rownames(lr_output) <- 1:nrow(lr_output)


# Two ---------------------------------------------------------------------

Feature_scaled_4areas_lr <- function(Feature_df,area_name,season_num,fw1,fw2){
  F_scaled <- FD_NLgte4 %>% filter(biome_4areas==area_name,season==season_num) %>% 
    dplyr::select(contains(
      c(
        'biome_4areas',
        'Ncondition',
        'FRPNoon2SetMeanFire',
        'ONB_Event_YoN',
        fw1,fw2
      )
    )) %>% dplyr::select(ONB_Event_YoN:fw2) %>% #summarise(data.frame(lapply(.,fn)))%>% 
    mutate(ONB_Event_YoN =ifelse(ONB_Event_YoN =='OBEs', 'L1', 'L0' ))
  
  F_scaled
}


collectionfw1 = c('FWI','FWI','FWI','BUI','BUI','DMC')
collectionfw2 = c('BUI','DMC','DC','DMC','DC','DC')


f_lr_boreal = function(i) {
  library(caret)
  library(foreach)
  library(doParallel)
  library(parallel)
  library(tcltk)
  library(dplyr)
  set.seed(1000)
  fw1=collectionfw1[i]
  fw2=collectionfw2[i]
  lr_newboreal_summer <-
    train(
      ONB_Event_YoN ~ .,
      data = Feature_scaled_4areas_lr(FD_NLgte4,'Boreal','summer',fw1,fw2),
      method = 'glm',
      family = 'binomial',
      metric = 'ROC',
      trControl = ctrl_lr
    )#glmStepAIC
  lr_newboreal_summer
  roc=round(lr_newboreal_summer$results[['ROC']], 2)
  y = car::vif(lr_newboreal_summer$finalModel)
  x = confusionMatrix(lr_newboreal_summer)
  recall = round((x[["table"]][2,2])/(x[["table"]][1,2]+x[["table"]][2,2]),3)
  z = paste(fw1,fw2,sep = "+")
  precision = round((x[["table"]][2,2])/(x[["table"]][2,1]+x[["table"]][2,2]),3)
  f1 = 2*as.numeric(recall)*as.numeric(precision)/(as.numeric(recall)+as.numeric(precision))
  op = c(z,y,roc,recall,precision,f1)
}

no_cores<-detectCores()-6
cl <- makeSOCKcluster(no_cores)
registerDoSNOW(cl)
pb <- txtProgressBar(min=1, max=6, style=3)#length(NAfire_year)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress=progress)

start_time <- Sys.time()
lr_newboreal_summer_2 <- foreach(i=1:6, .options.snow = opts, .combine = 'rbind') %dopar%  f_lr_boreal(i)#, .combine = 'rbind'
lr_newboreal_summer_2 = as.data.frame(lr_newboreal_summer_2)
colnames(lr_newboreal_summer_2) = c('variable','variable1_vif','variable2_vif','AUC','Recall','Precision','F1')
lr_newboreal_summer_2$st = 'Boreal_summer'
stopCluster(cl)
end_time <- Sys.time()
end_time - start_time



f_lr_tmountain_summer = function(i) {
  library(caret)
  library(foreach)
  library(doParallel)
  library(parallel)
  library(tcltk)
  library(dplyr)
  set.seed(1000)
  fw1=collectionfw1[i]
  fw2=collectionfw2[i]
  lr_Tmountain_summer <-
    train(
      ONB_Event_YoN ~ .,
      data = Feature_scaled_4areas_lr(FD_NLgte4,'Temperate mountain system','summer',fw1,fw2),
      method = 'glm',
      family = 'binomial',
      metric = 'ROC',
      trControl = ctrl_lr
    )#glmStepAIC
  lr_Tmountain_summer
  roc=round(lr_Tmountain_summer$results[['ROC']], 2)
  y = car::vif(lr_Tmountain_summer$finalModel)
  x = confusionMatrix(lr_Tmountain_summer)
  recall = round((x[["table"]][2,2])/(x[["table"]][1,2]+x[["table"]][2,2]),3)
  z = paste(fw1,fw2,sep = "+")
  precision = round((x[["table"]][2,2])/(x[["table"]][2,1]+x[["table"]][2,2]),3)
  f1 = 2*as.numeric(recall)*as.numeric(precision)/(as.numeric(recall)+as.numeric(precision))
  op = c(z,y,roc,recall,precision,f1)
}



no_cores<-detectCores()-6
cl <- makeSOCKcluster(no_cores)
registerDoSNOW(cl)
pb <- txtProgressBar(min=1, max=6, style=3)#length(NAfire_year)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress=progress)

start_time <- Sys.time()
lr_Tmountain_summer_2 <- foreach(i=1:6, .options.snow = opts, .combine = 'rbind') %dopar%  f_lr_tmountain_summer(i)#, .combine = 'rbind'
lr_Tmountain_summer_2 = as.data.frame(lr_Tmountain_summer_2)
colnames(lr_Tmountain_summer_2) = c('variable','variable1_vif','variable2_vif','AUC','Recall','Precision','F1')
lr_Tmountain_summer_2$st = 'Tmountain_summer'

stopCluster(cl)
end_time <- Sys.time()
end_time - start_time


f_lr_tmountain_fall = function(i) {
  library(caret)
  library(foreach)
  library(doParallel)
  library(parallel)
  library(tcltk)
  library(dplyr)
  set.seed(1000)
  fw1=collectionfw1[i]
  fw2=collectionfw2[i]
  lr_Tmountain_fallr <-
    train(
      ONB_Event_YoN ~ .,
      data = Feature_scaled_4areas_lr(FD_NLgte4,'Temperate mountain system','fall',fw1,fw2),
      method = 'glm',
      family = 'binomial',
      metric = 'ROC',
      trControl = ctrl_lr
    )#glmStepAIC
  lr_Tmountain_fallr
  roc=round(lr_Tmountain_fallr$results[['ROC']], 2)
  y = car::vif(lr_Tmountain_fallr$finalModel)
  x = confusionMatrix(lr_Tmountain_fallr)
  recall = round((x[["table"]][2,2])/(x[["table"]][1,2]+x[["table"]][2,2]),3)
  z = paste(fw1,fw2,sep = "+")
  precision = round((x[["table"]][2,2])/(x[["table"]][2,1]+x[["table"]][2,2]),3)
  f1 = 2*as.numeric(recall)*as.numeric(precision)/(as.numeric(recall)+as.numeric(precision))
  op = c(z,y,roc,recall,precision,f1)
}


no_cores<-detectCores()-6
cl <- makeSOCKcluster(no_cores)
registerDoSNOW(cl)
pb <- txtProgressBar(min=1, max=6, style=3)#length(NAfire_year)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress=progress)

start_time <- Sys.time()
lr_Tmountain_fall_2 <- foreach(i=1:6, .options.snow = opts, .combine = 'rbind') %dopar%  f_lr_tmountain_fall(i)#, .combine = 'rbind'
lr_Tmountain_fall_2 = as.data.frame(lr_Tmountain_fall_2)
colnames(lr_Tmountain_fall_2) = c('variable','variable1_vif','variable2_vif','AUC','Recall','Precision','F1')
lr_Tmountain_fall_2$st = 'Tmountain_fall'

stopCluster(cl)
end_time <- Sys.time()
end_time - start_time


f_lr_smountain_summer = function(i) {
  library(caret)
  library(foreach)
  library(doParallel)
  library(parallel)
  library(tcltk)
  library(dplyr)
  set.seed(1000)
  fw1=collectionfw1[i]
  fw2=collectionfw2[i]
  
  lr_Smountain_summer <-
    train(
      ONB_Event_YoN ~ .,
      data = Feature_scaled_4areas_lr(FD_NLgte4,'Subtropical mountain system','summer',fw1,fw2),
      method = 'glm',
      family = 'binomial',
      metric = 'ROC',
      trControl = ctrl_lr
    )#glmStepAIC
  lr_Smountain_summer
  roc=round(lr_Smountain_summer$results[['ROC']], 2)
  y = car::vif(lr_Smountain_summer$finalModel)
  x = confusionMatrix(lr_Smountain_summer)
  recall = round((x[["table"]][2,2])/(x[["table"]][1,2]+x[["table"]][2,2]),3)
  z = paste(fw1,fw2,sep = "+")
  precision = round((x[["table"]][2,2])/(x[["table"]][2,1]+x[["table"]][2,2]),3)
  f1 = 2*as.numeric(recall)*as.numeric(precision)/(as.numeric(recall)+as.numeric(precision))
  op = c(z,y,roc,recall,precision,f1)
}

no_cores<-detectCores()-6
cl <- makeSOCKcluster(no_cores)
registerDoSNOW(cl)
pb <- txtProgressBar(min=1, max=6, style=3)#length(NAfire_year)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress=progress)

start_time <- Sys.time()
lr_Smountain_summer_2 <- foreach(i=1:6, .options.snow = opts, .combine = 'rbind') %dopar%  f_lr_smountain_summer(i)#, .combine = 'rbind'
lr_Smountain_summer_2 = as.data.frame(lr_Smountain_summer_2)
colnames(lr_Smountain_summer_2) = c('variable','variable1_vif','variable2_vif','AUC','Recall','Precision','F1')
lr_Smountain_summer_2$st = 'Smountain_summer'

stopCluster(cl)
end_time <- Sys.time()
end_time - start_time


f_lr_smountain_fall = function(i) {
  library(caret)
  library(foreach)
  library(doParallel)
  library(parallel)
  library(tcltk)
  library(dplyr)
  set.seed(1000)
  fw1=collectionfw1[i]
  fw2=collectionfw2[i]
  
  lr_Smountain_fall <-
    train(
      ONB_Event_YoN ~ .,
      data = Feature_scaled_4areas_lr(FD_NLgte4,'Subtropical mountain system','fall',fw1,fw2),
      method = 'glm',
      family = 'binomial',
      metric = 'ROC',
      trControl = ctrl_lr
    )#glmStepAIC
  lr_Smountain_fall
  roc=round(lr_Smountain_fall$results[['ROC']], 2)
  y = car::vif(lr_Smountain_fall$finalModel)
  x = confusionMatrix(lr_Smountain_fall)
  recall = round((x[["table"]][2,2])/(x[["table"]][1,2]+x[["table"]][2,2]),3)
  z = paste(fw1,fw2,sep = "+")
  precision = round((x[["table"]][2,2])/(x[["table"]][2,1]+x[["table"]][2,2]),3)
  f1 = 2*as.numeric(recall)*as.numeric(precision)/(as.numeric(recall)+as.numeric(precision))
  op = c(z,y,roc,recall,precision,f1)
}


no_cores<-detectCores()-6
cl <- makeSOCKcluster(no_cores)
registerDoSNOW(cl)
pb <- txtProgressBar(min=1, max=6, style=3)#length(NAfire_year)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress=progress)

start_time <- Sys.time()
lr_Smountain_fall_2 <- foreach(i=1:6, .options.snow = opts, .combine = 'rbind') %dopar%  f_lr_smountain_fall(i)#, .combine = 'rbind'
lr_Smountain_fall_2 = as.data.frame(lr_Smountain_fall_2)
colnames(lr_Smountain_fall_2) = c('variable','variable1_vif','variable2_vif','AUC','Recall','Precision','F1')
lr_Smountain_fall_2$st = 'Smountain_fall'
stopCluster(cl)
end_time <- Sys.time()
end_time - start_time


lr_output_2 = rbind(lr_newboreal_summer_2,lr_Tmountain_summer_2,lr_Tmountain_fall_2,lr_Smountain_summer_2,lr_Smountain_fall_2)
rownames(lr_output_2) <- 1:nrow(lr_output_2)
lr_output_2 = lr_output_2 %>% filter(!(as.numeric(variable1_vif)   > 2 |as.numeric(variable2_vif)   > 2))
lr_output_2 = lr_output_2 %>% dplyr::select(variable,AUC,Recall,Precision,F1,st)

# Three -------------------------------------------------------------------

Feature_scaled_4areas_lr <- function(Feature_df,area_name,season_num,fw1,fw2,fw3){
  F_scaled <- FD_NLgte4 %>% filter(biome_4areas==area_name,season==season_num) %>% 
    dplyr::select(contains(
      c(
        'biome_4areas',
        'Ncondition',
        'FRPNoon2SetMeanFire',
        'ONB_Event_YoN',
        fw1,fw2,fw3
      )
    )) %>% dplyr::select(ONB_Event_YoN:fw3) %>% #summarise(data.frame(lapply(.,fn)))%>% 
    mutate(ONB_Event_YoN =ifelse(ONB_Event_YoN =='OBEs', 'L1', 'L0' ))
  
  F_scaled
}


collectionfw1 = c('FWI','FWI','FWI','BUI')
collectionfw2 = c('BUI','BUI','DMC','DMC')
collectionfw3 = c('DMC','DC','DC','DC')


f_lr_boreal = function(i) {
  library(caret)
  library(foreach)
  library(doParallel)
  library(parallel)
  library(tcltk)
  library(dplyr)
  set.seed(1000)
  fw1=collectionfw1[i]
  fw2=collectionfw2[i]
  fw3=collectionfw3[i]
  
  lr_newboreal_summer <-
    train(
      ONB_Event_YoN ~ .,
      data = Feature_scaled_4areas_lr(FD_NLgte4,'Boreal','summer',fw1,fw2,fw3),
      method = 'glm',
      family = 'binomial',
      metric = 'ROC',
      trControl = ctrl_lr
    )#glmStepAIC
  lr_newboreal_summer
  roc=round(lr_newboreal_summer$results[['ROC']], 2)
  y = car::vif(lr_newboreal_summer$finalModel)
  x = confusionMatrix(lr_newboreal_summer)
  recall = round((x[["table"]][2,2])/(x[["table"]][1,2]+x[["table"]][2,2]),3)
  z = paste(fw1,fw2,fw3,sep = "+")
  precision = round((x[["table"]][2,2])/(x[["table"]][2,1]+x[["table"]][2,2]),3)
  f1 = 2*as.numeric(recall)*as.numeric(precision)/(as.numeric(recall)+as.numeric(precision))
  op = c(z,y,roc,recall,precision,f1)
}

no_cores<-detectCores()-6
cl <- makeSOCKcluster(no_cores)
registerDoSNOW(cl)
pb <- txtProgressBar(min=1, max=4, style=3)#length(NAfire_year)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress=progress)

start_time <- Sys.time()
lr_newboreal_summer_3 <- foreach(i=1:4, .options.snow = opts, .combine = 'rbind') %dopar%  f_lr_boreal(i)#, .combine = 'rbind'
lr_newboreal_summer_3 = as.data.frame(lr_newboreal_summer_3)
colnames(lr_newboreal_summer_3) = c('variable','variable1_vif','variable2_vif','variable3_vif','AUC','Recall','Precision','F1')
lr_newboreal_summer_3$st = 'Boreal_summer'
stopCluster(cl)
end_time <- Sys.time()
end_time - start_time



f_lr_tmountain_summer = function(i) {
  library(caret)
  library(foreach)
  library(doParallel)
  library(parallel)
  library(tcltk)
  library(dplyr)
  set.seed(1000)
  fw1=collectionfw1[i]
  fw2=collectionfw2[i]
  fw3=collectionfw3[i]
  
  lr_Tmountain_summer <-
    train(
      ONB_Event_YoN ~ .,
      data = Feature_scaled_4areas_lr(FD_NLgte4,'Temperate mountain system','summer',fw1,fw2,fw3),
      method = 'glm',
      family = 'binomial',
      metric = 'ROC',
      trControl = ctrl_lr
    )#glmStepAIC
  lr_Tmountain_summer
  roc=round(lr_Tmountain_summer$results[['ROC']], 2)
  y = car::vif(lr_Tmountain_summer$finalModel)
  x = confusionMatrix(lr_Tmountain_summer)
  recall = round((x[["table"]][2,2])/(x[["table"]][1,2]+x[["table"]][2,2]),3)
  z = paste(fw1,fw2,fw3,sep = "+")
  precision = round((x[["table"]][2,2])/(x[["table"]][2,1]+x[["table"]][2,2]),3)
  f1 = 2*as.numeric(recall)*as.numeric(precision)/(as.numeric(recall)+as.numeric(precision))
  op = c(z,y,roc,recall,precision,f1)
}



no_cores<-detectCores()-6
cl <- makeSOCKcluster(no_cores)
registerDoSNOW(cl)
pb <- txtProgressBar(min=1, max=4, style=3)#length(NAfire_year)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress=progress)

start_time <- Sys.time()
lr_Tmountain_summer_3 <- foreach(i=1:4, .options.snow = opts, .combine = 'rbind') %dopar%  f_lr_tmountain_summer(i)#, .combine = 'rbind'
lr_Tmountain_summer_3 = as.data.frame(lr_Tmountain_summer_3)
colnames(lr_Tmountain_summer_3) = c('variable','variable1_vif','variable2_vif','variable3_vif','AUC','Recall','Precision','F1')
lr_Tmountain_summer_3$st = 'Tmountain_summer'

stopCluster(cl)
end_time <- Sys.time()
end_time - start_time


f_lr_tmountain_fall = function(i) {
  library(caret)
  library(foreach)
  library(doParallel)
  library(parallel)
  library(tcltk)
  library(dplyr)
  set.seed(1000)
  fw1=collectionfw1[i]
  fw2=collectionfw2[i]
  fw3=collectionfw3[i]
  
  lr_Tmountain_fallr <-
    train(
      ONB_Event_YoN ~ .,
      data = Feature_scaled_4areas_lr(FD_NLgte4,'Temperate mountain system','fall',fw1,fw2,fw3),
      method = 'glm',
      family = 'binomial',
      metric = 'ROC',
      trControl = ctrl_lr
    )#glmStepAIC
  lr_Tmountain_fallr
  roc=round(lr_Tmountain_fallr$results[['ROC']], 2)
  y = car::vif(lr_Tmountain_fallr$finalModel)
  x = confusionMatrix(lr_Tmountain_fallr)
  recall = round((x[["table"]][2,2])/(x[["table"]][1,2]+x[["table"]][2,2]),3)
  z = paste(fw1,fw2,fw3,sep = "+")
  precision = round((x[["table"]][2,2])/(x[["table"]][2,1]+x[["table"]][2,2]),3)
  f1 = 2*as.numeric(recall)*as.numeric(precision)/(as.numeric(recall)+as.numeric(precision))
  op = c(z,y,roc,recall,precision,f1)
}


no_cores<-detectCores()-6
cl <- makeSOCKcluster(no_cores)
registerDoSNOW(cl)
pb <- txtProgressBar(min=1, max=4, style=3)#length(NAfire_year)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress=progress)

start_time <- Sys.time()
lr_Tmountain_fall_3 <- foreach(i=1:4, .options.snow = opts, .combine = 'rbind') %dopar%  f_lr_tmountain_fall(i)#, .combine = 'rbind'
lr_Tmountain_fall_3 = as.data.frame(lr_Tmountain_fall_3)
colnames(lr_Tmountain_fall_3) = c('variable','variable1_vif','variable2_vif','variable3_vif','AUC','Recall','Precision','F1')
lr_Tmountain_fall_3$st = 'Tmountain_fall'

stopCluster(cl)
end_time <- Sys.time()
end_time - start_time


f_lr_smountain_summer = function(i) {
  library(caret)
  library(foreach)
  library(doParallel)
  library(parallel)
  library(tcltk)
  library(dplyr)
  set.seed(1000)
  fw1=collectionfw1[i]
  fw2=collectionfw2[i]
  fw3=collectionfw3[i]
  
  lr_Smountain_summer <-
    train(
      ONB_Event_YoN ~ .,
      data = Feature_scaled_4areas_lr(FD_NLgte4,'Subtropical mountain system','summer',fw1,fw2,fw3),
      method = 'glm',
      family = 'binomial',
      metric = 'ROC',
      trControl = ctrl_lr
    )#glmStepAIC
  lr_Smountain_summer
  roc=round(lr_Smountain_summer$results[['ROC']], 2)
  y = car::vif(lr_Smountain_summer$finalModel)
  x = confusionMatrix(lr_Smountain_summer)
  recall = round((x[["table"]][2,2])/(x[["table"]][1,2]+x[["table"]][2,2]),3)
  z = paste(fw1,fw2,fw3,sep = "+")
  precision = round((x[["table"]][2,2])/(x[["table"]][2,1]+x[["table"]][2,2]),3)
  f1 = 2*as.numeric(recall)*as.numeric(precision)/(as.numeric(recall)+as.numeric(precision))
  op = c(z,y,roc,recall,precision,f1)
}

no_cores<-detectCores()-6
cl <- makeSOCKcluster(no_cores)
registerDoSNOW(cl)
pb <- txtProgressBar(min=1, max=4, style=3)#length(NAfire_year)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress=progress)

start_time <- Sys.time()
lr_Smountain_summer_3 <- foreach(i=1:4, .options.snow = opts, .combine = 'rbind') %dopar%  f_lr_smountain_summer(i)#, .combine = 'rbind'
lr_Smountain_summer_3 = as.data.frame(lr_Smountain_summer_3)
colnames(lr_Smountain_summer_3) = c('variable','variable1_vif','variable2_vif','variable3_vif','AUC','Recall','Precision','F1')
lr_Smountain_summer_3$st = 'Smountain_summer'

stopCluster(cl)
end_time <- Sys.time()
end_time - start_time


f_lr_smountain_fall = function(i) {
  library(caret)
  library(foreach)
  library(doParallel)
  library(parallel)
  library(tcltk)
  library(dplyr)
  set.seed(1000)
  fw1=collectionfw1[i]
  fw2=collectionfw2[i]
  fw3=collectionfw3[i]
  
  lr_Smountain_fall <-
    train(
      ONB_Event_YoN ~ .,
      data = Feature_scaled_4areas_lr(FD_NLgte4,'Subtropical mountain system','fall',fw1,fw2,fw3),
      method = 'glm',
      family = 'binomial',
      metric = 'ROC',
      trControl = ctrl_lr
    )#glmStepAIC
  lr_Smountain_fall
  roc=round(lr_Smountain_fall$results[['ROC']], 2)
  y = car::vif(lr_Smountain_fall$finalModel)
  x = confusionMatrix(lr_Smountain_fall)
  recall = round((x[["table"]][2,2])/(x[["table"]][1,2]+x[["table"]][2,2]),3)
  z = paste(fw1,fw2,fw3,sep = "+")
  precision = round((x[["table"]][2,2])/(x[["table"]][2,1]+x[["table"]][2,2]),3)
  f1 = 2*as.numeric(recall)*as.numeric(precision)/(as.numeric(recall)+as.numeric(precision))
  op = c(z,y,roc,recall,precision,f1)
}


no_cores<-detectCores()-6
cl <- makeSOCKcluster(no_cores)
registerDoSNOW(cl)
pb <- txtProgressBar(min=1, max=4, style=3)#length(NAfire_year)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress=progress)

start_time <- Sys.time()
lr_Smountain_fall_3 <- foreach(i=1:4, .options.snow = opts, .combine = 'rbind') %dopar%  f_lr_smountain_fall(i)#, .combine = 'rbind'
lr_Smountain_fall_3 = as.data.frame(lr_Smountain_fall_3)
colnames(lr_Smountain_fall_3) = c('variable','variable1_vif','variable2_vif','variable3_vif','AUC','Recall','Precision','F1')
lr_Smountain_fall_3$st = 'Smountain_fall'
stopCluster(cl)
end_time <- Sys.time()
end_time - start_time

lr_output_3 = rbind(lr_newboreal_summer_3,lr_Tmountain_summer_3,lr_Tmountain_fall_3,lr_Smountain_summer_3,lr_Smountain_fall_3)
rownames(lr_output_3) <- 1:nrow(lr_output_3)
lr_output_3 = lr_output_3 %>% filter(!(as.numeric(variable1_vif)   > 2 |as.numeric(variable2_vif)   > 2 | as.numeric(variable3_vif)   > 2))
lr_output_3 = lr_output_3 %>% dplyr::select(variable,AUC,Recall,Precision,F1,st)

Pred_df = rbind(lr_output,lr_output_2,lr_output_3)
Pred_df = Pred_df %>% mutate(sum=as.numeric(AUC)+as.numeric(Recall))
bs = Pred_df %>% filter(st=='Boreal_summer')
ts = Pred_df %>% filter(st=='Tmountain_summer')
tf = Pred_df %>% filter(st=='Tmountain_fall')
ss = Pred_df %>% filter(st=='Smountain_summer')
sf = Pred_df %>% filter(st=='Smountain_fall')





# Final models ------------------------------------------------------------

repGrid <- expand.grid(.mtry=c(1))

data <- FD_NLgte4 %>% filter(biome_4areas=='Boreal',season=='summer') %>% 
  dplyr::select(contains(
    c(
      'biome_4areas',
      'Ncondition',
      'FRPNoon2SetMeanFire',
      'ONB_Event_YoN',
      'DMC','FWI'
    )
  )) %>% dplyr::select(ONB_Event_YoN:FWI) %>% #summarise(data.frame(lapply(.,fn)))%>% 
  mutate(ONB_Event_YoN =ifelse(ONB_Event_YoN =='OBEs', 'L1', 'L0' ))

set.seed(1000)
lr_newboreal_summer <-
  train(
    ONB_Event_YoN ~ .,
    data = data,
    method = 'glm',
    family = 'binomial',
    metric = 'ROC',
    trControl = ctrl_lr
  )#glmStepAIC
lr_newboreal_summer
summary(lr_newboreal_summer)
car::vif(lr_newboreal_summer$finalModel)


data <- FD_NLgte4 %>% filter(biome_4areas=='Temperate mountain system',season=='summer') %>% 
  dplyr::select(contains(
    c(
      'biome_4areas',
      'Ncondition',
      'FRPNoon2SetMeanFire',
      'ONB_Event_YoN',
      'FWI','BUI'
    )
  )) %>% dplyr::select(ONB_Event_YoN:BUI) %>% #summarise(data.frame(lapply(.,fn)))%>% 
  mutate(ONB_Event_YoN =ifelse(ONB_Event_YoN =='OBEs', 'L1', 'L0' ))

set.seed(1000)
lr_Tmountain_summer <-
  train(
    ONB_Event_YoN ~ .,
    data = data,
    method = 'glm',
    family = 'binomial',
    metric = 'ROC',
    trControl = ctrl_lr
  )#glmStepAIC
lr_Tmountain_summer
summary(lr_Tmountain_summer)
car::vif(lr_Tmountain_summer$finalModel)
(confusionMatrix(lr_Tmountain_summer))[["table"]]


data <- FD_NLgte4 %>% filter(biome_4areas=='Temperate mountain system',season=='fall') %>% 
  dplyr::select(contains(
    c(
      'biome_4areas',
      'Ncondition',
      'FRPNoon2SetMeanFire',
      'ONB_Event_YoN',
      'BUI',
      'FWI'
    )
  )) %>% dplyr::select(ONB_Event_YoN:FWI) %>% #summarise(data.frame(lapply(.,fn)))%>% 
  mutate(ONB_Event_YoN =ifelse(ONB_Event_YoN =='OBEs', 'L1', 'L0' ))

set.seed(1000)
lr_Tmountain_fall <-
  train(
    ONB_Event_YoN ~ .,
    data = data,
    method = 'glm',
    family = 'binomial',
    metric = 'ROC',
    trControl = ctrl_lr
  )#glmStepAIC
lr_Tmountain_fall
summary(lr_Tmountain_fall)
car::vif(lr_Tmountain_fall$finalModel)
(confusionMatrix(lr_Tmountain_fall))[["table"]]


data <- FD_NLgte4 %>% filter(biome_4areas=='Subtropical mountain system',season=='summer') %>% 
  dplyr::select(contains(
    c(
      'biome_4areas',
      'Ncondition',
      'FRPNoon2SetMeanFire',
      'ONB_Event_YoN',
      'DMC'
    )
  )) %>% dplyr::select(ONB_Event_YoN:DMC) %>% #summarise(data.frame(lapply(.,fn)))%>% 
  mutate(ONB_Event_YoN =ifelse(ONB_Event_YoN =='OBEs', 'L1', 'L0' ))

set.seed(1000)
lr_Smountain_summer <-
  train(
    ONB_Event_YoN ~ .,
    data = data,
    method = 'glm',
    family = 'binomial',
    metric = 'ROC',
    trControl = ctrl_lr
  )#glmStepAIC
lr_Smountain_summer
summary(lr_Smountain_summer)
car::vif(lr_Smountain_summer$finalModel)
(confusionMatrix(lr_Smountain_summer))[["table"]]


data <- FD_NLgte4 %>% filter(biome_4areas=='Subtropical mountain system',season=='fall') %>% 
  dplyr::select(contains(
    c(
      'biome_4areas',
      'Ncondition',
      'FRPNoon2SetMeanFire',
      'ONB_Event_YoN',
      'DMC',
      'FWI'
    )
  )) %>% dplyr::select(ONB_Event_YoN:FWI) %>% #summarise(data.frame(lapply(.,fn)))%>% 
  mutate(ONB_Event_YoN =ifelse(ONB_Event_YoN =='OBEs', 'L1', 'L0' ))


set.seed(1000)
lr_Smountain_fall <-
  train(
    ONB_Event_YoN ~ .,
    data = data,
    method = 'glm',
    family = 'binomial',
    metric = 'ROC',
    trControl = ctrl_lr
  )#glmStepAIC
lr_Smountain_fall
summary(lr_Smountain_fall)
car::vif(lr_Smountain_fall$finalModel)
(confusionMatrix(lr_Smountain_fall))[["table"]]


lr_newboreal_summer
lr_Tmountain_summer
lr_Tmountain_fall
lr_Smountain_summer
lr_Smountain_fall

summary(lr_newboreal_summer)
summary(lr_Tmountain_summer)
summary(lr_Tmountain_fall)
summary(lr_Smountain_summer)
summary(lr_Smountain_fall)

x = confusionMatrix(lr_newboreal_summer)
x[["table"]]
x=confusionMatrix(lr_Tmountain_summer)
x[["table"]]
x=confusionMatrix(lr_Tmountain_fall)
x[["table"]]
x =confusionMatrix(lr_Smountain_summer)
x[["table"]]
x =confusionMatrix(lr_Smountain_fall)
x[["table"]]

# Final models plots ------------------------------------------------------


## Boreal summer
dd.roc.bs <- sapply(X = unique(lr_newboreal_summer$pred$Resample),
                    FUN = function(x) {
                      r <- lr_newboreal_summer$pred[lr_newboreal_summer$pred$Resample == x,]
                      R <- roc(response = r$obs, predictor = r$L1)
                      data.frame(sensitivities = R$sensitivities,
                                 specificities = 1-R$specificities)
                    }, simplify = F) %>%
  bind_rows(.id = "Resample") %>%
  as_tibble() %>%
  arrange(specificities)

d.roc.bs <- roc(response = lr_newboreal_summer$pred$obs, predictor = lr_newboreal_summer$pred$L1)
#> Setting levels: control = No, case = Yes
#> Setting direction: controls < cases

d.roc.bs <- data.frame(sensitivities = d.roc.bs$sensitivities,
                       specificities = 1-d.roc.bs$specificities)

pbs <- ggplot(dd.roc.bs, aes(x=specificities,y=sensitivities))+
  # geom_point(colour = "tomato", alpha = 0.1) +
  # geom_density2d() +
  geom_path(aes(group = Resample),color='mistyrose3', alpha = 0.1, size = 0.8) +
  # geom_smooth(colour = "orange", size = 1,level = 0.5) +
  geom_line(data = d.roc.bs, color='red', size = 0.8) +
  geom_abline(aes(slope = 1, intercept = 0),color='black',linetype = "dashed") +
  coord_cartesian(xlim = c(0,1),
                  ylim = c(0,1),
                  expand = FALSE)+
  #scale_x_reverse(limit = c(1,0)) +
  #scale_colour_manual(values = c("red","red4"), name = "") +
  theme_bw() +
  theme(
    plot.background = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank())+
  theme(legend.position = "bottom",aspect.ratio=1,
        text = element_text(size = 7))+
  labs(x='False postive rate',y='True positive rate',
       title = "Boreal",
       # subtitle = "Plot of length by dose",
       #caption = "Boreal summer"
  )


pbs

## TMS summer
dd.roc.ts <- sapply(X = unique(lr_Tmountain_summer$pred$Resample),
                    FUN = function(x) {
                      r <- lr_Tmountain_summer$pred[lr_Tmountain_summer$pred$Resample == x,]
                      R <- roc(response = r$obs, predictor = r$L1)
                      data.frame(sensitivities = R$sensitivities,
                                 specificities = 1-R$specificities)
                    }, simplify = F) %>%
  bind_rows(.id = "Resample") %>%
  as_tibble() %>%
  arrange(specificities)

dd.roc.ts
d.roc.ts <- roc(response = lr_Tmountain_summer$pred$obs, predictor = lr_Tmountain_summer$pred$L1)
#> Setting levels: control = No, case = Yes
#> Setting direction: controls < cases

d.roc.ts <- data.frame(sensitivities = d.roc.ts$sensitivities,
                       specificities = 1-d.roc.ts$specificities)


## TMS fall
dd.roc.tf <- sapply(X = unique(lr_Tmountain_fall$pred$Resample),
                    FUN = function(x) {
                      r <- lr_Tmountain_fall$pred[lr_Tmountain_fall$pred$Resample == x,]
                      R <- roc(response = r$obs, predictor = r$L1)
                      data.frame(sensitivities = R$sensitivities,
                                 specificities = 1-R$specificities)
                    }, simplify = F) %>%
  bind_rows(.id = "Resample") %>%
  as_tibble() %>%
  arrange(specificities)

dd.roc.tf
d.roc.tf <- roc(response = lr_Tmountain_fall$pred$obs, predictor = lr_Tmountain_fall$pred$L1)
#> Setting levels: control = No, case = Yes
#> Setting direction: controls < cases

d.roc.tf <- data.frame(sensitivities = d.roc.tf$sensitivities,
                       specificities = 1-d.roc.tf$specificities)


pt <- ggplot()+
  geom_path(data=dd.roc.ts,mapping=aes(x=specificities,y=sensitivities,group = Resample), color='mistyrose3',alpha = 0.05, size = 0.8) +
  geom_path(data=dd.roc.tf,mapping=aes(x=specificities,y=sensitivities,group = Resample), color='tan',alpha = 0.05, size = 0.8) +
  geom_line(data = d.roc.ts, aes(x=specificities,y=sensitivities),color='red', size = 0.8)+
  geom_line(data = d.roc.tf, aes(x=specificities,y=sensitivities),color='orange',  size = 0.8)+
  geom_abline(aes(slope = 1, intercept = 0),color='black',linetype = "dashed") +
  coord_cartesian(xlim = c(0,1),
                  ylim = c(0,1),
                  expand = FALSE)+
  #scale_x_reverse(limit = c(1,0)) +
  theme_bw() +
  theme(
    plot.background = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank())+
  theme(legend.position = "bottom",aspect.ratio=1,
        text = element_text(size = 7))+
  labs(x='False postive rate',y='True positive rate',
       title = "Temperate mountain system",
       # subtitle = "Plot of length by dose",
       #caption = "Boreal summer"
  )

pt


## SMS summer
dd.roc.ss <- sapply(X = unique(lr_Smountain_summer$pred$Resample),
                    FUN = function(x) {
                      r <- lr_Smountain_summer$pred[lr_Smountain_summer$pred$Resample == x,]
                      R <- roc(response = r$obs, predictor = r$L1)
                      data.frame(sensitivities = R$sensitivities,
                                 specificities = 1-R$specificities)
                    }, simplify = F) %>%
  bind_rows(.id = "Resample") %>%
  as_tibble() %>%
  arrange(specificities)

dd.roc.ss
d.roc.ss <- roc(response = lr_Smountain_summer$pred$obs, predictor = lr_Smountain_summer$pred$L1)
#> Setting levels: control = No, case = Yes
#> Setting direction: controls < cases

d.roc.ss <- data.frame(sensitivities = d.roc.ss$sensitivities,
                       specificities = 1-d.roc.ss$specificities)


## SMS fall
dd.roc.sf <- sapply(X = unique(lr_Smountain_fall$pred$Resample),
                    FUN = function(x) {
                      r <- lr_Smountain_fall$pred[lr_Smountain_fall$pred$Resample == x,]
                      R <- roc(response = r$obs, predictor = r$L1)
                      data.frame(sensitivities = R$sensitivities,
                                 specificities = 1-R$specificities)
                    }, simplify = F) %>%
  bind_rows(.id = "Resample") %>%
  as_tibble() %>%
  arrange(specificities)

dd.roc.sf
d.roc.sf <- roc(response = lr_Smountain_fall$pred$obs, predictor = lr_Smountain_fall$pred$L1)
#> Setting levels: control = No, case = Yes
#> Setting direction: controls < cases

d.roc.sf <- data.frame(sensitivities = d.roc.sf$sensitivities,
                       specificities = 1-d.roc.sf$specificities)

ps <- ggplot()+
  geom_path(data=dd.roc.ss,mapping=aes(x=specificities,y=sensitivities,group = Resample,colour = "ROC per(summer)"),alpha = 0.05, size = 0.8) +
  geom_path(data=dd.roc.sf,mapping=aes(x=specificities,y=sensitivities,group = Resample,colour = "ROC per (fall)"),alpha = 0.05, size = 0.8) +
  geom_line(data = d.roc.ss, aes(x=specificities,y=sensitivities,colour = "ROC over (summer)"), size = 0.8)+
  geom_line(data = d.roc.sf, aes(x=specificities,y=sensitivities,colour = "ROC over (fall)"), size = 0.8)+
  geom_abline(aes(slope = 1, intercept = 0),color='black',linetype = "dashed") +
  coord_cartesian(xlim = c(0,1),
                  ylim = c(0,1),
                  expand = FALSE)+
  #scale_x_reverse(limit = c(1,0)) +
  theme_bw() +
  theme(
    plot.background = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank())+
  theme(legend.position = "bottom",aspect.ratio=1,legend.box="horizontal", 
        text = element_text(size = 7))+
  labs(x='False postive rate',y='True positive rate',
       title = "Subtropical mountain system",
       # subtitle = "Plot of length by dose",
       #caption = "Boreal summer"
  )+  scale_colour_manual(values = rev(c('mistyrose3','tan','red','orange')), name = "")
ps


p <- plot_grid(pbs, pt,ps,  ncol = 3, align = "v")
p




