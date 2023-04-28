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
    -inci_name,
    -AFTempDayMeanFire,
    -AFareaDayMeanFire,
    -AFTempNightMeanFire,
    -AFareaNightMeanFire
  )
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
      (biome == 25 &
         WorEzone == 'W') ~ 'Subtropical mountain system',
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


# fire weather comparison fig 3a,3b, supp fig s2 --------------------------

FD_df_long <- FD_NLgte4 %>%
  dplyr::select(contains(
    c(
      'year',
      'seq',
      'ID',
      'ONB_FIRE_YoN',
      'ONB_Event_YoN',
      'biome_4areas',
      'season',
      'Ncondition',
      'FRPNoon2SetMeanFire',
      'BUI',
      'DMC',
      'DC',
      'FWI',
      'vpd_daymax',
      'ffmc_daymax',
      'isi_daymax',
      #'emc_daymin',
      'vpd_nightmin',
      'ffmc_nightmin',
      'isi_nightmin',
      #'emc_nightmax',
      # 'prec_daysum',
      # 'prec_nightsum',
      'temp_daymax',
      'temp_nightmin',
      'rh_daymin',
      'rh_nightmax'
      # 'winds_daymean',
      # 'winds_nightmean'
    )
  )) %>%
  gather(BUI:rh_nightmax, key = variable, value = value) %>% drop_na(biome_4areas)

FD_df_long[FD_df_long == "isi_daymax"] <- 'ISI Dmax'
FD_df_long[FD_df_long == "isi_nightmin"] <- "ISI Nmin"
FD_df_long["variable"][FD_df_long["variable"] == "ffmc_daymax"] <-
  'FFMC Dmax'
FD_df_long["variable"][FD_df_long["variable"] == "ffmc_nightmin"] <-
  "FFMC Nmin"
FD_df_long["variable"][FD_df_long["variable"] == "vpd_daymax"] <-
  "VPD Dmax"
FD_df_long["variable"][FD_df_long["variable"] == "vpd_nightmin"] <-
  "VPD Nmin"
FD_df_long["variable"][FD_df_long["variable"] == "temp_daymax"] <-
  "T Dmax"
FD_df_long["variable"][FD_df_long["variable"] == "temp_nightmin"] <-
  "T Nmin"
FD_df_long["variable"][FD_df_long["variable"] == "rh_daymin"] <-
  "RH Dmin"
FD_df_long["variable"][FD_df_long["variable"] == "rh_nightmax"] <-
  "RH Nmax"

FD_df_long$variable <-
  factor(
    FD_df_long$variable,
    levels = c(
      'FWI',
      'BUI',
      'DMC',
      'DC',
      'ISI Dmax',
      "ISI Nmin",
      'FFMC Dmax',
      "FFMC Nmin" ,
      'VPD Dmax',
      "VPD Nmin",
      'T Dmax',
      'T Nmin',
      'RH Dmin',
      'RH Nmax'
    )
  )
FD_df_long$Ncondition <-
  factor(FD_df_long$Ncondition, levels = c('OBE', 'NEE', 'Non-NBE'))
FD_df_long$season <-
  factor(FD_df_long$season,
         levels = c('spring', 'summer', 'fall', 'winter'))

FD_df_long$ONB_Event_YoN <-
  factor(FD_df_long$ONB_Event_YoN, levels = c('OBEs',
                                              'Non events'))

library(ggnewscale)
g1 = ggplot() +
  # geom_boxplot(notch = T,outlier.size = 0.2,size=0.4,notchwidth = 0.5)+
  geom_density(
    data = FD_df_long %>%
      filter(biome_4areas == 'Boreal', season %in% c('summer')),
    aes(x = value, color = ONB_Event_YoN, y = ..density..)
  ) +
  scale_color_manual(values = c('red', 'gray')) +
  # new_scale_color() +
  # geom_density(data=FD_df_long %>%
  #                filter(biome_4areas=='Boreal',season%in%c('fall')),
  #              aes(x=value,color=ONB_Event_YoN,y = - ..density..))+
  # scale_color_manual(values=c('orange','black'))+
  facet_wrap( ~ variable, scale = "free", nrow = 2) +
  # stat_summary(fun.y=mean, geom="point", shape=2, size=0.6, color="yellow",
  #              position = position_dodge(0.75),show.legend = FALSE) +
  #theme(legend.position = c(0.92,0.1))+
  guides(fill = guide_legend(title = "")) +
  #scale_color_manual(values=c('darkred','gray'))+
  envalysis::theme_publish(
    base_size = 6,
    base_family = "",
    line_size = 0.25,
    base_rect_size = 0.25
  ) +
  scale_x_continuous(n.breaks	= 4) +
  scale_y_continuous(n.breaks	= 3) +
  labs(title = 'Boreal') +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    text = element_text(size = 6),
    #axis.text.x=element_blank(),
    strip.text.x = element_text(size = 6, color = "black", face = "plain"),
    aspect.ratio = 1,
    plot.title = element_text(
      color = "black",
      size = 6,
      face = "bold",
      vjust = -1
    )
  ) +
  labs(fill = '')
g1

# x <- paste0('015.5_Plots\\fig.comparison.supp.boreal2','.pdf')
# ggsave(plot=g1,x,width = 20.3,height =6.5,units = 'cm')

g2 = ggplot() +
  # geom_boxplot(notch = T,outlier.size = 0.2,size=0.4,notchwidth = 0.5)+
  geom_density(
    data = FD_df_long %>%
      filter(
        biome_4areas == 'Temperate mountain system',
        season %in% c('summer')
      ),
    aes(x = value, color = ONB_Event_YoN, y = ..density..)
  ) +
  scale_color_manual(values = c('red', 'gray')) +
  new_scale_color() +
  geom_density(
    data = FD_df_long %>%
      filter(
        biome_4areas == 'Temperate mountain system',
        season %in% c('fall')
      ),
    aes(
      x = value,
      color = ONB_Event_YoN,
      y = -..density..
    )
  ) +
  scale_color_manual(values = c('orange', 'black')) +
  facet_wrap( ~ variable, scale = "free", nrow = 2) +
  # stat_summary(fun.y=mean, geom="point", shape=2, size=0.6, color="yellow",
  #              position = position_dodge(0.75),show.legend = FALSE) +
  #theme(legend.position = c(0.92,0.1))+
  scale_x_continuous(n.breaks	= 4) +
  scale_y_continuous(n.breaks	= 4.5) +
  guides(fill = guide_legend(title = "")) +
  #scale_color_manual(values=c('darkred','gray'))+
  envalysis::theme_publish(
    base_size = 6,
    base_family = "",
    line_size = 0.25,
    base_rect_size = 0.25
  ) +
  labs(title = 'Temperate mountain system') +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    text = element_text(size = 6),
    #axis.text.x=element_blank(),
    strip.text.x = element_text(size = 6, color = "black", face = "plain"),
    aspect.ratio = 1,
    plot.title = element_text(
      color = "black",
      size = 6,
      face = "bold",
      vjust = -1
    )
  ) +
  labs(fill = '')
g2

# x <- paste0('015.5_Plots\\fig.comparison.supp.tms2','.pdf')
# ggsave(plot=g2,x,width = 20.3,height =6.5,units = 'cm')

g3 = ggplot() +
  # geom_boxplot(notch = T,outlier.size = 0.2,size=0.4,notchwidth = 0.5)+
  geom_density(
    data = FD_df_long %>%
      filter(
        biome_4areas == 'Subtropical mountain system',
        season %in% c('summer')
      ),
    aes(x = value, color = ONB_Event_YoN, y = ..density..)
  ) +
  scale_color_manual(values = c('red', 'gray')) +
  new_scale_color() +
  geom_density(
    data = FD_df_long %>%
      filter(
        biome_4areas == 'Temperate mountain system',
        season %in% c('fall')
      ),
    aes(
      x = value,
      color = ONB_Event_YoN,
      y = -..density..
    )
  ) +
  scale_color_manual(values = c('orange', 'black')) +
  facet_wrap( ~ variable, scale = "free", nrow = 2) +
  # stat_summary(fun.y=mean, geom="point", shape=2, size=0.6, color="yellow",
  #              position = position_dodge(0.75),show.legend = FALSE) +
  #theme(legend.position = c(0.92,0.1))+
  guides(fill = guide_legend(title = "")) +
  scale_x_continuous(n.breaks	= 4) +
  scale_y_continuous(n.breaks	= 4) +
  #scale_color_manual(values=c('darkred','gray'))+
  envalysis::theme_publish(
    base_size = 6,
    base_family = "",
    line_size = 0.25,
    base_rect_size = 0.25
  ) +
  labs(title = 'Subtropical mountain system') +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    text = element_text(size = 6),
    #axis.text.x=element_blank(),
    strip.text.x = element_text(size = 6, color = "black", face = "plain"),
    aspect.ratio = 1,
    plot.title = element_text(
      color = "black",
      size = 6,
      face = "bold",
      vjust = -1
    )
  ) +
  labs(fill = '')
g3


# fire weather comparison statistics --------------------------------------

FD_df <- FD_NLgte4 %>%
  dplyr::select(contains(
    c(
      'year',
      'seq',
      'ID',
      'ONB_FIRE_YoN',
      'ONB_Event_YoN',
      'biome_4areas',
      'season',
      'Ncondition',
      'FRPNoon2SetMeanFire',
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
      'temp_nightmin',
      'rh_daymin',
      'rh_nightmax'
    )
  ))

regionff = unique((FD_df %>% drop_na())$biome_4areas)
seasonff = unique((FD_df %>% drop_na())$season)
variableff = c(
  'FWI',
  'BUI',
  'DMC',
  'DC',
  'isi_daymax',
  "isi_nightmin",
  'ffmc_daymax',
  "ffmc_nightmin" ,
  'vpd_daymax',
  "vpd_nightmin",
  'temp_daymax',
  'temp_nightmin',
  'rh_daymin',
  'rh_nightmax'
)
options(scipen = 10)

### U-test
df_total = data.frame()
for (regionf in regionff) {
  for (seasonf in seasonff) {
    for (variablef in variableff) {
      xx = FD_df  %>% filter(biome_4areas == regionf,
                             season == seasonf,
                             ONB_Event_YoN == 'OBEs')
      if (nrow(xx) > 100) {
        dobe = as.matrix(
          FD_df  %>% filter(
            biome_4areas == regionf,
            season == seasonf,
            ONB_Event_YoN == 'OBEs'
          ) %>%
            dplyr::select(one_of(variablef))
        )
        dnonobe = as.matrix(
          FD_df %>% filter(
            biome_4areas == regionf,
            season == seasonf,
            ONB_Event_YoN == 'Non events'
          ) %>%
            dplyr::select(one_of(variablef))
        )
        Norm_obe = (shapiro.test(dobe))$p.value
        Norm_non = (shapiro.test(dnonobe[sample(nrow(dnonobe), 1000),]))$p.value
        
        
        x = wilcox.test(dobe,
                        dnonobe, alternative = 'greater')#wilcox.test,var.equal = F
        pvalue = round(x[['p.value']], 10)
        df <-
          data.frame(regionf, seasonf, variablef, pvalue, Norm_obe, Norm_non)
        df_total <- rbind(df_total, df)
      }
    }
  }
}
df_sig <- df_total %>% filter(pvalue <= 0.05 & pvalue >= 0.001)
df_sig1 <- df_total %>% filter(pvalue > 0.05)

# diff comparison supp fig s3 ---------------------------------------------

FD_df_long <- FD_NLgte4 %>%
  dplyr::select(contains(
    c(
      'year',
      'seq',
      'ID',
      'ONB_FIRE_YoN',
      'ONB_Event_YoN',
      'biome_4areas',
      'season',
      'Ncondition',
      'FRPNoon2SetMeanFire',
      'isi_diff_Dmax_Nmin',
      'ffmc_diff_Dmax_Nmin',
      'vpd_diff_Dmax_Nmin',
      'temp_diff_Dmax_Nmin',
      'rh_diff_Dmin_Nmax'
    )
  )) %>%
  gather(isi_diff_Dmax_Nmin:rh_diff_Dmin_Nmax,
         key = variable,
         value = value) %>% drop_na(biome_4areas)

FD_df_long[FD_df_long == "isi_diff_Dmax_Nmin"] <- 'ISIDmax-'
FD_df_long[FD_df_long == "ffmc_diff_Dmax_Nmin"] <- "FFMCDmax-"
FD_df_long["variable"][FD_df_long["variable"] == "vpd_diff_Dmax_Nmin"] <-
  'VPDDmax-'
FD_df_long["variable"][FD_df_long["variable"] == "temp_diff_Dmax_Nmin"] <-
  "TDmax-"
FD_df_long["variable"][FD_df_long["variable"] == "rh_diff_Dmin_Nmax"] <-
  "RHDmin-"

FD_df_long$Ncondition <-
  factor(FD_df_long$Ncondition, levels = c('OBE', 'NEE', 'Non-NBE'))
FD_df_long$season <-
  factor(FD_df_long$season,
         levels = c('spring', 'summer', 'fall', 'winter'))

FD_df_long$ONB_Event_YoN <-
  factor(FD_df_long$ONB_Event_YoN, levels = c('OBEs',
                                              'Non events'))

library(ggnewscale)
g1 = ggplot() +
  # geom_boxplot(notch = T,outlier.size = 0.2,size=0.4,notchwidth = 0.5)+
  geom_density(
    data = FD_df_long %>%
      filter(biome_4areas == 'Boreal', season %in% c('summer')),
    aes(x = value, color = ONB_Event_YoN, y = ..density..)
  ) +
  scale_color_manual(values = c('red', 'gray')) +
  # new_scale_color() +
  # geom_density(data=FD_df_long %>%
  #                filter(biome_4areas=='Boreal',season%in%c('fall')),
  #              aes(x=value,color=ONB_Event_YoN,y = - ..density..))+
  # scale_color_manual(values=c('orange','black'))+
  facet_wrap( ~ variable, scale = "free", nrow = 1) +
  # stat_summary(fun.y=mean, geom="point", shape=2, size=0.6, color="yellow",
  #              position = position_dodge(0.75),show.legend = FALSE) +
  #theme(legend.position = c(0.92,0.1))+
  guides(fill = guide_legend(title = "")) +
  #scale_color_manual(values=c('darkred','gray'))+
  envalysis::theme_publish(
    base_size = 7,
    base_family = "",
    line_size = 0.25,
    base_rect_size = 0.25
  ) +
  labs(title = 'Boreal') +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    text = element_text(size = 7),
    #axis.text.x=element_blank(),
    strip.text.x = element_text(size = 7, color = "black", face = "plain"),
    plot.title = element_text(
      color = "black",
      size = 7,
      face = "bold",
      vjust = -1
    )
  ) +
  labs(fill = '')
g1

g2 = ggplot() +
  # geom_boxplot(notch = T,outlier.size = 0.2,size=0.4,notchwidth = 0.5)+
  geom_density(
    data = FD_df_long %>%
      filter(
        biome_4areas == 'Temperate mountain system',
        season %in% c('summer')
      ),
    aes(x = value, color = ONB_Event_YoN, y = ..density..)
  ) +
  scale_color_manual(values = c('red', 'gray')) +
  new_scale_color() +
  geom_density(
    data = FD_df_long %>%
      filter(
        biome_4areas == 'Temperate mountain system',
        season %in% c('fall')
      ),
    aes(
      x = value,
      color = ONB_Event_YoN,
      y = -..density..
    )
  ) +
  scale_color_manual(values = c('orange', 'black')) +
  facet_wrap( ~ variable, scale = "free", nrow = 1) +
  # stat_summary(fun.y=mean, geom="point", shape=2, size=0.6, color="yellow",
  #              position = position_dodge(0.75),show.legend = FALSE) +
  #theme(legend.position = c(0.92,0.1))+
  guides(fill = guide_legend(title = "")) +
  #scale_color_manual(values=c('darkred','gray'))+
  envalysis::theme_publish(
    base_size = 7,
    base_family = "",
    line_size = 0.25,
    base_rect_size = 0.25
  ) +
  labs(title = 'Temperate mountain system') +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    text = element_text(size = 7),
    #axis.text.x=element_blank(),
    strip.text.x = element_text(size = 7, color = "black", face = "plain"),
    plot.title = element_text(
      color = "black",
      size = 7,
      face = "bold",
      vjust = -1
    )
  ) +
  labs(fill = '')
g2

g3 = ggplot() +
  # geom_boxplot(notch = T,outlier.size = 0.2,size=0.4,notchwidth = 0.5)+
  geom_density(
    data = FD_df_long %>%
      filter(
        biome_4areas == 'Subtropical mountain system',
        season %in% c('summer')
      ),
    aes(x = value, color = ONB_Event_YoN, y = ..density..)
  ) +
  scale_color_manual(values = c('red', 'gray')) +
  new_scale_color() +
  geom_density(
    data = FD_df_long %>%
      filter(
        biome_4areas == 'Temperate mountain system',
        season %in% c('fall')
      ),
    aes(
      x = value,
      color = ONB_Event_YoN,
      y = -..density..
    )
  ) +
  scale_color_manual(values = c('orange', 'black')) +
  facet_wrap( ~ variable, scale = "free", nrow = 1) +
  # stat_summary(fun.y=mean, geom="point", shape=2, size=0.6, color="yellow",
  #              position = position_dodge(0.75),show.legend = FALSE) +
  #theme(legend.position = c(0.92,0.1))+
  guides(fill = guide_legend(title = "")) +
  #scale_color_manual(values=c('darkred','gray'))+
  envalysis::theme_publish(
    base_size = 7,
    base_family = "",
    line_size = 0.25,
    base_rect_size = 0.25
  ) +
  labs(title = 'Subtropical mountain system') +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    text = element_text(size = 7),
    #axis.text.x=element_blank(),
    strip.text.x = element_text(size = 7, color = "black", face = "plain"),
    plot.title = element_text(
      color = "black",
      size = 7,
      face = "bold",
      vjust = -1
    )
  ) +
  labs(fill = '')
g3

g <-
  plot_grid(
    g1,
    g2,
    g3,
    ncol = 1,
    align = "v",
    rel_heights = c(0.8, 1, 1)
  )
g

# x <- paste0('015.5_Plots\\fig.range111','.pdf')
# ggsave(plot=g,x,width = 18,height =14,units = 'cm')


# diff comparison statistics ----------------------------------------------

FD_df <- FD_NLgte4 %>%
  dplyr::select(contains(
    c(
      'year',
      'seq',
      'ID',
      'ONB_FIRE_YoN',
      'ONB_Event_YoN',
      'biome_4areas',
      'season',
      'Ncondition',
      'FRPNoon2SetMeanFire',
      'isi_diff_Dmax_Nmin',
      'ffmc_diff_Dmax_Nmin',
      'vpd_diff_Dmax_Nmin',
      'temp_diff_Dmax_Nmin',
      'rh_diff_Dmin_Nmax'
    )
  ))

regionff = unique((FD_df %>% drop_na())$biome_4areas)
seasonff = unique((FD_df %>% drop_na())$season)
variableff = c(
  'isi_diff_Dmax_Nmin',
  'ffmc_diff_Dmax_Nmin',
  'vpd_diff_Dmax_Nmin',
  'temp_diff_Dmax_Nmin',
  'rh_diff_Dmin_Nmax'
)
options(scipen = 10)

df_total = data.frame()
for (regionf in regionff) {
  for (seasonf in seasonff) {
    for (variablef in variableff) {
      xx = FD_df  %>% filter(biome_4areas == regionf,
                             season == seasonf,
                             ONB_Event_YoN == 'OBEs')
      if (nrow(xx) > 100) {
        dobe = as.matrix(
          FD_df  %>% filter(
            biome_4areas == regionf,
            season == seasonf,
            ONB_Event_YoN == 'OBEs'
          ) %>%
            dplyr::select(one_of(variablef))
        )
        dnonobe = as.matrix(
          FD_df %>% filter(
            biome_4areas == regionf,
            season == seasonf,
            ONB_Event_YoN == 'Non events'
          ) %>%
            dplyr::select(one_of(variablef))
        )
        Norm_obe = (shapiro.test(dobe))$p.value
        Norm_non = (shapiro.test(dnonobe[sample(nrow(dnonobe), 1000),]))$p.value
        x = wilcox.test(dobe,
                        dnonobe, alternative = 'less')#wilcox.test
        pvalue = round(x[['p.value']], 10)
        df <-
          data.frame(regionf, seasonf, variablef, pvalue, Norm_obe, Norm_non)
        df_total <- rbind(df_total, df)
      }
    }
  }
}
df_sig <- df_total %>% filter(pvalue <= 0.001)
df_sig <- df_total %>% filter(pvalue <= 0.05 & pvalue >= 0.001)
df_sig1 <- df_total %>% filter(pvalue > 0.05)
