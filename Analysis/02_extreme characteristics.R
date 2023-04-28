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
  read.csv('FeatureDesign\\FD_All_by2FireHr_gt1000.csv')
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


# concentration -----------------------------------------------------------


ONB_SingleEvent <-
  FD_NLgte4 %>% filter(ONB_Event_YoN == 'OBEs') %>% group_by(year, seq) %>% filter(n() ==
                                                                                     1)
ONB_multiEvent <-
  FD_NLgte4 %>% filter(ONB_Event_YoN == 'OBEs') %>% group_by(year, seq) %>% filter(n() >
                                                                                     1)

obe_mul.stat <- ONB_multiEvent %>%   mutate(
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
    (biome == 41 | 42 | 43) ~ 'Boreal',
    (biome == 50) ~ 'Polar'
  )
) %>% group_by(biome) %>% summarise(counts = n())

ONB_stat <-
  FD_NLgte4 %>% filter(ONB_Event_YoN == 'OBEs') %>% group_by(year, seq, biome, country, lat, long) %>% mutate(daydiff =
                                                                                                                max(day) - min(day)) %>%
  summarise(
    counts = n(),
    daydiff = mean(daydiff),
    daymin = min(day),
    daymax = max(day)
  ) %>% mutate(daydiff.avg = case_when((counts == 1) ~ 0, (counts >= 1) ~ ((daydiff +
                                                                              1) / counts)))
ONB_stat1 <-
  ONB_stat %>% group_by(counts) %>% summarise(
    ncounts = n(),
    events = sum(counts),
    daysum = sum(daydiff) + ncounts
  ) %>% mutate(daydiff.avg = daysum / events)


library(gglorenz)
p2a = ONB_stat %>%
  ggplot(aes(counts)) +
  stat_lorenz(desc = F, color = 'darkred') +
  coord_fixed() +
  geom_abline(linetype = "dashed") +
  stat_lorenz(data = ONB_stat %>% filter(biome == 25),
              desc = F,
              color = 'olivedrab1') +
  stat_lorenz(data = ONB_stat %>% filter(biome == 35),
              desc = F,
              color = 'lightgoldenrod2') +
  #stat_lorenz(data=ONB_stat %>% filter(biome==34),desc = F,color='green')+
  stat_lorenz(
    data = ONB_stat %>% filter(biome == 41 ||
                                 biome == 42 || biome == 43),
    desc = F,
    color = 'slateblue3'
  ) +
  hrbrthemes::scale_x_percent() +
  hrbrthemes::scale_y_percent() +
  hrbrthemes::theme_ipsum_rc() +
  geom_segment(
    aes(
      x = 0,
      y = 0.498155,
      xend = 0.869697,
      yend = 0.498155
    ),
    linetype = 'dashed',
    color = 'darkred'
  ) +
  geom_segment(
    aes(
      x = 0.869697,
      y = 0,
      xend = 0.869697,
      yend = 0.498155
    ),
    linetype = 'dashed',
    color = 'darkred'
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 7),
    strip.text.x = element_text(size = 7, color = "black", face = "bold"),
    aspect.ratio = 1,
    #panel.grid.minor = element_blank(), panel.grid.major = element_blank()
  ) +
  labs(x = "Cumulative Percentage of fires with OBEs",
       y = "Cumulative Percentage of OBEs")
p2a


# number of days between consecutive OBEs ---------------------------------


daydiff_obe = ONB_multiEvent %>%  group_by(year, seq) %>% mutate(lagday =
                                                                   lag(day), daydiff = day - lagday) %>%
  group_by(daydiff) %>% summarise(counts = n()) %>% mutate(sumcounts = daydiff *
                                                             counts)
x1 = data.frame(daydiff_obe$daydiff) %>% drop_na()
summary(x1)
sd(x1$daydiff_obe.daydiff)

p2b = ggplot() + geom_bar(
  data = daydiff_obe,
  aes(x = daydiff, y = counts),
  fill = 'gray',
  stat = "identity"
) +
  geom_point(
    data = daydiff_obe,
    aes(x = daydiff, y = counts),
    stat = "identity",
    color = 'black',
    size = 0.8
  ) +
  # geom_smooth(data=daydiff_obe,aes(x=daydiff,y=counts),color='slateblue3',fill='slateblue3')+
  
  theme_bw() +
  xlab('Days between consecutive OBEs') + ylab('Quantity') +
  theme(
    text = element_text(size = 7),
    strip.text.x = element_text(size = 7, color = "black", face = "bold"),
    aspect.ratio = 1,
    #panel.grid.minor = element_blank(), panel.grid.major = element_blank()
  )

p2b


# number of days between ignition and first OBE ---------------------------

FirstOBE = FD_NLgte4 %>% filter(ONB_Event_YoN == 'OBEs') %>% group_by(year, seq, ID, country) %>%
  arrange(day) %>%  summarise(firstobe = day[1])
Ignitionday = FD_NLgte4 %>% group_by(year, seq, ID) %>%
  arrange(day) %>% summarise(startday = day[1])
comb = left_join(FirstOBE, Ignitionday) %>% mutate(daydiff = firstobe -
                                                     startday) %>%
  mutate(x_bins = cut(daydiff, breaks = c(-0.0001, 0, 2, 5, 10, 100)))

summary(comb$daydiff)
combsumry = comb %>% group_by(daydiff) %>% summarise(counts = n())
p2d = ggplot() + geom_bar(
  data = combsumry,
  aes(x = daydiff, y = counts),
  stat = "identity",
  fill = 'gray'
) + ylab('Quantity') +
  geom_point(
    data = combsumry,
    aes(x = daydiff, y = counts),
    color = 'black',
    size = 0.8
  ) + xlab('Days between ignition and first OBE') +
  # geom_smooth(data=x3,aes(x=daydiff,y=counts),color='slateblue3',fill='slateblue3')+
  
  theme_bw() +
  theme(
    text = element_text(size = 7),
    strip.text.x = element_text(size = 7, color = "black", face = "bold"),
    aspect.ratio = 1,
    #panel.grid.minor = element_blank(), panel.grid.major = element_blank()
  )
p2d


# burned area between fire with and without OBE ---------------------------

BA = FD_NLgte4 %>% group_by(year, seq, ID, country, POLY_HA) %>%
  summarise(POLY_HA = POLY_HA[1], ONB_Event_YoN = ONB_Event_YoN[1])
firewOBE = BA %>% filter(ONB_Event_YoN == 'OBEs')
firewoutOBE = BA %>% filter(ONB_Event_YoN != 'OBEs')
summary(firewOBE)
summary(firewoutOBE)


BAdf = FD_NLgte4 %>% filter(ONB_Event_YoN == 'OBEs') %>%
  group_by(year, seq, POLY_HA, biome) %>% summarise(counts = n()) %>%
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
      (biome == 41 | 42 | 43) ~ 'Boreal',
      (biome == 50) ~ 'Polar'
    )
  )
#mutate_at(4,~replace(., is.na(.), 'Others'))

BAdf$biome <- factor(
  BAdf$biome,
  levels = c(
    'Boreal',
    "Temperate continental forest",
    "Temperate desert",
    "Temperate mountain system",
    "Temperate steppe",
    "Subtropical desert",
    "Subtropical mountain system",
    "Subtropical steppe",
    "Tropical moist forest"
  )
)

windowsFonts(Times = windowsFont("Times New Roman"))

options(scipen = 1)
p2c <- ggplot(data = BAdf, aes(x = counts, y = POLY_HA / 100000)) +
  geom_point(
    data = BAdf,
    aes(x = counts, y = POLY_HA / 100000),
    color = 'gray',
    size = 0.8
  ) +
  geom_point(
    data = BAdf %>% filter(
      biome == 'Boreal' |
        biome == 'Temperate mountain system' |
        biome == 'Subtropical mountain system'
    ),
    #biome=='Temperate desert'|
    aes(
      x = counts,
      y = POLY_HA / 100000,
      color = biome
    ),
    size = 0.8
  ) +
  geom_smooth(
    data = BAdf,
    aes(x = counts, y = POLY_HA / 100000),
    method = lm,
    color = 'darkred',
    fill = 'darkred',
    size = 0.8
  ) +
  geom_smooth(
    data = BAdf %>% filter(biome == 'Boreal'),
    aes(x = counts, y = POLY_HA / 100000),
    method = lm,
    color = 'slateblue3',
    fill = 'slateblue3',
    size = 0.8
  ) +
  geom_smooth(
    data = BAdf %>% filter(biome == 'Temperate mountain system'),
    aes(x = counts, y = POLY_HA / 100000),
    method = lm,
    color = 'lightgoldenrod2',
    fill = 'lightgoldenrod2',
    size = 0.8
  ) +
  geom_smooth(
    data = BAdf %>% filter(biome == 'Subtropical mountain system'),
    aes(x = counts, y = POLY_HA / 100000),
    method = lm,
    color = 'olivedrab1',
    fill = 'olivedrab1',
    size = 0.8
  ) +
  ylab(bquote('Fire size' ~ '(' ~ 10 ^ 5 ~ 'ha)')) +
  xlab('Number of OBE') +
  scale_color_manual(values = c('slateblue3',
                                #'lightgoldenrod3',
                                'lightgoldenrod2',
                                "olivedrab1")) +
  theme_bw() +
  theme(
    text = element_text(size = 7),
    # legend.position=c(0.5,0.8),
    legend.position = 'none',
    aspect.ratio = 1,
    #panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
    legend.box.background = element_rect(color = "black"),
    legend.box.margin = margin(t = 1, l = 1, b = 20)
  )

p2c


model1 = lm(POLY_HA ~ counts, data = BAdf %>% filter(biome == 'Boreal'))
summary(model1)

model2 = lm(POLY_HA ~ counts,
            data = BAdf %>% filter(biome == 'Temperate mountain system'))
summary(model2)

model3 = lm(POLY_HA ~ counts,
            data = BAdf %>% filter(biome == 'Subtropical mountain system'))
summary(model3)

model4 = lm(POLY_HA ~ counts, data = BAdf)
summary(model4)


cor((BAdf %>% filter(biome == 'Boreal'))$counts, (BAdf %>% filter(biome ==
                                                                    'Boreal'))$POLY_HA)
cor((BAdf %>% filter(biome == 'Temperate mountain system'))$counts, (BAdf %>% filter(biome ==
                                                                                       'Temperate mountain system'))$POLY_HA)
cor((BAdf %>% filter(biome == 'Subtropical mountain system'))$counts, (BAdf %>% filter(biome ==
                                                                                         'Subtropical mountain system'))$POLY_HA)
cor(BAdf$counts, BAdf$POLY_HA)

#burned area 95%ci
tgc = Rmisc::summarySE(BA,
                       measurevar = "POLY_HA",
                       groupvars = c("ONB_Event_YoN"))
tgc["ONB_Event_YoN"][tgc["ONB_Event_YoN"] == "OBEs"] <-
  'Overnight\nfires'
tgc["ONB_Event_YoN"][tgc["ONB_Event_YoN"] == "Non events"] <-
  'Other\nfires'

pd <- position_dodge(0.1) # move them .05 to the left and right

p2extra <- ggplot(tgc, aes(x=ONB_Event_YoN, y=POLY_HA/10000, fill=ONB_Event_YoN)) + 
  geom_bar(stat="identity",position="dodge",width = 0.5)+
  geom_errorbar(aes(ymin=POLY_HA/10000-ci/10000, ymax=POLY_HA/10000+ci/10000), width=.2, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd)+
  ylab(bquote('Fire size'~'('~10^4~'ha)'))+
  theme_classic()+
  theme(legend.position = "none",axis.title.x=element_blank())+
  theme(text = element_text(size = 7),
        # axis.text.x = element_text(vjust=0.5, hjust=0.5)
        # axis.text.x = element_text( size=10,face='bold'),
        #panel.grid.minor = element_blank(), panel.grid.major = element_blank()
  )+
  scale_fill_manual(values = c('gray','darkred'))

p2extra

g = plot_grid(p2a,p2b,p2c,p2d, ncol =2, align = "hv",rel_widths = c(1,1))
g



