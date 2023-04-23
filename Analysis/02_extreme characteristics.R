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


ONB_SingleEvent <- FD_NLgte4 %>% filter(ONB_Event_YoN=='OBEs') %>% group_by(year,seq) %>% filter(n()==1)
ONB_multiEvent <- FD_NLgte4 %>% filter(ONB_Event_YoN=='OBEs') %>% group_by(year,seq) %>% filter(n()>1)

obe_mul.stat <- ONB_multiEvent %>%   mutate(biome = case_when(
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
  (biome == 41|42|43) ~ 'Boreal',
  (biome == 50) ~ 'Polar'
)) %>% group_by(biome) %>% summarise(counts=n())

ONB_stat <- FD_NLgte4 %>% filter(ONB_Event_YoN=='OBEs') %>% group_by(year,seq,biome,country,lat,long) %>% mutate(daydiff=max(day)-min(day)) %>% 
  summarise(counts=n(),daydiff=mean(daydiff),daymin=min(day),daymax=max(day)) %>% mutate(daydiff.avg=case_when((counts==1) ~ 0,(counts>=1) ~ ((daydiff+1)/counts)))
ONB_stat1 <- ONB_stat %>% group_by(counts) %>% summarise(ncounts=n(),events=sum(counts),daysum=sum(daydiff)+ncounts) %>% mutate(daydiff.avg=daysum/events)


library(gglorenz)
p6 = ONB_stat %>%
  ggplot(aes(counts)) +
  stat_lorenz(desc = F,color='darkred') +
  coord_fixed() +
  geom_abline(linetype = "dashed") +
  stat_lorenz(data=ONB_stat %>% filter(biome==25),desc = F,color='olivedrab1')+
  stat_lorenz(data=ONB_stat %>% filter(biome==35),desc = F,color='lightgoldenrod2')+
  #stat_lorenz(data=ONB_stat %>% filter(biome==34),desc = F,color='green')+
  stat_lorenz(data=ONB_stat %>% filter(biome == 41||biome == 42||biome == 43),desc = F,color='slateblue3')+
  hrbrthemes::scale_x_percent() +
  hrbrthemes::scale_y_percent() +
  hrbrthemes::theme_ipsum_rc() +
  geom_segment(aes(x = 0, y = 0.498155, xend = 0.869697, yend = 0.498155),linetype='dashed',color='darkred')+
  geom_segment(aes(x = 0.869697, y = 0, xend = 0.869697, yend = 0.498155),linetype='dashed',color='darkred')+
  theme_bw()+
  theme(text = element_text(size = 7),
        strip.text.x = element_text(size = 7, color = "black", face = "bold"),aspect.ratio=1,
        #panel.grid.minor = element_blank(), panel.grid.major = element_blank()
  )+
  labs(x = "Cumulative Percentage of fires with OBEs",
       y = "Cumulative Percentage of OBEs")
p6
# Fire2OBE <- ONB_stat %>% mutate( x_bins = cut( counts, breaks = c(0,1,5,10,20,50) )) %>% 
#   group_by(x_bins) %>% 
#   summarise(Firebincounts=n(),OBEcounts=sum(counts)) %>% 
#   mutate(fp=Firebincounts/330,obep=OBEcounts/1084)
# Fire2OBE$x_bins <- factor(Fire2OBE$x_bins, levels = rev(levels(Fire2OBE$x_bins)))
# 
# ggplot(melt(Fire2OBE %>% dplyr::select(x_bins,fp,obep)), aes(x=variable, y=value, fill=x_bins)) + 
#   geom_bar(stat = 'identity', width=0.5, col=NA)  + 
#   geom_segment(data=Fire2OBE %>% arrange(by=desc(x_bins)) %>% 
#                  mutate(GroupA=cumsum(fp)) %>% 
#                  mutate(GroupB=cumsum(obep)), 
#                aes(x=1.25, xend=1.75, y=GroupA, yend=GroupB),stat = 'identity',linetype = "dashed")+
#   theme_bw()+
#   theme(text = element_text(size = 7),axis.title.x=element_blank(),
#         strip.text.x = element_text(size = 7, color = "black", face = "bold"),aspect.ratio=1,
#         #panel.grid.minor = element_blank(), panel.grid.major = element_blank()
#   )+  scale_fill_manual(values=c('darkred','#5B202A','#655F62','#9B9397','#EEEAED'))+
#   ylab('Percentage')+xlab('')

## OBEs stat by OF,OBEs,density

ONBEvents <- FD_NLgte4 %>% filter(ONB_Event_YoN=='OBEs') %>% 
  group_by(year,seq,season) %>% 
  mutate(counts=n()) %>% 
  group_by(year,seq,season) %>% 
  mutate(biome = case_when(
    (biome == 12) ~ 'Tropical\nmoist forest',
    (biome == 21) ~ 'Subtropical\nhumid forest',
    (biome == 22) ~ 'Subtropical\ndry forest',
    (biome == 23) ~ 'Subtropical\nsteppe',
    (biome == 24) ~ 'Subtropical\ndesert',
    (biome == 25) ~ 'Subtropical\nmountain system',
    (biome == 31) ~ 'Temperate\noceanic forest',
    (biome == 32) ~ 'Temperate\ncontinental forest',
    (biome == 33) ~ 'Temperate\nsteppe',
    (biome == 34) ~ 'Temperate\ndesert',
    (biome == 35) ~ 'Temperate\nmountain system',
    (biome == 41|42|43) ~ 'Boreal',
    (biome == 50) ~ 'Polar')) %>% 
  filter(biome=='Boreal'|biome=='Temperate\nmountain system'|biome=='Subtropical\nmountain system') %>% 
  summarise(year=year[1],
            seq=seq[1],
            counts=counts[1],
            biome=biome[1],
  )  %>% 
  group_by(biome,season) %>% 
  summarise(counts=sum(counts)) %>% 
  mutate(biome1=case_when(
    (biome=='Boreal')~1,
    (biome=='Temperate\nmountain system')~2,
    (biome=='Subtropical\nmountain system')~3
  ))
ONBEvents$season <- factor(ONBEvents$season, levels = c('spring','summer','fall','winter'))
# ONBEvents$biome <- factor(ONBEvents$biome, levels = rev(c('Boreal',
#                                                       "Temperate continental forest","Temperate desert","Temperate mountain system","Temperate steppe",
#                                                       "Subtropical desert","Subtropical dry forest","Subtropical humid forest","Subtropical mountain system","Subtropical steppe",
#                                                       "Tropical moist forest")))

Fire_summary = FD_NLgte4 %>% group_by(year,seq) %>% 
  summarise(year=year[1],
            seq=seq[1],
            biome=biome[1],
            ONB_FIRE_YoN=ONB_FIRE_YoN[1],
            ONBcounts=sum(ONB_Event_YoN=='OBEs')) %>% 
  mutate(firetype=case_when(
    (ONB_FIRE_YoN==0) ~'fire without OBEs',
    (ONB_FIRE_YoN==1&ONBcounts>1) ~'multi-OBE fire',
    (ONB_FIRE_YoN==1&ONBcounts==1) ~'single-OBE fire'
  ))%>% 
  mutate(biome = case_when(
    (biome == 12) ~ 'Tropical\nmoist forest',
    (biome == 21) ~ 'Subtropical\nhumid forest',
    (biome == 22) ~ 'Subtropical\ndry forest',
    (biome == 23) ~ 'Subtropical\nsteppe',
    (biome == 24) ~ 'Subtropical\ndesert',
    (biome == 25) ~ 'Subtropical\nmountain system',
    (biome == 31) ~ 'Temperate\noceanic forest',
    (biome == 32) ~ 'Temperate\ncontinental forest',
    (biome == 33) ~ 'Temperate\nsteppe',
    (biome == 34) ~ 'Temperate\ndesert',
    (biome == 35) ~ 'Temperate\nmountain system',
    (biome == 41|42|43) ~ 'Boreal',
    (biome == 50) ~ 'Polar')) %>% 
  filter(biome=='Boreal'|biome=='Temperate\nmountain system'|biome=='Subtropical\nmountain system') %>% 
  group_by(biome,firetype) %>% 
  summarise(year=year[1], seq=seq[1],
            counts=n())%>% 
  mutate(biome1=case_when(
    (biome=='Boreal')~1,
    (biome=='Temperate\nmountain system')~2,
    (biome=='Subtropical\nmountain system')~3
  ))
Fire_summary$firetype <- factor(Fire_summary$firetype, levels = rev(c('fire without OBEs','single-OBE fire','multi-OBE fire')))

OBEtype <- FD_NLgte4 %>% filter(ONB_Event_YoN=='OBEs') %>% 
  group_by(year,seq) %>% 
  mutate(counts=n()) %>% 
  mutate(biome = case_when(
    (biome == 12) ~ 'Tropical\nmoist forest',
    (biome == 21) ~ 'Subtropical\nhumid forest',
    (biome == 22) ~ 'Subtropical\ndry forest',
    (biome == 23) ~ 'Subtropical\nsteppe',
    (biome == 24) ~ 'Subtropical\ndesert',
    (biome == 25) ~ 'Subtropical\nmountain system',
    (biome == 31) ~ 'Temperate\noceanic forest',
    (biome == 32) ~ 'Temperate\ncontinental forest',
    (biome == 33) ~ 'Temperate\nsteppe',
    (biome == 34) ~ 'Temperate\ndesert',
    (biome == 35) ~ 'Temperate\nmountain system',
    (biome == 41|42|43) ~ 'Boreal',
    (biome == 50) ~ 'Polar')) %>% 
  group_by(biome) %>% 
  mutate(obetype=case_when(
    (counts>1) ~'multi-OBE',
    (counts==1) ~'single-OBE')) %>% 
  group_by(biome,obetype) %>% summarise(counts=n()) %>% 
  filter(biome=='Boreal'|biome=='Temperate\nmountain system'|biome=='Subtropical\nmountain system')%>% 
  mutate(biome1=case_when(
    (biome=='Boreal')~1,
    (biome=='Temperate\nmountain system')~2,
    (biome=='Subtropical\nmountain system')~3
  ))
OBEtype$obetype <- factor(OBEtype$obetype, levels = rev(c('single-OBE','multi-OBE')))


windowsFonts(Times=windowsFont("Times New Roman"))
library(ggnewscale)
p7 <- ggplot()+
  geom_bar(data=Fire_summary,aes(x=biome1-0.3 ,y=counts,fill=firetype),stat = "identity",width = 0.15)+
  scale_fill_manual(values=c('#5B202A','darkred','gray'))+
  new_scale_fill()+
  geom_bar(data=OBEtype,aes(x=biome1,y=counts,fill=obetype),stat = "identity",width = 0.15)+
  scale_fill_manual(values=c('#5B202A','darkred'))+
  new_scale_fill()+
  geom_bar(data=ONBEvents,aes(x=biome1+0.2,y=counts,fill=season),stat = "identity",position='stack',width = 0.15)+
  scale_fill_manual(values=c('yellow','red','orange','blue'))+
  #scale_y_continuous(expand = c(0.001, 0.1)) +
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        text = element_text(size = 7),
        legend.position = 'top', legend.box = "vertical",
        legend.key.size = unit(0.15,'inches'),
        panel.grid.minor.x = element_blank(),aspect.ratio=0.4
        #panel.grid.major.x = element_blank()
  )+ylim(0,500)

p7

XX=unique((FD_NLgte4 %>% filter(ONB_Event_YoN=='OBEs'))[c('year','seq','day')])

x <- FD_NLgte4 %>% filter(biome_4zones=='boreal',
                          ONB_Event_YoN=='OBEs')
x1 <- FD_NLgte4 %>% filter(biome_4zones!='boreal',
                           #biome ==c(35,25),
                           WorEzone=='W',
                           ONB_Event_YoN=='OBEs') 

test <- FD_NLgte4 %>% filter(ONB_Event_YoN=='OBEs')

xx = FD_NLgte4 %>% filter(ONB_Event_YoN=='OBEs') %>% group_by(year,season,biome) %>% 
  summarise(count=n())

summary(x1$NightLength)
sd(x1$NightLength)
ggplot+geom_density(data=as.data.frame(x$NightLength))

daydiff_obe = ONB_multiEvent %>%  group_by(year,seq) %>% mutate(lagday=lag(day),daydiff=day-lagday) %>% 
  group_by(daydiff) %>% summarise(counts=n()) %>% mutate(sumcounts=daydiff*counts)
x1 = data.frame(daydiff_obe$daydiff) %>% drop_na()
summary(x1)
sd(x1$daydiff_obe.daydiff)

p1 = ggplot()+geom_bar(data=daydiff_obe,aes(x=daydiff,y=counts),fill='gray',stat="identity")+
  geom_point(data=daydiff_obe,aes(x=daydiff,y=counts),stat="identity",color='black',size=0.8)+
  # geom_smooth(data=daydiff_obe,aes(x=daydiff,y=counts),color='slateblue3',fill='slateblue3')+
  
  theme_bw()+
  xlab('Days between consecutive OBEs')+ylab('Quantity')+
  theme(text = element_text(size = 7),
        strip.text.x = element_text(size = 7, color = "black", face = "bold"),aspect.ratio=1,
        #panel.grid.minor = element_blank(), panel.grid.major = element_blank()
  )

p1
ONB_stat_sf <- st_as_sf(ONB_stat%>% filter(counts>=15), coords = c("long", "lat"), 
                        crs = 4326 , agr = "constant") 
ggplot()+geom_sf(data=shp.us,aes(color=as.factor(NAME)))+
  geom_sf(data=ONB_stat_sf)

#how many days between startdate and first OBE
x = FD_NLgte4 %>% filter(ONB_Event_YoN=='OBEs') %>% group_by(year,seq,ID,country) %>% 
  arrange(day) %>%  summarise(firstobe=day[1])
x1 = FD_NLgte4 %>% group_by(year,seq,ID) %>% 
  arrange(day) %>% summarise(startday=day[1])
x2 = left_join(x,x1) %>% mutate(daydiff=firstobe-startday) %>% 
  mutate( x_bins = cut( daydiff, breaks = c(-0.0001,0,2,5,10,100) ))

summary(x2$daydiff)
x3 = x2 %>% group_by(daydiff) %>% summarise(counts=n())
p2 = ggplot()+geom_bar(data=x3,aes(x=daydiff,y=counts),stat="identity",fill='gray')+ylab('Quantity')+
  geom_point(data=x3,aes(x=daydiff,y=counts),color='black',size=0.8)+xlab('Days between ignition and first OBE')+
  # geom_smooth(data=x3,aes(x=daydiff,y=counts),color='slateblue3',fill='slateblue3')+
  
  theme_bw()+
  theme(text = element_text(size = 7),
        strip.text.x = element_text(size = 7, color = "black", face = "bold"),aspect.ratio=1,
        #panel.grid.minor = element_blank(), panel.grid.major = element_blank()
  )
p2


#burned area between fire with obes and no obes
x = FD_NLgte4 %>% group_by(year,seq,ID,country,POLY_HA) %>% 
  summarise(POLY_HA=POLY_HA[1],ONB_Event_YoN=ONB_Event_YoN[1])
x1 = x %>% filter(ONB_Event_YoN=='OBEs')
x2 = x %>% filter(ONB_Event_YoN!='OBEs')
summary(x1)
summary(x2)
ggplot(data=x)+geom_boxplot(aes(x=ONB_Event_YoN,y=POLY_HA/10000))+
  ylab(bquote('Burned area'~'('~10^5~'ha)'))

x3 = FD_NLgte4 %>% filter(ONB_Event_YoN=='OBEs') %>% 
  group_by(year,seq,POLY_HA,biome) %>% summarise(counts=n())%>% 
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
    (biome == 41|42|43) ~ 'Boreal',
    (biome == 50) ~ 'Polar'
  ))
#mutate_at(4,~replace(., is.na(.), 'Others'))

x3$biome <- factor(x3$biome, levels = c('Boreal',
                                        "Temperate continental forest","Temperate desert","Temperate mountain system","Temperate steppe",
                                        "Subtropical desert","Subtropical mountain system","Subtropical steppe", 
                                        "Tropical moist forest"))

windowsFonts(Times=windowsFont("Times New Roman"))

options(scipen=1)
p4 <- ggplot(data=x3,aes(x=counts,y=POLY_HA/100000))+
  geom_point(data=x3,aes(x=counts,y=POLY_HA/100000),color='gray',size=0.8)+ 
  geom_point(data=x3%>% filter(biome=='Boreal'|biome=='Temperate mountain system'|biome=='Subtropical mountain system'),#biome=='Temperate desert'|
             aes(x=counts,y=POLY_HA/100000,color=biome),size=0.8)+
  geom_smooth(data=x3,aes(x=counts,y=POLY_HA/100000),method = lm,color='darkred',fill='darkred',size=0.8)+
  geom_smooth(data=x3 %>% filter(biome=='Boreal'),aes(x=counts,y=POLY_HA/100000),method = lm,color='slateblue3',fill='slateblue3',size=0.8)+
  #geom_smooth(data=x3 %>% filter(biome=='Temperate desert'),aes(x=counts,y=POLY_HA/100000),method = lm,color='lightgoldenrod3',fill='lightgoldenrod3')+
  geom_smooth(data=x3 %>% filter(biome=='Temperate mountain system'),aes(x=counts,y=POLY_HA/100000),method = lm,color='lightgoldenrod2',fill='lightgoldenrod2',size=0.8)+
  geom_smooth(data=x3 %>% filter(biome=='Subtropical mountain system'),aes(x=counts,y=POLY_HA/100000),method = lm,color='olivedrab1',fill='olivedrab1',size=0.8)+
  ylab(bquote('Fire size'~'('~10^5~'ha)'))+
  xlab('Number of OBE')+
  scale_color_manual(values=c('slateblue3',
                              #'lightgoldenrod3',
                              'lightgoldenrod2',
                              "olivedrab1"))+
  theme_bw()+
  theme(text = element_text(size = 7),
        # legend.position=c(0.5,0.8),
        legend.position='none',
        aspect.ratio=1,#panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        legend.box.background = element_rect(color = "black"),
        legend.box.margin = margin(t = 1, l = 1,b=20))

p4
# x <- paste0('015.5_Plots\\fig.1.ba.lm','.pdf')
# ggsave(plot=p,x,width = 50,height =50,units = 'mm')
# # theme(legend.position = c(1, 0),
# #       legend.justification = c(1,0),
#       legend.background = element_blank(),
#       legend.box.background = element_rect(colour = "black"),
#       legend.title=element_blank())





model1 = lm(POLY_HA~counts,data=x3 %>% filter(biome=='Boreal'))
summary(model1)

model2 = lm(POLY_HA~counts,data=x3 %>% filter(biome=='Temperate mountain system'))
summary(model2)

model3 = lm(POLY_HA~counts,data=x3 %>% filter(biome=='Subtropical mountain system'))
summary(model3)

model4 = lm(POLY_HA~counts,data=x3 %>% filter(biome=='Temperate desert'))
summary(model4)

model5 = lm(POLY_HA~counts,data=x3)
summary(model5)


cor( (x3 %>% filter(biome=='Boreal')) $counts,(x3 %>% filter(biome=='Boreal'))$POLY_HA)
cor( (x3 %>% filter(biome=='Temperate mountain system')) $counts,(x3 %>% filter(biome=='Temperate mountain system'))$POLY_HA)
cor( (x3 %>% filter(biome=='Subtropical mountain system')) $counts,(x3 %>% filter(biome=='Subtropical mountain system'))$POLY_HA)
cor( (x3 %>% filter(biome=='Temperate desert')) $counts,(x3 %>% filter(biome=='Temperate desert'))$POLY_HA)

#burned area 95%ci
tgc = Rmisc::summarySE(x, measurevar="POLY_HA", groupvars=c("ONB_Event_YoN"))
tgc["ONB_Event_YoN"][tgc["ONB_Event_YoN"] == "OBEs"] <- 'Overnight\nfires'
tgc["ONB_Event_YoN"][tgc["ONB_Event_YoN"] == "Non events"] <- 'Other\nfires'

pd <- position_dodge(0.1) # move them .05 to the left and right

p3 <- ggplot(tgc, aes(x=ONB_Event_YoN, y=POLY_HA/10000, fill=ONB_Event_YoN)) + 
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

p3


p5 = plot_grid(p3,p3,nrow=1,align='hv',axis='1')

p5
g = plot_grid(p6,p1,p4,p2, ncol =2, align = "hv",rel_widths = c(1,1))
g


x <- paste0('015.5_Plots\\fig.2.new.3','.pdf')
ggsave(plot=g,x,width = 12,height =12,units = 'cm')



library(ggridges)
ggplot(x, aes(x = log(POLY_HA), y= ONB_Event_YoN, fill = ONB_Event_YoN)) +
  geom_density_ridges() +
  theme_ridges() + 
  ylab("")+
  xlab('ln(Burned area)')+
  envalysis::theme_publish()  +
  theme(legend.position = "none")

sum((ONB_stat1 %>% filter(counts>5))$ncounts)/330*100
sum((ONB_stat1 %>% filter(counts>5))$events)/1084*100


ggplot(tgc, aes(x = ONB_Event_YoN, y= POLY_HA, fill = ONB_Event_YoN)) + 
  geom_errorbar(aes(ymin=POLY_HA-ci, ymax=POLY_HA+ci, colour=ONB_Event_YoN), width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=2, shape=21, aes(colour=ONB_Event_YoN)) + # 21 is filled circle
  xlab("") +
  ylab("Burned area (ha)") +
  expand_limits(y=0) +                        # Expand y range
  theme_bw() +
  theme(legend.position = "none") 








# x <- paste0('015.5_Plots\\fig.2.days2obes','.pdf')
# ggsave(plot=p1,x,width = 6,height =6,units = 'cm')
# 
# x <- paste0('015.5_Plots\\fig.2.daysig','.pdf')
# ggsave(plot=p2,x,width = 6,height =6,units = 'cm')
# 
# d <- density(x2$daydiff) # returns the density data
# plot(d) # plots the results




x2df <- x2 %>% group_by(x_bins) %>% 
  count() %>% ungroup() %>%   
  mutate(perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

ggplot(x2df, aes(x = "", y = -perc, fill = x_bins)) +
  geom_col(color = "black") +
  # geom_label(aes(label = labels),
  #            position = position_stack(vjust = 0.5),
  #            show.legend = FALSE) +
  guides(fill = guide_legend(title = "Answer")) +
  scale_fill_manual(values=c('darkred','#5B202A','#655F62','#9B9397','#EEEAED')) +
  coord_polar(theta = "y") + 
  theme_void()+theme(text = element_text(family='Times',size = 10))

df2 <- df %>% 
  mutate(csum = rev(cumsum(rev(n))), 
         pos = n/2 + lead(csum, 1),
         pos = if_else(is.na(pos), n/2, pos))

ggplot(df, aes(x = "", y = n, fill = fct_inorder(x_bins))) +
  geom_col(width = 1, color = 1) +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  guides(fill = guide_legend(title = "Group")) +
  scale_y_continuous(breaks = df2$pos, labels = df$x_bins) +
  theme(axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(size = 15), 
        legend.position = "none", # Removes the legend
        panel.background = element_rect(fill = "white"))



##quantities stats
OBE.q = FD_NLgte4 %>% 
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
    (biome == 41|42|43) ~ 'Boreal',
    (biome == 50) ~ 'Polar'
  ))%>% filter(ONB_Event_YoN=='OBEs') %>% 
  group_by(biome) %>% summarise(counts=n())
#mutate_at(4,~replace(., is.na(.), 'Others'))

AF.q1 <- FD_NLgte4 %>% 
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
    (biome == 41|42|43) ~ 'Boreal',
    (biome == 50) ~ 'Polar'
  ))
AF.q = unique(AF.q1[c('ID','biome')]) %>% group_by(biome) %>% summarise(counts=n())


OFq1 <- FD_NLgte4 %>% 
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
    (biome == 41|42|43) ~ 'Boreal',
    (biome == 50) ~ 'Polar'
  )) %>% filter(ONB_FIRE_YoN==1)
OFq = unique(OFq1[c('ID','biome')]) %>% group_by(biome) %>% summarise(counts=n())




