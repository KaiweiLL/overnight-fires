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
    ))) %>%
  gather(isi_diff_Dmax_Nmin:rh_diff_Dmin_Nmax,key = variable, value = value) %>% drop_na(biome_4areas)

FD_df_long[FD_df_long == "isi_diff_Dmax_Nmin"] <- 'ISIDmax-'
FD_df_long[FD_df_long == "ffmc_diff_Dmax_Nmin"] <- "FFMCDmax-"
FD_df_long["variable"][FD_df_long["variable"] == "vpd_diff_Dmax_Nmin"] <- 'VPDDmax-'
FD_df_long["variable"][FD_df_long["variable"] == "temp_diff_Dmax_Nmin"] <- "TDmax-"
FD_df_long["variable"][FD_df_long["variable"] == "rh_diff_Dmin_Nmax"] <- "RHDmin-"

FD_df_long$Ncondition <- factor(FD_df_long$Ncondition,levels = c('OBE','NEE','Non-NBE'))
FD_df_long$season <- factor(FD_df_long$season,levels = c('spring','summer','fall','winter'))

FD_df_long$ONB_Event_YoN <- factor(FD_df_long$ONB_Event_YoN,levels = c( 'OBEs', 
                                                                        'Non events'))




#############Boreal
Psummary <- Rmisc::summarySE(FD_df_long %>% filter(biome_4areas=='Boreal',season%in%c('summer')), measurevar="value", 
                             groupvars=c("ONB_Event_YoN", "season", "variable",'biome_4areas'), na.rm = TRUE) %>% mutate(PlotONBseason=case_when(
                               (season=='spring' & ONB_Event_YoN =='OBEs') ~ 'OBEs in spring',
                               (season=='summer' & ONB_Event_YoN =='OBEs') ~ 'OBEs in summer',
                               (ONB_Event_YoN =='Non events') ~ 'Non Events'
                             ))
x = FD_df_long %>% filter(biome_4areas=='Boreal',
                          season%in%c('spring'),
                          ONB_Event_YoN=='OBEs',variable=='FWI')
mean(x$value)
options(scipen = 999)
df = FD_df_long %>% 
  group_by(biome_4areas,season,variable,ONB_Event_YoN) %>% 
  summarize(mean = round(mean(value),1),
            sd = round(sd(value),1),counts=n())
#write.csv(df,'df.csv')
dff = df %>% group_by(biome_4areas,season,variable) %>% 
  summarise(v1=mean[1],v2=mean[2],times=(mean[1]-mean[2])/mean[2]*100) %>% 
  filter(!((biome_4areas == 'Boreal' & season == 'fall') |
             (biome_4areas == 'Subtropical mountain system' & season == 'spring') |
             (biome_4areas == 'Subtropical mountain system' & season == 'winter') |
             (biome_4areas == 'Temperate desert' & season == 'spring') |
             (biome_4areas == 'Temperate desert' & season == 'fall') |
             (biome_4areas == 'Temperate mountain system' & season == 'spring') |
             (biome_4areas == 'Temperate mountain system' & season == 'winter') |
             (variable=='prec_nightsum')|
             (variable=='prec_daysum')|
             (variable=='winds_daymean')|
             (variable=='winds_nightmean')|
             (variable=='emc_daymin')|
             (variable=='emc_nightmax')))

Psummary$PlotONBseason <- factor(Psummary$PlotONBseason, levels = c('OBEs in spring','OBEs in summer','Non Events'))

p1 <- ggplot(Psummary, aes(x=season, y=value,group=PlotONBseason,fill=ONB_Event_YoN)) +#, fill=PlotONBseason
  geom_bar(stat="identity", position="dodge",width = 0.5)+ 
  facet_wrap(~variable,scales = 'free_y',nrow = 1) + 
  geom_errorbar(aes(ymin=value-ci, ymax=value+ci), size=0.25, color='black',
                width=.125,position=position_dodge(.5)) +
  scale_fill_manual(values=c('slateblue3','gray'))+
  envalysis::theme_publish(base_size = 7, base_family = "", line_size = 0.25,base_rect_size =0.25)+
  labs(title='Boreal')+
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
        text = element_text(size = 7),
        #axis.text.x=element_blank(),
        strip.text.x = element_text(size = 7, color = "black", face = "plain"),
        plot.title = element_text(color="black", size=7, face="bold",vjust = - 1))+
  labs(fill='')

p1





#############Temperate mountain system
Psummary <- Rmisc::summarySE(data=FD_df_long %>% filter(biome_4areas=='Temperate mountain system',season%in%c('summer','fall')), measurevar="value", 
                             groupvars=c("ONB_Event_YoN", "season", "variable"), na.rm = TRUE) %>% mutate(PlotONBseason=case_when(
                               (season=='spring' & ONB_Event_YoN =='OBEs') ~ 'OBEs in spring',
                               (season=='summer' & ONB_Event_YoN =='OBEs') ~ 'OBEs in summer',
                               (ONB_Event_YoN =='Non events') ~ 'Non Events'
                             ))
Psummary$PlotONBseason <- factor(Psummary$PlotONBseason, levels = c('OBEs in spring','OBEs in summer','Non Events'))

p2 <- ggplot(Psummary, aes(x=season, y=value,fill=ONB_Event_YoN)) +#, fill=PlotONBseason
  geom_bar(stat="identity", position="dodge")+ 
  facet_wrap(~variable,scales = 'free_y',nrow = 1) + 
  geom_errorbar(aes(ymin=value-ci, ymax=value+ci), size=0.25, color='black',
                width=.25,position=position_dodge(.9)) +
  scale_fill_manual(values=c('lightgoldenrod2','gray'))+
  envalysis::theme_publish(base_size = 7, base_family = "", line_size = 0.25,base_rect_size =0.25)+
  labs(title='Temperate mountain system')+
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
        text = element_text(size = 7),
        #axis.text.x=element_blank(),
        strip.text.x = element_text(size = 7, color = "black", face = "plain"),
        plot.title = element_text(color="black", size=7, face="bold",vjust = - 1))+
  labs(fill='')

p2
#############Subtropical mountain system
Psummary <- Rmisc::summarySE(data=FD_df_long %>% filter(biome_4areas=='Subtropical mountain system',season%in%c('summer','fall')), measurevar="value", 
                             groupvars=c("ONB_Event_YoN", "season", "variable"), na.rm = TRUE) %>% mutate(PlotONBseason=case_when(
                               (season=='spring' & ONB_Event_YoN =='OBEs') ~ 'OBEs in spring',
                               (season=='summer' & ONB_Event_YoN =='OBEs') ~ 'OBEs in summer',
                               (ONB_Event_YoN =='Non events') ~ 'Non Events'
                             ))
Psummary$PlotONBseason <- factor(Psummary$PlotONBseason, levels = c('OBEs in spring','OBEs in summer','Non Events'))


p3 <- ggplot(Psummary, aes(x=season, y=value,fill=ONB_Event_YoN)) +#, fill=PlotONBseason
  geom_bar(stat="identity", position="dodge")+ 
  facet_wrap(~variable,scales = 'free_y',nrow = 1) + 
  geom_errorbar(aes(ymin=value-ci, ymax=value+ci), size=0.25, color='black',
                width=.25,position=position_dodge(.9)) +
  scale_fill_manual(values=c('olivedrab1','gray'))+
  envalysis::theme_publish(base_size = 7, base_family = "", line_size = 0.25,base_rect_size =0.25)+
  labs(title='Subtropical mountain system')+
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
        #axis.text.x=element_blank(),
        text = element_text(size = 7),
        
        strip.text.x = element_text(size = 7, color = "black", face = "plain"),
        plot.title = element_text(color="black", size=7, face="bold",vjust = - 1))+
  labs(fill='')
p3
#############Subtropical mountain system

Psummary <- Rmisc::summarySE(data=FD_df_long %>% filter(biome_4areas=='Temperate desert',season%in%c('summer')), measurevar="value", 
                             groupvars=c("ONB_Event_YoN", "season", "variable"), na.rm = TRUE) %>% mutate(PlotONBseason=case_when(
                               (season=='spring' & ONB_Event_YoN =='OBEs') ~ 'OBEs in spring',
                               (season=='summer' & ONB_Event_YoN =='OBEs') ~ 'OBEs in summer',
                               (ONB_Event_YoN =='Non events') ~ 'Non Events'
                             ))
Psummary$PlotONBseason <- factor(Psummary$PlotONBseason, levels = c('OBEs in spring','OBEs in summer','Non Events'))


p4 <- ggplot(Psummary, aes(x=season, y=value,fill=ONB_Event_YoN)) +#, fill=PlotONBseason
  geom_bar(stat="identity", position="dodge")+ 
  facet_wrap(~variable,scales = 'free_y',nrow = 1) + 
  geom_errorbar(aes(ymin=value-ci, ymax=value+ci), size=0.25, color='black',
                width=.25,position=position_dodge(.9)) +
  scale_fill_manual(values=c('darkred','gray'))+
  envalysis::theme_publish(base_size = 7, base_family = "", line_size = 0.25,base_rect_size =0.25)+
  labs(title='Temperate desert')+
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
        #axis.text.x=element_blank(),
        text = element_text(size = 7),
        
        strip.text.x = element_text(size = 7, color = "black", face = "plain"),
        plot.title = element_text(color="black", size=7, face="bold",vjust = - 1))+
  labs(fill='')
p4

p <- plot_grid(p1, p2,p3,  ncol = 1, align = "v")
p
# x <- paste0('015.5_Plots\\fig.6.111','.pdf')
# ggsave(plot=p,x,width = 5,height =5,units = 'in')


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
    )))

regionff=unique((FD_df%>% drop_na())$biome_4areas )
seasonff=unique((FD_df%>% drop_na())$season )
variableff=c(      'isi_diff_Dmax_Nmin',
                   'ffmc_diff_Dmax_Nmin',
                   'vpd_diff_Dmax_Nmin',
                   'temp_diff_Dmax_Nmin',
                   'rh_diff_Dmin_Nmax')
options(scipen=10)

df_total = data.frame()
for (regionf in regionff) {
  for (seasonf in seasonff) {
    for (variablef in variableff) {
      xx = FD_df  %>% filter(biome_4areas==regionf,
                             season==seasonf,
                             ONB_Event_YoN == 'OBEs')
      if (nrow(xx)>100) {
        # x = boot.t.test(FD_df  %>% filter(biome_4areas==regionf,
        #                                   season==seasonf,
        #                                   ONB_Event_YoN == 'OBEs')%>% 
        #                   dplyr::select(one_of(variablef)),
        #                 FD_df %>% filter(biome_4areas==regionf,
        #                                  season==seasonf,
        #                                  ONB_Event_YoN == 'Non events') %>% 
        #                   dplyr::select(one_of(variablef)) )
        
        
        dobe = as.matrix(FD_df  %>% filter(biome_4areas==regionf,
                                           season==seasonf,
                                           ONB_Event_YoN == 'OBEs')%>% 
                           dplyr::select(one_of(variablef))) 
        dnonobe = as.matrix(FD_df %>% filter(biome_4areas==regionf,
                                             season==seasonf,
                                             ONB_Event_YoN == 'Non events') %>% 
                              dplyr::select(one_of(variablef)))
        Norm_obe = (shapiro.test(dobe))$p.value
        Norm_non = (shapiro.test(dnonobe[sample(nrow(dnonobe), 1000), ]))$p.value
        
        x = wilcox.test(dobe,
                        dnonobe,alternative = 'less')#wilcox.test
        
        
        
        pvalue = round(x[['p.value']],10)
        
        df <- data.frame(regionf,seasonf,variablef,pvalue,Norm_obe,Norm_non)
        df_total <- rbind(df_total,df)
      }
    }
  }
}
df_sig <- df_total %>% filter(pvalue<=0.001)
df_sig <- df_total %>% filter(pvalue<=0.05&pvalue>=0.001)
df_sig1 <- df_total %>% filter(pvalue>0.05)



p1 <- ggplot(data=FD_df_long %>% 
               filter(biome_4areas=='Boreal',season%in%c('summer')),
             aes(x=season,y=value,fill=ONB_Event_YoN))+
  geom_boxplot(notch = T,outlier.size = 0.2,size=0.4,notchwidth = 0.5)+
  facet_wrap(~variable, scale="free",nrow=1)+
  stat_summary(fun.y=mean, geom="point", shape=2, size=1.2, color="yellow", 
               position = position_dodge(0.75),show.legend = FALSE) +
  #theme(legend.position = c(0.92,0.1))+
  guides(fill=guide_legend(title=""))+
  scale_fill_manual(values=c('darkred','gray'))+
  scale_color_manual(values=c('darkred','gray'))+
  envalysis::theme_publish(base_size = 7, base_family = "", line_size = 0.25,base_rect_size =0.25)+
  labs(title='Boreal')+
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
        text = element_text(size = 7),
        #axis.text.x=element_blank(),
        strip.text.x = element_text(size = 7, color = "black", face = "plain"),
        plot.title = element_text(color="black", size=7, face="bold",vjust = - 1))+
  labs(fill='')

#ggsave(plot=p,paste('Boxplot_boreal','.bmp'),width = 30,height =15,units = 'cm')
p1


p2 <- ggplot(data=FD_df_long %>% 
               filter(biome_4areas=='Temperate mountain system',season%in%c('summer','fall')),
             aes(x=season,y=value,fill=ONB_Event_YoN))+
  geom_boxplot(notch = T,outlier.size = 0.2,size=0.4,notchwidth = 0.5)+
  facet_wrap(~variable, scale="free",nrow=1)+
  stat_summary(fun.y=mean, geom="point", shape=2, size=1.2, color="yellow", 
               position = position_dodge(0.75),show.legend = FALSE) +
  #theme(legend.position = c(0.92,0.1))+
  guides(fill=guide_legend(title=""))+
  scale_fill_manual(values=c('darkred','gray'))+
  scale_color_manual(values=c('darkred','gray'))+
  envalysis::theme_publish(base_size = 7, base_family = "", line_size = 0.25,base_rect_size =0.25)+
  labs(title='Temperate mountain system')+
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
        text = element_text(size = 7),
        #axis.text.x=element_blank(),
        strip.text.x = element_text(size = 7, color = "black", face = "plain"),
        plot.title = element_text(color="black", size=7, face="bold",vjust = - 1))+
  labs(fill='')

#ggsave(plot=p,paste('Boxplot_boreal','.bmp'),width = 30,height =15,units = 'cm')
p2


p3 <- ggplot(data=FD_df_long %>% 
               filter(biome_4areas=='Subtropical mountain system',season%in%c('summer','fall')),
             aes(x=season,y=value,fill=ONB_Event_YoN))+
  geom_boxplot(notch = T,outlier.size = 0.2,size=0.4,notchwidth = 0.5)+
  facet_wrap(~variable, scale="free",nrow=1)+
  stat_summary(fun.y=mean, geom="point", shape=2, size=1.2, color="yellow", 
               position = position_dodge(0.75),show.legend = FALSE) +
  #theme(legend.position = c(0.92,0.1))+
  guides(fill=guide_legend(title=""))+
  scale_fill_manual(values=c('darkred','gray'))+
  scale_color_manual(values=c('darkred','gray'))+
  envalysis::theme_publish(base_size = 7, base_family = "", line_size = 0.25,base_rect_size =0.25)+
  labs(title='Subtropical mountain system')+
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
        text = element_text(size = 7),
        #axis.text.x=element_blank(),
        strip.text.x = element_text(size = 7, color = "black", face = "plain"),
        plot.title = element_text(color="black", size=7, face="bold",vjust = - 1))+
  labs(fill='')

#ggsave(plot=p,paste('Boxplot_boreal','.bmp'),width = 30,height =15,units = 'cm')
p3

p <- plot_grid(p1, p2,p3,  ncol = 1, align = "v")
p

# x <- paste0('015.5_Plots\\fig.range1','.pdf')
# ggsave(plot=p,x,width = 15,height =18,units = 'cm')


library(ggnewscale)
g1 = ggplot()+
  # geom_boxplot(notch = T,outlier.size = 0.2,size=0.4,notchwidth = 0.5)+
  geom_density(data=FD_df_long %>% 
                 filter(biome_4areas=='Boreal',season%in%c('summer')),
               aes(x=value,color=ONB_Event_YoN,y=..density..))+
  scale_color_manual(values=c('red','gray'))+
  # new_scale_color() +
  # geom_density(data=FD_df_long %>% 
  #                filter(biome_4areas=='Boreal',season%in%c('fall')),
  #              aes(x=value,color=ONB_Event_YoN,y = - ..density..))+
  # scale_color_manual(values=c('orange','black'))+
  facet_wrap(~variable, scale="free",nrow=1)+
  # stat_summary(fun.y=mean, geom="point", shape=2, size=0.6, color="yellow",
  #              position = position_dodge(0.75),show.legend = FALSE) +
  #theme(legend.position = c(0.92,0.1))+
  guides(fill=guide_legend(title=""))+
  #scale_color_manual(values=c('darkred','gray'))+
  envalysis::theme_publish(base_size = 7, base_family = "", line_size = 0.25,base_rect_size =0.25)+
  labs(title='Boreal')+
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
        text = element_text(size = 7),
        #axis.text.x=element_blank(),
        strip.text.x = element_text(size = 7, color = "black", face = "plain"),
        plot.title = element_text(color="black", size=7, face="bold",vjust = - 1))+
  labs(fill='')
g1

g2 = ggplot()+
  # geom_boxplot(notch = T,outlier.size = 0.2,size=0.4,notchwidth = 0.5)+
  geom_density(data=FD_df_long %>% 
                 filter(biome_4areas=='Temperate mountain system',season%in%c('summer')),
               aes(x=value,color=ONB_Event_YoN,y=..density..))+
  scale_color_manual(values=c('red','gray'))+
  new_scale_color() +
  geom_density(data=FD_df_long %>%
                 filter(biome_4areas=='Temperate mountain system',season%in%c('fall')),
               aes(x=value,color=ONB_Event_YoN,y = - ..density..))+
  scale_color_manual(values=c('orange','black'))+
  facet_wrap(~variable, scale="free",nrow=1)+
  # stat_summary(fun.y=mean, geom="point", shape=2, size=0.6, color="yellow",
  #              position = position_dodge(0.75),show.legend = FALSE) +
  #theme(legend.position = c(0.92,0.1))+
  guides(fill=guide_legend(title=""))+
  #scale_color_manual(values=c('darkred','gray'))+
  envalysis::theme_publish(base_size = 7, base_family = "", line_size = 0.25,base_rect_size =0.25)+
  labs(title='Temperate mountain system')+
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
        text = element_text(size = 7),
        #axis.text.x=element_blank(),
        strip.text.x = element_text(size = 7, color = "black", face = "plain"),
        plot.title = element_text(color="black", size=7, face="bold",vjust = - 1))+
  labs(fill='')
g2

g3 = ggplot()+
  # geom_boxplot(notch = T,outlier.size = 0.2,size=0.4,notchwidth = 0.5)+
  geom_density(data=FD_df_long %>% 
                 filter(biome_4areas=='Subtropical mountain system',season%in%c('summer')),
               aes(x=value,color=ONB_Event_YoN,y=..density..))+
  scale_color_manual(values=c('red','gray'))+
  new_scale_color() +
  geom_density(data=FD_df_long %>%
                 filter(biome_4areas=='Temperate mountain system',season%in%c('fall')),
               aes(x=value,color=ONB_Event_YoN,y = - ..density..))+
  scale_color_manual(values=c('orange','black'))+
  facet_wrap(~variable, scale="free",nrow=1)+
  # stat_summary(fun.y=mean, geom="point", shape=2, size=0.6, color="yellow",
  #              position = position_dodge(0.75),show.legend = FALSE) +
  #theme(legend.position = c(0.92,0.1))+
  guides(fill=guide_legend(title=""))+
  #scale_color_manual(values=c('darkred','gray'))+
  envalysis::theme_publish(base_size = 7, base_family = "", line_size = 0.25,base_rect_size =0.25)+
  labs(title='Subtropical mountain system')+
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
        text = element_text(size = 7),
        #axis.text.x=element_blank(),
        strip.text.x = element_text(size = 7, color = "black", face = "plain"),
        plot.title = element_text(color="black", size=7, face="bold",vjust = - 1))+
  labs(fill='')
g3

g <- plot_grid(g1, g2,g3,  ncol = 1, align = "v",rel_heights = c(0.8, 1,1))
g

x <- paste0('015.5_Plots\\fig.range111','.pdf')
ggsave(plot=g,x,width = 18,height =14,units = 'cm')