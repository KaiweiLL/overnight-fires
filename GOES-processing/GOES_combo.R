rm(list = ls())
getwd()
setwd("D:\\000_collections\\010_nightfire\\011_Data_dante")
library(dplyr)
################################################################################2017
g2017_16_part1 <- read.csv('GOES/2017/G16_2017_part1_projected.csv')
g2017_16_part2a <- read.csv('GOES/2017/G16_2017_part2a_projected.csv')
g2017_16_part2b <- read.csv('GOES/2017/G16_2017_part2b_projected.csv')

GOES2017 <- rbind(g2017_16_part1,g2017_16_part2a,g2017_16_part2b)
GOES2017_16_alldata <- GOES2017 %>% mutate(year=as.integer(substr(info,6,9)),
                                           day=as.integer(substr(info,10,12)),
                                           hr=as.integer(substr(info,13,14)),
                                           min=as.integer(substr(info,15,16))) %>% select(-info)
unique(GOES2017_16_alldata$year)
write.table(GOES2017_16_alldata,file='GOES/2017/GOES2017_16_alldata.csv',sep = ',', row.names=FALSE)

################################################################################2018
g2018_16_1to99 <- read.csv('GOES/2018/G16_2018_days001_projected.csv')
g2018_16_100to199 <- read.csv('GOES/2018/G16_2018_days100_projected.csv')
g2018_16_200to299 <- read.csv('GOES/2018/G16_2018_days200_projected.csv')
g2018_16_300toend <- read.csv('GOES/2018/G16_2018_days300_projected.csv')

g2018_17_before <- read.csv('GOES/2018/G17_2018_before_switch_proj.csv')
g2018_17_after <- read.csv('GOES/2018/G17_2018_after_switch_proj.csv')

g2018_16_all <- rbind(g2018_16_1to99,g2018_16_100to199,g2018_16_200to299,g2018_16_300toend)
g2018_16_all_ydm <- g2018_16_all %>% mutate(year=as.integer(substr(info,6,9)),
                                            day=as.integer(substr(info,10,12)),
                                            hr=as.integer(substr(info,13,14)),
                                            min=as.integer(substr(info,15,16))) %>% select(-info)
unique(g2018_16_all_ydm$year)

g2018_17_all <- rbind(g2018_17_before,g2018_17_after)
g2018_17_all_ydm <- g2018_17_all %>% mutate(year=as.integer(substr(info,8,11)),
                                            day=as.integer(substr(info,12,14)),
                                            hr=as.integer(substr(info,15,16)),
                                            min=as.integer(substr(info,17,18))) %>% select(-info)
unique(g2018_17_all_ydm$year)

GOES2018_1617_alldata <- rbind(g2018_16_all_ydm,g2018_17_all_ydm)
write.table(GOES2018_1617_alldata,file='GOES/2018/GOES2018_1617_alldata.csv',sep = ',', row.names=FALSE)

################################################################################2019
g2019_16_1to99 <- read.csv('GOES/2019/G16_2019_days001_projected.csv')
g2019_16_100to199 <- read.csv('GOES/2019/G16_2019_days100_projected.csv')
g2019_16_200to299 <- read.csv('GOES/2019/G16_2019_days200_projected.csv')
g2019_16_300toend <- read.csv('GOES/2019/G16_2019_days300_projected.csv')

g2019_17_1to99 <- read.csv('GOES/2019/G17_2019_days001_projected.csv')
g2019_17_100to300 <- read.csv('GOES/2019/G17_2019_proj.csv')
g2019_17_301toend <- read.csv('GOES/2019/G17_2019_days300_projected.csv')

##GOES 16
g2019_16_all <- rbind(g2019_16_1to99,g2019_16_100to199,g2019_16_200to299,g2019_16_300toend)
g2019_16_all_ydm <- g2019_16_all %>% mutate(year=as.integer(substr(info,6,9)),
                                            day=as.integer(substr(info,10,12)),
                                            hr=as.integer(substr(info,13,14)),
                                            min=as.integer(substr(info,15,16))) %>% select(-info)
unique(g2019_16_all_ydm$year)

##GOES 17-1
g2019_17_1to99_ydm <- g2019_17_1to99 %>% mutate(year=as.integer(substr(info,6,9)),
                                                day=as.integer(substr(info,10,12)),
                                                hr=as.integer(substr(info,13,14)),
                                                min=as.integer(substr(info,15,16))) %>% select(-info)
unique(g2019_17_1to99_ydm$year)
##GOES 17-2
g2019_17_100to300_ydm <- g2019_17_100to300 %>% mutate(year=as.integer(substr(info,11,14)),
                                                      day=as.integer(substr(info,15,17)),
                                                      hr=as.integer(substr(info,18,19)),
                                                      min=as.integer(substr(info,20,21))) %>% select(-info)
unique(g2019_17_100to300_ydm$year)
##GOES 17-3
g2019_17_301toend_ydm <- g2019_17_301toend %>% mutate(year=as.integer(substr(info,6,9)),
                                                      day=as.integer(substr(info,10,12)),
                                                      hr=as.integer(substr(info,13,14)),
                                                      min=as.integer(substr(info,15,16))) %>% select(-info)
unique(g2019_17_301toend_ydm$year)


GOES2019_1617_alldata <- rbind(g2019_16_all_ydm,g2019_17_1to99_ydm,g2019_17_100to300_ydm,g2019_17_301toend_ydm)
unique(GOES2019_1617_alldata$year)

write.table(GOES2019_1617_alldata,file='GOES/2019/GOES2019_1617_alldata.csv',sep = ',', row.names=FALSE)

