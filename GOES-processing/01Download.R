
getwd()
setwd("D:\\000_collections\\GOES1617_2122years")
library(aws.s3)

test=get_bucket_df(bucket = 'noaa-goes16', prefix="ABI-L2-FDCF/2021",max = Inf)
head(test)
tail(test)

for(i in 1:nrow(test)){
  
save_object(test[i,1],bucket = 'noaa-goes16',file=paste0('goes16-',test[i,1]))
  print(paste0(i,test[i,1]))
}


test=get_bucket_df(bucket = 'noaa-goes16', prefix="ABI-L2-FDCF/2022",max = Inf)
head(test)
tail(test)

for(i in 1:nrow(test)){
  
  save_object(test[i,1],bucket = 'noaa-goes16',file=paste0('goes16-',test[i,1]))
  print(paste0(i,test[i,1]))
}



# goes17 ------------------------------------------------------------------

test=get_bucket_df(bucket = 'noaa-goes17', prefix="ABI-L2-FDCF/2021",max = Inf)
head(test)
tail(test)

for(i in 1:nrow(test)){
  
  save_object(test[i,1],bucket = 'noaa-goes17',file=paste0('goes17-',test[i,1]))
  print(paste0(i,test[i,1]))
  
}


test=get_bucket_df(bucket = 'noaa-goes17', prefix="ABI-L2-FDCF/2022",max = Inf)
head(test)
tail(test)

for(i in 1:nrow(test)){
  
  save_object(test[i,1],bucket = 'noaa-goes17',file=paste0('goes17-',test[i,1]))
  print(paste0(i,test[i,1]))
}



# goes16_2017=get_bucket_df(bucket = 'noaa-goes16', prefix="ABI-L2-FDCF/2017",max = Inf)
# goes16_2018=get_bucket_df(bucket = 'noaa-goes16', prefix="ABI-L2-FDCF/2018",max = Inf)
# goes16_2019=get_bucket_df(bucket = 'noaa-goes16', prefix="ABI-L2-FDCF/2019",max = Inf)
# goes16_2020=get_bucket_df(bucket = 'noaa-goes16', prefix="ABI-L2-FDCF/2020",max = Inf)
goes16 = rbind(goes16_2017,goes16_2018,goes16_2019,goes16_2020)

# goes17_2018=get_bucket_df(bucket = 'noaa-goes17', prefix="ABI-L2-FDCF/2018",max = Inf)
# goes17_2019=get_bucket_df(bucket = 'noaa-goes17', prefix="ABI-L2-FDCF/2019",max = Inf)
# goes17_2020=get_bucket_df(bucket = 'noaa-goes17', prefix="ABI-L2-FDCF/2020",max = Inf)
goes17 = rbind(goes17_2018,goes17_2019,goes17_2020)


library(dplyr)
library(ggplot2)
goes16_info = goes16 %>% mutate(year=substr(Key,13,16),
                                day=substr(Key,18,20),
                                hr = substr(Key,22,23),
                                mode = substr(Key,40,41))

goes17_info = goes17 %>% mutate(year=substr(Key,13,16),
                                day=substr(Key,18,20),
                                hr = substr(Key,22,23),
                                mode = substr(Key,40,41))

goes16.p = goes16_info %>% group_by(year,mode) %>% summarise(counts=n()) %>% mutate(goes='goes16')
goes17.p = goes17_info %>% group_by(year,mode) %>% summarise(counts=n()) %>% mutate(goes='goes16')
p=ggplot()+
  geom_bar(data=goes16.p,aes(x=year,y=counts,fill=mode),position="stack", stat="identity",width = .7)+
  theme(text = element_text(size = 7),
        strip.text.x = element_text(size = 7, color = "black", face = "plain"),
        plot.title = element_text(color="black", size=7, face="bold",vjust = - 1))
p
# x <- paste0('goes16_counts','.pdf')
# ggsave(plot=p,x,width = 8,height =4,units = 'cm')
p=ggplot()+
  geom_bar(data=goes17.p,aes(x=year,y=counts,fill=mode),position="stack", stat="identity",width = .7)+
  theme(text = element_text(size = 7),
        strip.text.x = element_text(size = 7, color = "black", face = "plain"),
        plot.title = element_text(color="black", size=7, face="bold",vjust = - 1))
p
# x <- paste0('goes17_counts','.pdf')
# ggsave(plot=p,x,width = 8,height =4,units = 'cm')
