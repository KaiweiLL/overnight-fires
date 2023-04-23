# This is an example to process 2020 GOES-16 active fire products, extract information
# of hotspots, and combine and output as csv format.
# The final output would contain all information of detected fire hotspots by both GOES-16 and
# GOES-17 in each year from 2017 to 2020.
rm(list = ls())
setwd("E:/G16 2020/")
library(raster)
e=extent(-5434894.7010,5434895.2182,-5434895.2182,5434894.7010)

files=list.files(path="E:/G16 2020/",pattern="*.nc", recursive=T, full.names = T)
length(files)
head(files)

for(i in 1:length(files)){
  dat01=raster(files[i],varname="Area")
  dat02=raster(files[i],varname="Temp")
  dat03=raster(files[i],varname="Mask")
  dat04=raster(files[i],varname="Power")
  dat05=raster(files[i],varname="DQF")
  dat04[dat04==-9]=NA
  dat04[dat04==-1]=NA
  dat04[dat04==0]=NA
  dat04[dat04==-99]=NA
dat06=stack(dat01,dat02,dat03,dat04,dat05)
extent(dat06)=e
dat07=as.data.frame(dat06,xy=T)
dat08=dat07[complete.cases(dat07[,6]),]
write.csv(dat08,paste0(substr(files[i],39,57),".csv"),row.names=F)
}

setwd("E:/G16 2020/")

files=list.files(pattern="*.csv")
length(files)
dat00=data.frame(matrix(nrow=0,ncol=0))

for(i in 1:length(files)){
  dat01=read.csv(files[i])
  names(dat01)=c("x","y","area","temp","mask","frp","flag")
  if(nrow(dat01)>0){
    dat01$info=files[i]
    dat00=rbind(dat00,dat01)
    rm(dat01)  
  }else{
    dat01=data.frame(cbind(x=NA,y=NA,area=NA,temp=NA,mask=NA,frp=NA,flag=NA))
    dat01$info=files[i]
    dat00=rbind(dat00,dat01)
    rm(dat01)  
  }
}

write.csv(dat00,"G16_2020.csv",row.names=F)






