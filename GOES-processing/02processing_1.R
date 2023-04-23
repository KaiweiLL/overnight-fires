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





rm(list = ls())
setwd("E:/G16 2020/")
library(parallel)
f<-function(x){
  library(raster)
  e=extent(-5434894.7010,5434895.2182,-5434895.2182,5434894.7010)
  dat01=raster(x,varname="Area")
  dat02=raster(x,varname="Temp")
  dat03=raster(x,varname="Mask")
  dat04=raster(x,varname="Power")
  dat05=raster(x,varname="DQF")
  dat04[dat04==-9]=NA
  dat04[dat04==-1]=NA
  dat04[dat04==0]=NA
  dat04[dat04==-99]=NA
  dat06=stack(dat01,dat02,dat03,dat04,dat05)
  rm(dat01,dat02,dat03,dat04,dat05)
  extent(dat06)=e
  dat07=as.data.frame(dat06,xy=T)
  rm(dat06)
  dat08=dat07[complete.cases(dat07[,6]),]
  rm(dat07)
  write.csv(dat08,paste0(substr(x,39,57),".csv"),row.names=F)

}
files=list.files(path="E:/G16 2020/",pattern="*.nc", recursive=T, full.names = T)
length(files)
head(files)
x<-array(files,dim=c(1,length(files)/1)) 
no_cores<-detectCores()-6
cl<-makeCluster(no_cores)
parApply(cl,x,2,f) 
stopCluster(cl)
proc.time() - ptm







