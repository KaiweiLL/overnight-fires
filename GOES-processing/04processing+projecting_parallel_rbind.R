################################### g16 -75 g17 -137
rm(list = ls())
setwd("E:/G16 2020/")
library(foreach)
library(doParallel)
library(parallel)

f<-function(x){
  library(raster)
  library(sp)
  library(sf)
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
  names(dat08)=c("x","y","area","temp","mask","frp","flag")
  if(nrow(dat08)>0){
    dat08$info=substr(x,28,46)
  }else{ 
    dat08=data.frame(cbind(x=NA,y=NA,area=NA,temp=NA,mask=NA,frp=NA,flag=NA))
    dat08$info=NA
  }
  dat08
  dat08$y=-1*dat08$y
  dat09=dat08[complete.cases(dat08$x),]
  dat10 <- st_as_sf(x = dat09, 
                    coords = c("x", "y"),
                    crs = "+proj=geos +sweep=x +lon_0=-75 +h=35786023 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs")
  dat11=st_transform(dat10,crs=4326)
  dat12=cbind(st_coordinates(dat11),dat09)
  names(dat12)=c("long","lat","x","y","area","temp","mask","frp","dqf","info")
  rm(dat08,dat09,dat10,dat11)
  dat12
  write.csv(dat12,paste0(substr(x,28,46),".csv"),row.names=F)
  
}
files=list.files(pattern="*.nc", recursive=T, full.names = T)
length(files)
head(files)

no_cores<-detectCores()-8
cl<-makeCluster(no_cores)
registerDoParallel(cl)

start_time <- Sys.time()
bb <- foreach(x=files_4) %dopar%  f(x)#, .combine = 'rbind'
stopCluster(cl)
end_time <- Sys.time()
end_time - start_time

#write.csv(bb,paste0('g2020_16','_10001to20000',".csv"),row.names=F)

##############################################################################double check and rerun
rm(list = ls())
setwd("E:\\G16 2020 csv/")
processed <- list.files(pattern="*.csv")
read <- paste0(substr(files,28,46),".csv")
aa <- setdiff(read,processed)
bb <- intersect(read,processed)
length(unique(read))

cc <- NULL

for (i in 1:length(aa)) {
  cc[i] <- grep(substr(aa[i],1,19), files, value = T)
}
setwd("E:/G16 2020/")
no_cores<-detectCores()-10
cl<-makeCluster(no_cores)
registerDoParallel(cl)

start_time <- Sys.time()
bb <- foreach(x=cc) %dopar%  f(x)#, .combine = 'rbind'
stopCluster(cl)
end_time <- Sys.time()
end_time - start_time

###############################################################################combine
rm(list = ls())
setwd("E:\\G16 2020 csv/")
files <- list.files(pattern="*.csv")

dat00=data.frame(matrix(nrow=0,ncol=0))
for(i in 1:10){
  dat01=read.csv(files[i])
  if(nrow(dat01)>0){
    dat00=rbind(dat00,dat01)
    rm(dat01)  
  }else{
    dat01=data.frame(cbind(long=NA,lat=NA,x=NA,y=NA,area=NA,temp=NA,mask=NA,frp=NA,dqf=NA,info=NA))
    dat00=rbind(dat00,dat01)
    rm(dat01)  
  }
}

write.csv(dat00,"G16_2020.csv",row.names=F)

