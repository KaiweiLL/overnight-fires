# project of GOES active fire detection, codes are an example for GOES-16
# replace -75 to -137 if projecting GOES-17
files=list.files(pattern="*.csv")
library(sf)

## g16 -75 g17 -137
for(i in 6){
dat01a=read.csv(files[i])
print(head(dat01a))
dat01a$y=-1*dat01a$y
dat01=dat01a[complete.cases(dat01a$x),]
dat02 <- st_as_sf(x = dat01, 
                  coords = c("x", "y"),
                  crs = "+proj=geos +sweep=x +lon_0=-75 +h=35786023 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs")
dat03=st_transform(dat02,crs=4326)
dat04=cbind(st_coordinates(dat03),dat01)
names(dat04)=c("long","lat","x","y","area","temp","mask","frp","dqf","info")
write.csv(dat04, paste0(files[i],"_projected.csv" ) ,row.names=F)
rm(dat01,dat02,dat03,dat04)
}

