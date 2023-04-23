setwd("E:/G16 2020/")

files=list.files(pattern="*.csv")
length(files)
#i=5
#17500
#2237
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
