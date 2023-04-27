# Here is an example to download goes-16 fdcf product from aws
getwd()
setwd("D:\\")
library(aws.s3)

test=get_bucket_df(bucket = 'noaa-goes16', prefix="ABI-L2-FDCF/2020",max = Inf)
head(test)
tail(test)

for(i in 1:nrow(test)){
  
save_object(test[i,1],bucket = 'noaa-goes16',file=paste0('goes16-',test[i,1]))
  print(paste0(i,test[i,1]))
}

