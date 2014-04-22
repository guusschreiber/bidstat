# This function computes the mean of n partitions of the data
# arguments: 
#   data: a data frame with a column "Year" and a column "V1"
#   psize: size of the partition
# result: 
#   2-column matrix with means of Year/V1 per partition 

partition<-function(data,psize){
  data<-data[!is.na(data$V1),]
  result<-matrix(nrow=0,ncol=2,byrow=TRUE)
  for(i in 0:(nrow(data)/psize)){
    indexes<-seq(psize)+i*psize
    tmp<-as.matrix(c(mean(data[indexes,]$Year,na.rm=TRUE),
        mean(data[indexes,]$V1,na.rm=TRUE)),byrow=TRUE,nrow=1,ncol=2)
    result<-rbind(result,t(tmp))
  }
  return(result)
}
