# 
f<-function(data,partitionsize){
  data<-data[!is.na(data$contested_auction),]
  result<-matrix(nrow=0,ncol=2,byrow=TRUE)
  for(i in 0:(nrow(data)/partitionsize)){
    indexes<-seq(100)+i*100
    tmp<-as.matrix(c(mean(data[indexes,]$Year,na.rm=TRUE),
                   mean(data[indexes,]$contested_auction,na.rm=TRUE)),byrow=TRUE,nrow=1,ncol=2)
    result<-rbind(result,t(tmp))
  }
  return(result)
}