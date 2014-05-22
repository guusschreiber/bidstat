
# These two functions work with the deal profiles:
#
# dealbids1: computes average imp score of every bid compared to all
#            other bids,
#            for all deals that match the record
# dealbids2: compares average imps diff of two bids that occur on the same deal
#            for all deals that match the record
#            example: profile: weak 6M, compare 2D and 2M bids

bs.dealbids1 <- function(data1,data2) {
  dt1 <- read.table(data1, header=TRUE)
  dt2 <- read.table(data2, header=TRUE)
  dt <- rbind(dt1, dt2)
  
  dt <- within(dt, {
    aimps = imps / n2 })
  
  attach(dt)
  tmp1 <- aggregate(aimps, by=list(bid,suit), FUN=sum)
  tmp2 <- aggregate(n1, by=list(bid,suit), FUN=sum)
  detach(dt)

  dtt <- cbind(tmp1, tmp2$x)
  names(dtt) <- c("suit", "bid", "imps", "f.bid")
  tmp <- c(mean(dtt$suit), mean(dtt$bid), sum(dtt$imps), sum(dtt$f.bid))
  dtt <- rbind(dtt, tmp)
  
  dtt <- within(dtt, {
    mi.bid = imps / f.bid
  })

  return(dtt)
}

bs.dealbids2 <- function(data1, data2) {
  dt1 <- read.table(data1, header=TRUE)
  dt2 <- read.table(data2, header=TRUE)
  dt <- rbind(dt1, dt2)

  dt <- within(dt, {
    aimps = imps / n2 })
  
  attach(dt)
  tmp1 <- aggregate(aimps, by=list(suit), FUN=sum)
  tmp2 <- aggregate(n1, by=list(suit), FUN=sum)
  tmp3 <- aggregate(n2, by=list(suit), FUN=sum)
  detach(dt)

  dtt <- cbind(tmp1, tmp2$x, tmp3$x)
  names(dtt) <- c("suit", "imps", "f.b1", "f.b2")
  tmp <- c(mean(dtt$suit), sum(dtt$imps), sum(dtt$f.b1), sum(dtt$f.b2))
  dtt <- rbind(dtt, tmp)
  
  dtt <- within(dtt, {
    mi.b2 = imps / (-1 * f.b2)
    mi.b1 = imps / f.b1
  })

  return(dtt)
}


# #bids and auction length in (un)contested auctions

bs.ca <- function(data, periodsize) {
  ifelse(is.data.frame(data),
    ca <- data, 
    ca <- read.table(data, header=TRUE))

  ca <- transform(ca, period = bs.period(year,periodsize))
  ca0 <- subset(ca, bool==0)
  ca1 <- subset(ca, bool==1)
  attach(ca0)
  ca0.b <- aggregate(nbids, by=list(period), FUN=mean )
  ca0.a <- aggregate(alength, by=list(period), FUN=mean )
  detach(ca0)
  attach(ca1)
  ca1.b <- aggregate(nbids, by=list(period), FUN=mean )
  ca1.a <- aggregate(alength, by=list(period), FUN=mean )
  detach(ca1)

  cat <- cbind(ca0.b, ca0.a$x, ca1.b$x, ca1.a$x)
  names(cat) <- c("period", "nbids u", "alength u",
                  "nbids c", "alength c"  )
  rbind(cat, apply(cat, 2, mean))
  
  return(cat)
}

# 6M weak

bs.wm <- function(data) {
  ifelse(is.data.frame(data),
    wm <- data, 
    wm <- read.table(data, header=TRUE))

  wm <- na.omit(wm)

  ws <- subset(wm, suit==1 & 
    (bid==0 | bid==14 | bid==22 | bid==23 | bid == 24 | bid==34))
  attach(ws)
  ws1 <- cbind(table(bid))
  ws2 <- aggregate(hcp, by=list(bid), FUN=mean)
  ws3 <- aggregate(phcp, by=list(bid), FUN=mean)
  ws4 <- aggregate(imps, by=list(bid), FUN=mean)
  ws5 <- aggregate(suitq, by=list(bid), FUN=mean)
  detach(ws)
  ws.t <- as.data.frame(cbind(ws1, ws2$x, ws3$x, ws4$x, ws5$x))
  names(ws.t) <- c("freq", "hcp", "phcp", "imps", "suitq")
  
  wh <- subset(wm, suit==2 & 
          (bid==0 | bid==13 | bid==22 | bid==23 | bid==33))
  attach(wh)
  wh1 <- cbind(table(bid))
  wh2 <- aggregate(hcp, by=list(bid), FUN=mean)
  wh3 <- aggregate(phcp, by=list(bid), FUN=mean)
  wh4 <- aggregate(imps, by=list(bid), FUN=mean)
  wh5 <- aggregate(suitq, by=list(bid), FUN=mean)
  detach(wh)
  wh.t <- as.data.frame(cbind(wh1, wh2$x, wh3$x, wh4$x, wh5$x))
  names(wh.t) <- c("freq", "hcp", "phcp", "imps", "suitq")

  return(rbind(ws.t, wh.t))
}

# Statistical feature: weak_two_1st

bs.ws <- function(data,periodsize) {
  ifelse(is.data.frame(data),
    ws <- data, 
    ws <- read.table(data, header=TRUE))

  ws <- na.omit(ws)
  ws <- transform(ws, period = bs.period(year, periodsize))
  
  ws.f <- addmargins(table(ws$period, ws$level))
  ws.f <- as.data.frame(cbind(ws.f))

  ws.p <- addmargins(prop.table(table(ws$period, ws$level), 1))
  ws.p <- as.data.frame(cbind(ws.p))

  ws.b <- prop.table(table(ws$level, ws$period), 2)
  ws.b <- rbind(ws.b[3:5,])
  barplot(ws.b, 
          xlab="Period",ylab="Frequency",
          ylim=c(0,0.5),
 #         legend.tex=TRUE,
          args.legend=list(x="topleft",inset=.05,title="Level"),
          col=gray.colors(3,start=0.7, end=0.3))

  result <- cbind(ws.f[1],ws.p[1],ws.f[2],ws.p[2],
                  ws.f[3],ws.p[3],ws.f[4],ws.p[4],
                  ws.f[5],ws.p[5],ws.f[6])
  return(result)
}


# Statistical feature: contested_auction
# Linear model through partitioning

bs.caplin <- function(data,psize) {
  ifelse(is.data.frame(data),
    ca <- data, 
    ca <- read.table(data, header=TRUE))

  cap <- bs.partition(ca,psize)
  y <- cap[,1]
  c <- cap[,2]
  cap.lm <- lm(c~y)
  plot(y,c, ylim=c(0,1),
       xlab="Year", ylab="Freqeuncy contested auction")
  abline(cap.lm)
  
  return(cap.lm)
}

bs.caplog <- function(data,psize) {
  ifelse(is.data.frame(data),
    ca <- data, 
    ca <- read.table(data, header=TRUE))

  cap <- bs.partition(ca,psize)
  y <- cap[,1]
  c <- cap[,2]
  cap.lm <- lm(c~log(y))
  plot(y,c, ylim=c(0,1),
       xlab="Year", ylab="Freqeuncy contested auction")
  abline(cap.lm)
  
  return(cap.lm)
}

# Statistical feature: preempt

bs.pre <- function(data,periodsize) {
  ifelse(is.data.frame(data),
    pre <- data, 
    pre <- read.table(data, header=TRUE))

  pre <- within(pre, {
    bool <- NA
    bool[lvalue==0] <- 0
    bool[lvalue>0] <- 1})
  pre.f <- bs.bool(pre, periodsize, "no preempt", "preempt")

  pre.l <- subset(pre, lvalue > 0) 
  pre.l <- transform(pre.l, period=bs.period(year,periodsize))
  attach(pre.l)
  pre.l <- aggregate(lvalue, by=list(period), FUN=mean)
  detach(pre.l)

  pre.l <- rbind(pre.l, c(NA, NA))
  result <- cbind(pre.f, pre.l$x)
  names(result)[5] <- "lvalue"
  return(result)
}
  
# Function: bidstat bool (aggregated per period)
#
# Assumes three variables: id, year and bool (0 or 1)
# Example: results of query "opening_11_12_bal_2"
# Works on data frame or on txt file containing a data table

bs.bool <- function(data,periodsize,falselabel,truelabel){
  ifelse(is.data.frame(data),
    oop <- data, 
    oop <- read.table(data, header=TRUE))
  
  oop <- na.omit(oop)
  oop <- transform(oop, period = bs.period(year,periodsize))
  attach(oop)
  oop <- as.data.frame(cbind(table(period, bool)))
  names(oop)[1:2] <- c(falselabel, truelabel)
  oop <- transform(oop, total = oop[,1] + oop[,2])
  tmp <- apply(oop, 2, sum)
  oop <- rbind(oop, tmp)
  oop <- transform(oop, freq = oop[,2]/total)
  detach(oop)
  
  return(oop)
}

# Categorizes a year into period

bs.period <- function(year,periodsize) {
  period <- ( year %/% periodsize ) * periodsize
  return(period)
}

# formatting a table as html

bs.html <- function(table) {
  tmp <- xtable(table)
  print(tmp, type="html")
}

# This function computes the mean of n partitions of the data
# arguments: 
#   data: a data frame with a column "Year" and a column "V1"
#   psize: size of the partition
# result: 
#   2-column matrix with means of Year/V1 per partition 

bs.partition<-function(data,psize){
  data<-data[!is.na(data$bool),]
  result<-matrix(nrow=0,ncol=2,byrow=TRUE)
  for(i in 0:(nrow(data)/psize)){
    indexes<-seq(psize)+i*psize
    tmp<-as.matrix(c(mean(data[indexes,]$year,na.rm=TRUE),
        mean(data[indexes,]$bool,na.rm=TRUE)),byrow=TRUE,nrow=1,ncol=2)
    result<-rbind(result,t(tmp))
  }
  return(result)
}
