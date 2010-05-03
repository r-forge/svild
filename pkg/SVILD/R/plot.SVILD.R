plot.SVILD <-
function(object,n = 12, ...){
library(lattice)
y = sapply(object,"[[",2)
names(y) = NULL
N =  sapply(y,length)
ID = rep(unlist(sapply(object,"[[",1)),N)
names(ID) = NULL
y = sapply(object,"[[",2)
names(y) = NULL
t = sapply(object,"[[",6)
names(t) = NULL
plotdata = data.frame(ID = ID, Y = unlist(y), Time = unlist(t))
uid = unique(plotdata$ID)
if (n > length(uid)) n = length(uid)
plotID = sample(uid, size = n)
sub.plotdata = plotdata[which(plotdata$ID %in% plotID),]
options(warn=-1) 
xyplot(Y~Time | ID, data = sub.plotdata,
      prepanel = function(x, y) prepanel.loess(x, y, family="gaussian"),
      xlab = "Time", ylab = "Y",
      panel = function(x, y) {
      panel.xyplot(x, y)
      panel.loess(x,y, family="gaussian") },
      as.table=TRUE)
#invisible()
}

