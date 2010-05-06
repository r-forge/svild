SVILD.default <-
function(id, time, y, timeFormat = "%d%b%y:%H:%M:%S", ...){
if (mode(y) != "numeric" ) 
stop("y must be a numeric variable")                           #check input

data = data.frame(id = id, time = time, y = y)                 # create a data frame
data = na.omit(data)                                           #remove any missing rows
new.time = strptime(as.character(data$time), format = timeFormat)        #convert to a time variable
data = cbind(data,new.time=new.time)                                     #crete a new data frame
data = data[order(data$id,data$new.time),]                               #sort by id and time
uid = unique(data$id)                                          # unique ID

# Create a list peopleof length n whose elements are named "person1 - N"  where each element 
# of the list is a list with 7 components: id, time,y, lag.time, lag.y, diff.time and diff.y, respectively; 

people = vector("list",length(uid))
for (i in 1:length(uid)){

sub.data = data[which(data$id == uid[i]),]
sub.y = sub.data$y
diff.y = rep(NA,length(sub.y))
diff.y[2:length(sub.y)] = sub.y[2:length(sub.y)] - sub.y[1:(length(sub.y)-1)]
lag.y = rep(NA,length(sub.y))
lag.y[2:length(sub.y)] = sub.y[1:(length(sub.y)-1)]
sub.time =sub.data$new.time
time = as.numeric(difftime(sub.time, rep(sub.time[1],length(sub.time)), units="mins"))
#(sub.time - sub.time[1])/60
diff.time = rep(NA,length(time))
diff.time[2: length(time)]= (time[2:length(time)] - time[1:(length(time)-1)])
lag.time = rep(NA,length(time))
lag.time[2:length(time)] =time[1:(length(time)-1)]

people[[i]] = list(ID = uid[i], y = sub.y, lag.y = lag.y, diff.y = diff.y,
                          fmttime = sub.time, time = time, lag.time = lag.time, diff.time = diff.time)
}
names(people) = paste("person",1:length(uid),sep = "")
class(people) = "SVILD"
return(people)
}

