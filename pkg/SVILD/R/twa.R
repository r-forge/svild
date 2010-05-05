twa <-
function(object, interpolate = c("linear", "leftstep","rightstep","spline"), ...){
temp = vector("list", length(object))
for (i in 1:length(object)){
temp[[i]]$ID = object[[i]]$ID
if (interpolate == "linear"){
trap.rule = function (x, y) sum(diff(x) * (y[-1] + y[-length(y)]))/2 
temp[[i]]$auc = trap.rule(object[[i]]$time,object[[i]]$y)
                  temp[[i]]$twa = temp[[i]]$auc/(max(object[[i]]$time))
}else if (interpolate == "leftstep"){
trap.rule = function (x, y) sum(diff(x) * (y[-length(y)]))/2 
temp[[i]]$auc = trap.rule(object[[i]]$time,object[[i]]$y)
temp[[i]]$twa = temp[[i]]$auc/(max(object[[i]]$time))
}else if (interpolate == "rightstep"){
trap.rule = function (x, y) sum(diff(x) * (y[-1]))/2 
temp[[i]]$auc = trap.rule(object[[i]]$time,object[[i]]$y)
temp[[i]]$twa = temp[[i]]$auc/(max(object[[i]]$time))
}else if (interpolate == "spline"){
sp = spline(object[[i]]$time,object[[i]]$y, ...) 
trap.rule = function (x, y) sum(diff(x) * (y[-1] + y[-length(y)]))/2 
temp[[i]]$auc = trap.rule(sp$x,sp$y)
temp[[i]]$twa = temp[[i]]$auc/(max(object[[i]]$time))
}else {
warning ("interpolate must be 'linear', 'leftstep','rightstep' or 'spline'")
}
}

names(temp) = paste("person",1:length(object),sep = "")

ID = unlist(sapply(temp,"[[",1))
names(ID) = NULL
TWA = round(unlist(sapply(temp,"[[",3)),digit = 2)
names(TWA) = NULL
res = data.frame(ID = ID, TWA = TWA)
return(res)
}

