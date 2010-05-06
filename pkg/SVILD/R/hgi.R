hgi <-
function(object, ..., cutoff = 110){

temp = vector("list", length(object))

for (i in 1:length(object)){    # loop through each patient

temp[[i]]$ID = object[[i]]$ID
temp[[i]]$trapezoid[[1]] = 0
temp[[i]]$HGI_sum = 0

for (j in 2:length(object[[i]]$y)){   # loop through each BG point
y_j= object[[i]]$y[[j]]
y_j_prev = object[[i]]$y[[j-1]]
time_j = object[[i]]$time[[j]]
time_j_prev = object[[i]]$time[[j-1]]

if (y_j_prev <= cutoff & y_j > cutoff){
t_ratio = (y_j - cutoff)/(y_j - y_j_prev)
temp[[i]]$trapezoid[[j]] = ((y_j-cutoff)* t_ratio * (time_j - time_j_prev))/2
temp[[i]]$HGI_sum = temp[[i]]$trapezoid[[j]] + temp[[i]]$HGI_sum

}else if (y_j_prev > cutoff & y_j > cutoff & y_j > y_j_prev){
temp_trapezoid = ((time_j - time_j_prev)*(y_j - y_j_prev))/2
temp[[i]]$trapezoid[[j]] = temp_trapezoid + (time_j - time_j_prev)*(y_j_prev - cutoff)
temp[[i]]$HGI_sum = temp[[i]]$trapezoid[[j]] + temp[[i]]$HGI_sum

}else if (y_j_prev > cutoff & y_j > cutoff & y_j < y_j_prev){
temp_trapezoid = ((time_j - time_j_prev)*(y_j_prev - y_j))/2
temp[[i]]$trapezoid[[j]] = temp_trapezoid + (time_j - time_j_prev)*(y_j-cutoff)
temp[[i]]$HGI_sum = temp[[i]]$trapezoid[[j]] + temp[[i]]$HGI_sum

}else if (y_j_prev > cutoff & y_j <= cutoff){
t_ratio = (y_j_prev - cutoff)/(y_j_prev - y_j)
temp[[i]]$trapezoid[[j]] = ((y_j_prev - cutoff)* t_ratio * (time_j - time_j_prev))/2
temp[[i]]$HGI_sum = temp[[i]]$trapezoid[[j]] + temp[[i]]$HGI_sum

}else {
temp[[i]]$trapezoid[[j]] = 0
temp[[i]]$HGI_sum = temp[[i]]$trapezoid[[j]] + temp[[i]]$HGI_sum
}
}
temp[[i]]$HGI = round(temp[[i]]$HGI_sum / max(object[[i]]$time), digit = 2)
}

names(temp) = paste("person",1:length(object),sep = "")

ID = unlist(sapply(temp,"[[",1))
names(ID) = NULL
HGI = unlist(sapply(temp,"[[",4))
names(HGI) = NULL
res = data.frame(ID = ID, HGI = HGI)
return(res)
}

