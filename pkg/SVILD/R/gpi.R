gpi <-
function(object, ...){
#object = a
temp = vector("list", length(object))

for (i in 1:length(object)){    # loop through each patient
#i= 1

temp[[i]]$ID = object[[i]]$ID
#temp[[i]]$trapezoid[[1]] = 0
#temp[[i]]$GPI_sum = 0

for (j in 1:length(object[[i]]$y)){   # loop through each BG point
y_j= object[[i]]$y[[j]]
#time_j = object[[i]]$time[[j]]
#time_j_prev = object[[i]]$time[[j-1]]

if (y_j < 20){
temp[[i]]$hypo[[j]] = 100
temp[[i]]$hypo_ind[[j]] = 1
temp[[i]]$hyper[[j]] = 0
temp[[i]]$hyper_ind[[j]] = 0

}else if (20 <= y_j & y_j < 80){
temp[[i]]$hypo[[j]] = 7.4680 * (80 - y_j) ^ 0.6337
temp[[i]]$hypo_ind[[j]] = 1
temp[[i]]$hyper[[j]] = 0
temp[[i]]$hyper_ind[[j]] = 0

}else if (110 < y_j & y_j <= 250){
temp[[i]]$hyper[[j]] = 6.1767 * (y_j - 110) ^ 0.5635
temp[[i]]$hyper_ind[[j]] = 1
temp[[i]]$hypo[[j]] = 0
temp[[i]]$hypo_ind[[j]] = 0

}else if (y_j > 250){
temp[[i]]$hyper[[j]] = 100
temp[[i]]$hyper_ind[[j]] = 1
temp[[i]]$hypo[[j]] = 0
temp[[i]]$hypo_ind[[j]] = 0
}else{
temp[[i]]$hyper[[j]] = 0
temp[[i]]$hyper_ind[[j]] = 0
temp[[i]]$hypo[[j]] = 0
temp[[i]]$hypo_ind[[j]] = 0
}
}
temp[[i]]$GPI = round((sum(temp[[i]]$hyper) + sum(temp[[i]]$hypo))/length(object[[i]]$y), digit = 2)
if ((sum(temp[[i]]$hypo) + sum(temp[[i]]$hyper))!= 0){
temp[[i]]$HYPO_perc = round(sum(temp[[i]]$hypo)/(sum(temp[[i]]$hypo) + sum(temp[[i]]$hyper)), digit = 4)*100
temp[[i]]$HYPER_perc = round(sum(temp[[i]]$hyper)/(sum(temp[[i]]$hypo) + sum(temp[[i]]$hyper)), digit = 4)*100
}else {
temp[[i]]$HYPO_perc = 0
temp[[i]]$HYPER_perc = 0
}

}
names(temp) = paste("person",1:length(object),sep = "")

ID = unlist(sapply(temp,"[[",1))
names(ID) = NULL
GPI = unlist(sapply(temp,"[[",6))
names(GPI) = NULL
HYPO_perc = unlist(sapply(temp,"[[",7))
names(HYPO_perc) = NULL
HYPER_perc = unlist(sapply(temp,"[[",8))
names(HYPER_perc) = NULL
res = data.frame(ID = ID, GPI = GPI, HYPO_perc, HYPER_perc)
return(res)
}

