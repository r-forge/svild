gri <-
function(object,...){

temp = vector("list", length(object))

for (i in 1:length(object)){    # loop through each patient

temp[[i]]$ID = object[[i]]$ID
y = object[[i]]$y
temp[[i]]$FBG = 1.509 * (log(y)^1.084 - 5.381) 
temp[[i]]$RBG = 10*(temp[[i]]$FBG)^2
lowindex = which(temp[[i]]$FBG > 0)
temp[[i]]$RLBG = temp[[i]]$RBG
temp[[i]]$RLBG[lowindex] = 0
highindex = which(temp[[i]]$FBG < 0)
temp[[i]]$RHBG = temp[[i]]$RBG
temp[[i]]$RHBG[highindex] = 0

temp[[i]]$LBGI = round(mean(temp[[i]]$RLBG), digit = 1)
temp[[i]]$HBGI = round(mean(temp[[i]]$RHBG),digit = 1)
temp[[i]]$BGRI = round(temp[[i]]$LBGI + temp[[i]]$HBGI,digit = 1) 
}
names(temp) = paste("person",1:length(object),sep = "")

ID = unlist(sapply(temp,"[[",1))
names(ID) = NULL
LBGI = unlist(sapply(temp,"[[",6))
names(LBGI) = NULL
HBGI = unlist(sapply(temp,"[[",7))
names(HBGI) = NULL
BGRI = unlist(sapply(temp,"[[",8))
names(BGRI) = NULL
res = data.frame(ID = ID, LBGI = LBGI, HBGI = HBGI, BGRI = BGRI)
return(res)
}

