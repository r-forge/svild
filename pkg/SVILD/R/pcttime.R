pcttime <-
function(object, cutoff = c(80,110,200)){

getname = function(cutoff){
n = length(cutoff)+ 1
pctname = character(n)
for (i in 1:n){
if (i == 1){
pctname[1] = paste('pctLS',cutoff[1],sep='')
}else if ( i < n){
pctname[i] = paste('pctBT',cutoff[i-1],'and', cutoff[i], sep='')
}else {
pctname[n] = paste('pctGT',cutoff[n -1], sep='')
}
}
return(pctname)
}

person = object
n = length(person)              #how mant unique ID
nc = length(cutoff)         #how many cutoff points
for (k in 1:n){             #loop all unique ID

n1 = length(person[[k]]$y) #how many y in a person
N = (n1-1)*(nc+1)
person[[k]]$pt = matrix(rep(0,N),ncol = nc+1) #this vector saves time < 80, time < 110, time < 200, total time 
#for all sequence time points within a person 
person[[k]]$ptime = matrix(rep(0,N),ncol = nc+1)#this vector saves time of y< 80, time of y = 80-110, time of y= 110-200 
# and time of y > 200 for all sequence time points within a person

for (j in 2:n1){# loop all y values
for (i in 1:nc){ #loop all cutoff points
if (cutoff[i] < person[[k]]$y[j] & cutoff[i] > person[[k]]$y[j-1]){
person[[k]]$pt[j-1,i] = (person[[k]]$time[j]- person[[k]]$time[j - 1])*(cutoff[i] - person[[k]]$y[j-1])/(person[[k]]$y[j] - person[[k]]$y[j - 1])
}else if (cutoff[i] > person[[k]]$y[j] & cutoff[i] < person[[k]]$y[j-1]){
person[[k]]$pt[j-1,i] = (person[[k]]$time[j]- person[[k]]$time[j - 1])*( cutoff[i] - person[[k]]$y[j] )/(person[[k]]$y[j-1] - person[[k]]$y[j])
}else if(cutoff[i] > person[[k]]$y[j] & cutoff[i] > person[[k]]$y[j-1]){
person[[k]]$pt[j-1,i] = person[[k]]$time[j]- person[[k]]$time[j - 1]
}else {
person[[k]]$pt[j-1,i] = 0
}
}
person[[k]]$pt[j-1,nc+1] = person[[k]]$diff.time[j]
person[[k]]$ptime[1,1] = person[[k]]$pt[1,1]
for(f in 2:(nc+1)){
person[[k]]$ptime[j-1,f] = person[[k]]$pt[j-1,f]-person[[k]]$pt[j-1,f-1]
}
}
}

print(sapply(person,'[[', 10))
#create a matrix;
PTIME = vector('list',length(person))
BB = sapply(person,'[[',10)
for (i in 1:length(person)){
PTIME[[i]] = t(matrix(apply(BB[[i]],2,sum)))
}

ptmatrix = do.call('rbind',PTIME)
ptmatrix = 100 * (ptmatrix) /(rowSums(ptmatrix))
ptmatrix = apply(ptmatrix,2,function (x) round(x,digits=2))
colnames(ptmatrix) = getname(cutoff)

ptmatrix = cbind(uid = 1:length(object),ptmatrix) 

data1 = data.frame(ptmatrix)

ID = unlist(sapply(object,"[[",1))
names(ID) = NULL

t = sapply(object,"[[",6)
names(t) = NULL

LengthMins = sapply(t,max)

data2 = data.frame(uid = 1:length(object),ID = ID, LengthMins = LengthMins)
res = merge(data2,data1,by= 'uid')
res = res[,-1]
return(res)

}

