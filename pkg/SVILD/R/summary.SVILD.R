summary.SVILD <-
function(object, ...){

ID = unlist(sapply(object,"[[",1))
names(ID) = NULL

y = sapply(object,"[[",2)
names(y) = NULL
N =  sapply(y,length)
Mean = round(sapply(y,mean),digits = 2)
SD =   round(sqrt(sapply(y,var)),digits = 2)
Median = (sapply(y,median))
Min = (sapply(y,min))
Max = (sapply(y,max))

t = sapply(object,"[[",6)
names(t) = NULL

LengthMins = sapply(t,max)

res = data.frame(ID = ID, N = N,LengthMins = LengthMins, Mean = Mean, 
SD = SD, Median = Median, Min = Min, Max = Max)
return(res)
}

