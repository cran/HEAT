read6city <-
function(mydata, code){

citydata=subset(mydata,mydata[,1]==code)
ncc=dim(mydata)[2]
nrr=dim(mydata)[1]

cbind(citydata)
}
