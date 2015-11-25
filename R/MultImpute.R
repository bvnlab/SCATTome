MultImpute <-
function(d, Type=c("Train","Test")){

library(Amelia)

if(Type=="Train"){
d2<-d[,-c(1,dim(d)[2])];
}else{
d2<-d[,-1];
}

k<-dim(d2)[2];

for(i in 3:k){

dt<-d2[(i-2):i];
dtp<-na.omit(dt);
if((dim(dt)[1]-dim(dtp)[1])>0){

aout<-amelia(dt, m=100);

for(j in 1:100){

if(j==1)dtimp<-aout$imputations[[j]];
if(j>1)dtimp<-dtimp+aout$imputations[[j]];

}

dtimp1<-dtimp/100;

d2[(i-2):i]<-dtimp1;
}
}

if(Type=="Train"){
d3<-as.data.frame(cbind(d[,1],d2,d[,dim(d)[2]]))
}else{
d3<-as.data.frame(cbind(d[,1],d2))
}
colnames(d3)<-colnames(d);

return(d3);
}
