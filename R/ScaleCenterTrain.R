ScaleCenterTrain <-
function(d){

d1<-d[,-c(1,dim(d)[2])];

Mean<-apply(d1,2,FUN=mean);
SD<-apply(d1,2,FUN=sd);

d2<-apply(d1,2,FUN=function(x){return((x-mean(x))/sd(x))});

Data<-as.data.frame(cbind(d[,1],d2,d[,dim(d)[2]]));
colnames(Data)<-colnames(d);

Dd<-list(Mean=Mean, SD=SD, Data=Data);

return(Dd);
}
