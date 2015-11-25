ScaleCenterTest <-
function(d,Mean,SD){

d1<-d[,-1];

d2<-(d1-rep(1,dim(d1)[1])%*%t(Mean))*(rep(1,dim(d1)[1])%*%t(1/SD));

d3<-as.data.frame(cbind(d[,1],d2));

colnames(d3)<-colnames(d);

return(d3);
}
