FeatureSelectionLasso <-
function(d){

library(glmnet);

y<-d$Status;
x<-as.matrix(d[,-c(1,dim(d)[2])]);
l<-cv.glmnet(x,y,nfolds=10, family="binomial");

lambdam<-l$lambda[which(l$cvm==min(l$cvm))];

m<-glmnet(x,y,family="binomial", lambda=lambdam);
m1<-glmnet(x,y,family="binomial");

pdf("LASSO_Plots.pdf")
plot(l);
plot(m1);
dev.off();
c<-rownames(m$beta)[which(!(m$beta==0))];
return(c);


}
