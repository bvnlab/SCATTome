TrainROC <-
function(train,ImportantGenes,GeneNumbers=7){

index<-sample.int(dim(train)[1], size=floor(0.8*dim(train)[1]));

dtrain<-train[index,-1];
dtest<-train[-index,-c(1,dim(train)[2])];
TestStatus<-train[-index,dim(train)[2]];

library(randomForest)
library(e1071)

trcol<-colnames(dtrain);
tecol<-colnames(dtest);

SelectedSet<-ImportantGenes[1:GeneNumbers];

trind<-c();
tsind<-c();

for(i in 1:length(SelectedSet)){

trind<-c(trind, which(trcol==SelectedSet[i]));
tsind<-c(tsind, which(tecol==SelectedSet[i]));

}

dtr<-dtrain[,c(trind,dim(dtrain)[2])];
dts<-dtest[,tsind];

rf<-randomForest(as.factor(Status)~., ntree=1000, data=dtr);
prf<-predict(rf, newdata=dts, type="prob");
prf1<-prf[,2];

#---------------------------

threshold<-seq(0,1,0.01);

tp<-c();
fp<-c();

for(i in 1:length(threshold)){

temp<-ceiling(prf1-threshold[i]);
conf<-xtabs(~temp+TestStatus);
if(dim(conf)[1]==1){
if(unique(temp)==1){
tp=c(tp,1);
fp=c(fp,1);
}else{
tp=c(tp,0);
fp=c(fp,0);
}
}else{
tp<-c(tp,conf[2,2]/(conf[1,2]+conf[2,2]));
fp<-c(fp,conf[2,1]/(conf[1,1]+conf[2,1]));
}

}

AUC_RF<-sum(tp)*0.01;

pdf("ROC_CURVES_OF_TRAIN_SET.pdf")

plot(fp,tp, ylab="True Positives", xlab="False Positives", main="Sample ROC of Random Forest", type="l", col=4);
lines(c(0,1),c(0,1),lty=2)
#---------------------------

sv<-svm(as.factor(Status)~., data=dtr, kernel="radial", type="C-classification", cross=10, probability=TRUE);
psv<-predict(sv, newdata=dts, probability=TRUE);

prob<-attr(psv,"probabilities");
prob1<-prob[,2];

#---------------------------


tp<-c();
fp<-c();

for(i in 1:length(threshold)){

temp<-ceiling(prob1-threshold[i]);
conf<-xtabs(~temp+TestStatus);
if(dim(conf)[1]==1){
if(unique(temp)==1){
tp=c(tp,1);
fp=c(fp,1);
}else{
tp=c(tp,0);
fp=c(fp,0);
}
}else{
tp<-c(tp,conf[2,2]/(conf[1,2]+conf[2,2]));
fp<-c(fp,conf[2,1]/(conf[1,1]+conf[2,1]));
}

}

AUC_SVM1<-sum(tp)*0.01;

plot(fp,tp, ylab="True Positives", xlab="False Positives", main="Sample ROC of SVM(Gaussian Kernel)", type="l", col=4);
lines(c(0,1),c(0,1),lty=2)

#---------------------------


sv1<-svm(as.factor(Status)~., data=dtr, kernel="sigmoid", type="C-classification", cross=10, probability=TRUE);
psv1<-predict(sv, newdata=dts, probability=TRUE);

prob<-attr(psv1,"probabilities");
prob2<-prob[,2];

#---------------------------


tp<-c();
fp<-c();

for(i in 1:length(threshold)){

temp<-ceiling(prob2-threshold[i]);
conf<-xtabs(~temp+TestStatus);
if(dim(conf)[1]==1){
if(unique(temp)==1){
tp=c(tp,1);
fp=c(fp,1);
}else{
tp=c(tp,0);
fp=c(fp,0);
}
}else{
tp<-c(tp,conf[2,2]/(conf[1,2]+conf[2,2]));
fp<-c(fp,conf[2,1]/(conf[1,1]+conf[2,1]));
}

}

AUC_SVM2<-sum(tp)*0.01;


plot(fp,tp, ylab="True Positives", xlab="False Positives", main="Sample ROC of SVM (Sigmoidal Kernel)", type="l", col=4);
lines(c(0,1),c(0,1),lty=2)

#---------------------------


ens.p=(prf1+prob1+prob2)/3;

#---------------------------


tp<-c();
fp<-c();

for(i in 1:length(threshold)){

temp<-ceiling(ens.p-threshold[i]);
conf<-xtabs(~temp+TestStatus);
if(dim(conf)[1]==1){
if(unique(temp)==1){
tp=c(tp,1);
fp=c(fp,1);
}else{
tp=c(tp,0);
fp=c(fp,0);
}
}else{
tp<-c(tp,conf[2,2]/(conf[1,2]+conf[2,2]));
fp<-c(fp,conf[2,1]/(conf[1,1]+conf[2,1]));
}

}

AUC_Ens<-sum(tp)*0.01;


plot(fp,tp, ylab="True Positives", xlab="False Positives", main="Sample ROC of Ensemble Models", type="l", col=4);
lines(c(0,1),c(0,1),lty=2)
dev.off();
#---------------------------

Method<-c("Random Forest", "SVM(Gaussian Kernel)", "SVM(Sigmoidal Kernel)", "Ensemble");
AUCROC<-c(AUC_RF, AUC_SVM1, AUC_SVM2, AUC_Ens); 

AUC<-as.data.frame(cbind(Method, AUCROC));

write.csv(AUC, "AUCROC_Train.csv", quote=FALSE, row.names=FALSE);
return(NULL);

}
