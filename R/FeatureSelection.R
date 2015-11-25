FeatureSelection <-
function(d){


d1<-d[,2:dim(d)[2]];

for(i in 1:(dim(d1)[2]-1)){

if(is.factor(d1[,i]))d1[,i]<-as.numeric(as.character(d[,i]));

}

library(randomForest);

rf<-randomForest(as.factor(Status)~., ntree=500, data=d1, replace=TRUE, na.action=na.omit)

importanceOrder<-order(-rf$importance);

pdf("Gene_Importance_Plot.pdf");
varImpPlot(rf)
dev.off();

n<-dim(d1)[1];

Boot=100;

RFAcc<-as.data.frame(matrix(nrow=Boot, ncol=length(importanceOrder)));

for(i in 1:length(importanceOrder)){

dt<-d1[,c(importanceOrder[1:i],dim(d1)[2])];

for(k in 1:Boot){

index<-sample.int(n, size=floor(0.8*n));

train<-dt[index,];

test<-dt[-index,];

r1<-randomForest(as.factor(Status)~., ntree=1000, data=train);

p1<-predict(r1, newdata=test, type="response");

a1<-acc(p1, test$Status);

#RFAcc[k,1]<-a1[1,1]/(a1[1,1]+a1[1,2]);
#RFAcc[k,2]<-a1[2,2]/(a1[2,1]+a1[2,2]);
RFAcc[k,i]<-(a1[1,1]+a1[2,2])/(a1[1,1]+a1[1,2]+a1[2,1]+a1[2,2]);

}
}
colnames(RFAcc)<-seq(1,length(importanceOrder),1);
write.csv(RFAcc, "Random_Forest_Accuracy_With_Importance_Ordering.csv", quote=FALSE, row.names=FALSE);

Mean<-apply(RFAcc, 2, FUN=mean)
SD<-apply(RFAcc, 2, FUN=sd)

pdf("Accuracy_Plot_With_Model_Size.pdf");
plot(seq(1,length(Mean),1),Mean, type="b", ylim=c(0.8,1))
lines(seq(1,length(Mean),1),Mean-SD, type="l", col=4)
lines(seq(1,length(Mean),1),Mean+SD, type="l", col=4)
dev.off();

return(list(Order=importanceOrder, ImportantGenes=rownames(rf$importance)[importanceOrder]));


}
