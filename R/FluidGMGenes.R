FluidGMGenes <-
function(train, ColumnThreshold, RowThreshold){

source("Fluidigm.r");

#Training Data 

dtrain=read.table(train, sep=",", header=TRUE, fill=TRUE, as.is=TRUE);

ct<-checkData(dtrain, Type="Train");

if(ct==1){
d3<-ExcludeData(dtrain, ColumnThreshold=0.5, RowThreshold=0.5, Type="Train");
}

d4<-MultImpute(d3$Data, Type="Train");
d5<-MultImpute(d4, Type="Train");
d5<-na.omit(d5)

d6<-ScaleCenterTrain(d5)

d7<-FeatureSelection(d6$Data);

d8<-FeatureSelectionLasso(d6$Data);

write.table(c("GeneListObject"), "GeneListObject.txt", quote=FALSE, row.names=FALSE, col.names=FALSE);
write.csv(d6$Data, "TrainDataMod.csv", quote=FALSE, row.names=FALSE);
write.csv(cbind(d6$Mean,d6$SD), "TrainMeanSD.csv", quote=FALSE, row.names=FALSE);
write.csv(d7$ImportantGenes, "RandomForestgeneList.csv", quote=FALSE, row.names=FALSE);
write.csv(d8, "LASSOgeneList.csv", quote=FALSE, row.names=FALSE);

return(list(Index="GeneListObject", TrainData=d6$Data, CenterMean=d6$Mean, CenterSD=d6$SD, ImportantGeneList=d7$ImportantGenes, LassoGenes=d8));

}
