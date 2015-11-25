ExcludeGenes <-
function(d,Threshold=0.5, Type=c("Train","Test")){

if(Type=="Train"){
Gene<-d[,2:(dim(d)[2]-1)];
}else{
Gene<-d[,2:(dim(d)[2])];
}

PercentMissing<-apply(Gene,2,FUN=function(x){return((length(x)-length(na.omit(x)))/length(x))});
Exclusion<-which(PercentMissing>=Threshold);
ExcludedGenes<-colnames(Gene)[Exclusion];
if(length(ExcludedGenes)>0){
Dat<-d[,-(Exclusion+1)];
Dd<-list(ExcludedGeneList=ExcludedGenes,Data=Dat);
return(Dd);

}else{
Dat<-d;
Dd<-list(ExcludedGeneList=ExcludedGenes,Data=Dat);
return(Dd);
}
}
