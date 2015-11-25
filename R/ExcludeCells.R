ExcludeCells <-
function(d,Threshold=0.5){

PercentMissing<-apply(d,1,FUN=function(x){return((length(x)-length(na.omit(x)))/length(x))});
Exclusion<-which(PercentMissing>=Threshold);
ExclusionCells<-d[,1][Exclusion];
if(length(Exclusion)>0){
Dat<-d[-Exclusion,];
Dd<-list(ExclusionList=ExclusionCells, Data=Dat);
}else{
Dat<-d;
Dd<-list(ExclusionList=ExclusionCells, Data=Dat);
}
return(Dd);

}
