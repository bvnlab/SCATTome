FluidigmDataConversion <-
function(d, Cell=2, Gene=5, Value=7){

d1<-as.data.frame(d[,c(Cell, Gene, Value)]);

for(i in 1:length(d1[,3])){
if(d1[i,3]==999)d1[i,3]=NA;
}

colnames(d1)<-c("c","g","v");

CellNames<-na.omit(unique(d1[,1]));
GeneNames<-na.omit(unique(d1[,2]));

dat<-as.data.frame(matrix(nrow=length(CellNames), ncol=length(GeneNames)));

colnames(dat)<-GeneNames;
rownames(dat)<-CellNames;

for(i in 1:length(CellNames)){


for(j in 1:length(GeneNames)){


dat[i,j]<-d1[which((as.character(d1$c)==as.character(CellNames[i])) & (as.character(d1$g)==as.character(GeneNames[j]))),3];

}
}

dat<-cbind(CellNames, dat);
colnames(dat)<-c("CellNames",GeneNames);


write.csv(dat, "FluidigmDataConverted.csv", quote=FALSE, row.names=FALSE);
return(NULL);
}
