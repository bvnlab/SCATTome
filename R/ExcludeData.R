ExcludeData <-
function(d, ColumnThreshold=0.5, RowThreshold=0.5, Type=c("Train","Test")){

d1<-ExcludeGenes(d=d,Threshold=ColumnThreshold,Type=Type);
ExcludedColumns=d1$ExcludedGeneList;
d2<-ExcludeCells(d1$Data,RowThreshold);
ExcludedRows=d2$ExclusionList;
Dd=list(ExcludedGenes=ExcludedColumns, 
ExcludedCells=ExcludedRows,
Data=d2$Data);
return(Dd);

}
