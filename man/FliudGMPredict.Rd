\name{FliudGMPredict}
\alias{FliudGMPredict}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Prediction.
}
\description{
Main prediction function using an ensemble of GLMM, Random forest, and SVM. 
}
\usage{
FliudGMPredict(TrainGene = NULL, test, ColumnThreshold, RowThreshold, GeneNumbers, readfile = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{TrainGene}{
%%     ~~Describe \code{TrainGene} here~~
}
  \item{test}{
%%     ~~Describe \code{test} here~~
}
  \item{ColumnThreshold}{
%%     ~~Describe \code{ColumnThreshold} here~~
}
  \item{RowThreshold}{
%%     ~~Describe \code{RowThreshold} here~~
}
  \item{GeneNumbers}{
%%     ~~Describe \code{GeneNumbers} here~~
}
  \item{readfile}{
%%     ~~Describe \code{readfile} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (TrainGene = NULL, test, ColumnThreshold, RowThreshold, 
    GeneNumbers, readfile = 0) 
{
    source("Fluidigm.r")
    if (readfile == 1) {
        GeneListObject = read.table("GeneListObject.txt", header = FALSE, 
            sep = " ", as.is = TRUE)
        TrainData = read.csv("TrainDataMod.csv", header = TRUE, 
            as.is = TRUE, fill = TRUE)
        Center = read.csv("TrainMeanSD.csv", header = TRUE, as.is = TRUE, 
            fill = TRUE)
        CenterMean = Center[, 1]
        CenterSD = Center[, 2]
        ImportantGeneList = read.csv("RandomForestgeneList.csv", 
            header = TRUE, as.is = TRUE, fill = TRUE)
        LassoGenes = read.csv("LASSOgeneList.csv", header = TRUE, 
            as.is = TRUE, fill = TRUE)
        TrainGene = list(Index = GeneListObject[[1]], TrainData = TrainData, 
            CenterMean = CenterMean, CenterSD = CenterSD, ImportantGeneList = ImportantGeneList[, 
                1], LassoGenes = LassoGenes[, 1])
    }
    if (!(TrainGene$Index == "GeneListObject")) {
        print("GeneListObject Not Detected. Exit NULL")
        return(NULL)
    }
    dtest <- read.csv(test, as.is = TRUE, fill = TRUE, header = TRUE)
    ct <- checkData(dtest, Type = "Test")
    if (ct == 1) {
        b3 <- ExcludeData(dtest, ColumnThreshold = ColumnThreshold, 
            RowThreshold = RowThreshold, Type = "Test")
    }
    TestCells <- b3$Data[, 1]
    b4 <- MultImpute(b3$Data, Type = "Test")
    b5 <- MultImpute(b4, Type = "Test")
    b5 <- na.omit(b5)
    b6 <- ScaleCenterTest(b5, TrainGene$CenterMean, TrainGene$CenterSD)
    GeneImp <- c()
    for (i in 1:length(TrainGene$ImportantGeneList)) {
        if (length(which(TrainGene$LassoGenes == TrainGene$ImportantGeneList[i])) > 
            0) 
            GeneImp <- c(GeneImp, TrainGene$ImportantGeneList[i])
    }
    b6[, 1] <- TestCells
    Final <- PredictEnsemble(train = TrainGene$TrainData, test = b6, 
        GeneImp, GeneNumbers)
    testID <- colnames(test)
    inclID <- colnames(b3)
    exclID <- c()
    for (i in 1:length(testID)) {
        if (length(which(inclID == testID[i])) == 0) 
            exclID <- c(exclID, testID[i])
    }
    b6[, 1] <- TestCells
    write.csv(TrainGene$TrainData, "TrainDataAfterExclusion.csv", 
        row.names = FALSE, quote = FALSE)
    write.csv(b6[, -dim(b6)[2]], "TestDataAfterExclusion.csv", 
        row.names = FALSE, quote = FALSE)
    TrainROC(train = TrainGene$TrainData, GeneImp, GeneNumbers)
    return(list(ImportantGenes = GeneImp[1:GeneNumbers], ExcludedGene_Test = exclID))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
