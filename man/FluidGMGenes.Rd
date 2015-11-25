\name{FluidGMGenes}
\alias{FluidGMGenes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Gene selection using Random Forest only. 
}
\description{
Uses node purity based importance ordering of genes to select important genes.
}
\usage{
FluidGMGenes(train, ColumnThreshold, RowThreshold)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{train}{
%%     ~~Describe \code{train} here~~
}
  \item{ColumnThreshold}{
%%     ~~Describe \code{ColumnThreshold} here~~
}
  \item{RowThreshold}{
%%     ~~Describe \code{RowThreshold} here~~
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
function (train, ColumnThreshold, RowThreshold) 
{
    source("Fluidigm.r")
    dtrain = read.table(train, sep = ",", header = TRUE, fill = TRUE, 
        as.is = TRUE)
    ct <- checkData(dtrain, Type = "Train")
    if (ct == 1) {
        d3 <- ExcludeData(dtrain, ColumnThreshold = 0.5, RowThreshold = 0.5, 
            Type = "Train")
    }
    d4 <- MultImpute(d3$Data, Type = "Train")
    d5 <- MultImpute(d4, Type = "Train")
    d5 <- na.omit(d5)
    d6 <- ScaleCenterTrain(d5)
    d7 <- FeatureSelection(d6$Data)
    d8 <- FeatureSelectionLasso(d6$Data)
    write.table(c("GeneListObject"), "GeneListObject.txt", quote = FALSE, 
        row.names = FALSE, col.names = FALSE)
    write.csv(d6$Data, "TrainDataMod.csv", quote = FALSE, row.names = FALSE)
    write.csv(cbind(d6$Mean, d6$SD), "TrainMeanSD.csv", quote = FALSE, 
        row.names = FALSE)
    write.csv(d7$ImportantGenes, "RandomForestgeneList.csv", 
        quote = FALSE, row.names = FALSE)
    write.csv(d8, "LASSOgeneList.csv", quote = FALSE, row.names = FALSE)
    return(list(Index = "GeneListObject", TrainData = d6$Data, 
        CenterMean = d6$Mean, CenterSD = d6$SD, ImportantGeneList = d7$ImportantGenes, 
        LassoGenes = d8))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
