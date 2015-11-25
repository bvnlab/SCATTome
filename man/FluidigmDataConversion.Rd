\name{FluidigmDataConversion}
\alias{FluidigmDataConversion}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Correct orientation of data for model analysis. 
}
\description{
Corrects data organization for model analysis. The genes are transferred along columns and samples are transferred along rows for further analysis. 
}
\usage{
FluidigmDataConversion(d, Cell = 2, Gene = 5, Value = 7)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{d}{
%%     ~~Describe \code{d} here~~
}
  \item{Cell}{
%%     ~~Describe \code{Cell} here~~
}
  \item{Gene}{
%%     ~~Describe \code{Gene} here~~
}
  \item{Value}{
%%     ~~Describe \code{Value} here~~
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
function (d, Cell = 2, Gene = 5, Value = 7) 
{
    d1 <- as.data.frame(d[, c(Cell, Gene, Value)])
    for (i in 1:length(d1[, 3])) {
        if (d1[i, 3] == 999) 
            d1[i, 3] = NA
    }
    colnames(d1) <- c("c", "g", "v")
    CellNames <- na.omit(unique(d1[, 1]))
    GeneNames <- na.omit(unique(d1[, 2]))
    dat <- as.data.frame(matrix(nrow = length(CellNames), ncol = length(GeneNames)))
    colnames(dat) <- GeneNames
    rownames(dat) <- CellNames
    for (i in 1:length(CellNames)) {
        for (j in 1:length(GeneNames)) {
            dat[i, j] <- d1[which((as.character(d1$c) == as.character(CellNames[i])) & 
                (as.character(d1$g) == as.character(GeneNames[j]))), 
                3]
        }
    }
    dat <- cbind(CellNames, dat)
    colnames(dat) <- c("CellNames", GeneNames)
    write.csv(dat, "FluidigmDataConverted.csv", quote = FALSE, 
        row.names = FALSE)
    return(NULL)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
