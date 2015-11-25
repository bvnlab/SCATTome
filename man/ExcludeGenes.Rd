\name{ExcludeGenes}
\alias{ExcludeGenes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Excludes genes.
}
\description{
Excludes genes whose observations are unusable due to missingness or measurement error.
}
\usage{
ExcludeGenes(d, Threshold = 0.5, Type = c("Train", "Test"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{d}{
%%     ~~Describe \code{d} here~~
}
  \item{Threshold}{
%%     ~~Describe \code{Threshold} here~~
}
  \item{Type}{
%%     ~~Describe \code{Type} here~~
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
function (d, Threshold = 0.5, Type = c("Train", "Test")) 
{
    if (Type == "Train") {
        Gene <- d[, 2:(dim(d)[2] - 1)]
    }
    else {
        Gene <- d[, 2:(dim(d)[2])]
    }
    PercentMissing <- apply(Gene, 2, FUN = function(x) {
        return((length(x) - length(na.omit(x)))/length(x))
    })
    Exclusion <- which(PercentMissing >= Threshold)
    ExcludedGenes <- colnames(Gene)[Exclusion]
    if (length(ExcludedGenes) > 0) {
        Dat <- d[, -(Exclusion + 1)]
        Dd <- list(ExcludedGeneList = ExcludedGenes, Data = Dat)
        return(Dd)
    }
    else {
        Dat <- d
        Dd <- list(ExcludedGeneList = ExcludedGenes, Data = Dat)
        return(Dd)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
