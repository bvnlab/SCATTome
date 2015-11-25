\name{ScaleCenterTrain}
\alias{ScaleCenterTrain}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Scale center train set. 
}
\description{
Scaling and centering of train set for standardization. 
}
\usage{
ScaleCenterTrain(d)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{d}{
%%     ~~Describe \code{d} here~~
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
function (d) 
{
    d1 <- d[, -c(1, dim(d)[2])]
    Mean <- apply(d1, 2, FUN = mean)
    SD <- apply(d1, 2, FUN = sd)
    d2 <- apply(d1, 2, FUN = function(x) {
        return((x - mean(x))/sd(x))
    })
    Data <- as.data.frame(cbind(d[, 1], d2, d[, dim(d)[2]]))
    colnames(Data) <- colnames(d)
    Dd <- list(Mean = Mean, SD = SD, Data = Data)
    return(Dd)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
