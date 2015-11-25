\name{MultImpute}
\alias{MultImpute}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Multiple Imputation.	
}
\description{
Multiple imputation of missing data as per the expectation maximization algorithm. 
}
\usage{
MultImpute(d, Type = c("Train", "Test"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{d}{
%%     ~~Describe \code{d} here~~
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
function (d, Type = c("Train", "Test")) 
{
    library(Amelia)
    if (Type == "Train") {
        d2 <- d[, -c(1, dim(d)[2])]
    }
    else {
        d2 <- d[, -1]
    }
    k <- dim(d2)[2]
    for (i in 3:k) {
        dt <- d2[(i - 2):i]
        dtp <- na.omit(dt)
        if ((dim(dt)[1] - dim(dtp)[1]) > 0) {
            aout <- amelia(dt, m = 100)
            for (j in 1:100) {
                if (j == 1) 
                  dtimp <- aout$imputations[[j]]
                if (j > 1) 
                  dtimp <- dtimp + aout$imputations[[j]]
            }
            dtimp1 <- dtimp/100
            d2[(i - 2):i] <- dtimp1
        }
    }
    if (Type == "Train") {
        d3 <- as.data.frame(cbind(d[, 1], d2, d[, dim(d)[2]]))
    }
    else {
        d3 <- as.data.frame(cbind(d[, 1], d2))
    }
    colnames(d3) <- colnames(d)
    return(d3)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
