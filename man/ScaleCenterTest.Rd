\name{ScaleCenterTest}
\alias{ScaleCenterTest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sclae and Center data. 
}
\description{
Scaling and centering of data columns for standardization. 
}
\usage{
ScaleCenterTest(d, Mean, SD)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{d}{
%%     ~~Describe \code{d} here~~
}
  \item{Mean}{
%%     ~~Describe \code{Mean} here~~
}
  \item{SD}{
%%     ~~Describe \code{SD} here~~
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
function (d, Mean, SD) 
{
    d1 <- d[, -1]
    d2 <- (d1 - rep(1, dim(d1)[1]) \%*\% t(Mean)) * (rep(1, dim(d1)[1]) \%*\% 
        t(1/SD))
    d3 <- as.data.frame(cbind(d[, 1], d2))
    colnames(d3) <- colnames(d)
    return(d3)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
