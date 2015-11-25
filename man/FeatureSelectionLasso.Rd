\name{FeatureSelectionLasso}
\alias{FeatureSelectionLasso}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
LASSO feature selection. 
}
\description{
Feature selection using only LASSO.
}
\usage{
FeatureSelectionLasso(d)
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
    library(glmnet)
    y <- d$Status
    x <- as.matrix(d[, -c(1, dim(d)[2])])
    l <- cv.glmnet(x, y, nfolds = 10, family = "binomial")
    lambdam <- l$lambda[which(l$cvm == min(l$cvm))]
    m <- glmnet(x, y, family = "binomial", lambda = lambdam)
    m1 <- glmnet(x, y, family = "binomial")
    pdf("LASSO_Plots.pdf")
    plot(l)
    plot(m1)
    dev.off()
    c <- rownames(m$beta)[which(!(m$beta == 0))]
    return(c)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
