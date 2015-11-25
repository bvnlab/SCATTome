\name{checkData}
\alias{checkData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Checks consistence of data and corrects for missingness. Maps the missing genes and deletes genes that are unusable. 
}
\description{
Checks consistence of data and corrects for missingness. Maps the missing genes and deletes genes that are unusable. 
}
\usage{
checkData(d, Type = c("Train", "Test"))
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
    DataStatus = is.data.frame(d)
    if (DataStatus == FALSE) {
        print("Data is not a data frame. Kindly check data format")
        return(0)
    }
    ID <- d[, 1]
    ID.Status = is.character(na.omit(ID))
    if (ID.Status == FALSE) {
        print("Cell ID not valid character. Check cell ID format")
        return(0)
    }
    else {
        n1 <- length(ID) - length(na.omit(ID))
        if (n1 > 0) {
            print("Cell ID Missing. Missing cell ID not allowed.")
            return(0)
        }
        n2 <- length(ID) - length(unique(ID))
        if (n2 > 0) {
            print("Repetitive cell ID not allowed")
            return(0)
        }
    }
    if (Type == "Train") {
        Status = d[, dim(d)[2]]
        Status = na.omit(unique(Status))
        ClassCat = length(Status)
        l1 <- which(Status == 1)
        l2 <- which(Status == 0)
        if (!(ClassCat == 2)) {
            print("Two classes are required. Data does not have two classes.")
            return(0)
        }
        if ((length(l1) == 0) || (length(l2) == 0)) {
            print("Class labels need to be 0 and 1. Please check data")
            return(0)
        }
    }
    if (Type == "Train") {
        Gene <- d[, 2:(dim(d)[2] - 1)]
    }
    else {
        Gene <- d[, 2:(dim(d)[2])]
    }
    n3 <- length(colnames(Gene)) - length(unique(colnames(Gene)))
    if (n3 > 0) {
        print("Repetitive variable names not allowed")
        return(0)
    }
    for (i in 1:dim(Gene)[2]) {
        DataType <- is.numeric(na.omit(Gene[, i]))
        DataLength = length(na.omit(Gene[, i]))
        if (DataLength > 0) {
            if (DataType == FALSE) {
                print("Non-numeric data in gene expression")
                print(paste("Check the following gene: ", colnames(Gene)[i], 
                  sep = ""))
                return(0)
            }
        }
        else {
            print(paste("Warning: ", colnames(Gene)[i], " contains all NAs.", 
                sep = ""))
        }
    }
    print("Cell ID check OK")
    print("Class Status check OK")
    print("Gene expression data type check OK")
    return(1)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
