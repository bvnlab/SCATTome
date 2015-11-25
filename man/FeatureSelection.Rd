\name{FeatureSelection}
\alias{FeatureSelection}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Selects important genes.
}
\description{
Selects important genes by a combination of LASSO, Elastic Net and Random Forest Ranking of genes in terms of importance order.
}
\usage{
FeatureSelection(d)
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
    d1 <- d[, 2:dim(d)[2]]
    for (i in 1:(dim(d1)[2] - 1)) {
        if (is.factor(d1[, i])) 
            d1[, i] <- as.numeric(as.character(d[, i]))
    }
    library(randomForest)
    rf <- randomForest(as.factor(Status) ~ ., ntree = 500, data = d1, 
        replace = TRUE, na.action = na.omit)
    importanceOrder <- order(-rf$importance)
    pdf("Gene_Importance_Plot.pdf")
    varImpPlot(rf)
    dev.off()
    n <- dim(d1)[1]
    Boot = 100
    RFAcc <- as.data.frame(matrix(nrow = Boot, ncol = length(importanceOrder)))
    for (i in 1:length(importanceOrder)) {
        dt <- d1[, c(importanceOrder[1:i], dim(d1)[2])]
        for (k in 1:Boot) {
            index <- sample.int(n, size = floor(0.8 * n))
            train <- dt[index, ]
            test <- dt[-index, ]
            r1 <- randomForest(as.factor(Status) ~ ., ntree = 1000, 
                data = train)
            p1 <- predict(r1, newdata = test, type = "response")
            a1 <- acc(p1, test$Status)
            RFAcc[k, i] <- (a1[1, 1] + a1[2, 2])/(a1[1, 1] + 
                a1[1, 2] + a1[2, 1] + a1[2, 2])
        }
    }
    colnames(RFAcc) <- seq(1, length(importanceOrder), 1)
    write.csv(RFAcc, "Random_Forest_Accuracy_With_Importance_Ordering.csv", 
        quote = FALSE, row.names = FALSE)
    Mean <- apply(RFAcc, 2, FUN = mean)
    SD <- apply(RFAcc, 2, FUN = sd)
    pdf("Accuracy_Plot_With_Model_Size.pdf")
    plot(seq(1, length(Mean), 1), Mean, type = "b", ylim = c(0.8, 
        1))
    lines(seq(1, length(Mean), 1), Mean - SD, type = "l", col = 4)
    lines(seq(1, length(Mean), 1), Mean + SD, type = "l", col = 4)
    dev.off()
    return(list(Order = importanceOrder, ImportantGenes = rownames(rf$importance)[importanceOrder]))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
