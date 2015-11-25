\name{PredictEnsemble}
\alias{PredictEnsemble}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Ensemble generation function. 
}
\description{
This function generates the enseble prediction of gene response. 
}
\usage{
PredictEnsemble(train, test, ImportantGenes, GeneNumbers = 7)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{train}{
%%     ~~Describe \code{train} here~~
}
  \item{test}{
%%     ~~Describe \code{test} here~~
}
  \item{ImportantGenes}{
%%     ~~Describe \code{ImportantGenes} here~~
}
  \item{GeneNumbers}{
%%     ~~Describe \code{GeneNumbers} here~~
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
function (train, test, ImportantGenes, GeneNumbers = 7) 
{
    dtrain <- train[, -1]
    dtest <- test[, -1]
    testcells <- test[, 1]
    library(randomForest)
    library(e1071)
    trcol <- colnames(dtrain)
    tecol <- colnames(dtest)
    SelectedSet <- ImportantGenes[1:GeneNumbers]
    trind <- c()
    tsind <- c()
    for (i in 1:length(SelectedSet)) {
        trind <- c(trind, which(trcol == SelectedSet[i]))
        tsind <- c(tsind, which(tecol == SelectedSet[i]))
    }
    if (!(length(trind) == length(tsind))) {
        print("Test and Train set do not have same important genes")
        return(0)
    }
    dtr <- dtrain[, c(trind, dim(dtrain)[2])]
    dts <- dtest[, tsind]
    rf <- randomForest(as.factor(Status) ~ ., ntree = 1000, data = dtr)
    prf <- predict(rf, newdata = dts, type = "prob")
    prf1 <- prf[, 2]
    sv <- svm(as.factor(Status) ~ ., data = dtr, kernel = "radial", 
        type = "C-classification", cross = 10, probability = TRUE)
    psv <- predict(sv, newdata = dts, probability = TRUE)
    prob <- attr(psv, "probabilities")
    prob1 <- prob[, 2]
    sv1 <- svm(as.factor(Status) ~ ., data = dtr, kernel = "sigmoid", 
        type = "C-classification", cross = 10, probability = TRUE)
    psv1 <- predict(sv, newdata = dts, probability = TRUE)
    prob <- attr(psv1, "probabilities")
    prob2 <- prob[, 2]
    ens.p = (prf1 + prob1 + prob2)/3
    Prob <- as.data.frame(cbind(ID = test[, 1], Prob = ens.p))
    write.csv(Prob, "Test_Probabilities.csv", quote = FALSE, 
        row.names = FALSE)
    pdf("Predicted_Probabilities_of_Single_Cells.pdf")
    plot(seq(1, length(ens.p)), ens.p, type = "n", ylim = c(0, 
        1), main = "Probability for Test Data")
    text(x = seq(1, length(ens.p), 1), y = ens.p, labels = test$ID, 
        col = ceiling(ens.p * 3), cex = 0.5)
    plot(seq(1, length(ens.p)), prf1, type = "n", ylim = c(0, 
        1), main = "Probability for Test Data (From Random Forest)")
    text(x = seq(1, length(ens.p), 1), y = prf1, labels = test$ID, 
        col = ceiling(ens.p * 3), cex = 0.5)
    plot(seq(1, length(ens.p)), prob1, type = "n", ylim = c(0, 
        1), main = "Probability for Test Data (From SVM 1)")
    text(x = seq(1, length(ens.p), 1), y = prob1, labels = test$ID, 
        col = ceiling(ens.p * 3), cex = 0.5)
    plot(seq(1, length(ens.p)), prob2, type = "n", ylim = c(0, 
        1), main = "Probability for Test Data (From SVM 2)")
    text(x = seq(1, length(ens.p), 1), y = prob2, labels = test$ID, 
        col = ceiling(ens.p * 3), cex = 0.5)
    dev.off()
    return(0)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
