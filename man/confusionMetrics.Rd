\title{
  Metrics of a confusion matrix
}
\name{confusionMetrics}
\alias{confusionMetrics}
\description{
Compute various metrics of a confusion matrix : 
  \itemize{
  \item Prevalence, type I and II errors, sensibility, specificity
  \item Positive Predictive Rate and its 95% CI
  \item Negative Predictive Rate and its 95% CI
  \item Odds Ratio and its 95% CI and associated p-value
  \item Risk Ratio and its 95% CI and associated p-value
  \item Inter-rater reliability measures : Cohen's Kappa and Yule'sQ
    }
 
}\usage{
  confusionMetrics(tab = NULL,
                x.true = NULL,
                x.test = NULL,
                n.decimal = 3)
}\arguments{
  \item{tab}{
    The confusion matrix. The top right cell is considered to be the true positive.
}
  \item{x.true}{
The true values vector. Must be either numerical (0 and 1) or boolean. This is ignored if \code{tab} is provided.
}
  \item{x.test}{
The predicted values vector. Must be either numerical (0 and 1) or boolean. This is ignored if \code{tab} is provided.
}
  \item{n.decimal}{
Number of decimals included in the output metrics, default is 3.
}
}\value{
  A list of metrics
}\author{
  Hugo Marthinet
}
\seealso{
  \code{\link{cohenKappa}}, \code{\link{yuleQ}}
}
\examples{
#Dummy matrix
(tab <- matrix(c(67, 2, 4, 7),
              nrow = 2, byrow = TRUE))
unlist(confusionMetrics(tab = tab))

#Dummy vectors
x.vrai <- c(rep(1, 71), rep(0, 9))
x.test <- c(rep(1, 67), rep(0, 11), rep(1, 2))
unlist(confusionMetrics(x.true = x.true,
               x.test = x.test))
}
