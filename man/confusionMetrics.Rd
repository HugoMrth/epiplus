\title{
  Metrics of a confusion matrix
}
\name{confusionMetrics}
\alias{confusionMetrics}
\description{
Compute various metrics of a confusion matrix :
  \itemize{
  \item Various measures : prevalence, type I and II errors, sensibility, specificity
  \item Positive Predictive Value (PPV) and its 95CI
  \item Negative Predictive Value (NPV) and its 95CI
  \item Odds Ratio (OR) and its 95CI and associated p-value
  \item Risk Ratio (RR) and its 95CI and associated p-value
  \item Inter-rater reliability measures (IRR) : Cohen's Kappa and Yule'sQ
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
  A list of all the metrics
}
\details{
Positive and Negative Preditive Value are calculated using two different methods :
the first one correspond to the formulas using only the TP, FP, TN and FN value, wheras the second one relies on sensibility, specifity adn prevalence. The two methods are marginally equivalent, but the second one allows for PPV and NPV calculation even if the exact contingency table isn't available (cf. https://en.wikipedia.org/wiki/Positive_and_negative_predictive_values).
}
\references{
Thurin et al. (2021) Intra-database validation of case-identifying algorithms using reconstituted electronic health records from healthcare claims data
}
\author{
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
