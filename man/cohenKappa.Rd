\name{cohenKappa}
\alias{cohenKappa}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Cohen's kappa
}
\description{
Compute the Cohen's kappa coefficient for two raters reliability of binary variable.
Output is bounded between -1 (complete dissagreement) and 1 (complete agreement). \cr

This only works for the specific case where there are two raters and two possible outcomes.
The Fleiss' kappa is  the generalized metric for any given number of raters and/or categories.
}
\usage{
  cohenKappa(table)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{table}{
A two by two contingency table corresponding to the inter-rater reliability table.
}
}
\details{
Kappa is a chance-corrected measure of agreement.
However, it is not clear if taking chance into consideration is reasonable in
performance analysis in all applications. Besides, as in this measure something
that may or may not is presented, is removed, and it is difficult to interpret its
value. On the other hand, Yule’s Q (cf. \code{seealso} section) is recommended as a more reliable measure
of agreement level. The Yule’s Q test is the odds ratio (OR) i.e. the odds
of agreeing compared to not agreeing. Yule’s Q is thus a test specifically for
assessing the difference between concordant and discordant responses between
two raters making dichotomous ratings. Since Yule’s Q statistic produces a
lower value when agreement levels fall below reasonable levels this may act as
a better alert in comparison with Kappa.
}
\value{
}
\references{
Zolanvari (2022) A Literature Review on Rater Agreement Metrics
}
\author{
  Hugo Marthinet
}
\seealso{
  \code{\link{yuleQ}}, \code{\link{fleissKappa}}
}
\examples{
tab22 <- matrix(data = NA, ncol = 2, nrow = 2)
tab22[, 1] <- sample(1:19, 2)
tab22[, 2] <- 20 - tab22[, 1]

cohenKappa(tab22)
}
