\name{cohenKappa}
\alias{cohenKappa}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Cohen's kappa
}
\description{
Compute the Cohen's kappa coefficient for inter-rater reliability of categorical variable.
Output is bounded between -1 (complete dissagreement) and 1 (complete agreement).
This only works for he specific case where there are two raters and two possible outcomes.
The Fleiss' kappa is  the generalized metric for any given number of raters and categories.
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
%%  ~~ If necessary, more details than the description above ~~
}
\value{
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
  Hugo Marthinet
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{fleissKappa}}

}
\examples{
tab22 <- matrix(data = NA, ncol = 2, nrow = 2)
tab22[, 1] <- sample(1:19, 2)
tab22[, 2] <- 20 - tab22[, 1]

cohenKappa(tab22)
}
