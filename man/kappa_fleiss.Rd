\name{kappa_fleiss}
\alias{kappa_fleiss}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Fleiss' kappa
}
\description{
Fleiss' kappa coefficient for assessing the reliability of agreement between a fixed number of raters when assigning categorical ratings.
Output is bounded between -1 and 1. A negative value corresponds to a poor agreement between raters, whereas a value of 1 correspond to a complete agreement.
This is the genralized version of the two raters, two categories original Cohen's kappa.
}
\usage{
  kappa_cohen(table)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{table}{
An agrement table.
The categories are presented in the columns, while the subjects are presented in the rows.
Each cell lists the number of raters who assigned the indicated (row) subject to the indicated (column) category.
Therefore, the sum of each row should be equal to the number of raters, and every row sums should have the same value.
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
  \code{\link{kappa_cohen}}
}
\examples{
tab22 <- matrix(data = NA, ncol = 2, nrow = 2)
tab22[, 1] <- sample(1:19, 2)
tab22[, 2] <- 20 - tab22[, 1]

tab33 <- matrix(data = NA, ncol = 3, nrow = 3)
tab33[, 1] <- sample(1:19, 3)
tab33[, 2] <- sample(1:19, 3)
tab33[, 3] <- 45 - rowSums(tab33[, 1:2])

kappa_fleiss(tab22)
kappa_fleiss(tab33)
}
